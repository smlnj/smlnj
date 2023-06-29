(* cm/smlfile/dbm/smlinfo.sml
 *
 * Bundling information pertaining to an SML source file.
 *   - only includes information that does not require running
 *     the machine-dependent part of the compiler
 *
 * (C) 2023 The Fellowship of SML/NJ
 *
 * Author: Matthias Blume (matthias.blume@gmail.com)
 * Edited: DBM, 2023.6
 *)

signature SMLINFO =
sig

    type info

    (* some additional arguments of the info' function
     * no external references to attribs type;
     * indirectly referred to in compile/compile.sml, bootstrap/build-initdg.sml, where
     * attribs records are constructed and passed. *)
    type attribs =
	{ is_rts: bool,
	  noguid: bool,
	  explicit_core_sym: Symbol.symbol option,
	  extra_compenv: StaticEnv.staticEnv option }

    type controller =
	 { save'restore : unit -> unit -> unit,
	   set : unit -> unit }

    (* some of the arguments of the INFO constructor and info' function,
     * not referenced externally *)
    type info_args =
	{ file: File.file,
	  group: File.file * region,
	  sh_spec: Sharing.request,
	  setup: string option * string option,
	  locl: bool,
	  controllers: controller list }

    val eq : info * info -> bool	    (* equality of file components (by stable ids) *)
    val compareInfo : info * info -> order  (* compare file components (by stable ids) *)

    (* The idea behind "newGeneration" is the following:
     * Before parsing .cm files (on behalf of CM.make/recomp or CMB.make etc.)
     * we start a new generation.  While parsing, when we encounter a new
     * SML source we re-use existing information and bump its generation
     * number to "now".  After we are done with one group we can safely
     * evict all info records for files in this group if their generation
     * is not "now".
     * Moreover, if we encounter an entry that has a different owner group,
     * we can either signal an error (if the generation is "now" which means
     * that the file was found in another group during the same parse) or
     * issue a "switched groups" warning (if the generation is older than
     * now which means that the file used to be in another group). *)
    val newGeneration : unit -> unit

(* remove export to see if there are any external calls 
    (* may not be called externally at all (no external calls found so far) *)
    val mkinfo : bool -> GeneralParams.info -> info_args -> info
*)

    (* called externally once? in bootstrap/build-initdg.sml *)
    val mkinfo' : GeneralParams.info -> attribs -> info_args -> info

    (* accessing fields of the INFO record *)
    val file : info -> File.file
    val group : info -> File.file
    val skelpath : info -> Path.path
    val binpath : info -> Path.path
    val sh_spec : info -> Sharing.request
    val sh_mode : info -> Sharing.mode
    val attribs : info -> attribs
    val lastseen : info -> TStamp.t
    val setup : info -> string option * string option
    val controllers : info -> controller list
    val is_local : info -> bool

    (* setting and getting the guid string *)
    val setguid : info * string -> unit
    val getguid : info -> (unit -> string)  (* thunk that sets guid and returns it *)

    val set_sh_mode : info * Sharing.mode -> unit

    val parsetree : GeneralParams.info -> info -> (Ast.dec * SR.source) option
    val exports : GeneralParams.info -> info  -> SymbolSet.set option
    val skeleton : GeneralParams.info -> info -> Skeleton.decl option

    val error : GeneralParams.info -> info -> ErrorMsg.complainer

    (* forget a parse tree if/when we are done with it *)
    val forgetParsetree : info -> unit

    (* Evict all elements that belong to a given group but which are not of the current
     * generation. "cleanGroup" should be called right after parsing the group file.
     * If the boolean flag ("nowStable") is set to true, then all members of the group are
     * dismissed regardless of their generation. This is used to delete infos for members
     * of now-stable libraries. *)
    val cleanGroup : bool -> File.file -> unit

    (* See if a given info (sml source file) is still known *)
    val isKnown : info -> bool

    (* Delete all known info. *)
    val reset : unit -> unit

    val errorLocation : GeneralParams.info -> info -> string

end (* signature SMLINFO *)


structure SmlInfo :> SMLINFO =
struct

local  (* top local *)

  structure S  = Symbol
  structure SR = Source
  structure SM = SourceMap
  structure PPSM = PPSourceMap
  structure EM = ErrorMsg
  structure SE = StaticEnv

  structure GP = GeneralParams
  structure F = File
  structure P = Path
  structure SF = SmlFile
  structure SH = Sharing
  structure FNP = FilenamePolicy
  structure SKC = SkelCvt

in (* top local *)

  type attribs = { is_rts: bool,
		   noguid: bool,
		   explicit_core_sym: S.symbol option,
		   extra_compenv: SE.staticEnv option }

  type controller =
       { save'restore : unit -> unit -> unit,
	 set : unit -> unit }

  (* a subset of the INFO constructor arguments *)
  type info_args = { file: F.file,
		     group: F.file * SM.region,
		     sh_spec: SH.request,
		     setup: string option * string option,
		     locl: bool,
		     controllers: controller list }

  type generation = unit ref

  (* [DBM] How do the persinfo attributes of an info differ from the direct info attributes (fields)?
   * are they more ephemeral, changeable? They are all ref values, except for setguid and getguid, which
   * deal with the "changeable" guid attribute.
   * This would not appear to apply to the "group" to which the file belongs, so group has been
   * moved from persinfo to info. *)
  (* [BLUME] sh_mode is an elaboration of sh_spec;  it must be persistent
   * and gets properly re-computed when there is a new sh_spec *)
  datatype persinfo =
      PERS of { generation: generation ref,
		lastseen: TStamp.t ref,
		parsetree: (Ast.dec * source) option ref,
		skeleton: Skeleton.decl option ref,
		sh_mode: SH.mode ref,
		setguid: string -> unit,
		getguid: unit -> string }  (* why does guid/getguid need to be thunkified? *)
		(* is it because a guid file may be written "later" (at a different (TStamp.t) time?)
                 * so that the time it is created affects the content of the guid string/file. *)

  (* If flattened, INFO has 19 arguments!  Why so many?
   * Three components were thunkified (skelname, binname, guid).  *)
  datatype info =
      INFO of {file: F.file,       (* was "sourcepath", now file *)
	       group: F.file * SM.region,  (* group file and region of "member" line for this file  *)
	       binpath: P.path,    (* unthunkified, path; derived from file.path through "policy" *)
	       skelpath: P.path,   (* unthunkified, path; derived from file.path through "policy" *)
	       sh_spec: SH.request, (* in only known call of info': DONTCARE *)
	       setup: string option * string option,  (* in only known call of info': (NONE, NONE) *)
	       locl:  bool,                           (* in only known call of info': false *)
	       controllers: controller list,  (* only one is control for enabling "overload" decls? *)
	       attribs: attribs,
	       persinfo: persinfo}


  local (* generation *)
      val generation : unit ref ref = ref (ref ())
  in

     (* now : unit -> generation *)
     fun now () = !generation

     (* newGeneration : unit -> unit *)
     fun newGeneration () = generation := ref ()

  end (* generation *)

(* info field selection functions *)

  (* file : info -> F.file *)
  fun file (INFO { file, ... }) = file
  (* skelpath : info -> P.path *)
  fun skelpath (INFO { skelpath, ... }) = skelpath
  (* binpath : info -> P.path *)
  fun binpath (INFO { binpath, ... }) = binpath
  (* sh_spec : info -> SH.request *)
  fun sh_spec (INFO { sh_spec, ... }) = sh_spec
  (* sh_mode : info -> SH.mode *)
  fun sh_mode (INFO { persinfo = PERS { sh_mode, ... }, ... }) = !sh_mode (* pattern exhaustive *)
  (* set_sh_mode : info -> unit *)
  fun set_sh_mode (INFO { persinfo = PERS { sh_mode, ... }, ... }, mode) =
      sh_mode := mode
  (* attribs : info -> attribs *)
  fun attribs (INFO { attribs, ... }) = attribs
  (* setup : info -> string option * string option *)
  fun setup (INFO { setup, ... }) = setup
  (* controllers : info -> controller list *)
  fun controllers (INFO { controllers, ... }) = controllers
  (* is_local : info -> bool *)
  fun is_local (INFO { locl, ... }) = locl
  (* group : info -> F.file *)
  fun group (INFO { group = (file, _), ... }) = file
  (* lastseen : info -> TStamp.t *)
  fun lastseen (INFO { persinfo = PERS { lastseen, ... }, ... }) = !lastseen
  (* filepath : info -> string *)
  fun path (INFO {file, ...}) = F.path file

(* error functions *)

  (* gerror : GP.info -> F.file * SM.region -> EM.complainer *)
  (* gpi argument is used to access the groupreg (for the current? group?) *)
  fun gerror (gpi: GP.info) = GroupReg.error (#groupreg gpi)

  (* error : GP.info -> info -> SM.region -> EM.complainer *)
  fun error (gpi: GP.info) (INFO { persinfo = PERS { group, ... }, ... }) =
      gerror gpi group


  (* compareInfo : info * info -> order *)
  (* used to defined finite sets of info and maps over info (SmlInfoSet, SmlInfoMap) *)
  fun compareInfo (INFO { file = f1, ... }, INFO { file = f2, ... }) =
      F.compareFile (f1, f2)

  (* eq : info * info -> bool *)
  fun eq (info1, info2) =
      (case compareInfo (info1, info2)
	 of EQUAL => true
	  | _ => false)


  (* knownInfo: a fixed ref containing a file to persinfo mapping *)
  val knownInfo : persinfo FileMap.map ref = ref (FileMap.empty: persinfo FileMap.map)

  (* isKnown : info -> bool *)
  fun isKnown (INFO { file, ... }) =
      isSome (FileMap.find (!knownInfo, file))

  (* countParseTrees : unit -> int *)
  fun countParseTrees () =
      let fun inc (PERS { parsetree = ref (SOME _), ... }, i) = i + 1
	    | inc (_, i) = i
       in FileMap.foldl inc 0 (!knownInfo)
      end

  (* forgetParsetree : info -> unit *)
  fun forgetParsetree (INFO { persinfo = PERS { parsetree, ... }, ... }) =
      parsetree := NONE

  (* cleanGroup : bool -> F.file -> unit *)
  fun cleanGroup (nowStable: bool) (file: F.file) =
      let val this_gen = now ()
	  fun isCurrent (PERS { generation = ref gen, group = (file', _), ... }) =
	      ((not nowStable) andalso gen = this_gen)
	      orelse
	      (case F.compare (file, file')
		 of EQUAL => false
		  | _ => true)
       in knownInfo := FileMap.filter isCurrent (!knownInfo)
      end

  (* reset : unit -> unit *)
  (* reset the knownInfo map ref to empty *)
  fun reset () = knownInfo := FileMap.empty

  (* validate : F.file * persinfo -> unit *)
  (* check timestamp and throw away any invalid cache *)
  fun validate (file: F.file,
		PERS { lastseen, generation, parsetree, skeleton, ...})
      let val ts = !lastseen
	  val nts = F.tstamp file
       in if TStamp.needsUpdate { source = nts, target = ts }
	  then (lastseen := nts;
	        generation := now ();
	        parsetree := NONE;
	        skeleton := NONE)
	  else ()
      end

  (* info' : GP.info -> attribs -> info_args -> info *)
  (* called once externally in bootstrap/build-initdg.sml *)
  fun info' (gp: GP.info)
	    (attribs: attribs)
	    ({file, group = gr as (group, region), sh_spec, setup, locl, controllers} : info_args) =
      let val policy = #fnpolicy (#param gp)
	  val groupreg = #groupreg gp

	  val path : P.path = F.path file
	  fun binpath : P.path = FNP.mkBinName policy path
	  val skelpath : P.path = FNP.mkSkelName policy path
	  fun guidpath : P.path = FNP.mkGUidName policy path

	  val (getguid, setguid) =
	      if #noguid attribs
	      then (fn () => "", fn _ => ())
	      else let val guid_cache : string option ref = ref NONE
		       val guidFpath : string = P.pathToFpath guidpath

		       (* frombin : unit -> string option *)
		       fun frombin () =
			     SafeIO.perform { openIt =
						fn () => BinIO.openIn (P.pathToFpath binpath),
					      closeIt = BinIO.closeIn,
					      work = SOME o Binfile.readGUid,
					      cleanup = fn _ => () }
			     handle IO.Io _ => NONE

		       (* fromGuidFile : unit -> string option *)
		       fun fromGuidFile () =
			     SafeIO.perform { openIt = fn () => TextIO.openIn guidFpath,
					      closeIt = TextIO.closeIn,
					      work = SOME o TextIO.inputAll,
					      cleanup = fn _ => () }
			     handle IO.Io _ => NONE

		       (* tofile : string -> unit *)
		       fun tofile (guid: string) =
			     SafeIO.perform {openIt = fn () => AutoDir.openTextOut guidFpath,
					     closeIt = TextIO.closeOut,
					     work = fn s => TextIO.output (s, guid),
					     cleanup = fn _ => OS.FileSys.remove guidFpath }

		       (* setguid : string -> unit *)
		       fun setguid (guid: string) = (tofile guid; guid_cache := SOME guid)

		       (* getguid : unit -> string *)
		       fun getguid () =
			     (case !guid_cache
			        of SOME guid => guid
				 | NONE =>
				     (case frombin ()
				        of SOME guid => (setguid guid; guid)
					 | NONE =>
					     (case fromGuidFile ()
						of SOME guid => guid
						 | NONE => 
						     let val guid =
							     concat ["guid-", P.pathToFpath path, "-",
								     Time.toString (Time.now ()), "\n"]
						      in setguid guid;
							 guid
						     end)))

		    in (getguid, setguid)
		   end

	  (* newpersinfo : unit -> persinfo *)
	  fun newpersinfo () =
	      let val ts = File.tstamp file
		  val pi = PERS {group = gr, lastseen = ref ts,
				 parsetree = ref NONE,
				 skeleton = ref NONE,
				 sh_mode = ref (Sharing.SHARE false),
				 generation = ref (now ()),
				 setguid = setguid,
				 getguid = getguid}

	       in knownInfo := FileMap.insert (!knownInfo, file, pi);
		  pi
	      end

	  val persinfo : persinfo =
	      case FileMap.find (!knownInfo, file)
	        of NONE => newpersinfo ()
		 | SOME (pi as PERS { group = gr' as (g, r), generation, ... }) =>
		     (case File.compareFile (group, g)
			of EQUAL => (validate (file, pi); pi)
		         | _ => 
			    let val n = fileToFpath file
			     in if !generation = now ()
			        then (gerror gp gr EM.COMPLAIN
					    (concat ["ML source file ", n,
						     " appears in more than one group"])
					    EM.nullErrorBody;
				      gerror gp gr' EM.COMPLAIN
					     (concat ["(previous occurence of ", n, ")"])
					     EM.nullErrorBody)
				else gerror gp gr EM.WARN
					    (concat ["ML source file ", n,
						     " has switched groups"])
					    EM.nullErrorBody;
				newpersinfo ()
			    end

   in INFO {file = file,
	    skelpath = skelpath,
	    binpath = binpath,
	    persinfo = persinfo,
	    sh_spec = sh_spec,
	    attribs = attribs,
	    setup = setup,
	    locl = locl,
	    controllers = controllers}
  end (* fun info' *)

  (* info : bool -> GP.info -> info_args -> info *)
  fun info (gp: GP.info) (noguid: bool) =
      info' gp 
	    ({extra_compenv = NONE,
	      is_rts = false,
	      noguid = noguid,
	      explicit_core_sym = NONE} : attribs)

  (* the following functions are only concerned with getting the data,
   * not with checking time stamps *)
  fun getParseTree gp (i as INFO {file, persinfo = PERS { parsetree, ... },
				  controllers, ... },
		       quiet) =
      let fun err msg = error gp i EM.COMPLAIN msg EM.nullErrorBody
	  val file_path = P.pathToFpath (path i)
       in case !parsetree
	    of SOME pt => SOME pt
	     | NONE => 
		 let val orig_settings =
			 map (fn c => #save'restore c ()) controllers
		     fun work stream =
			 let val _ = if quiet
				     then ()
				     else Say.vsay ["[parsing ", file_path, "]\n"]
			     val source =
				 SR.newSource (file_path, stream, false)
			  in app (fn c => #set c ()) controllers;
			     (SF.parse source, source)
			     before app (fn r => r ()) orig_settings
			 end

		     fun openIt () = TextIO.openIn (Path.pathToFpath (F.path file)
		     fun cleanup _ = app (fn r => r ()) orig_settings
		     val pto = SOME (SafeIO.perform { openIt = openIt,
						      closeIt = TextIO.closeIn,
						      work = work,
						      cleanup = cleanup })
	      (* Counting the trees explicitly may be a bit slow,
	       * but maintaining an accurate count is difficult, so
	       * this method should be robust.  (I don't think that
	       * the overhead of counting will make a noticeable
	       * difference.) *)
		     val ntrees = countParseTrees ()
		     val treelimit = #get StdConfig.parse_caching ()
		  in if ntrees < treelimit
		     then parsetree := pto
		     else ();
		     pto
		 end
		 handle exn as IO.Io _ => (err (General.exnMessage exn); NONE)
		      | CompileExn.Compile msg => (err msg; NONE)
      end (* fun getParseTree *)

  fun skeleton gp (i as INFO { file, skelname, persinfo = PERS { skeleton, lastseen, ... }, ... }) =
	(case !skeleton
	   of SOME sk => SOME sk
	    | NONE =>
		(case SkelIO.read (skelname, !lastseen)
		   of SOME sk => (skeleton := SOME sk; SOME sk)
		    | NONE =>
			(case getParseTree gp (i, false)
			   of SOME (tree, source) =>
				   let fun err sv region s =
					   EM.error source region sv s EM.nullErrorBody
				       val { skeleton = sk, complain } =
					   SKC.convert { tree = tree, err = err }
				    in complain ();
				       if EM.anyErrors (EM.errors source)
				       then error gp i EM.COMPLAIN "error(s) in ML source file"
						  EM.nullErrorBody
				       else (SkelIO.write (skelname, sk, !lastseen);
					     skeleton := SOME sk);
				       SOME sk
				   end
			    | NONE => NONE))


  (* we only complain at the time of getting the exports *)
  fun exports gp i = Option.map SkelExports.exports (skeleton gp i)

  fun parsetree gp i = getParseTree gp (i, true)

  (* errorLocation : GP.info -> info -> string *)
  fun errorLocation (gp: GP.info) (INFO i) =
      let val { persinfo = PERS { group = (group, region), ... }, ... } = i
	  val source : SR.source = GroupReg.lookup (#groupreg gp) group
       in case SM.sourceRegion (source, region)
	    of SOME sourceRegion => SM.sourceRegionToString sourceRegion
	     | NONE => #fileOpened source
      end

  fun guid (INFO { persinfo = PERS { guid, ... }, ... }) = guid ()

  fun setguid (INFO { persinfo = PERS { setguid, ... }, ... }, g) = setguid g

end (* top local *)
end (* structure SmlInfo :> SMLINFO *)

(* NOTES

1. We are attempting to "merge" the notions of "Group" and "Library", eliminating Groups
in favor of Libraries.  How far can we get by just assuming that Group actually means Library?
I.e. where does the functionality for groups and libraries diverge?

2. There are lots of places where smlinfo.sml has to changed to agree with the new treatment
of paths (SrcPath -> Path, File, AnchorEnv, etc.)

3. Deleted "ord_key" type. Just build ad hoc arg structure for RedBlackXXX applications to SmlInfo
(in smlinfomap.sml, smlinfoset.sml).

4. [Q] The getguid field of persinfo is a thunk, to be called in the "future". This sets the guid
string, including a current time.  Does it matter when the guid is generated?  Does it need to be
delayed?  If so, when is the right time to force this thunk and generate the guid?  I assume that
the thunk will only be forced once!

5. moved "group" field from persinfo to info (a "static" property of a file)

*)
