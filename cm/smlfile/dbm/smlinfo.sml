(*
 * Bundling information pertaining to one SML source file.
 *   - only includes information that does not require running
 *     the machine-dependent part of the compiler
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
signature SMLINFO = sig

    type info
    type ord_key = info

    type complainer = ErrorMsg.complainer
    type ast = Ast.dec
    type region = SourceMap.region
    type source = Source.source

    type attribs =
	{ is_rts: bool,
	  noguid: bool,
	  explicit_core_sym: Symbol.symbol option,
	  extra_compenv: StaticEnv.staticEnv option }

    type controller =
	 { save'restore : unit -> unit -> unit,
	   set : unit -> unit }

    type info_args =
	{ file: File.file,
	  group: File.file * region,
	  sh_spec: Sharing.request,
	  setup: string option * string option,
	  locl: bool,
	  controllers: controller list }

    val eq : info * info -> bool	(* compares files (by stable ids) *)
    val compare : info * info -> order	(* compares files (by stable ids) *)

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

    val info : bool -> GeneralParams.info -> info_args -> info

    val info' : attribs -> GeneralParams.info -> info_args -> info

    val file : info -> File.file
    val skelname : info -> string
    val binname : info -> string
    val group : info -> File.file
    val error : GeneralParams.info -> info -> complainer

    val parsetree : GeneralParams.info -> info -> (ast * source) option
    val exports : GeneralParams.info -> info  -> SymbolSet.set option
    val skeleton : GeneralParams.info -> info -> Skeleton.decl option
    val sh_spec : info -> Sharing.request
    val set_sh_mode : info * Sharing.mode -> unit
    val sh_mode : info -> Sharing.mode
    val attribs : info -> attribs
    val lastseen : info -> TStamp.t
    val setup : info -> string option * string option
    val controllers : info -> controller list
    val is_local : info -> bool
    val setguid : info * string -> unit
    val guid : info -> string

    (* forget a parse tree that we are done with *)
    val forgetParsetree : info -> unit

    (* Evict all elements that belong to a given group but which
     * are not of the current generation. "cleanGroup" should be
     * called right after finishing to parse the group file.
     * If the boolean flag ("nowStable") is set to true, then all
     * members of the group are dismissed regardless of their
     * generation. This is used to get rid of the information for
     * members of now-stable libraries. *)
    val cleanGroup : bool -> File.file -> unit

    (* See if a given piece of info is (still) known here: *)
    val isKnown : info -> bool

    (* Delete all known info. *)
    val reset : unit -> unit

    (* different ways of describing an sml file using group and source *)
    val descr : info -> string		(* sname *)

    val errorLocation : GeneralParams.info -> info -> string
end

structure SmlInfo :> SMLINFO = struct

    structure SR = Source
    structure SM = SourceMap
    structure PPSM = PPSourceMap
    structure SF = SmlFile
    structure EM = ErrorMsg
    structure FNP = FilenamePolicy

    type source = SR.source
    type ast = Ast.dec
    type region = SM.region

    type complainer = EM.complainer

    type attribs = { is_rts: bool,
		     noguid: bool,
		     explicit_core_sym: Symbol.symbol option,
		     extra_compenv: StaticEnv.staticEnv option }

    type controller =
	 { save'restore : unit -> unit -> unit,
	   set : unit -> unit }

    type info_args = { file: File.file,
		       group: File.file * region,
		       sh_spec: Sharing.request,
		       setup: string option * string option,
		       locl: bool,
		       controllers: controller list }

    type generation = unit ref

    (* sh_mode is an elaboration of sh_spec;  it must be persistent
     * and gets properly re-computed when there is a new sh_spec *)
    datatype persinfo =
	PERS of { group: File.file * region,
		  generation: generation ref,
		  lastseen: TStamp.t ref,
		  parsetree: (ast * source) option ref,
		  skeleton: Skeleton.decl option ref,
		  sh_mode: Sharing.mode ref,
		  setguid: string -> unit,
		  guid: unit -> string }

    datatype info =
	INFO of { file: File.file,
		  skelname: string,   (* unthunkified *)
		  binname: string,    (* unthunkified *)
		  persinfo: persinfo,
		  sh_spec: Sharing.request,
		  attribs: attribs,
		  setup: string option * string option,
		  locl:  bool,
		  controllers: controller list }

    type ord_key = info

    local
	val generation : unit ref ref = ref (ref ())
    in
	fun now () = !generation
	fun newGeneration () = generation := ref ()
    end

    fun file (INFO { file, ... }) = file
    fun skelname (INFO { skelname, ... }) = skelname
    fun binname (INFO { binname, ... }) = binname
    fun sh_spec (INFO { sh_spec = s, ... }) = s
    fun sh_mode (INFO { persinfo = PERS { sh_mode = ref m, ... }, ... }) = m
    fun set_sh_mode (INFO { persinfo = PERS { sh_mode, ... }, ... }, m) =
	sh_mode := m
    fun attribs (INFO { attribs = a, ... }) = a
    fun setup (INFO { setup = s, ... }) = s
    fun controllers (INFO { controllers = c, ... }) = c
    fun is_local (INFO { locl, ... }) = locl

    fun gerror (gp: GeneralParams.info) = GroupReg.error (#groupreg gp)

    fun error gp (INFO { persinfo = PERS { group, ... }, ... }) =
	gerror gp group

    fun group (INFO { persinfo = PERS { group = (g, _), ... }, ... }) = g

    fun compare (INFO { file = f1, ... }, INFO { file = f2, ... }) =
	File.compare (f1, f2)
    fun eq (i, i') = compare (i, i') = EQUAL

    fun lastseen (INFO { persinfo = PERS { lastseen, ... }, ... }) =
	!lastseen

    val knownInfo = ref (FileMap.empty: persinfo FileMap.map)

    fun isKnown (INFO { file, ... }) =
	isSome (FileMap.find (!knownInfo, file))

    fun countParseTrees () =
	let fun inc (PERS { parsetree = ref (SOME _), ... }, i) = i + 1
	      | inc (_, i) = i
	 in FileMap.foldl inc 0 (!knownInfo)
	end

    fun forgetParsetree (INFO { persinfo = PERS { parsetree, ... }, ... }) =
	parsetree := NONE

    fun cleanGroup nowStable (g: File.file) =
	let val n = now ()
	    fun isCurrent (PERS { generation = ref gen, group = (g', _), ... }) =
		((not nowStable) andalso gen = n)
		orelse
		(case File.compare (g, g') of EQUAL => false | _ => true)
	 in knownInfo := FileMap.filter isCurrent (!knownInfo)
	end

    fun reset () = knownInfo := FileMap.empty

    (* check timestamp and throw away any invalid cache *)
    fun validate (file: File.file, PERS pir) =
	let (* don't use "..." pattern to have the compiler catch later
	     * additions to the type! *)
	    val { group, lastseen, parsetree, skeleton,
		  sh_mode, generation, guid, setguid } = pir
	    val ts = !lastseen
	    val nts = File.tstamp file
	in if TStamp.needsUpdate { source = nts, target = ts }
	   then
	     (lastseen := nts;
	      generation := now ();
	      parsetree := NONE;
	      skeleton := NONE)
	   else ()
	end

    fun info' attribs (gp: GeneralParams.info) arg =
	let val { file, group = gr as (group, region), sh_spec, setup,
		  locl, controllers }
		= arg
	    val policy = #fnpolicy (#param gp)
	    fun mkSkelname () = FNP.mkSkelName policy file
	    fun mkBinname () = FNP.mkBinName policy file
	    fun mkguidname () = FNP.mkGUidName policy file
	    val groupreg = #groupreg gp

	    val (getguid, setguid) =
		if #noguid attribs
		then (fn () => "", fn _ => ())
		else
		  let val guid_cache = ref NONE
		      fun frombin () =
			  SafeIO.perform { openIt =
					     fn () => BinIO.openIn (mkBinname ()),
					   closeIt = BinIO.closeIn,
					   work = SOME o Binfile.readGUid,
					   cleanup = fn _ => () }
			  handle IO.Io _ => NONE
		      fun fromfile () =
			  SafeIO.perform { openIt =
					     fn () => TextIO.openIn (mkguidname ()),
					   closeIt = TextIO.closeIn,
					   work = SOME o TextIO.inputAll,
					   cleanup = fn _ => () }
			  handle IO.Io _ => NONE
		      fun tofile g = let
			  val gf = mkguidname ()
		      in
			  SafeIO.perform { openIt = fn () => AutoDir.openTextOut gf,
					   closeIt = TextIO.closeOut,
					   work = fn s => TextIO.output (s, g),
					   cleanup = fn _ => OS.FileSys.remove gf }
		      end
		      fun setguid g = (guid_cache := SOME g; tofile g)
		      fun saveguid g = (setguid g; g)
		      fun getguid () = let
			  fun newguid () =
			      concat ["guid-", SrcPath.descr sourcepath, "-",
				      Time.toString (Time.now ()), "\n"]
		      in
			  case !guid_cache of
			      SOME g => g
			    | NONE =>
			      (case frombin () of
				   SOME g => saveguid g
				 | NONE =>
				   (case fromfile () of
					SOME g => g
				      | NONE => saveguid (newguid ())))
		      end
		   in (getguid, setguid)
		  end

	    fun newpersinfo () =
		let val ts = SrcPath.tstamp file
		    val pi = PERS { group = gr, lastseen = ref ts,
				    parsetree = ref NONE, skeleton = ref NONE,
				    sh_mode = ref (Sharing.SHARE false),
				    generation = ref (now ()),
				    setguid = setguid,
				    guid = getguid }
		 in knownInfo := FileMap.insert (!knownInfo, file, pi);
		    pi
		end

	    fun persinfo () =
		case FileMap.find (!knownInfo, file)
		  of NONE => newpersinfo ()
		   | SOME (pi as PERS { group = gr' as (g, r), generation, ... }) =>
			if SrcPath.compare (group, g) <> EQUAL
			then let val n = SrcPath.descr file
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
			else (validate (file, pi); pi)

	 in INFO {file = file,
		  mkSkelname = mkSkelname,
		  mkBinname = mkBinname,
		  persinfo = persinfo (),
		  sh_spec = sh_spec,
		  attribs = attribs,
		  setup = setup,
		  locl = locl,
		  controllers = controllers}
	end (* fun info' *)

    fun info noguid =
	info' { extra_compenv = NONE,
		is_rts = false, noguid = noguid,
		explicit_core_sym = NONE }

    (* the following functions are only concerned with getting the data,
     * not with checking time stamps *)
    fun getParseTree gp (i as INFO ir, quiet) =
	let val { file, persinfo = PERS { parsetree, ... },
		  controllers, ... } = ir
	    fun err m = error gp i EM.COMPLAIN m EM.nullErrorBody
	 in case !parsetree
	      of SOME pt => SOME pt
	       | NONE => 
		   let val orig_settings =
			   map (fn c => #save'restore c ()) controllers
		       fun work stream =
			   let val _ = if quiet then ()
				       else Say.vsay ["[parsing ",
						      SrcPath.descr sourcepath, "]\n"]
			       val source =
				   SR.newSource (SrcPath.osstring' sourcepath, stream, false)
			    in app (fn c => #set c ()) controllers;
			       (SF.parse source, source)
			       before app (fn r => r ()) orig_settings
			   end
			       
		       fun openIt () = TextIO.openIn (SrcPath.osstring sourcepath)
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

    fun skeleton gp (i as INFO ir) =
	let val { file, mkSkelname, persinfo = PERS pir, ... } = ir
	    val { skeleton, lastseen, ... } = pir
	 in case !skeleton
	      of SOME sk => SOME sk
	       | NONE =>
		   let val skelname = mkSkelname ()
		    in case SkelIO.read (skelname, !lastseen)
		         of SOME sk => (skeleton := SOME sk; SOME sk)
			  | NONE =>
			     (case getParseTree gp (i, false)
				of SOME (tree, source) =>
				     let fun err sv region s =
					     EM.error source region sv s EM.nullErrorBody
					 val { skeleton = sk, complain } =
					     SkelCvt.convert { tree = tree, err = err }
				      in complain ();
					 if EM.anyErrors (EM.errors source)
					 then error gp i EM.COMPLAIN "error(s) in ML source file"
						    EM.nullErrorBody
					 else (SkelIO.write (skelname, sk, !lastseen);
					       skeleton := SOME sk);
					 SOME sk
				     end
				 | NONE => NONE)
		   end
	end (* fun skeleton *)

    (* we only complain at the time of getting the exports *)
    fun exports gp i = Option.map SkelExports.exports (skeleton gp i)

    fun parsetree gp i = getParseTree gp (i, true)

    fun descr (INFO { file, ... }) = SrcPath.descr file

    (* errorLocation : GeneralParams.info -> info -> string *)
    fun errorLocation (gp: GeneralParams.info) (INFO i) =
	let val { persinfo = PERS { group = (group, region), ... }, ... } = i
	    val source : SR.source = GroupReg.lookup (#groupreg gp) group
	 in case SM.sourceRegion (source, region)
	      of SOME sourceRegion => SM.sourceRegionToString sourceRegion
	       | NONE => #fileOpened source
	end

    fun guid (INFO { persinfo = PERS { guid = g, ... }, ... }) = g ()

    fun setguid (INFO { persinfo = PERS { setguid = sg, ... }, ... }, g) = sg g
end
