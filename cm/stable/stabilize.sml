(* stabilize.sml
 *
 * COPYRIGHT (c) 2021 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Reading, generating, and writing stable libraries.
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)

local
    structure DG = DependencyGraph
    structure GG = GroupGraph
    structure EM = ErrorMsg

    structure PP = PrettyPrint
    structure SM = SourceMap
    structure GP = GeneralParams
    structure Pid = PersStamps
    structure P = PickMod
    structure UP = UnpickMod
    structure MI = ModuleId
in

signature STABILIZE = sig

    val libStampIsValid : GP.info
	-> (SrcPath.file * DG.sbnode list * GG.subgrouplist) * Version.t option
	-> bool

    type groupgetter =
	 GP.info * SrcPath.file * Version.t option * SrcPath.rebindings ->
	 GG.group option

    val loadStable :
	{ getGroup: groupgetter, anyerrors: bool ref } -> groupgetter

    val stabilize : GP.info -> { group: GG.group, anyerrors: bool ref,
				 rebindings: SrcPath.rebindings } ->
		    GG.group option
end

functor StabilizeFn (val arch : string
		     structure StabModmap : STAB_MODMAP
		     val recomp : GP.info -> GG.group ->
			 (SmlInfo.info ->
			  { contents: Binfile.bfContents,
			    stats: Binfile.stats }) option
		     val getII : SmlInfo.info -> IInfo.info) :> STABILIZE =
struct
    type groupgetter =
	 GP.info * SrcPath.file * Version.t option * SrcPath.rebindings ->
	 GG.group option

    structure BF = Binfile

    structure SSMap = MapFn
	(struct
	     type ord_key = SymbolSet.set
	     val compare = SymbolSet.compare
	end)

    structure PU = PickleUtil
    structure UU = UnpickleUtil

    val libstamp_nbytes = 16

    type map = { ss: PU.id SSMap.map, sn: PU.id SmlInfoMap.map, pm: P.map }

    val emptyMap : map =
	{ ss = SSMap.empty, sn = SmlInfoMap.empty, pm = P.emptyMap }

    val lifter =
	{ extract = fn (m: map) => #pm m,
	  patchback = fn (m: map, pm) => { ss = #ss m, sn = #sn m, pm = pm } }

    infix 3 $

    (* type info *)
    val (BN, SN, SBN, SS, SI, FSBN, IMPEXP, SHM, G, AP,
	 PRIM, EXPORTS, PRIV, VERSION, SG, RB) =
	(1001, 1002, 1003, 1004, 1005, 1006, 1007, 1008, 1009, 1010,
	 1011, 1012, 1013, 1014, 1015, 1016)

    val SSs =
	{ find = fn (m: map, k) => SSMap.find (#ss m, k),
	  insert = fn ({ ss, sn, pm }, k, v) =>
	               { sn = sn, ss = SSMap.insert (ss, k, v), pm = pm } }
    val SNs =
	{ find = fn (m: map, DG.SNODE k) => SmlInfoMap.find (#sn m,#smlinfo k),
	  insert = fn ({ ss, sn, pm }, DG.SNODE k, v) =>
		      { ss = ss,
			sn = SmlInfoMap.insert (sn, #smlinfo k, v),
			pm = pm } }

    fun fetch_pickle s = let
	fun bytesIn n = let
	    val bv = BinIO.inputN (s, n)
            in
              if n = Word8Vector.length bv then bv
              else (
                TextIO.output (TextIO.stdErr, concat [
                    "## bytesIn: wanted ", Int.toString n, ", but got ",
                    Int.toString (Word8Vector.length bv), "\n"
                  ]);
                raise UU.Format)
            end
	val libstamp = bytesIn libstamp_nbytes	(* ignored *)
	val dg_sz = LargeWord.toIntX (PackWord32Big.subVec (bytesIn 4, 0))
	val dg_pickle = Byte.bytesToString (bytesIn dg_sz)
        in
          { size = dg_sz, pickle = dg_pickle }
        end

    fun mkPickleFetcher mksname () =
	SafeIO.perform { openIt = BinIO.openIn o mksname,
			 closeIt = BinIO.closeIn,
			 work = #pickle o fetch_pickle,
			 cleanup = fn _ => () }

    fun mkInverseMap sublibs = let
	(* Here we build a mapping that maps each BNODE to the
	 * position of its exporting sub-library and a representative
	 * symbol that can be used to find the BNODE within the
	 * exports of that library. *)
	fun oneB i (sy, (nth, _, _), m) =
	    case nth () of
		(_, DG.SB_BNODE (DG.BNODE n, _, _)) =>
		(* we blindly override existing info for the same bnode;
		 * this means that the last guy wins... *)
		StableMap.insert (m, #bininfo n, (i, sy))
	      | _ => m
	(* ... but we want the first guy to win, so we do foldr
	 * and count from the top. *)
	fun oneSL (g as GG.GROUP { exports, ... }, (m, i)) =
	    (SymbolMap.foldli (oneB i) m exports, i - 1)
	  | oneSL (_, (m, i)) = (m, i - 1)
	fun oneSL' ((_, gth, _), a) = oneSL (gth (), a)
	val (im, _) =
	    foldr oneSL' (StableMap.empty, length sublibs - 1) sublibs
	fun look i =
	    case StableMap.find (im, i) of
		SOME p => p
	      | NONE => EM.impossible "stabilize: bad inverse map"
    in
	look
    end

    (* A stamp for a library is created by "pickling" the dependency graph
     * of the library in a cursory fashion, thereby recording the ii pids
     * of external references.  The so-created pickle string is never used
     * for unpickling.  Instead, it is hashed and recorded as part of
     * the regular library file.  In paranoia mode CM checks if the recorded
     * hash is identical to the one that _would_ be created if one were
     * to re-build the library now. *)
    fun libStampOf (grouppath, export_nodes, sublibs) = let
	fun sbn_cmp (DG.SB_BNODE (DG.BNODE n, _, _),
		     DG.SB_BNODE (DG.BNODE n', _, _)) =
	    BinInfo.compare (#bininfo n, #bininfo n')
	  | sbn_cmp (DG.SB_BNODE _, DG.SB_SNODE _) = GREATER
	  | sbn_cmp (DG.SB_SNODE _, DG.SB_BNODE _) = LESS
	  | sbn_cmp (DG.SB_SNODE (DG.SNODE n), DG.SB_SNODE (DG.SNODE n')) =
	    SmlInfo.compare (#smlinfo n, #smlinfo n')

	(* To deal with the init group (where export nodes come in
	 * in an ad-hoc order not derived from the export map),
	 * we first sort the list of export nodes, thereby getting rid
	 * of duplicates.  This should normally canonicalize the list.
	 * However, the resulting order is unfortunately not persistent.
	 * Most of the time this should not matter, though. *)
	val export_nodes = ListMergeSort.uniqueSort sbn_cmp export_nodes

	val inverseMap = mkInverseMap sublibs

	val pid = PickleSymPid.w_pid
	val share = PU.ah_share
	val symbol = PickleSymPid.w_symbol
	val string = PU.w_string
	val list = PU.w_list
	val int = PU.w_int

	fun abspath p = let
	    val op $ = PU.$ AP
	    val ll = SrcPath.pickle { warn = fn _ => () }
			{ file = SrcPath.pre p, relativeTo = grouppath }
	in
	    "p" $ [list (list string) ll]
	end

	fun sn n = let
	    val op $ = PU.$ SN
	    fun raw_sn (DG.SNODE n) =
		"a" $ [list sn (#localimports n), list fsbn (#globalimports n)]
	in
	    share SNs raw_sn n
	end

	and sbn x = let
	    val op $ = PU.$ SBN
	in
	    case x of
		DG.SB_BNODE (DG.BNODE { bininfo = i, ... }, ii, _) => let
		    val (i, sy) = inverseMap i
		    val { statpid, ... } = ii
		in
		    "2" $ [int i, pid statpid]
		end
	      | DG.SB_SNODE n => "3" $ [sn n]
	end

	and fsbn (_, n) = let val op $ = PU.$ FSBN in "f" $ [sbn n] end

	fun group () = let
	    val op $ = PU.$ G
	in "g" $ [list sbn export_nodes]
	end
    in
	P.pickle2hash (Byte.stringToBytes (PU.pickle emptyMap (group ())))
    end

    (* Comparison of old and new library stamps. *)
    fun libStampIsValid (gp: GP.info) (a as (grouppath, _, _), version) = let
	val newStamp = Byte.bytesToString (Pid.toBytes (libStampOf a))
	val policy = #fnpolicy (#param gp)
	val sname = FilenamePolicy.mkStableName policy (grouppath, version)
	fun work s = let
	    val oldStamp =
		Byte.bytesToString (BinIO.inputN (s, libstamp_nbytes))
	in
	    oldStamp = newStamp
	end
    in
	SafeIO.perform { openIt = fn () => BinIO.openIn sname,
			 closeIt = BinIO.closeIn,
			 work = work,
			 cleanup = fn _ => () }
	handle _ => false
    end

    fun loadStable { getGroup, anyerrors } (gp, group, version, rebinds) = let

	val gp = GeneralParams.bind gp rebinds

	val errcons = #errcons (gp: GeneralParams.info)
	val grpSrcInfo = (errcons, anyerrors)
	val gdescr = SrcPath.descr group
	fun error l = EM.errorNoFile (errcons, anyerrors) SM.nullRegion
              EM.COMPLAIN (concat ("(stable) " :: gdescr :: ": " :: l))
                EM.nullErrorBody
	exception Format = UU.Format

	val penv = #penv (#param gp)
	val policy = #fnpolicy (#param gp)

	fun mksname () = FilenamePolicy.mkStableName policy (group, version)

	fun work s = let

	    fun getGroup' (gp, p, vo, rb) =
		case getGroup (gp, p, vo, rb) of
		    SOME g => g
		  | NONE => (error ["unable to find ", SrcPath.descr p,
				    " (", SrcPath.osstring p, ")"];
			     raise Format)

	    val { size = dg_sz, pickle = dg_pickle } = fetch_pickle s
	    val offset_adjustment = dg_sz + 4 + libstamp_nbytes
	    val { getter, dropper } =
		UU.stringGetter' (SOME dg_pickle, mkPickleFetcher mksname)
	    val session = UU.mkSession getter

	    val sgListM = UU.mkMap ()
	    val ssM = UU.mkMap ()
	    val ssoM = UU.mkMap ()
	    val boolOptionM = UU.mkMap ()
	    val siM = UU.mkMap ()
	    val snM = UU.mkMap ()
	    val snListM = UU.mkMap ()
	    val sbnM = UU.mkMap ()
	    val fsbnM = UU.mkMap ()
	    val fsbnListM = UU.mkMap ()
	    val impexpM = UU.mkMap ()
	    val impexpListM = UU.mkMap ()
	    val groupM = UU.mkMap ()
	    val apM = UU.mkMap ()
	    val exportsM = UU.mkMap ()
	    val privilegesM = UU.mkMap ()
	    val poM = UU.mkMap ()
	    val stringListM = UU.mkMap ()
	    val stringListListM = UU.mkMap ()
	    val versionM = UU.mkMap ()
	    val versionOptM = UU.mkMap ()
	    val sgM = UU.mkMap ()
	    val rbM = UU.mkMap ()
	    val rblM = UU.mkMap ()

	    fun list m r = UU.r_list session m r
	    val string = UU.r_string session

	    fun option m r = UU.r_option session m r
	    val int = UU.r_int session
	    val bool = UU.r_bool session
	    fun share m r = UU.share session m r
	    fun nonshare r = UU.nonshare session r
	    val bool = UU.r_bool session
	    val pid = UnpickleSymPid.r_pid (session, string)

	    val stringlist = list stringListM string

	    val stringlistlist = list stringListListM stringlist

	    fun list2path c sl =
		c (SrcPath.unpickle penv { pickled = sl, relativeTo = group })
		handle SrcPath.Format => raise Format

	    fun abspath () = let
		fun ap #"p" = list2path SrcPath.file (stringlistlist ())
		  | ap _ = raise Format
	    in
		share apM ap
	    end

	    fun version () = let
		fun v #"v" =
		    (case Version.fromString (string ()) of
			 SOME v => v
		       | NONE => raise Format)
		  | v _ = raise Format
	    in
		share versionM v
	    end

	    fun rb () = let
		fun r #"b" =
		    { anchor = string (),
		      value = list2path (fn x => x) (stringlistlist ()) }
		  | r _ = raise Format
	    in
		share rbM r
	    end

	    fun sg () = let
		fun doit getRbl =
		    let val p = abspath ()
			val vo = option versionOptM version ()
			val rbl = getRbl ()
			fun gth () = getGroup' (gp, p, vo, rbl)
		    in
			(p, Memoize.memoize gth, rbl)
		    end
		fun xsg #"s" = doit (fn () => []) (* backward-compatible *)
		  | xsg #"S" = doit (list rblM rb)
		  | xsg _ = raise Format
	    in
		share sgM xsg
	    end

	    fun gr #"g" =
		let val version = option versionOptM version ()
		    val sublibs = list sgListM sg ()

		    fun getSublib i =
			(case #2 (List.nth (sublibs, i)) () of
			     GG.GROUP x => x
			   | GG.ERRORGROUP =>
			       EM.impossible "loadStable: ERRORGROUP")
			handle General.Subscript => raise Format

		    fun context NONE = raise Format
		      | context (SOME (pos, sy)) = let
			    val { exports, ... } = getSublib pos
			in
			    case SymbolMap.find (exports, sy) of
				SOME (nth, _, _) =>
				  (case nth () of
				       (_, DG.SB_BNODE (DG.BNODE bn, x, _)) =>
				         (StabModmap.addEnv (#statenv x ()),
					  fn () => BinInfo.describe
						       (#bininfo bn))
				     | _ => raise Format)
			      | NONE => raise Format
			end

		    val { statenv, symbol, symbollist } =
			UP.mkUnpicklers
			{ session = session,
			  stringlist = stringlist }
			context

		    val lazy_statenv = UU.r_lazy session statenv

		    fun symbolset () = let
			fun s #"s" =
			    SymbolSet.addList (SymbolSet.empty, symbollist ())
			  | s _ = raise Format
		    in
			share ssM s
		    end

		    val filter = option ssoM symbolset

		    fun shm () = let
			fun s #"a" = Sharing.SHARE true
			  | s #"b" = Sharing.SHARE false
			  | s #"c" = Sharing.DONTSHARE
			  | s _ = raise Format
		    in
			nonshare s
		    end

		    val pidoption = option poM pid

		    fun si () = let
			fun s #"s" =
			    let val spec = string ()
				val locs = string ()
				val offset = int () + offset_adjustment
				val rts_pid = pidoption ()
				val sh_mode = shm ()
				val error = EM.errorNoSource grpSrcInfo locs
			    in
				BinInfo.new { group = group,
					      mkStablename = mksname,
					      error = error,
					      spec = spec,
					      offset = offset,
					      rts_pid = rts_pid,
					      sh_mode = sh_mode }
			    end
			  | s _ = raise Format
		    in
			share siM s
		    end

		    (* this is the place where what used to be an
		     * SNODE changes to a BNODE! *)
		    fun sn () = let
			fun sn' #"a" =
			    DG.BNODE { bininfo = si (),
				       localimports = snlist (),
				       globalimports = fsbnlist () }
			  | sn' _ = raise Format
		    in
			share snM sn'
		    end

		    and snlist () = list snListM sn ()

		    (* this one changes from farsbnode to plain farbnode *)
		    and sbn () = let
			fun sbn' #"2" = let
				val pos = int ()
				val sy = symbol ()
				val { exports = slexp, ... } = getSublib pos
			    in
				case SymbolMap.find (slexp, sy) of
				    SOME (nth, _, _) =>
				      (case nth () of
					   (_, DG.SB_BNODE (n, _, _)) =>
					   (n, SOME pos)
					 | _ => raise Format)
				  | NONE => raise Format
			    end
			  | sbn' #"3" = (sn (), NONE)
			  | sbn' _ = raise Format
		    in
			share sbnM sbn'
		    end

		    and fsbn () = let
			fun f #"f" =
			    let val f = filter ()
				val (n, pos) = sbn ()
			    in
				(f, n, pos)
			    end
			  | f _ = raise Format
		    in
			share fsbnM f
		    end

		    and fsbnlist () = list fsbnListM lazy_fsbn ()

		    and lazy_fsbn () = UU.r_lazy session fsbn ()

		    fun impexp () = let
			fun ie #"i" =
			    let val sy = symbol ()
				(* really reads farbnodes! *)
				val nth = lazy_fsbn ()
				val ge = lazy_statenv ()
				val statpid = pid ()
				val guid = string ()
				val allsyms = symbolset ()
				fun ieth () = let
				    val (f, n, pos) = nth ()
				    val ii = { statenv = ge,
					       statpid = statpid,
					       guid = guid }
				in
				    (f, DG.SB_BNODE (n, ii, pos))
				end
				val e = Statenv2DAEnv.cvtMemo ge
				(* put a filter in front to avoid having
				 * the FCTENV being queried needlessly
				 * (avoids spurious module loadings) *)
				val e' = DAEnv.FILTER (SymbolSet.singleton sy, e)
			    in
				(sy, (Memoize.memoize ieth, e', allsyms))
			    end
			  | ie #"j" = let
				val sy = symbol ()
				val nth = lazy_fsbn ()
				val allsyms = symbolset ()
				(* This seems (is?) a bit clumsy... *)
				fun xth () = let
				    val (f, n, pos) = nth ()
				    val (sbnth, e, _) =
					valOf (SymbolMap.find
						   (#exports
							(getSublib
							     (valOf pos)), sy))
					handle _ => raise Format
				in
				    (f, n, pos, sbnth, e)
				end
				val xth = Memoize.memoize xth
				fun eth () = #5 (xth ())
				val e' = DAEnv.FILTER
					 (SymbolSet.singleton sy,
					  DAEnv.SUSPEND eth)
				fun ieth () = let
				    val (f, n, pos, sbnth, _) = xth ()
				    val ii =
					case #2 (sbnth ()) of
					    DG.SB_BNODE (_, ii, _) => ii
					  | _ => raise Format
				in
				    (f, DG.SB_BNODE (n, ii, pos))
				end
			    in
				(sy, (Memoize.memoize ieth, e', allsyms))
			    end
			  | ie _ = raise Format
		    in
			share impexpM ie
		    end

		    val impexplist = list impexpListM impexp

		    fun r_exports () = let
			fun e #"e" =
			    foldl SymbolMap.insert'
			          SymbolMap.empty (impexplist ())
			  | e _ = raise Format
		    in
			share exportsM e
		    end

		    fun privileges () = let
			fun p #"p" =
			    StringSet.addList (StringSet.empty, stringlist ())
			  | p _ = raise Format
		    in
			share privilegesM p
		    end

		    val exports = r_exports ()
		    val required = privileges ()
		in
		    GG.GROUP { exports = exports,
			       kind = GG.LIB { version = version,
					       kind = GG.STABLE dropper },
			       required = required,
			       grouppath = group,
			       sources = SrcPathMap.empty,
			       sublibs = sublibs }
		end
	      | gr _ = raise Format
	in
	    share groupM gr
	end
    in
	SOME (SafeIO.perform { openIt = BinIO.openIn o mksname,
			       closeIt = BinIO.closeIn,
			       work = work,
			       cleanup = fn _ => () })
	handle (exn as Format) => (
            error ["file is corrupted (old version?)"];
            List.app (fn s => error["    ", s, "\n"]) (SMLofNJ.exnHistory exn);
            NONE)
        | IO.Io _ => NONE
    end

    fun stabilize _ { group = GG.ERRORGROUP, ... } = NONE
      | stabilize gp { group = g as GG.GROUP grec, anyerrors, rebindings } =
	let val policy = #fnpolicy (#param gp)

	    fun doit (wrapped, getBFC, vers) = let

		val grouppath = #grouppath grec
		val sublibs = #sublibs grec
		val exports = #exports grec

		fun force f = f ()

		val libstamp =
		    libStampOf (grouppath,
				map (#2 o force o #1)
				    (SymbolMap.listItems exports),
				    sublibs)

		fun writeBFC s (i, { code, data, env }) = let
		    val { contents, stats } = getBFC i
		    val { code = c, data = d, env = e } = stats
                    in
                      ignore (BF.write { stream = s, contents = contents, nopickle = true });
                      { code = code + c, data = data + d, env = env + e }
                    end

		fun sizeBFC i = BF.size { contents = #contents (getBFC i), nopickle = true }
		fun pidBFC i = BF.staticPidOf (#contents (getBFC i))

		val _ = Say.vsay ["[stabilizing ", SrcPath.descr grouppath, "]\n"]

		val _ =
		    if StringSet.isEmpty wrapped then ()
		    else
			Say.say
			    ("$Stabilize: wrapping the following privileges:\n"
			     :: map (fn s => ("  " ^ s ^ "\n"))
			            (StringSet.listItems wrapped))

		val grpSrcInfo = (#errcons gp, anyerrors)

		val required = StringSet.difference (#required grec, wrapped)

(* FIXME: this description does not match what the code does!!! *)
		(* The format of a stable archive is the following:
		 *  - It starts with the size s of the pickled dependency
		 *    graph. This size itself is written as a four-byte string.
		 *  - The size t of the pickled environment for the entire
		 *    library (using the pickleEnvN interface of the pickler)
		 *    in the same format as s.
		 *  - The pickled dependency graph.  This graph contains
		 *    integer offsets of the binfiles for the individual ML
		 *    members. These offsets need to be adjusted by adding
		 *    s + t + 8. The pickled dependency graph also contains
		 *    integer offsets relative to other stable groups.  These
		 *    offsets need no further adjustment.
		 *  - Individual binfile contents (concatenated) but without
		 *    their static environments. *)

		val inverseMap = mkInverseMap sublibs

		val members = ref []
		val (registerOffset, getOffset) = let
		    val dict = ref SmlInfoMap.empty
		    val cur = ref 0
		    fun get0 i = SmlInfoMap.find (!dict, i)
		    fun reg (i, sz) =
			case get0 i of
			    (* This test is necessary because of a tiny chance
			     * that a portion of a pickle needs to be re-done
			     * by the pickler because it underestimated its
			     * size during lazy pickling. Ideally, the pickler
			     * should run without side-effects, but in the
			     * present case all we need is idempotence. *)
			    SOME os => os
			  | NONE => let
				val os = !cur
			    in
				cur := os + sz;
				dict := SmlInfoMap.insert (!dict, i, os);
				members := i :: (!members);
				os
			    end
		    val get = valOf o get0
		in
		    (reg, get)
		end

		fun prepath2list what p = let
		    fun warn_relabs (abs, descr) = let
			val relabs = if abs then "absolute" else "relative"
			val gdesc = SrcPath.descr grouppath
			fun ppb pps = let
			    fun space () = PP.break pps {nsp=1,offset=0}
			    fun string s = PP.string pps s
			    fun ss s = (string s; space ())
			    fun nl () = PP.newline pps
			in
			    nl ();
			    PP.openHOVBox pps (PP.Rel 0);
			    app ss ["The", "path", "specifying"];
			    app ss [what, descr, "is"];
			    string relabs; string "."; nl ();
			    app ss ["(This", "means", "that", "in", "order",
				    "to", "be", "able", "to", "use", "the",
				    "stabilized", "library"];
			    string gdesc; ss ",";
			    app ss ["it", "will", "be", "necessary", "to",
				    "keep", "all", "imported", "libraries",
				    "with", "names", "derived", "from", "or",
				    "equal", "to"];
			    ss descr;
			    app ss ["in", "the", "same"];
			    ss relabs;
			    app ss ["location", "as", "they", "are"];
			    string "now.)";
			    PP.closeBox pps
			end
		    in
			EM.errorNoFile
			    (#errcons gp, anyerrors) SM.nullRegion EM.WARN
			    (gdesc ^ ": uses non-anchored path") ppb
		    end
		in
		    SrcPath.pickle { warn = warn_relabs }
				   { file = p, relativeTo = grouppath }
		end

		(* Collect all BNODEs that we see and build
		 * a context suitable for P.envPickler. *)
		val libctxt = let
		    fun lst f [] k s = k s
		      | lst f (h :: t) k s = f h (lst f t k) s

		    fun sbn n k (s as (bnodes, snodes)) =
			case n of
			    DG.SB_BNODE (DG.BNODE { bininfo = i, ... },
					 ii, _) =>
			    let val (pos, sy) = inverseMap i
				val bnodes' =
				    StableMap.insert (bnodes, i,
						      ((pos, sy), #statenv ii))
			    in
				k (bnodes', snodes)
			    end
			  | DG.SB_SNODE n => sn n k s

		    and sn (DG.SNODE n) k (bnodes, snodes) = let
			val i = #smlinfo n
			val li = #localimports n
			val gi = #globalimports n
		    in
			if SmlInfoSet.member (snodes, i) then
			    k (bnodes, snodes)
			else let
				val snodes' = SmlInfoSet.add (snodes, i)
			    in
				lst sn li (lst fsbn gi k) (bnodes, snodes')
			    end
		    end

		    and fsbn (_, n) k s = sbn n k s

		    fun impexp (nth, _, _) k s = fsbn (nth ()) k s

		    val bnodes =
			lst impexp (SymbolMap.listItems exports)
		            #1
		            (StableMap.empty, SmlInfoSet.empty)

		    val bnodel = ListMergeSort.sort
				     (fn (x, y) => (#1 (#1 x) > #1 (#1 y)))
				     (StableMap.listItems bnodes)

		    fun libArg ([], _) = []
		      | libArg ((lsm, ge) :: t, m) = let
			    val m' = GenModIdMap.mkMap' (ge (), m)
			in
			    (SOME lsm, m') :: libArg (t, m')
			end
		in
		    libArg (bnodel, MI.emptyTmap)
		end

		val env_orig = P.envPickler (fn _ => ()) (P.LIBRARY libctxt)
		val env = PU.lift_pickler lifter env_orig
		val lazy_env = PU.w_lazy env

		val bool = PU.w_bool
		val int = PU.w_int
		val symbol = PickleSymPid.w_symbol
		val pid = PickleSymPid.w_pid
		val share = PU.ah_share
		val option = PU.w_option
		val list = PU.w_list
		val string = PU.w_string
		val bool = PU.w_bool
		val int = PU.w_int

		fun symbolset ss = let
		    val op $ = PU.$ SS
		    fun raw_ss ss =
			"s" $ [list symbol (SymbolSet.listItems ss)]
		in
		    share SSs raw_ss ss
		end

		val filter = option symbolset

		val op $ = PU.$ SHM
		fun shm (Sharing.SHARE true) = "a" $ []
		  | shm (Sharing.SHARE false) = "b" $ []
		  | shm Sharing.DONTSHARE = "c" $ []

		fun si i = let
		    (* FIXME: this is not a technical flaw, but perhaps one
		     * that deserves fixing anyway:  If we only look at spec,
		     * then we are losing information about sub-grouping
		     * within libraries.  However, the spec in BinInfo.info
		     * is only used for diagnostics and has no impact on the
		     * operation of CM itself. *)
		    val spec = SrcPath.osstring_relative (SmlInfo.sourcepath i)
		    val locs = SmlInfo.errorLocation gp i
		    val offset = registerOffset (i, sizeBFC i)
		    val { is_rts, ... } = SmlInfo.attribs i
		    val sh_mode = SmlInfo.sh_mode i
		    val op $ = PU.$ SI
		    val rts_pid = if is_rts then SOME (pidBFC i) else NONE
		in
		    "s" $ [string spec, string locs, int offset,
			   option pid rts_pid, shm sh_mode]
		end

		fun abspath p = let
		    val op $ = PU.$ AP
		in
		    "p" $ [list (list string) (prepath2list "library"
							    (SrcPath.pre p))]
		end

		fun sn n = let
		    val op $ = PU.$ SN
		    fun raw_sn (DG.SNODE n) =
			"a" $ [si (#smlinfo n), list sn (#localimports n),
			       list lazy_fsbn' (#globalimports n)]
		in
		    share SNs raw_sn n
		end

		(* Here we ignore the interface info because we will not
		 * need it anymore when we unpickle. *)
		and sbn x = let
		    val op $ = PU.$ SBN
		in
		    case x of
			DG.SB_BNODE (DG.BNODE { bininfo = i, ... }, _, _) =>
			let val (pos, sy) = inverseMap i
			in
			    "2" $ [int pos, symbol sy]
			end
		      | DG.SB_SNODE n => "3" $ [sn n]
		end

		and fsbn (f, n) = let
		    val op $ = PU.$ FSBN
		in
		    "f" $ [filter f, sbn n]
		end

		and lazy_fsbn arg = PU.w_lazy fsbn arg

		and lazy_fsbn' arg = lazy_fsbn (fn () => arg)

		(* Here is the place where we need to write interface info. *)
		fun impexp (s, (nth, _, allsyms)) = let
		    val op $ = PU.$ IMPEXP
		in
		    case nth () of
			(_, DG.SB_SNODE (DG.SNODE { smlinfo, ... })) =>
			(* this is the case of an actual internal node *)
			let val { statenv, statpid, guid } = getII smlinfo
			in
			    "i" $ [symbol s,
				   lazy_fsbn nth,
				   lazy_env statenv,
				   pid statpid,
				   string guid,
				   symbolset allsyms]
			end
		      | (f, DG.SB_BNODE (DG.BNODE n, _, _)) =>
			(* This is the case of a simple re-export;
			 * we avoid pickling any environments here because
			 * they can be re-fetched from the sublib directly
			 * when unpickling. *)
			"j" $ [symbol s, lazy_fsbn nth, symbolset allsyms]
		end

		fun w_exports e = let
		    val op $ = PU.$ EXPORTS
		in
		    "e" $ [list impexp (SymbolMap.listItemsi e)]
		end

		fun privileges p = let
		    val op $ = PU.$ PRIV
		in
		    "p" $ [list string (StringSet.listItems p)]
		end

		fun version v = let
		    val op $ = PU.$ VERSION
		in
		    "v" $ [string (Version.toString v)]
		end

		fun rb { anchor, value } = let
		    val op $ = PU.$ RB
		in
		    "b" $ [string anchor,
			   list (list string)
				(prepath2list "anchor binding" value)]
		end

		fun sg (p, gth, rbl) = let
		    val op $ = PU.$ SG
		    val vo = case gth () of
				GG.GROUP { kind = GG.LIB x, ... } => #version x
			      | _ => NONE
		in
		    "S" $ [abspath p, option version vo, list rb rbl]
		end

		fun group () = let
		    val op $ = PU.$ G
		in
		    (* Pickle the sublibs first because we need to already
		     * have them back when we unpickle BNODEs. *)
		    "g" $ [option version vers,
			   list sg sublibs,
			   w_exports exports,
			   privileges required]
		end

		val dg_pickle =
		    Byte.stringToBytes (PU.pickle emptyMap (group ()))

		val dg_sz = Word8Vector.length dg_pickle

		val offset_adjustment = dg_sz + 4 + libstamp_nbytes

		(* We could generate the graph for a stable group here directly
		 * by transcribing the original graph.  However, it is
		 * cumbersome and is likely to result in a larger memory
		 * footprint because we don't get the benefit of lazy
		 * unpickling of environments.
		 * It seems easier to simply rely on "loadStable" to re-fetch
		 * the stable graph. *)
		fun refetchStableGroup () = let
		    fun getGroup (_, p, _, _) = let
			fun theSublib (q, _, _) =
			    SrcPath.compare (p, q) = EQUAL
			fun force f = f ()
		    in
			Option.map (force o #2) (List.find theSublib sublibs)
		    end
		in
		    loadStable { getGroup = getGroup, anyerrors = anyerrors }
		               (gp, grouppath, NONE, rebindings)
		end

		fun writeInt32 (s, i) = let
		    val a = Word8Array.array (4, 0w0)
		    val _ = PackWord32Big.update (a, 0, LargeWord.fromInt i)
		in
		    BinIO.output (s, Word8Array.vector a)
		end
		val memberlist = rev (!members)

		(* don't use version information for making the stable path! *)
		fun mksname () =
		    FilenamePolicy.mkStableName policy (grouppath, NONE)

		val libstamp_bytes = Pid.toBytes libstamp
		val _ =
		    if Word8Vector.length libstamp_bytes <> libstamp_nbytes
		    then EM.impossible "stabilize: libstamp size wrong"
		    else ()
		fun work outs =
		    (BinIO.output (outs, libstamp_bytes);
		     writeInt32 (outs, dg_sz);
		     BinIO.output (outs, dg_pickle);
		     let val { code, data, env } =
			     foldl (writeBFC outs)
				   { code = 0, data = 0, env = 0 }
				   memberlist
		     in
			 Say.vsay ["[code: ", Int.toString code,
				   ", data: ", Int.toString data,
				   ", env: ", Int.toString dg_sz,
				   " bytes]\n"]
		     end)
	    in
		(SafeIO.perform { openIt = AutoDir.openBinOut o mksname,
				  closeIt = BinIO.closeOut,
				  work = work,
				  cleanup = fn _ =>
					       (OS.FileSys.remove (mksname ())
						handle _ => ()) };
		 refetchStableGroup ())
		handle exn =>
		       (EM.errorNoFile (#errcons gp, anyerrors) SM.nullRegion
			   EM.COMPLAIN
			   (concat ["Exception raised while stabilizing ",
				    SrcPath.descr grouppath])
			   EM.nullErrorBody;
			   NONE)
	    end
	in
	    case #kind grec of
		GG.LIB { kind = GG.STABLE _, ... } => SOME g
	      | GG.NOLIB _ => EM.impossible "stabilize: no library"
	      | GG.LIB { kind = GG.DEVELOPED { wrapped, ... }, version } =>
		(case recomp gp g of
		     NONE => (anyerrors := true; NONE)
		   | SOME bfc_acc => let
			 fun notStable (_, gth, _) =
			     case gth () of
				 GG.GROUP { kind =
					    GG.LIB { kind = GG.STABLE _,
						     ... }, ... } =>
				 false
			       | _ => true
		     in
			 case List.filter notStable (#sublibs grec) of
			     [] => doit (wrapped, bfc_acc, version)
			   | l => let
				 val grammar =
				     case l of [_] => " is" | _ => "s are"
				 fun ppb pps = let
				     fun loop [] = ()
				       | loop ((p, _, _) :: t) =
					 (PP.string pps (SrcPath.descr p);
					  PP.newline pps;
					  loop t)
				 in
				     PP.newline pps;
				     PP.string pps
				    (concat ["because the following sub-group",
					     grammar, " not stable:"]);
				     PP.newline pps;
				     loop l
				 end
				 val errcons = #errcons gp
				 val gdescr = SrcPath.descr (#grouppath grec)
			     in
				 EM.errorNoFile (errcons, anyerrors)
					SM.nullRegion
					EM.COMPLAIN
					(gdescr ^ " cannot be stabilized")
					ppb;
				 NONE
			     end
		     end)
	end
end (* functor Stabilize *)

end (* local *)
