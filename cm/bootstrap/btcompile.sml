(*
 * The bootstrap compiler.
 *   (Formerly known as "batch" compiler.)
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
local
    structure EM = ErrorMsg
    structure SE = StaticEnv
    structure PS = PersStamps
    structure GG = GroupGraph
    structure DG = DependencyGraph
in
functor BootstrapCompileFn
	    (structure Backend : BACKEND
	     val useStream : TextIO.instream -> unit
	     val os : SMLofNJ.SysInfo.os_kind
	     val load_plugin : SrcPath.dir -> string -> bool) =
struct
    structure SSV = SpecificSymValFn (val arch = Backend.architecture
				      val os = os
				      val abi_variant = Backend.abi_variant)
    structure P = OS.Path
    structure F = OS.FileSys
    structure BF = Binfile

    val arch = Backend.architecture
    val osname = FilenamePolicy.kind2name os

    val archos = concat [arch, "-", osname]

    structure StabModmap = StabModmapFn ()

    structure Compile = CompileFn (structure Backend = Backend
				   structure StabModmap = StabModmap
				   val useStream = useStream
				   val compile_there =
				       Servers.compile o SrcPath.encode)

    structure BFC = BfcFn (val arch = Backend.architecture)

    (* instantiate Stabilize... *)
    structure Stabilize =
	StabilizeFn (val arch = Backend.architecture
		     structure StabModmap = StabModmap
		     fun recomp gp g = let
			 val { store, get } = BFC.new ()
			 fun dummy _ _ = ()
			 val { group, ... } =
			     Compile.newTraversal (dummy, store, g)
		     in
			 case group gp of
			     NONE => NONE
			   | SOME _ => SOME get
		     end
		     val getII = Compile.getII)

    structure VerifyStable = VerStabFn (structure Stabilize = Stabilize)

    (* ... and Parse *)
    structure Parse = ParseFn (structure Stabilize = Stabilize
			       structure StabModmap = StabModmap
			       val evictStale = Compile.evictStale
			       fun pending () = SymbolMap.empty)

    fun mkBootList g = let
	fun listName p =
	    case P.fromString p of
		{ vol = "", isAbs = false, arcs = _ :: arc1 :: arcn } => let
		    fun win32name () =
			concat (arc1 ::
				foldr (fn (a, r) => "\\" :: a :: r) [] arcn)
		in
		    case os of
			SMLofNJ.SysInfo.WIN32 => win32name ()
		      | _ => P.toString { isAbs = false, vol = "",
					  arcs = arc1 :: arcn }
		end
	      | _ => raise Fail ("BootstrapCompile:listName: bad name: " ^ p)
    in
	MkBootList.group listName g
    end

    fun internal_reset () =
	(Compile.reset ();
	 Parse.reset ();
	 StabModmap.reset ())

    fun reset () =
	(Say.vsay ["[CMB reset]\n"];
	 Servers.withServers (fn () => Servers.cmb_reset { archos = archos });
	 internal_reset ())

    val checkDirbase = let
	val prev = ref NONE
	fun ck db =
	    (case !prev of
		 NONE => prev := SOME db
	       | SOME db' =>
		 if db = db' then ()
		 else (Say.vsay ["[new dirbase is `", db,
				 "'; CMB reset]\n"];
		       internal_reset ();
		       prev := SOME db))
    in
	ck
    end

    fun mk_compile { master, root, dirbase = dbopt } = let

	val dirbase = getOpt (dbopt, BtNames.dirbaseDefault)
	val _ = checkDirbase dirbase
	val penvspec = BtNames.penvspec
	val initgspec = BtNames.initgspec
	val maingspec = BtNames.maingspec

	val bindir = concat [dirbase, BtNames.bin_infix, archos]
	val bootdir = concat [dirbase, BtNames.boot_infix, archos]

	val keep_going = #get StdConfig.keep_going ()

	val ctxt = SrcPath.cwd ()

	val listfile = P.joinDirFile { dir = bootdir, file = BtNames.bootlist }
	val pidmapfile = P.joinDirFile { dir = bootdir, file = BtNames.pidmap }

	val penv = SrcPath.newEnv ()
	val _ = SafeIO.perform { openIt = fn () => TextIO.openIn penvspec,
				 closeIt = TextIO.closeIn,
				 work = SrcPath.processSpecFile
					    { env = penv, specfile = penvspec,
					      say = Say.say },
				 cleanup = fn _ => () }
	val _ = SrcPath.sync ()

	fun stdpath s =
	    SrcPath.file (SrcPath.standard
			      { err = fn s => raise Fail s, env = penv }
			      { context = ctxt, spec = s })

	val initgspec = stdpath initgspec
	val maingspec =
	    case root of
		NONE => stdpath maingspec
	      | SOME r => SrcPath.decode penv r

	val fnpolicy =
	    FilenamePolicy.separate { bindir = bindir, bootdir = bootdir }
	        { arch = arch, os = os }

	fun param slave_mode =
	    { fnpolicy = fnpolicy,
	      penv = penv,
	      symval = SSV.symval,
	      archos = archos,
	      keep_going = keep_going,
	      slave_mode = slave_mode }

	val emptydyn = DynamicEnv.empty

	(* first, build an initial GeneralParam.info, so we can
	 * deal with the pervasive env and friends... *)

	val groupreg = GroupReg.new ()
	val errcons = EM.defaultConsumer ()
	val ginfo = { param = param false, groupreg = groupreg,
		      errcons = errcons,
		      youngest = ref TStamp.ancient }

	fun mk_main_compile arg = let

	    val { pervasive = perv_n, others, src } = arg

	    fun recompInitGroup () = let
		val ovldR = Control.overloadKW
		val savedOvld = !ovldR
		val _ = ovldR := true
		val sbnode = Compile.newSbnodeTraversal ()

		val perv_fsbnode = (NONE, perv_n)

		fun rt n = valOf (sbnode n ginfo)
		val pervasive = rt perv_n

		fun rt2ie (n, ii: IInfo.info) = let
		    val s = #statenv ii ()
		    val (dae, mkDomain) = Statenv2DAEnv.cvt s
		    val domain = mkDomain ()
		in
		    { ie = (fn () => (NONE, n), dae, domain), domain = domain }
		end
		
		fun add_exports (n, exports) = let
		    val { ie, domain } = rt2ie (n, rt n)
		    fun ins_ie (sy, m) = SymbolMap.insert (m, sy, ie)
		in
		    SymbolSet.foldl ins_ie exports domain
		end

		val special_exports = let
		    fun mkie (n, rtn) = #ie (rt2ie (n, rtn))
		in
		    SymbolMap.insert (SymbolMap.empty,
				      PervAccess.pervStrSym,
				      mkie (perv_n, pervasive))
		end
	    in
		GG.GROUP { exports = foldl add_exports special_exports others,
			   kind = GG.LIB {
			     kind = GG.DEVELOPED { wrapped = StringSet.empty,
						   subgroups = [] },
				version = NONE },
			   required = StringSet.singleton "primitive",
			   grouppath = initgspec,
			   (* hack: sources never used for this group *)
			   sources = SrcPathMap.empty,
			   sublibs = [] }
		before ovldR := savedOvld
	    end

	    (* just go and load the stable init group or signal failure *)
	    fun loadInitGroup () = let
		val lsarg =
		    { getGroup = fn _ => raise Fail "CMB: initial getGroup",
		      anyerrors = ref false }
	    in
		case Stabilize.loadStable lsarg (ginfo, initgspec, NONE, []) of
		    NONE => NONE
		  | SOME (g as GG.GROUP { exports, ... }) => SOME g
		  | SOME GG.ERRORGROUP => NONE
	    end
		    
	    (* Don't try to load the stable init group. Instead, recompile
	     * directly. *)
	    fun dontLoadInitGroup () = let
		(* Function recompileInitGroup will not use servers (hence no
		 * call to Servers.withServers), but since compile traversals
		 * invoke the scheduler anyway, we must still clear pending
		 * tasks when we hit an error or an interrupt. *)
		val g0 = SafeIO.perform { openIt = fn () => (),
					  closeIt = fn () => (),
					  work = recompInitGroup,
					  cleanup = Servers.reset }
		val stabarg = { group = g0, anyerrors = ref false,
				rebindings = [] }
	    in
		if master then
		    case Stabilize.stabilize ginfo stabarg of
			SOME g => (Parse.reset (); g)
		      | NONE => raise Fail "CMB: cannot stabilize init group"
		else g0
	    end

	    (* Try loading the init group from the stable file if possible;
	     * recompile if loading fails *)
	    fun tryLoadInitGroup () =
		case loadInitGroup () of
		    SOME g => g
		  | NONE => dontLoadInitGroup ()
			
	    (* Ok, now, based on "paranoid" and stable verification,
	     * call the appropriate function(s) to get the init group. *)
	    val init_group =
		if master then let
		    val export_nodes = perv_n :: others
		    val ver_arg = (initgspec, export_nodes, [],
				   SrcPathSet.empty, NONE)
		    val em = StableMap.empty
		in
		    if VerifyStable.verify' ginfo em ver_arg then
			tryLoadInitGroup ()
		    else dontLoadInitGroup ()
		end
		else valOf (loadInitGroup ()) (* failure caught at the end *)

	    val gr = GroupReg.new ()
	    val _ = GroupReg.register gr (initgspec, src)

	    fun parse_arg0 slave_mode (s, p) =
		{ load_plugin = load_plugin,
		  gr = gr,
		  param = param slave_mode,
		  stabflag = s,
		  group = maingspec,
		  init_group = init_group,
		  paranoid = p }

	    val parse_arg = parse_arg0 false
	    val slave_parse_arg = parse_arg0 true

	    val lonely_master = master andalso Servers.noServers ()

	    val initial_parse_result =
		if master then
		    if lonely_master then
			(* no slaves available; do everything alone
			 * (Still wrap "withServers" around it to make sure
			 * our queues get cleaned when an interrupt or error
			 * occurs.) *)
			Servers.withServers
			    (fn () => Parse.parse(parse_arg (SOME true, true)))
		    else
			(* slaves available; we want master
			 * and slave initialization to overlap, so
			 * we do the master's parsing in its own
			 * thread *)
			let fun worker () = let
				val c =
				    Concur.fork
					(fn () => Parse.parse
						      (parse_arg (NONE, true)))
			    in
				Servers.cmb
				    { dirbase = dirbase,
				      archos = archos,
				      root = SrcPath.encode maingspec };
				Concur.wait c
			    end
			in
			    Servers.withServers worker
			end
		else
		    (* slave case *)
		    Parse.parse (slave_parse_arg (NONE, false))
	in
	    case initial_parse_result of
		NONE => NONE
	      | SOME (g, gp) => let
		    fun finish (g, gp) = let
			val { l = bootitems, ss } = mkBootList g
			val bootitems = map #2 bootitems
			val stablelibs = Reachable.stableLibsOf g
			fun inSet bi = StableSet.member (ss, bi)
			val frontiers =
			    SrcPathMap.map (Reachable.frontier inSet)
					   stablelibs
			fun writeBootList s = let
			    fun wr str = TextIO.output (s, str ^ "\n")
			    val numitems = length bootitems
			    fun biggerlen (s, n) = Int.max (size s, n)
			    val maxlen = foldl biggerlen 0 bootitems
			in
			    wr (concat ["%", Int.toString numitems,
					" ", Int.toString maxlen]);
			    app wr bootitems
			end
			fun writePid s i = let
			    val sn = BinInfo.stablename i
			    val os = BinInfo.offset i
			    val descr = BinInfo.describe i
			    val bfc = BFC.getStable { stable = sn, offset = os,
						      descr = descr }
			in
			    case BF.exportPidOf bfc of
				NONE => ()
			      | SOME pid =>
				app (fn str => TextIO.output (s, str))
				    [" ", Int.toString os, ":", PS.toHex pid]
			end
			fun writePidLine s (p, set) =
			    if StableSet.isEmpty set then ()
			    else (TextIO.output (s, SrcPath.encode p);
				  StableSet.app (writePid s) set;
				  TextIO.output (s, "\n"))
			fun writePidMap s =
			    SrcPathMap.appi (writePidLine s) frontiers
		    in
			SafeIO.perform
			    { openIt = fn () => AutoDir.openTextOut listfile,
			      closeIt = TextIO.closeOut,
			      work = writeBootList,
			      cleanup = fn _ => (OS.FileSys.remove listfile
						 handle _ => ()) };
			SafeIO.perform
			    { openIt = fn () => AutoDir.openTextOut pidmapfile,
			      closeIt = TextIO.closeOut,
			      work = writePidMap,
			      cleanup = fn _ => (OS.FileSys.remove pidmapfile
						 handle _ => ()) };
			Say.say ["New boot directory has been built.\n"];
			true
		    end

		    (* the following thunk represents phase 2 (stabilization)
		     * of the master's execution path; it is never
		     * executed in slave mode *)
		    fun stabilize () =
			(* now we re-parse everything with stabilization
			 * turned on (and servers turned off) *)
			case Parse.parse (parse_arg (SOME true, false)) of
			    NONE => false
			  | SOME (g, gp) => finish (g, gp)

		    (* Don't do another traversal if this is a lonely master *)
		    fun just_stabilize () = finish (g, gp)

		    (* the following thunk is executed in "master" mode only;
		     * slaves just throw it away *)
		    fun compile_and_stabilize () = let

			(* make compilation traversal and execute it *)
			val { allgroups, ... } =
			    Compile.newTraversal (fn _ => fn _ => (),
						  fn _ => (),
						  g)
		    in
			if Servers.withServers (fn () => allgroups gp) then
			    (Compile.reset ();
			     stabilize ())
			else false
		    end
		in
		    SOME ((g, gp, penv),
			  if lonely_master then just_stabilize
			  else compile_and_stabilize)
		end
	end handle Option => (Compile.reset (); NONE)
	    	   (* to catch valOf failures in "rt" or slave's failure
		    * to load init group *)
    in
	case BuildInitDG.build ginfo initgspec of
	    SOME x => mk_main_compile x
	  | NONE => NONE
    end

    fun compile dbopt =
	(StabModmap.reset ();
	 case mk_compile { master = true, root = NONE, dirbase = dbopt } of
	     NONE => false
	   | SOME (_, thunk) => thunk ())

    local
	fun slave NONE = (internal_reset (); NONE)
	  | slave (SOME (dirbase, root)) =
	    (StabModmap.reset ();
	     #set (SSV.symval "CMB_SLAVE_MODE") (SOME 1);
	     #set (SSV.symval "NO_PLUGINS") (SOME 1);
	     case mk_compile { master = false, root = SOME root,
			       dirbase = SOME dirbase } of
		 NONE => NONE
	       | SOME ((g, gp, penv), _) => let
		     val trav = Compile.newSbnodeTraversal ()
		     fun trav' sbn = isSome (trav sbn gp)
		 in
		     SOME (g, trav', penv)
		 end)
    in
	val _ = CMBSlaveHook.init archos slave
    end

    val make' = compile
    fun make () = make' NONE
    val symval = SSV.symval
end
end (* local *)
