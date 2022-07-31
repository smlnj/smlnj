(* cm-boot.sml
 *
 * COPYRIGHT (c) 2022 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This is the module that actually puts together the contents of the
 * structure CM people find in $smlnj/cm/full.cm.
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)

functor LinkCM (structure HostBackend : BACKEND) = struct

  datatype envrequest = AUTOLOAD | BARE

  local
      structure E = Environment
      structure DE = DynamicEnv
      structure SE = StaticEnv
      structure ER = EnvRef
      structure S = Symbol
      structure EM = ErrorMsg
      structure P = OS.Path
      structure F = OS.FileSys
      structure DG = DependencyGraph
      structure GG = GroupGraph
      structure IM = IntMap

      val os = SMLofNJ.SysInfo.getOSKind ()
      val my_archos =
	  concat [HostBackend.architecture, "-", FilenamePolicy.kind2name os]

      structure SSV =
	  SpecificSymValFn (val arch = HostBackend.architecture
			    val os = os
			    val abi_variant = HostBackend.abi_variant)

      val system_values =
	  ref (SrcPathMap.empty: E.dynenv IntMap.map SrcPathMap.map)

      structure StabModmap = StabModmapFn ()

      val useStreamHook =
	  ref (fn _ => raise Fail "useStreamHook not initialized")
	  : (TextIO.instream -> unit) ref

      structure Compile =
	  CompileFn (structure Backend = HostBackend
		     structure StabModmap = StabModmap
		     fun useStream s = !useStreamHook s
		     val compile_there = Servers.compile o SrcPath.encode)

      structure BFC =
          BfcFn (val arch = HostBackend.architecture)

      structure Link =
	  LinkFn (structure BFC = BFC
		  val system_values = system_values)

      structure AutoLoad = AutoLoadFn
	  (structure C = Compile
	   structure L = Link
	   structure BFC = BFC)

      val mkBootList = #l o MkBootList.group (fn p => p)

      fun init_servers (GG.GROUP { grouppath, ... }) =
	  Servers.cm { archos = my_archos, project = SrcPath.encode grouppath }
	| init_servers GG.ERRORGROUP = ()

      fun recomp_runner gp g = let
	  val _ = init_servers g
	  fun store _ = ()
	  val { group, ... } = Compile.newTraversal (Link.evict, store, g)
      in
	  isSome (Servers.withServers (fn () => group gp))
	  before Link.cleanup gp
      end

      (* This function combines the actions of "recompile" and "exec".
       * When successful, it combines the results (thus forming a full
       * environment) and adds it to the toplevel environment. *)
      fun make_runner _ _ GG.ERRORGROUP = false
	| make_runner add_bindings gp (g as GG.GROUP grec) = let
	      val { required = rq, ... } = grec
	      val { store, get } = BFC.new ()
	      val _ = init_servers g
	      val { group = c_group, ... } =
		  Compile.newTraversal (Link.evict, store, g)
	      val { group = l_group, ... } =
		  Link.newTraversal (g, #contents o get)
	  in
	      case Servers.withServers (fn () => c_group gp) of
		  NONE => false
		| SOME { stat } =>
		  (* Before executing the code, we announce the privileges
		   * that are being invoked.  (For the time being, we assume
		   * that everybody has every conceivable privilege, but at
		   * the very least we announce which ones are being made
		   * use of.) *)
		  (Link.cleanup gp;
		   if StringSet.isEmpty rq then ()
		   else Say.say ("$Execute: required privileges are:\n" ::
		     map (fn s => ("  " ^ s ^ "\n")) (StringSet.listItems rq));
		   case l_group gp of
		       NONE => false
		     | SOME dyn =>
			   (if add_bindings then
				let val delta = E.mkenv { static = stat, dynamic = dyn }
				    val loc = ER.loc ()
				    val base = #get loc ()
				    val new = E.concatEnv (delta, base)
				in
				    #set loc new;
				    Say.vsay ["[New bindings added.]\n"]
				end
			    else ();
			    true))
	  end

      val al_greg = GroupReg.new ()

      (* Instantiate the stabilization mechanism. *)
      structure Stabilize =
	  StabilizeFn (val arch = HostBackend.architecture
		       structure StabModmap = StabModmap
		       fun recomp gp g = let
			   val { store, get } = BFC.new ()
			   val { group, ... } =
			       Compile.newTraversal (Link.evict, store, g)
		       in
			   case group gp of
			       NONE => NONE
			     | SOME _ => SOME get
		       end
		       val getII = Compile.getII)

      (* Access to the stabilization mechanism is integrated into the
       * parser. I'm not sure if this is the cleanest way, but it works
       * well enough. *)
      structure Parse = ParseFn (structure Stabilize = Stabilize
				 structure StabModmap = StabModmap
				 fun evictStale () =
				     (Compile.evictStale ();
				      Link.evictStale ())
				 val pending = AutoLoad.getPending)

      local
	  type kernelValues = { init_group : GG.group }

	  val fnpolicy = FilenamePolicy.colocate
	      { os = os, arch = HostBackend.architecture }

	  val theValues = ref (NONE: kernelValues option)

      in
          val penv = SrcPath.newEnv ()

	  (* cancelling anchors cannot affect the order of existing paths
	   * (it may invalidate some paths; but all other ones stay as
	   * they are) *)
	  fun setAnchor a v = SrcPath.set_anchor (penv, a, v)
	  (* same goes for reset because it just cancels all anchors... *)
	  fun resetPathConfig () = SrcPath.reset_anchors penv
	  (* get the current binding for an anchor *)
	  fun getAnchor a () = SrcPath.get_anchor (penv, a)
          (* make a source path from a string using the host OS pathname
           * conventions.
           *)
	  fun mkNativeSrcPath s =
                SrcPath.file
                  (SrcPath.native
                    { err = fn s => raise Fail s, env = penv }
                    { context = SrcPath.cwd (), spec = s })
          (* make a source path from a string using CM standard path syntax *)
	  fun mkStdSrcPath s =
                SrcPath.file
                  (SrcPath.standard
                    { err = fn s => raise Fail s, env = penv }
                    { context = SrcPath.cwd (), spec = s })

	  fun getPending () =
	      map (Symbol.describe o #1)
		  (SymbolMap.listItemsi (AutoLoad.getPending ()))

	  fun showBindings () = let
	      val loaded = map Symbol.describe (EnvRef.listBoundSymbols ())
	      val pending = getPending ()
	      fun pr s = Say.say [s, "\n"]
	  in
	      Say.say ["\n*** Symbols bound at toplevel:\n"];
	      app pr loaded;
	      Say.say ["\n*** Symbols registered for autoloading:\n"];
	      app pr pending
	  end

	  fun initPaths () = let
	      val lpcth = #get StdConfig.local_pathconfig ()
	      val p = (case lpcth () of NONE => [] | SOME f => [f])
	      val p = #get StdConfig.pathcfgspec () () :: p
	      fun processOne f = let
		  val work =  SrcPath.processSpecFile
				  { env = penv, specfile = f, say = Say.say }
	      in
		  SafeIO.perform { openIt = fn () => TextIO.openIn f,
				   closeIt = TextIO.closeIn,
				   work = work,
				   cleanup = fn _ => () }
	      end handle _ => ()
	  in
	      app processOne p;
	      SrcPath.sync ()
	  end

	  fun getTheValues () = valOf (!theValues)
	      handle Option => raise Fail "CMBoot: theValues not initialized"

	  fun param slave_mode =
	      { fnpolicy = fnpolicy,
		penv = penv,
		symval = SSV.symval,
		archos = my_archos,
		keep_going = #get StdConfig.keep_going (),
		slave_mode = slave_mode }

	  val init_group = #init_group o getTheValues

	  fun dropPickles () =
	      if #get StdConfig.conserve_memory () then
		  Parse.dropPickles ()
	      else ()

	  fun parse_arg0 slave_mode (gr, sflag, p) =
	      { load_plugin = load_plugin, gr = gr, param = param slave_mode,
	        stabflag = sflag, group = p,
		init_group = init_group (), paranoid = false }

	  and parse_arg x = parse_arg0 false x

	  and slave_parse_arg x = parse_arg0 true x

	  and autoload mkSrcPath s = let
	        val p = mkSrcPath s
	        in
	          (case Parse.parse (parse_arg (al_greg, NONE, p))
                   of NONE => false
		    | SOME (g, _) => (
		        AutoLoad.register (EnvRef.loc (), g);
		        true)
                  (* end case *))
	            before dropPickles ()
	        end

	  and run mkSrcPath sflag f s = let
	      val p = mkSrcPath s
	      val gr = GroupReg.new ()
	  in
	      (case Parse.parse (parse_arg (gr, sflag, p)) of
		   NONE => false
		 | SOME (g, gp) => f gp g)
	      before dropPickles ()
	  end

	  and load_plugin' p = let
	      val d = SrcPath.descr p
	      val _ = Say.vsay ["[attempting to load plugin ", d, "]\n"]
	      val gr = GroupReg.new ()
	      val success =
		  ((case Parse.parse (parse_arg (gr, NONE, p)) of
			NONE => false
		      | SOME (g, gp) => make_runner false gp g)
		   before dropPickles ())
		  handle _ => false
	  in
	      if success then
		  Say.vsay ["[plugin ", d, " loaded successfully]\n"]
	      else
		  Say.vsay ["[unable to load plugin ", d, "]\n"];
	      success
	  end

	  and load_plugin context s = let
	      fun badname s = Say.say ["[bad plugin name: ", s, "]\n"]
	      val pp = SrcPath.standard { env = penv, err = badname }
					{ context = context, spec = s }
	  in
	      load_plugin' (SrcPath.file pp)
	  end

	  fun cwd_load_plugin x = load_plugin (SrcPath.cwd ()) x

	  fun stabilize mkSrcPath recursively root = let
                fun stabilize_recomp_runner gp g = let
                      val _ = init_servers g
                      val { allgroups, ... } =
                            Compile.newTraversal (Link.evict, fn _ => (), g)
                      in
                        Servers.withServers (fn () => allgroups gp)
                      end
                fun stabilize_dummy_runner gp g = true
                fun phase1 () = run mkSrcPath NONE
                                    stabilize_recomp_runner root
                fun phase2 () = (Compile.reset ();(* a bit too draconian? *)
                                 run mkSrcPath (SOME recursively)
                                     stabilize_dummy_runner root)
                in
                (* Don't bother with the 2-phase thing if there are
                 * no compile servers attached.  (We still need
                 * the "withServers" call to clean up our queues in case
                 * of an interrupt or error.) *)
                  if Servers.noServers ()
                    then Servers.withServers phase2
	            else
                    (* We do this in two phases:
                     *    1. recompile everything without stabilization but
                     *       potentially using compile servers
                     *    2. do a local stabilization run (which should have
                     *       no need to compile anything); don't use servers
                     *)
                      phase1 () andalso phase2 ()
	        end

	  fun recomp mkSrcPath = run mkSrcPath NONE recomp_runner
          val makeNative = run mkNativeSrcPath NONE (make_runner true)
          val makeStd = run mkStdSrcPath NONE (make_runner true)

	  fun to_portable s = let
	      val gp = mkNativeSrcPath s
	      fun nativesrc s = let
		  val p = SrcPath.standard
			      { err = fn s => raise Fail s, env = penv }
			      { context = SrcPath.dir gp, spec = s }
	      in
		  SrcPath.osstring' (SrcPath.file p)
	      end
	      fun mkres (g, pl) = { graph = g, imports = pl,
				    nativesrc = nativesrc }
	  in
	      Option.map
                (mkres o ToPortable.export)
                (Parse.parse (parse_arg (GroupReg.new (), NONE, mkNativeSrcPath s)))
	  end

	  fun sources archos group =
	      let val policy =
		      case archos of
			  NONE => fnpolicy
			| SOME ao => FilenamePolicy.colocate_generic ao
		  fun snam (p, version) = FilenamePolicy.mkStableName
					      policy (p, version)
		  fun insert0 (a, f, x) = StringMap.insert (a, f, x)
		  fun insert (a, p, x) = insert0 (a, SrcPath.osstring p, x)
		  fun add (s, p) = SrcPathSet.add (s, p)

		  fun addSources (subgroups, sources, a, v) =
		      let fun findSG p =
			      List.find (fn (p', _, _) =>
					    SrcPath.compare (p, p') = EQUAL)
					subgroups
			  fun one (p, x as { class, derived }, (a, v)) =
			      if SrcPathSet.member (v, p) then (a, v)
			      else if class = "cm" then
				  case findSG p of
				      NONE => (a, v) (* unused group/library *)
				    | SOME (_, gth, _) =>
				        addGroup (p, x, gth, a, add (v, p))
			      else (insert (a, p, x), add (v, p))
		      in SrcPathMap.foldli one (a, v) sources
		      end

		  and addGroup (p, x, gth, a, v) =
		      case gth () of
			  GG.ERRORGROUP => (a, v)
			| GG.GROUP { kind, sources, ... } =>
			    (case kind of
				 GG.LIB { kind, version } =>
				   (case kind of
					GG.STABLE _ =>
					  (insert0 (a, snam (p, version), x), v)
				      | GG.DEVELOPED d =>
 					  addSources (#subgroups d, sources,
						      insert (a, p, x), v))
			       | GG.NOLIB n =>
				   addSources (#subgroups n, sources,
					       insert (a, p, x), v))

		  val p = mkNativeSrcPath group
		  val gr = GroupReg.new ()
		  val x0 = { class = "cm", derived = false }
		  fun doit () =
		      case Parse.parse (parse_arg (gr, NONE, p)) of
			  SOME (g, _) =>
			    let val (sm, _) =
				    addGroup (p, x0, fn () => g,
					      StringMap.empty,
					      SrcPathSet.singleton p)
				fun add (f, { class, derived }, l) =
				    { file = f, class = class,
				      derived = derived } :: l
			    in SOME (StringMap.foldli add [] sm)
			    end
			| NONE => NONE
	      in doit () before dropPickles ()
	      end

	  fun mk_standalone sflag { setup, project, wrapper, target } = let
	      val hsfx = SMLofNJ.SysInfo.getHeapSuffix ()
	      fun extendTarget () =
		  OS.Path.joinBaseExt { base = target, ext = SOME hsfx }
	      val target =
		  case OS.Path.splitBaseExt target of
		      { base, ext = NONE } => extendTarget ()
		    | { base, ext = SOME e } =>
		      if e = hsfx then target else extendTarget ()
	      val spopt = Option.map mkNativeSrcPath setup
	      val pp = mkNativeSrcPath project
	      val wp = mkNativeSrcPath wrapper
	      val ts = TStamp.fmodTime target
	      val gr = GroupReg.new ()
	      fun do_cmfile p =
		  case Parse.parse (parse_arg (gr, NONE, p)) of
		      NONE => NONE
		    | SOME (g, gp) =>
		      if recomp_runner gp g then SOME (mkBootList g)
		      else NONE
	      val setup_list =
		  case spopt of
		      SOME sp => getOpt (do_cmfile sp, [])
		    | NONE => []
	      fun in_setup (i, _) =
		  List.exists (MkBootList.same_info i o #1) setup_list
	      fun do_wrapper () =
		  case do_cmfile wp of
		      NONE => NONE
		    | SOME l =>
		        SOME (map #2 (setup_list @
				      List.filter (not o in_setup) l))
	  in
	      (case Parse.parse (parse_arg (gr, sflag, pp)) of
		   NONE => NONE
		 | SOME (g, gp) =>
		   if isSome sflag orelse recomp_runner gp g then
		       case (ts, !(#youngest gp)) of
			   (TStamp.TSTAMP tgt_t, TStamp.TSTAMP src_t) =>
			   if Time.< (tgt_t, src_t) then do_wrapper ()
			   else SOME []
			 | _ => do_wrapper ()
		   else NONE)
	      before dropPickles ()
	  end

	  fun redump_heap s : unit =
	      SMLofNJ.Cont.throw (!HostBackend.Interact.redump_heap_cont) s

	  fun slave () = let
	      val gr = GroupReg.new ()
	      fun parse p = Parse.parse (slave_parse_arg (gr, NONE, p))
	  in
	      #set (SSV.symval "CM_SLAVE_MODE") (SOME 1);
	      #set (SSV.symval "NO_PLUGINS") (SOME 1);
	      Slave.slave { penv = penv,
			    parse = parse,
			    my_archos = my_archos,
			    sbtrav = Compile.newSbnodeTraversal,
			    make = makeNative }
	  end

	  (* This function works on behalf of the ml-build script.
	   * Having it here avoids certain startup-costs and also
	   * keeps ML code together.  (It used to be part of the
	   * script, but that proved difficult to maintain.) *)
	  fun mlbuild buildargs =
	      let fun doit (setup, root, cmfile, heap, listfile, linkargsfile) =
		      case mk_standalone NONE { setup = setup,
						project = root,
						wrapper = cmfile,
						target = heap } of
			  NONE => (Say.say ["Compilation failed.\n"];
				   OS.Process.failure)
			| SOME [] =>
			    (Say.say ["Heap was already up-to-date.\n"];
			     OS.Process.success)
			| SOME l => let
			      fun wrf (f, l) =
				  let val s = TextIO.openOut f
				      fun wr str =
					  TextIO.output (s, str ^ "\n")
				  in
				      app wr l;
				      TextIO.closeOut s
				  end

			      val s = TextIO.openOut listfile
			      fun wr str = TextIO.output (s, str ^ "\n")
			      val n = length l
			      fun maxsz (s, n) = Int.max (size s, n)
			      val m = foldl maxsz 0 l
			  in
			      wrf (listfile,
				   concat ["%", Int.toString n, " ",
					   Int.toString m]
				   :: l);
			      wrf (linkargsfile,
				   [concat [" @SMLboot=", listfile]]);
			      OS.Process.success
			  end handle _ => OS.Process.failure
	      in
		  OS.Process.exit
		      (case buildargs of
			   [root, cmfile, heap, listfile, linkargsfile] =>
			     doit (NONE, root, cmfile, heap, listfile,
				   linkargsfile)
			 | [setup,
			    root, cmfile, heap, listfile, linkargsfile] =>
			     doit (SOME setup, root, cmfile, heap, listfile,
				   linkargsfile)
			 | _ => (Say.say ["bad arguments to @CMbuild\n"];
				 OS.Process.failure))
	      end

	  fun al_ginfo () = { param = param false,
			      groupreg = al_greg,
			      errcons = EM.defaultConsumer (),
			      youngest = ref TStamp.ancient }

	  val al_managers =
	      AutoLoad.mkManagers { get_ginfo = al_ginfo,
				    dropPickles = dropPickles }

	  fun reset () =
	      (Compile.reset ();
	       Link.reset ();
	       AutoLoad.reset ();
	       Parse.reset ();
	       SmlInfo.reset ();
	       StabModmap.reset ())

	  fun initTheValues (bootdir, de, er, autoload_postprocess, icm) = let
	      (* icm: "install compilation manager" *)
	      val _ = let
		  fun listDir ds = let
		      fun loop l =
			  case F.readDir ds of
			      NONE => l
			    | SOME x => loop (x :: l)
		  in
		      loop []
		  end
		  val fileList = SafeIO.perform
		      { openIt = fn () => F.openDir bootdir,
		        closeIt = F.closeDir,
			work = listDir,
			cleanup = fn _ => () }
		  fun isDir x = F.isDir x handle _ => false
		  fun subDir x = let
		      val d = P.concat (bootdir, x)
		  in
		      if isDir d then SOME (x, d) else NONE
		  end
		  val pairList = List.mapPartial subDir fileList
		  fun sa (x, d) = SrcPath.set_anchor (penv, x, SOME d)
	      in
		  app sa pairList
	      end

	      val pidmapfile = P.concat (bootdir, BtNames.pidmap)

	      fun readpidmap s = let
		  fun loop m = let
		      fun enter (d, pids) = let
			  fun enter1 (spec, pm) = let
			      val fromHex = PersStamps.fromHex
			  in
			      case String.tokens (fn c => c = #":") spec of
				  [pos, hexp] =>
				  (case (fromHex hexp, Int.fromString pos) of
				       (SOME p, SOME i) =>
				       (case DE.look de p of
					    NONE => pm
					  | SOME obj =>
					    IM.insert (pm, i,
						       DE.singleton (p, obj)))
				     | _ => pm)
				| _ => pm
			  end
		      in
			  SrcPathMap.insert (m, SrcPath.decode penv d,
					     foldl enter1 IM.empty pids)
		      end
		  in
		      case TextIO.inputLine s of
			  NONE => m
			| SOME line =>
			    (case String.tokens Char.isSpace line of
				 d :: pids => loop (enter (d, pids))
			       | _ => loop m)
		  end
		  val m = loop SrcPathMap.empty
	      in
		  system_values := m
	      end

	      val _ =
		  SafeIO.perform { openIt = fn () => TextIO.openIn pidmapfile,
				   closeIt = TextIO.closeIn,
				   work = readpidmap,
				   cleanup = fn _ => () }

	      val initgspec = mkStdSrcPath BtNames.initgspec
	      val ginfo = { param = { fnpolicy = fnpolicy,
				      penv = penv,
				      symval = SSV.symval,
				      archos = my_archos,
				      keep_going = false,
				      slave_mode = false },
			    groupreg = GroupReg.new (),
			    errcons = EM.defaultConsumer (),
			    youngest = ref TStamp.ancient }
	      fun loadInitGroup () =
		  Stabilize.loadStable
		      { getGroup = fn _ =>
				      raise Fail "CMBoot: initial getGroup",
			anyerrors = ref false }
		      (ginfo, initgspec, NONE, [])
	  in
	      case loadInitGroup () of
		  NONE => raise Fail "CMBoot: unable to load init group"
		| SOME init_group => let
		      val _ = Compile.reset ()
		      val _ = Link.reset ()

		      val { exports = ctm, ... } =
			  Compile.newTraversal (fn _ => fn _ => (),
						fn _ => (),
						init_group)
		      val { exports = ltm, ... } = Link.newTraversal
			  (init_group, fn _ => raise Fail "init: get bfc?")

		      fun getSymTrav (tr_m, sy) =
			  case SymbolMap.find (tr_m, sy) of
			      NONE => raise Fail "init: bogus init group (1)"
			    | SOME tr => tr

		      val perv_ct = getSymTrav (ctm, PervAccess.pervStrSym)
		      val perv_lt = getSymTrav (ltm, PervAccess.pervStrSym)

		      fun doTrav t =
			  case t ginfo of
			      SOME r => r
			    | NONE => raise Fail "init: bogus init group (2)"

		      val { stat = pervstat } = doTrav perv_ct
		      val pervdyn = doTrav perv_lt

		      val pervasive = E.mkenv { static = pervstat, dynamic = pervdyn }

		      fun bare_autoload x =
			  (Say.say
			    ["!* ", x,
			     ": \"autoload\" not available, using \"make\"\n"];
			   makeStd x)
		      val bare_preload = Preload.preload {
                              make = makeStd,
                              autoload = bare_autoload
                            }
		      val standard_preload = Preload.preload {
                              make = makeStd,
                              autoload = autoload mkStdSrcPath
                            }
		  in
		      #set ER.pervasive pervasive;
		      #set (ER.loc ()) E.emptyEnv;(* redundant? *)
		      theValues := SOME { init_group = init_group };
		      case er of
			  BARE =>
			      (bare_preload BtNames.bare_preloads;
			       system_values := SrcPathMap.empty;
			       NONE)
			| AUTOLOAD =>
			      (icm al_managers;
			       standard_preload BtNames.standard_preloads;
			       (* unconditionally drop all library pickles *)
			       Parse.dropPickles ();
			       SOME (autoload_postprocess ()))
		  end
	  end
      end
  in
    fun init (bootdir, de, er, useStream, useFile, errorwrap, icm) = let
	fun procCmdLine () = let
	    val autoload' = errorwrap (ignore o autoload mkStdSrcPath)
	    val make' = errorwrap (ignore o makeStd)
            fun processFile (file, mk, ext) = (case ext
		  of ("sml" | "sig" | "fun") => useFile file
		   | "cm" => mk file
		   | _ => Say.say [
			"!* unable to process '", file, "' (unknown extension '", ext, "')\n"
		      ]
		  (* end case *))
	    fun inc n = n + 1
	    fun show_controls (getarg, getval, padval) level = let
		fun walk indent (ControlRegistry.RTree rt) = let
		    open FormatComb
		    val { help, ctls, subregs, path } = rt

		    fun one ci = let
			val arg = concat (foldr (fn (s, r) => s :: "." :: r)
						[getarg ci] path)
			val value = getval ci
			val sz = size value
			val lw = !Control_Print.lineWidth
			val padsz = lw - 6 - size arg - indent
		    in
			if padsz < sz then
			    let val padsz' = Int.max (lw, sz + 8 + indent)
			    in
				format' Say.say (sp (indent + 6) o
						 text arg o nl o
						 padval padsz' (text value) o
						 nl)
			    end
			else format' Say.say (sp (indent + 6) o
					      text arg o
					      padval padsz (text value) o
					      nl)
		    end
		in
		    case (ctls, subregs) of
			([], []) => ()
		      | _ => (format' Say.say
				      (sp indent o text help o text ":" o nl);
			      app one ctls;
			      app (walk (indent + 1)) subregs)
		end
	    in
		walk 2 (ControlRegistry.controls
			    (BasicControl.topregistry, Option.map inc level))
	    end

	  (* !!Important!!
           * This message should be kept in sync with the manpage (doc/src/man/sml.1.txt)
           *)
	    fun help level = (Say.say [
		     "\
		     \sml [rtsargs] [options] [files]\n\
		     \\n\
		     \  rtsargs:\n\
		     \    @SMLversion      (echo the version of SML/NJ then exit)\n\
		     \    @SMLwordsize     (echo the wordsze for the system then exit)\n\
		     \    @SMLsuffix       (echo the heap suffix for the system then exit)\n\
		     \    @SMLload=<h>     (load specified heap image)\n\
		     \    @SMLcmdname=<n>  (set command name)\n\
		     \    @SMLalloc=<s>    (specify size of allocation area)\n\
                     \    @SMLrun=<rt>     (specify runtime system)\n\
		     \    @SMLquiet        (load heap image silently)\n\
		     \    @SMLverbose      (show heap image load progress)\n\
		     \    @SMLobjects      (show list of executable objects)\n\
		     \    @SMLdebug=<f>    (write debugging info to file)\n\
		     \\n\
		     \  options:\n\
		     \    -D<name>=<v>     (set CM variable to given value)\n\
		     \    -D<name>         (set CM variable to 1)\n\
		     \    -Uname           (unset CM variable)\n\
		     \    -C<control>=<v>  (set named control)\n\
		     \    -H               (produce complete help listing)\n\
		     \    -h               (produce minimal help listing)\n\
		     \    -h<level>        (help with obscurity limit)\n\
		     \    -S               (list all current settings)\n\
		     \    -s<level>        (limited list of settings)\n\
		     \    -E               (list all environment variables)\n\
		     \    -e<level>        (list limited list of environment variables)\n\n\
		     \\n\
		     \  files:\n\
		     \    <file>.cm        (CM.make or CM.autoload)\n\
		     \    -m               (switch to CM.make)\n\
		     \    -a               (switch to CM.autoload; default)\n\
		     \    <file>.sig       (use)\n\
		     \    <file>.sml       (use)\n\
		     \    <file>.fun       (use)\n\
		     \"
		  ];
		show_controls (Controls.name o #ctl,
			       fn ci =>
				  concat ["(", #help (Controls.info (#ctl ci)),
					  ")"],
			       FormatComb.pad FormatComb.left)
			      level)

	    fun showcur level =
		show_controls (fn ci => (Controls.name (#ctl ci) ^ "="),
			       fn ci => Controls.get (#ctl ci),
			       fn _ => fn ff => ff)
			      level

	    fun show_envvars level =
		show_controls (fn ci => (Controls.name (#ctl ci) ^ ":"),
			       fn ci => Option.getOpt (#envName (#info ci),
						       "(none)"),
			       FormatComb.pad FormatComb.left)
			      level

	    fun badopt opt f () =
		Say.say ["!* bad ", opt, " option: `", f, "'\n",
			 "!* try `-h' or `-h<level>' for help\n"]

	    fun quit () = OS.Process.exit OS.Process.success

	    fun quit_if true = quit ()
	      | quit_if false = ()

	    fun carg (opt as ("-C" | "-D"), f, _, _) =
		let val bad = badopt opt f
		    val spec = Substring.extract (f, 2, NONE)
		    val is_config = opt = "-C"
		    val (name, value) =
			Substring.splitl (fn c => c <> #"=") spec
		    val name = Substring.string name
		    val value = Substring.string
				    (if Substring.size value > 0 then
					 Substring.slice (value, 1, NONE)
				     else value)
		in
		    if name = "" then bad ()
		    else if is_config then
			let val names = String.fields (fn c => c = #".") name
			    val look = ControlRegistry.control
					   BasicControl.topregistry
			in
			    case look names of
				NONE => Say.say ["!* no such control: ",
						 name, "\n"]
			      | SOME sctl =>
				(Controls.set (sctl, value)
				 handle Controls.ValueSyntax vse =>
					Say.say ["!* unable to parse value `",
						 #value vse, "' for ",
						 #ctlName vse, " : ",
						 #tyName vse, "\n"])
			end
		    else if value = "" then #set (SSV.symval name) (SOME 1)
		    else (case Int.fromString value of
			      SOME i => #set (SSV.symval name) (SOME i)
			    | NONE => bad ())
		end
	      | carg ("-U", f, _, _) =
		  (case String.extract (f, 2, NONE) of
		       "" => badopt "-U" f ()
		     | var => #set (SSV.symval var) NONE)
	      | carg ("-h", f, _, last) =
		  (case String.extract (f, 2, NONE) of
		       "" => help (SOME 0)
		     | level => help (Int.fromString level);
		   quit_if last)
	      | carg ("-s", f, _, last) =
		  (case String.extract (f, 2, NONE) of
		       "" => showcur (SOME 0)
		     | level => showcur (Int.fromString level);
		   quit_if last)
	      | carg ("-e", f, _, last) =
		  (case String.extract (f, 2, NONE) of
		       "" => show_envvars (SOME 0)
		     | level => show_envvars (Int.fromString level);
		   quit_if last)
	      | carg (_, f, mk, _) =
		  processFile (f, mk, String.map Char.toLower
				       (getOpt (OS.Path.ext f, "<none>")))

	    fun args ([], _) = ()
	      | args ("-a" :: _, _) = nextarg autoload'
	      | args ("-m" :: _, _) = nextarg make'
	      | args (["-H"], _) = (help NONE; quit ())
	      | args ("-H" :: _ :: _, mk) = (help NONE; nextarg mk)
	      | args (["-S"], _) = (showcur NONE; quit ())
	      | args ("-S" :: _ :: _, mk) = (showcur NONE; nextarg mk)
	      | args (["-E"], _) = (show_envvars NONE; quit ())
	      | args ("-E" :: _ :: _, mk) = (show_envvars NONE; nextarg mk)
	      | args ("@CMbuild" :: rest, _) = mlbuild rest
	      | args (["@CMredump", heapfile], _) = redump_heap heapfile
	      | args (f :: rest, mk) =
		  (carg (String.substring (f, 0, 2)
			 handle General.Subscript => "",
			 f, mk, List.null rest);
		   nextarg mk)

	    and nextarg mk =
		let val l = SMLofNJ.getArgs ()
		in SMLofNJ.shiftArgs (); args (l, mk)
		end
	in
	    case SMLofNJ.getArgs () of
		["@CMslave"] => (#set StdConfig.verbose false; slave ())
	      | l => (SMLofNJ.shiftArgs (); args (l, autoload'))
	end
    in
	useStreamHook := useStream;
	initTheValues (bootdir, de, er,
		       fn () => (Cleanup.install initPaths;
				 procCmdLine),
		       icm)
    end

    structure CM = struct
	type 'a controller = { get : unit -> 'a, set : 'a -> unit }

	structure Anchor = struct
	    fun anchor a = { get = getAnchor a, set = setAnchor a }
	    val reset = resetPathConfig
	end

	structure Control = struct
	    val keep_going = StdConfig.keep_going
	    val verbose = StdConfig.verbose
	    val parse_caching = StdConfig.parse_caching
	    val warn_obsolete = StdConfig.warn_obsolete
	    val debug = StdConfig.debug
	    val conserve_memory = StdConfig.conserve_memory
	    val generate_index = StdConfig.generate_index
	end

	structure Library = struct
	    type lib = SrcPath.file
	    val known = Parse.listLibs
	    val descr = SrcPath.descr
	    val osstring = SrcPath.osstring
	    val dismiss = Parse.dismissLib
	    fun unshare lib = (Link.unshare lib; dismiss lib)
	end

	structure State = struct
	    val synchronize = SrcPath.sync
	    val reset = reset
	    val pending = getPending
	    val showBindings = showBindings
	end

	structure Server = struct
	    type server = Servers.server_handle
	    fun start x = Servers.start x
			  before SrcPath.scheduleNotification ()
	    val stop = Servers.stop
	    val kill = Servers.kill
	    val name = Servers.name
	end

        (* note that we use the "standard" path syntax during the bootstrapping
         * process, but switch to native paths for the use of CM in the REPL.
         *)
	val autoload = autoload mkNativeSrcPath
	val make = makeNative
	val recomp = recomp mkNativeSrcPath
	val stabilize = stabilize mkNativeSrcPath

	val sources = sources

	val symval = SSV.symval
	val load_plugin = cwd_load_plugin
	val mk_standalone = mk_standalone

	structure Graph = struct
	    val graph = to_portable
	end

	val cm_dir_arc = FilenamePolicy.cm_dir_arc

	val redump_heap = redump_heap
    end

    structure Tools = ToolsFn (val load_plugin' = load_plugin'
			       val penv = penv)

    val load_plugin = load_plugin
  end
end (* end functor *)
