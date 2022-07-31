(*
 * Compilation traversals.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
local
    structure GP = GeneralParams
    structure DG = DependencyGraph
    structure GG = GroupGraph
    structure SE = StaticEnv
    structure Pid = PersStamps
    structure PP = PrettyPrint
    structure EM = ErrorMsg
    structure SF = SmlFile

    type pid = Pid.persstamp
    type statenv = StaticEnv.staticEnv
    type result = { stat: statenv }
    type ed = IInfo.info
in
    signature COMPILE = sig

	type bfc
	type stats

	(* reset internal persistent state *)
	val reset : unit -> unit

	(* notify linkage module about recompilation *)
	type notifier = GP.info -> SmlInfo.info -> unit

	(* type of a function to store away the binfile contents *)
	type bfcReceiver =
	     SmlInfo.info * { contents: bfc, stats: stats } -> unit

	val getII : SmlInfo.info -> IInfo.info

	val evictStale : unit -> unit
	val evictAll : unit -> unit

	val newSbnodeTraversal : unit -> DG.sbnode -> GP.info -> ed option

	val newTraversal : notifier * bfcReceiver * GG.group ->
	    { group: GP.info -> result option,
	      allgroups: GP.info -> bool,
	      exports: (GP.info -> result option) SymbolMap.map }
    end

    functor CompileFn (structure Backend : BACKEND
		       structure StabModmap : STAB_MODMAP
		       val useStream : TextIO.instream -> unit
		       val compile_there : SrcPath.file -> bool) :>
	COMPILE where type bfc = Binfile.bfContents
	        where type stats = Binfile.stats =
    struct

	type notifier = GP.info -> SmlInfo.info -> unit

	structure BF = Binfile
	structure C = Backend.Compile

	type bfc = BF.bfContents
	type stats = BF.stats

	type bfcReceiver =
	     SmlInfo.info * { contents: bfc, stats: stats } -> unit

	structure FilterMap = MapFn
	    (struct
		type ord_key = pid * SymbolSet.set
		fun compare ((u, f), (u', f')) =
		    case Pid.compare (u, u') of
			EQUAL => SymbolSet.compare (f, f')
		      | unequal => unequal
	    end)

	type env = { envs: unit -> result, pids: PidSet.set }
	type envdelta = IInfo.info

	type memo = { ii: IInfo.info, ts: TStamp.t, cmdata: PidSet.set }

        (* version info for binfiles *)
        val version = BF.mkVersion {
                arch = Backend.architecture,
                smlnjVersion = SMLNJVersion.version'
              }

	(* persistent state! *)
	val filtermap = ref (FilterMap.empty: pid FilterMap.map)

	(* more persistent state! *)
	val globalstate = ref (SmlInfoMap.empty: memo SmlInfoMap.map)

	fun reset () =
	    (filtermap := FilterMap.empty;
	     globalstate := SmlInfoMap.empty)

	fun isValidMemo (memo: memo, provided, smlinfo) =
	    not (TStamp.needsUpdate { source = SmlInfo.lastseen smlinfo,
				      target = #ts memo })
	    andalso PidSet.equal (provided, #cmdata memo)

	fun memo2ii (memo: memo) = #ii memo

	fun memo2ed memo = memo2ii memo

	fun bfc2memo (bfc, ts, context_senv) = let
	    fun statenv () =
		let val mm0 = StabModmap.get ()
		    val m = GenModIdMap.mkMap' (context_senv, mm0)
		    fun context _ = (m, fn () => "bfc2memo")
		    val { pid, pickle } = BF.senvPickleOf bfc
		in UnpickMod.unpickleEnv context (pid, pickle)
		end
	    val ii = { statenv = Memoize.memoize statenv,
		       statpid = BF.staticPidOf bfc,
		       guid = BF.guidOf bfc }
	    val cmdata = PidSet.addList (PidSet.empty, BF.cmDataOf bfc)
	in
	    { ii = ii, ts = ts, cmdata = cmdata }
	end

	fun pidset pid = PidSet.singleton pid

	fun nofilter (ed: envdelta) = let
	    val { statenv, statpid, guid } = ed
	    val statenv' = Memoize.memoize statenv
	in
	    { envs = fn () => { stat = statenv' () },
	      pids = pidset statpid }
	end

	fun requiredFiltering set se = let
	    val dom = SymbolSet.addList (SymbolSet.empty,
					 BrowseStatEnv.catalog se)
	    val filt = SymbolSet.intersection (set, dom)
	in
	    if SymbolSet.equal (dom, filt) then NONE
	    else SOME filt
	end

	fun filter (ii, s) = let
	    val { statenv, statpid, guid } = ii
	    val ste = statenv ()
	in
	    case requiredFiltering s ste of
		NONE => { envs = fn () => { stat = ste },
			  pids = pidset statpid }
	      | SOME s => let
		    val ste' = SE.filter (ste, SymbolSet.listItems s)
		    val key = (statpid, s)
		    val statpid' =
			case FilterMap.find (!filtermap, key) of
			    SOME statpid' => statpid'
			  | NONE => let
				val statpid' = Rehash.rehash
					{ env = ste', orig_pid = statpid,
					  guid = guid }
			    in
				filtermap :=
				  FilterMap.insert (!filtermap, key, statpid');
				statpid'
			    end
		in
		    { envs = fn () => { stat = ste' },
		      pids = pidset statpid' }
		end
	end

	fun rlayer ({ stat }, { stat = stat' }) =
	    { stat = SE.consolidateLazy (SE.atop (stat, stat')) }

	val emptyEnv =
	    { envs = fn () => { stat = SE.empty }, pids = PidSet.empty }

	fun layer ({ envs = e, pids = p }, { envs = e', pids = p' }) =
	    { envs = fn () => rlayer (e (), e' ()), pids = PidSet.union (p, p') }

	(* I would rather not use an exception here, but short of a better
	 * implementation of concurrency I see no choice.
	 * The problem is that at each node we sequentiallay wait for the
	 * children nodes.  But the scheduler might (and probably will)
	 * let a child run that we are not currently waiting for, so an
	 * error there will not result in "wait" to immediately return
	 * as it should for clean error recovery.
	 * Using the exception avoids having to implement a
	 * "wait for any child -- whichever finishes first" kind of call. *)
	exception Abort

	fun layer'wait u (p, NONE) =
	    (ignore (Concur.waitU u p); NONE)
	  | layer'wait u (p, SOME e) =
	    (case Concur.waitU u p of
		 SOME e' => SOME (layer (e', e))
	       | NONE => NONE)

	fun mkTraversal (notify, storeBFC, getUrgency) = let
	    val localstate = ref SmlInfoMap.empty

	    fun storeBFC' (gp, i, x) = let
		val src = SmlInfo.sourcepath i
		val c = #contents x
	    in
		storeBFC (i, x)
	    end


	    fun sbnode gp (DG.SB_SNODE n) = snode gp n
	      (* The beauty of this scheme is that we don't have
	       * to do anything at all for SB_BNODEs:  Everything
	       * is prepared ready to be used when the library
	       * is unpickled: *)
	      | sbnode gp (DG.SB_BNODE (_, ii, _)) = SOME ii

	    and fsbnode gp (f, n) =
		case (sbnode gp n, f) of
		    (NONE, _) => NONE
		  | (SOME d, NONE) => SOME (nofilter d)
		  | (SOME d, SOME s) => SOME (filter (d, s))

	    and snode gp (DG.SNODE n) = let
		val youngest = #youngest gp
		val { smlinfo = i, localimports = li, globalimports = gi } = n
		val binname = SmlInfo.binname i
		val descr = SmlInfo.descr i

		fun pstats (s: BF.stats) = let
		    fun info ((sel, lab), (l, t)) =
			case sel s of
			    0 => (l, t)
			  | n => (lab :: ": " :: Int.toString n ::
				  t :: " " :: l,
				  ",")
		in
		    Say.vsay ("[" :: #1 (foldr info
					       (["bytes]\n"], "")
					       [(#code, "code"),
						(#data, "data"),
						(#env, "env")]))
		end

		fun loaded _ = Say.vsay ["[loading ", descr, "]\n"]
		fun received s =
		    (Say.vsay ["[receiving ", descr, "]\n"];
		     pstats s)

		fun fail () =
		    if #keep_going (#param gp) then NONE else raise Abort

		fun compile_here (stat, pids) = let
		    fun perform_setup _ NONE = ()
		      | perform_setup what (SOME code) =
			(Say.vsay ["[setup (", what, "): ", code, "]\n"];
			 SafeIO.perform
			     { openIt = fn () => TextIO.openString code,
			       closeIt = TextIO.closeIn,
			       work = useStream,
			       cleanup = fn _ => () })
		    fun save bfc = let
			fun writer s = let
			      val s = BF.write { stream = s, contents = bfc, nopickle = false }
			      in
                                pstats s; s
			      end
			fun cleanup _ = OS.FileSys.remove binname handle _ => ()
		    in
			notify gp i;
			(SafeIO.perform { openIt =
				           fn () => AutoDir.openBinOut binname,
					  closeIt = BinIO.closeOut,
					  work = writer,
					  cleanup = cleanup }
			 before TStamp.setTime (binname, SmlInfo.lastseen i))
			handle exn => let
			    fun ppb pps =
				(PP.newline pps;
				 PP.string pps (General.exnMessage exn))
			in
			    SmlInfo.error gp i EM.WARN
					  ("failed to write " ^ binname) ppb;
			    { code = 0, env = 0, data = 0 }
			end
		    end (* save *)
		in
		    case SmlInfo.parsetree gp i of
			NONE => fail ()
		      | SOME (ast, source) => let
			    val ast =
				case #explicit_core_sym (SmlInfo.attribs i) of
				    NONE => ast
				  | SOME sy => CoreHack.rewrite (ast, sy)
			    val cmData = PidSet.listItems pids
			    val (pre, post) = SmlInfo.setup i
			    val controllers = SmlInfo.controllers i
			    val topLevel = EnvRef.loc ()
			    val orig_settings =
				map (fn c => #save'restore c ()) controllers
(*
			    val orig_toplenv = #get topLevel ()
*)
			    fun reset _ =
				((* #set topLevel orig_toplenv; *)
				 app (fn r => r ()) orig_settings)
			    fun work () = let
				val _ = map (fn c => #set c ()) controllers
				val _ = perform_setup "pre" pre
				(* clear error flag (could still be set from
				 * earlier run) *)
				val _ = #anyErrors source := false
				(* we actually run the compiler here;
				 * Binfile is not doing it anymore *)
				val err = EM.errors source
				fun check phase =
				    if EM.anyErrors err then
					raise CompileExn.Compile
						  (phase ^ " failed")
				    else ()
				val cinfo = C.mkCompInfo { source = source,
							   transform = fn x => x }
				val guid = SmlInfo.guid i
				val { csegments, newstatenv, exportPid,
				      staticPid, imports, pickle = senvP,
				      ... } =
				    C.compile { source = source, ast = ast,
						statenv = stat,
						compInfo = cinfo, checkErr = check,
						guid = guid }
				val bfc = BF.create {
                                        version = version,
				        imports = imports,
                                        exportPid = exportPid,
                                        cmData = cmData,
                                        senv = { pickle = senvP, pid = staticPid },
                                        guid = guid,
                                        csegments = csegments
                                      }
				val memo = bfc2memo (bfc, SmlInfo.lastseen i, stat)
                                in
                                  perform_setup "post" post;
                                  reset ();
                                  storeBFC' (gp, i,
                                             { contents = bfc,
                                               stats = save bfc });
                                  SOME memo
                                end
			in
			    SafeIO.perform { openIt = fn () => (),
					     work = work,
					     closeIt = fn () => (),
					     cleanup = reset }
			end handle (EM.Error | CompileExn.Compile _)
				   (* At this point we handle only
				    * explicit compiler bugs and ordinary
				    * compilation errors because for those
				    * there will already have been
				    * explanatory messages.  Everything
				    * else "falls through" and will be
				    * treated at top level. *)
				   => fail ()
		end (* compile_here *)
		fun notlocal () = let
		    val _ = youngest := TStamp.max (!youngest,
						    SmlInfo.lastseen i)
		    val urgency = getUrgency i
		    (* Ok, it is not in the local state, so we first have
		     * to traverse all children before we can proceed... *)
		    fun loc li_n = Option.map nofilter (snode gp li_n)
		    fun glob gi_n = fsbnode gp gi_n
		    val gi_cl =
			map (fn gi_n => Concur.fork (fn () => glob gi_n)) gi
		    val li_cl =
			map (fn li_n => Concur.fork (fn () => loc li_n)) li
		    val e =
			foldl (layer'wait urgency)
			      (foldl (layer'wait urgency)
			             (SOME emptyEnv)
				     gi_cl)
			      li_cl
		in
		    case e of
			NONE => NONE
		      | SOME { envs, pids } => let
			    (* We have successfully traversed all
			     * children.  Now it is time to check the
			     * global map... *)
			    fun fromfile () = let
				val { stat } = envs ()
				val { extra_compenv, ... } = SmlInfo.attribs i
				val stat =
				    case extra_compenv of
					NONE => stat
				      | SOME s => SE.atop (stat, s)
				fun load () = let
				    val ts = TStamp.fmodTime binname
				    fun openIt () = BinIO.openIn binname
				    fun reader s = let
					val mm0 = StabModmap.get ()
					val m = GenModIdMap.mkMap' (stat, mm0)
					val { contents, stats } =
					    BF.read { stream = s, version = version }
				    in
					SmlInfo.setguid (i, BF.guidOf contents);
					(contents, ts, stats)
				    end
				in
				    SOME (SafeIO.perform
					  { openIt = openIt,
					    closeIt = BinIO.closeIn,
					    work = reader,
					    cleanup = fn _ => () })
				    handle _ => NONE
				end (* load *)
				fun tryload (sync, report, otherwise) =
				    case (sync (); load ()) of
					NONE => otherwise ()
				      | SOME (bfc, ts, stats) => let
					    val memo = bfc2memo (bfc, ts, stat)
					    val contst = { contents = bfc, stats = stats }
					in
					    if isValidMemo (memo, pids, i) then
						(notify gp i;
						 report stats;
						 storeBFC' (gp, i, contst);
						 SOME memo)
					    else otherwise ()
					end
				fun sy0 () = ()
				fun bottleneck () =
				    (* Are we the only runable task? *)
				    Servers.allIdle () andalso
				    Concur.noTasks ()
				fun compile_again () =
				    (Say.vsay ["[compiling ", descr, "]\n"];
				     compile_here (stat, pids))
				fun compile_there' p =
				    not (bottleneck ()) andalso
				    compile_there p
				fun compile () = let
				    val sp = SmlInfo.sourcepath i
				    fun sy () = let
					fun ready () =
					    OS.FileSys.fileSize binname > 0
					    handle _ => false
				    in
					(***** busy wait for file to appear;
					 * this is obviously very bad! *)
					while not (ready ()) do ()
				    end
				in
				    OS.FileSys.remove binname handle _ => ();
				    youngest := TStamp.NOTSTAMP;
				    if compile_there' sp then
					tryload (sy, received, compile_again)
				    else compile_again ()
				end
			    in
				(* If anything goes wrong loading the first
				 * time, we go and compile.  Compiling
				 * may mean compiling externally, and if so,
				 * we must load the result of that.
				 * If the second load also goes wrong, we
				 * compile locally to gather error messages
				 * and make everything look "normal". *)
				tryload (sy0, loaded, compile)
			    end (* fromfile *)
			    fun notglobal () =
				case fromfile () of
				    NONE => NONE
				  | SOME memo =>
					(globalstate :=
					 SmlInfoMap.insert (!globalstate, i,
							    memo);
					 SOME memo)
			in
			    case SmlInfoMap.find (!globalstate, i) of
				NONE => notglobal ()
			      | SOME memo =>
				    if isValidMemo (memo, pids, i) then
					SOME memo
				    else notglobal ()
			end
		end (* notlocal *)
	    in
		(* Here we just wait (no "waitU") so we don't get
		 * priority over threads that may have to clean up after
		 * errors. *)
		case SmlInfoMap.find (!localstate, i) of
		    SOME mopt_c => Option.map memo2ed (Concur.wait mopt_c)
		  | NONE => let
			val mopt_c = Concur.fork
			    (fn () => notlocal () before
			     (* "Not local" means that we have not processed
			      * this file before.  Therefore, we should now
			      * remove its parse tree... *)
			     SmlInfo.forgetParsetree i)
		    in
			localstate :=
			    SmlInfoMap.insert (!localstate, i, mopt_c);
			Option.map memo2ed (Concur.wait mopt_c)
		    end
	    end (* snode *)

	    fun impexp gp (nth, _, _) = fsbnode gp (nth ())
	in
	    { sbnode = sbnode, impexp = impexp }
	end

	fun newTraversal (_, _, GG.ERRORGROUP) =
	    { group = fn _ => NONE,
	      allgroups = fn _ => false,
	      exports = SymbolMap.empty }
	  | newTraversal (notify, storeBFC, g as GG.GROUP grec) = let
		val { exports, ... } = grec
		val um = Memoize.memoize (fn () => Indegree.indegrees g)
		fun getUrgency i = getOpt (SmlInfoMap.find (um (), i), 0)
		(* generate the traversal -- lazily *)
		val impexpth =
		    Memoize.memoize
			(fn () =>
			    #impexp
				(mkTraversal (notify, storeBFC, getUrgency)))

		fun many (gp, iel) = let
		    val eo_cl =
			map (fn x => Concur.fork (fn () => impexpth () gp x))
		            iel
		    val eo = foldl (layer'wait 0) (SOME emptyEnv) eo_cl
		in
		    case eo of
			NONE => (Servers.reset false; NONE)
		      | SOME e => SOME (#envs e ())
		end handle Abort => (Servers.reset false; NONE)

		fun group gp = many (gp, SymbolMap.listItems exports)

		fun allgroups gp = let
		    fun addgroup ((_, th, _), gl) = th () :: gl
		    fun collect ([], _, l) = l
		      | collect (GG.ERRORGROUP :: gl, done, l) =
			collect (gl, done, l)
		      | collect (GG.GROUP g :: gl, done, l) =
			if SrcPathSet.member (done, #grouppath g) then
			    collect (gl, done, l)
			else
			    collect (foldl addgroup gl (#sublibs g),
				     SrcPathSet.add (done, #grouppath g),
				     SymbolMap.foldl (op ::) l (#exports g))
		    val l = collect ([g], SrcPathSet.empty, [])
		in
		    isSome (many (gp, l))
		end

		fun mkExport ie gp =
		    case impexpth () gp ie handle Abort => NONE of
			NONE => (Servers.reset false; NONE)
		      | SOME e => SOME (#envs e ())
	    in
		{ group = group,
		  allgroups = allgroups,
		  exports = SymbolMap.map mkExport exports }
	    end

	fun newSbnodeTraversal () = let
	    val { sbnode, ... } =
		mkTraversal (fn _ => fn _ => (), fn _ => (), fn _ => 0)
	    fun sbn_trav n gp = let
		val r = sbnode gp n handle Abort => NONE
	    in
		if isSome r then () else Servers.reset false;
		r
	    end
	in
	    sbn_trav
	end

	fun evictStale () =
	    globalstate :=
	      SmlInfoMap.filteri (SmlInfo.isKnown o #1) (!globalstate)

	fun evictAll () = globalstate := SmlInfoMap.empty

	fun getII i = memo2ii (valOf (SmlInfoMap.find (!globalstate, i)))
    end
end
