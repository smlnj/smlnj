(*
 * The CM autoloading mechanism.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
local
    structure GP = GeneralParams
    structure DG = DependencyGraph
    structure ER = EnvRef
    structure GG = GroupGraph
    structure E = Environment
    structure EM = ErrorMsg
in
signature AUTOLOAD = sig

    val register : ER.envref * GG.group -> unit

    val mkManagers : { get_ginfo: unit -> GP.info, dropPickles: unit -> unit }
		     -> { manageImport: Ast.dec * ER.envref -> unit,
			  managePrint : Symbol.symbol * ER.envref -> unit,
			  getPending : unit -> Symbol.symbol list }

    val getPending : unit -> DG.impexp SymbolMap.map

    val reset : unit -> unit
end

functor AutoLoadFn (structure L : LINK
		    structure BFC : BFC
		    structure C : COMPILE where type stats = BFC.stats
		    sharing type C.bfc = L.bfc = BFC.bfc) :> AUTOLOAD = struct

    structure SE = StaticEnv

    type traversal = GP.info -> E.environment option
    (* We let the topLevel env *logically* sit atop the pending
     * autoload bindings.  This way we do not have to intercept every
     * change to the topLevel env.  However, it means that any addition
     * to "pending" must be subtracted from the topLevel env. *)
    val pending = ref (SymbolMap.empty: (DG.impexp * traversal) SymbolMap.map)

    fun reset () = pending := SymbolMap.empty

    fun register (_, GG.ERRORGROUP) = ()
      | register (ter: ER.envref, g as GG.GROUP { exports, ... }) =
	let val te = #get ter ()
	    (* toplevel bindings (symbol set) ... *)
	    val tss = foldl SymbolSet.add' SymbolSet.empty
			    (BrowseStatEnv.catalog (E.staticPart te))
	    (* "new" bindings (symbol set) ... *)
	    val nss = SymbolMap.foldli (fn (i, _, s) => SymbolSet.add (s, i))
				       SymbolSet.empty exports
	    (* to-be-retained bindings ... *)
	    val rss = SymbolSet.difference (tss, nss)
	    (* getting rid of unneeded bindings... *)
	    val te' = E.filterEnv (te, SymbolSet.listItems rss)
	    (* make traversal states *)
	    val { store, get } = BFC.new ()
	    val { exports = cTrav, ... } = C.newTraversal (L.evict, store, g)
	    val { exports = lTrav, ... } = L.newTraversal (g, #contents o get)
	    fun combine (ss, d) gp =
		case ss gp of
		    SOME { stat } =>
		    (case d gp of
			 SOME dyn => SOME (E.mkenv { static = stat, dynamic = dyn })
		       | NONE => NONE)
		  | NONE => NONE
	    fun mkNode (sy, ie) =
		(ie, combine (valOf (SymbolMap.find (cTrav, sy)),
			      valOf (SymbolMap.find (lTrav, sy))))
	    val newNodes = SymbolMap.mapi mkNode exports
	in
	    #set ter te';
	    pending := SymbolMap.unionWith #1 (newNodes, !pending)
	end

    fun mkManagers { get_ginfo, dropPickles } = let
	(* manage a list of symbols for which modules need to be loaded *)
	fun manage (genloadmap, ter: ER.envref, quiet) = let
	    val gp = get_ginfo ()

	    fun loadit m = let
		fun one ((_, tr), NONE) = NONE
		  | one ((_, tr), SOME e) =
		    (case tr gp of
			 NONE => NONE
		       | SOME e' => SOME (E.concatEnv (e', e)))
	    in
		(* make sure that there are no stale values around... *)
		L.cleanup gp;
		SymbolMap.foldl one (SOME E.emptyEnv) m
	    end

	    val te = #get ter ()
	    val ste = E.staticPart te

	    (* First, we get rid of anything in "pending" that has
	     * meanwhile been added to the toplevel. *)
	    fun notTopDefined (sy, _) =
		(SE.look (ste, sy); false) handle SE.Unbound => true
	    val pend = SymbolMap.filteri notTopDefined (!pending)
	    val _ = pending := pend

	    val loadmap0 = genloadmap pend

	    (* However, we want to avoid hanging on to stuff unnecessarily, so
	     * we now look for symbols that become available "for free" because
	     * their corresponding node has been picked. *)

	    fun add (((_, _, ss), _), allsyms) = SymbolSet.union (ss, allsyms)

	    val pickedsyms = SymbolMap.foldl add SymbolSet.empty loadmap0

	    fun isPicked ((_, _, ss), _) =
		not (SymbolSet.isEmpty (SymbolSet.intersection (ss, pickedsyms)))

	    val loadmap = SymbolMap.filter isPicked pend
	    val noloadmap = SymbolMap.filter (not o isPicked) pend
	in
	    if SymbolMap.isEmpty loadmap then ()
	    else
		(SrcPath.revalidateCwd ();
		 (* We temporarily turn verbosity off, so we need to wrap this
		  * with a SafeIO.perform... *)
		 SafeIO.perform
		     { openIt = fn () => #get StdConfig.verbose () before
					 #set StdConfig.verbose false,
	               closeIt = #set StdConfig.verbose, (* just in case *)
		       cleanup = fn _ => (),
		       work = fn orig_verbosity =>
				 (case loadit loadmap of
				      SOME e =>
				      (#set ter (E.concatEnv (e, te));
				       pending := noloadmap;
				       #set StdConfig.verbose orig_verbosity;
				       if not quiet then
					   Say.vsay ["[autoloading done]\n"]
				       else ())
				    | NONE => raise Fail "unable to load module(s)") }
		     handle Fail msg =>
			    Say.say ["[autoloading failed: ", msg, "]\n"];
		 dropPickles ())
	end

	fun mkAnnounce () =  let
	    val announced = ref false
	in
	    fn () =>
	       (if !announced then ()
		else (announced := true;
		      Say.vsay ["[autoloading]\n"]))
	end

	fun manageImports (ast, ter: ER.envref) = let
	    val { skeleton, ... } =
		SkelCvt.convert { tree = ast,
				  err = fn _ => fn _ => fn _ => () }
	    fun genloadmap pend = let
		val te = #get ter ()
		val ste = E.staticPart te
		val (dae, _) = Statenv2DAEnv.cvt ste
		val load = ref SymbolMap.empty
		val announce = mkAnnounce ()
		fun lookpend sy = let
		    fun otherwise _ = EM.impossible "Autoload:lookpend"
		in
		    case SymbolMap.find (pend, sy) of
			SOME (x as ((_, e, _), _)) =>
			(announce ();
			 load := SymbolMap.insert (!load, sy, x);
			 BuildDepend.look otherwise e sy)
		      | NONE => DAEnv.EMPTY
		end
		val lookimport = BuildDepend.look lookpend dae
		val _ = BuildDepend.processOneSkeleton lookimport skeleton
	    in
		!load
	    end
	in
	    manage (genloadmap, ter, false)
	end

	fun managePrint (sy, ter) = let
	    fun genloadmap pend = let
		(* val announce = mkAnnounce () *)
		fun announce () = ()	(* should not announce in the
					 * middle of pretty-printing... *)
	    in
		case SymbolMap.find (pend, sy) of
		    SOME x => (announce ();
			       SymbolMap.singleton (sy, x))
		  | NONE => SymbolMap.empty
	    end
	in
	    manage (genloadmap, ter, true)
	end
    in
	{ manageImport = manageImports,
	  managePrint = managePrint,
	  getPending = fn () => SymbolMap.listKeys (!pending) }
    end

    fun getPending () = SymbolMap.map #1 (!pending)
end
end
