(*
 * Link traversals.
 *   - manages shared state
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
local
    structure GP = GeneralParams
    structure DG = DependencyGraph
    structure GG = GroupGraph
    structure DE = DynamicEnv
    structure EM = ErrorMsg
    structure PP = PrettyPrint

    type env = DynamicEnv.env
    type posmap = env IntMap.map
in
    signature LINK = sig

	type bfc
	type bfcGetter = SmlInfo.info -> bfc

	(* Evict value from cache if it exists *)
	val evict : GP.info -> SmlInfo.info -> unit

	val evictStale : unit -> unit

	(* Check all values and evict those that depended on other
	 * meanwhile evicted ones. *)
	val cleanup : GP.info -> unit

	val newTraversal : GG.group * bfcGetter ->
	    { group: GP.info -> env option,
	      exports: (GP.info -> env option) SymbolMap.map }

	(* discard persistent state *)
	val reset : unit -> unit

	(* discard persistent state for a specific stable library *)
	val unshare : SrcPath.file -> unit
    end

    functor LinkFn (structure BFC : BFC where type bfc = Binfile.bfContents
		    val system_values : posmap SrcPathMap.map ref) :>
	    LINK where type bfc = BFC.bfc =
    struct

	exception Link of exn

	structure BF = Binfile
	structure EX = Execute

	type bfc = BF.bfContents
	type bfcGetter = SmlInfo.info -> bfc

	type bfun = GP.info -> env -> env

	datatype bnode =
	    B of bfun * BinInfo.info * bnode list

	val stablemap = ref (StableMap.empty: bnode StableMap.map)

	type smemo = env * SmlInfo.info list

	val smlmap = ref (SmlInfoMap.empty: smemo SmlInfoMap.map)

	val emptyStatic = StaticEnv.empty
	val emptyDyn = DynamicEnv.empty

	fun evict gp i = let
	    fun check () =
		case SmlInfo.sh_mode i of
		    Sharing.SHARE true =>
			SmlInfo.error gp i EM.WARN
			(concat ["sharing for ", SmlInfo.descr i,
				 " may be lost"])
			EM.nullErrorBody
		  | _ =>  ()
	in
	    (smlmap := #1 (SmlInfoMap.remove (!smlmap, i))
	     before check ())
	    handle LibBase.NotFound => ()
	end

	fun evictStale () =
	    smlmap := SmlInfoMap.filteri (SmlInfo.isKnown o #1) (!smlmap)

	fun cleanup gp = let
	    val visited = ref SmlInfoSet.empty
	    fun visit i =
		if SmlInfoSet.member (!visited, i) then true
		else
		    case SmlInfoMap.find (!smlmap, i) of
			NONE => false
		      | SOME (_, l) => let
			    val bl = map visit l
			    val b = List.all (fn x => x) bl
			in
			    if b then
				(visited := SmlInfoSet.add (!visited, i);
				 true)
			    else (evict gp i; false)
			end
	in
	    app (ignore o visit o #1) (SmlInfoMap.listItemsi (!smlmap))
	end

	fun newTraversal0 (GG.ERRORGROUP, _) =
	    { group = fn _ => NONE, exports = SymbolMap.empty }
	  | newTraversal0 (group as GG.GROUP grec, getBFC) = let

	    val { exports, grouppath, ... } = grec

	    fun exn_err (msg, error, descr, exn) = let
		fun ppb pps =
		    (PP.newline pps;
		     PP.string pps (General.exnMessage exn);
		     app (fn s => PP.string pps (s ^ "\n"))
			 (SMLofNJ.exnHistory exn);
		     PP.newline pps)
	    in
		error (concat [msg, " ", descr]) ppb;
		raise Link exn
	    end

	    fun link_stable (i, de) = let
		val stable = BinInfo.stablename i
		val os = BinInfo.offset i
		val descr = BinInfo.describe i
		val error = BinInfo.error i EM.COMPLAIN
		val bfc = BFC.getStable { stable = stable, offset = os,
					  descr = descr }
		    handle exn => exn_err ("unable to load library module",
					   error, descr, exn)
	    in
		BF.exec (bfc, de, Link)
		handle Link exn =>
		    exn_err ("link-time exception in library code",
			     error, descr, exn)
	    end

	    fun link_sml (gp, i, getBFC, getE, snl) = let
		fun fresh () = let
		    val bfc = getBFC i
		in
		    case getE gp of
			NONE => NONE
		      | SOME e =>
			    (SOME (BF.exec (bfc, e, Link))
			     handle Link exn =>
				exn_err ("link-time exception in user program",
					 SmlInfo.error gp i EM.COMPLAIN,
					 SmlInfo.descr i,
					 exn))
		end handle exn as Link _ => raise exn
			 | _ => NONE
	    in
		case SmlInfo.sh_mode i of
		    Sharing.SHARE _ =>
			(case SmlInfoMap.find (!smlmap, i) of
			     NONE =>
				 (case fresh () of
				      NONE => NONE
				    | SOME de => let
					  val m = (de, snl)
				      in
					  smlmap :=
					    SmlInfoMap.insert (!smlmap, i, m);
					  SOME de
				      end)
			   | SOME (de, _) => SOME de)
		  | Sharing.DONTSHARE => (evict gp i; fresh ())
	    end

	    val visited = ref SrcPathSet.empty

	    fun registerGroup GG.ERRORGROUP = ()
	      | registerGroup (g as GG.GROUP grec) = let
		    val { grouppath, kind, sublibs, ... } = grec
		    fun registerSublib NONE = ()
		      | registerSublib (SOME i) =
			registerGroup (#2 (List.nth (sublibs, i)) ())
		    fun registerStableLib GG.ERRORGROUP = ()
		      | registerStableLib (GG.GROUP sg) = let
			    val { exports, grouppath = sgp, ... } = sg
			    val posmap =
				let val (m', pm) =
					SrcPathMap.remove (!system_values, sgp)
				in system_values := m'; pm
				end handle LibBase.NotFound => IntMap.empty

			    val localmap = ref StableMap.empty
			    fun bn (DG.BNODE n) = let
				val i = #bininfo n
				val li = #localimports n
				val gi = #globalimports n
				fun mySysval () =
				    IntMap.find (posmap, BinInfo.offset i)

				fun new () =
				    case mySysval () of
					(* We short-circuit traversal
					 * construction (and the resulting
					 * traversal) whenever we find a
					 * node whose dynamic value was
					 * created at bootstrap time.
					 * This assumes that anything in
					 * sysval can be shared -- which
					 * is enforced by the way the
					 * PIDMAP file is constructed. *)
					SOME e => (fn gp => fn _ => e, NONE)
				      | NONE => let
					    val e0 = (fn _ => emptyDyn, [])
					    fun join ((f, NONE), (e, l)) =
						(fn gp =>
						    DE.atop (f gp emptyDyn,
							     e gp),
						 l)
					      | join ((f, SOME (i, l')),
						      (e, l)) =
						(e, B (f, i, l') :: l)
					    val ge =
						foldl join e0 (map lfbn gi)
					    val le = foldl join ge (map bn li)
					in
					    case (BinInfo.sh_mode i, le) of
						(Sharing.SHARE _, (e, [])) =>
						let fun thunk gp =
							link_stable (i, e gp)
						    val m_thunk =
							Memoize.memoize thunk
						in
						    (fn gp => fn _ =>
								 m_thunk gp,
						     NONE)
						end
					      | (Sharing.SHARE _, _) =>
						EM.impossible
						   "Link: sh_mode inconsistent"
					      | (Sharing.DONTSHARE, (e, l)) =>
						(fn gp => fn e' =>
						  link_stable
						      (i, DE.atop (e', e gp)),
						 SOME (i, l))
					end
			    in
				case StableMap.find (!stablemap, i) of
				    SOME (B (f, i, [])) =>
				    (case BinInfo.sh_mode i of
					 Sharing.DONTSHARE => (f, SOME (i, []))
				       | _ => (f, NONE))
				  | SOME (B (f, i, l)) => (f, SOME (i, l))
				  | NONE =>
				    (case StableMap.find (!localmap, i) of
					 SOME x => x
				       | NONE => let
					     val x = new ()
					 in
					     localmap := StableMap.insert
							     (!localmap, i, x);
					     x
					 end)
			    end

			    and fbn (_, n, p) = (registerSublib p; bn n)

			    and lfbn th = fbn (th ())

			    fun sbn (DG.SB_SNODE n) =
				EM.impossible "Link:SNODE in stable lib"
			      | sbn (DG.SB_BNODE (n as DG.BNODE bnrec, _, p)) =
				let val _ = registerSublib p
				    val bininfo = #bininfo bnrec
				    val b as B (_, i, _) =
					case bn n of
					    (f, NONE) => B (f, bininfo, [])
					  | (f, SOME (i, l)) => B (f, i, l)
				in
				    stablemap :=
				    StableMap.insert (!stablemap, i, b)
				end

			    fun fsbn (_, n) = sbn n
			    fun impexp (nth, _, _) = fsbn (nth ())
			in
			    SymbolMap.app impexp exports
			end
		    fun force f = f ()
		in
		    if SrcPathSet.member (!visited, grouppath) then ()
		    else (visited := SrcPathSet.add (!visited, grouppath);
			  case kind of
			      GG.LIB { kind = GG.STABLE _, ... } =>
			          registerStableLib g
			    | _ => app (registerGroup o force o #2) sublibs)
		end

	    val _ = registerGroup group

	    val l_stablemap = ref StableMap.empty
	    val l_smlmap = ref SmlInfoMap.empty

	    fun bnode (B (f, i, l)) =
		case StableMap.find (!l_stablemap, i) of
		    SOME th => th
		  | NONE => let
			val fl = map bnode l
			fun th gp = let
			    fun add (t, e) = DE.atop (t gp, e)
			in
			    f gp (foldl add emptyDyn fl)
			end
			val m_th = Memoize.memoize th
		    in
			l_stablemap :=
			  StableMap.insert (!l_stablemap, i, m_th);
			m_th
		    end

	    fun sbn (DG.SB_BNODE (DG.BNODE { bininfo, ... }, _, _)) = let
		    val b = valOf (StableMap.find (!stablemap, bininfo))
		    fun th gp = SOME (bnode b gp)
			handle exn as Link _ => raise exn
			     | _ => NONE
		in
		    (th, [])
		end
	      | sbn (DG.SB_SNODE n) = sn n

	    and sn (DG.SNODE n) = let
		val { smlinfo = i, localimports, globalimports } = n
	    in
		case SmlInfoMap.find (!l_smlmap, i) of
		    SOME th => (th, [i])
		  | NONE => let
			fun atop (NONE, _) = NONE
			  | atop (_, NONE) = NONE
			  | atop (SOME e, SOME e') = SOME (DE.atop (e, e'))
			fun add ((f, l), (f', l')) =
			    (fn gp => atop (f gp, f' gp), l @ l')
			val gi = foldl add (fn _ => SOME emptyDyn, [])
			                   (map fsbn globalimports)
			val (getE, snl) = foldl add gi (map sn localimports)
			fun thunk gp = link_sml (gp, i, getBFC, getE, snl)
			val m_thunk = Memoize.memoize thunk
		    in
			l_smlmap := SmlInfoMap.insert (!l_smlmap, i, m_thunk);
			(m_thunk, [i])
		    end
	    end

	    and fsbn (_, n) = sbn n

	    fun impexp (nth, _, _) gp = #1 (fsbn (nth ())) gp
		handle Link exn => raise EX.Link

	    val exports' = SymbolMap.map impexp exports

	    fun group' gp = let
		fun one (_, NONE) = NONE
		  | one (f, SOME e) =
		    (case f gp of
			 NONE => NONE
		       | SOME e' => SOME (DE.atop (e', e)))
	    in
		SymbolMap.foldl one (SOME emptyDyn) exports'
	    end
	in
	    { exports = exports', group = group' }
	end

	fun newTraversal (x as (GG.ERRORGROUP, _)) = newTraversal0 x
	  | newTraversal (x as (GG.GROUP { exports, ... }, _)) = let
		val tth = Memoize.memoize (fn () => (newTraversal0 x))
	    in
		{ group = fn gp => #group (tth ()) gp,
		  exports = SymbolMap.mapi
				(fn (sy, _) => fn gp =>
				    valOf (SymbolMap.find (#exports (tth ()),
							   sy))
					  gp)
				exports }
	    end

	fun reset () = (stablemap := StableMap.empty;
			smlmap := SmlInfoMap.empty)

	fun unshare group =
	    let fun other (i, _) =
		    SrcPath.compare (BinInfo.group i, group) <> EQUAL
		val sv = system_values
	    in
		stablemap := StableMap.filteri other (!stablemap);
		(sv := #1 (SrcPathMap.remove (!sv, group)))
		handle LibBase.NotFound => ()
	    end
    end
end
