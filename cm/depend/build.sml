(* build.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Build the dependency graph for one group/library.
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)

signature BUILDDEPEND = sig
    type impexp = DependencyGraph.impexp

    val build :
	{ imports: impexp SymbolMap.map,
	  smlfiles: (SmlInfo.info * SymbolSet.set) list,
	  localdefs: SmlInfo.info SymbolMap.map,
	  subgroups: 'a,
	  sources: 'b,
	  reqpriv: GroupGraph.privileges }
	* SymbolSet.set			(* filter *)
	* GeneralParams.info
	* DependencyGraph.farsbnode	(* pervasive env *)
	->
	impexp SymbolMap.map		(* exports *)
	* GroupGraph.privileges		(* required privileges (aggregate) *)
	* SymbolSet.set			(* imported symbols *)

    (* for the autoloader *)
    type looker = Symbol.symbol -> DAEnv.env
    val look : looker -> DAEnv.env -> looker
    val processOneSkeleton : looker -> Skeleton.decl -> unit
end

structure BuildDepend :> BUILDDEPEND = struct

    structure S = Symbol
    structure SS = SymbolSet
    structure SM = SymbolMap
    structure SK = Skeleton
    structure DG = DependencyGraph
    structure DE = DAEnv
    structure EM = ErrorMsg
    structure SP = SymPath
    structure PP = PrettyPrint

    type impexp = DG.impexp

    type looker = Symbol.symbol -> DAEnv.env

    fun look otherwise DE.EMPTY s = otherwise s
      | look otherwise (DE.BINDING (s', v)) s =
	if S.eq (s, s') then v else otherwise s
      | look otherwise (DE.LAYER (e, e')) s = look (look otherwise e') e s
      | look otherwise (DE.FCTENV looker) s =
	(case looker s of NONE => otherwise s | SOME v => v)
      | look otherwise (DE.FILTER (ss, e)) s =
	if SymbolSet.member (ss, s) then look otherwise e s else otherwise s
      | look otherwise (DE.SUSPEND eth) s = look otherwise (eth ()) s

    fun evalOneSkeleton lookimport = let
	(* build the lookup function for DG.env *)
	val lookup = look lookimport

	fun lookSymPath e (SP.SPATH []) = DE.EMPTY
	  | lookSymPath e (SP.SPATH (p as (h :: t))) = let
		(* again, if we don't find it here we just ignore
		 * the problem and let the compiler catch it later *)
		val lookup' = look (fn _ => DE.EMPTY)
		fun loop (e, []) = e
		  | loop (e, h :: t) = loop (lookup' e h, t)
	    in
		loop (lookup e h, t)
	    end

	(* "eval" -- compute the export environment of a skeleton *)
	fun eval sk = let
	    fun evalDecl e (SK.Bind (name, def)) =
		DE.BINDING (name, evalModExp e def)
	      | evalDecl e (SK.Local (d1, d2)) =
		evalDecl (DE.LAYER (evalDecl e d1, e)) d2
	      | evalDecl e (SK.Seq l) = evalSeqDecl e l
	      | evalDecl e (SK.Par []) = DE.EMPTY
	      | evalDecl e (SK.Par (h :: t)) =
		foldl (fn (x, r) => DE.LAYER (evalDecl e x, r))
		(evalDecl e h) t
	      | evalDecl e (SK.Open s) = evalModExp e s
	      | evalDecl e (SK.Ref s) =
		(SS.app (ignore o lookup e) s; DE.EMPTY)

	    and evalSeqDecl e [] = DE.EMPTY
	      | evalSeqDecl e (h :: t) = let
		    fun one (d, e') =
			DE.LAYER (evalDecl (DE.LAYER (e', e)) d, e')
		in
		    foldl one (evalDecl e h) t
		end

	    and evalModExp e (SK.Var sp) = lookSymPath e sp
	      | evalModExp e (SK.Decl l) = evalSeqDecl e l
	      | evalModExp e (SK.Let (d, m)) =
		evalModExp (DE.LAYER (evalSeqDecl e d, e)) m
	      | evalModExp e (SK.Ign1 (m1, m2)) =
		(ignore (evalModExp e m1); evalModExp e m2)
	in
	    evalDecl DE.EMPTY sk
	end
    in
	eval
    end

    fun processOneSkeleton lookimport sk =
	ignore (evalOneSkeleton lookimport sk)

    (* get the description for a symbol *)
    fun symDesc (s, r) =
	S.nameSpaceToString (S.nameSpace s) :: " " :: S.name s :: r

    fun build (coll, filter, gp, perv_fsbnode) = let
	val { imports, smlfiles, localdefs, subgroups,
	      sources, reqpriv } = coll

	val per_file_exports =
	    foldl (fn ((p, s), m) => SmlInfoMap.insert (m, p, s))
		  SmlInfoMap.empty smlfiles

	(* the "blackboard" where analysis results are announced *)
	(* (also used for cycle detection) *)
	val bb = ref SmlInfoMap.empty
	fun lock i = bb := SmlInfoMap.insert (!bb, i, NONE)
	fun release (i, r) = (bb := SmlInfoMap.insert (!bb, i, SOME r); r)
	fun fetch i = SmlInfoMap.find (!bb, i)

	(* We collect all imported symbols so that we can then narrow
	 * the list of libraries. *)
	val gi_syms = ref SymbolSet.empty
	fun add_gi_sym s = gi_syms := SymbolSet.add (!gi_syms, s)

	(* - get the result from the blackboard if it is there *)
	(* - otherwise trigger analysis *)
	(* - detect cycles using locking *)
	(* - maintain root set *)
	fun getResult (i, history) =
	    case fetch i of
		NONE => (lock i; release (i, analyze (i, history)))
	      | SOME (SOME r) => r
	      | SOME NONE => let	(* cycle found --> error message *)
		    val f = SmlInfo.sourcepath i
		    fun pphist pps = let
			fun recur (_, []) = () (* shouldn't happen *)
			  | recur (n'', (s, i') :: r) = let
				val f' = SmlInfo.sourcepath i'
				val n' = SrcPath.descr f'
				val _ =
				    if SmlInfo.eq (i, i') then ()
				    else recur (n', r)
				val l =
				    n' :: " refers to " ::
				    symDesc (s, [" defined in ", n''])
			    in
				app (PP.string pps) l;
				PP.newline pps
			    end
		    in
			PP.newline pps;
			recur (SrcPath.descr f, history)
		    end
		in
		    SmlInfo.error gp i EM.COMPLAIN
		         "cyclic ML dependencies" pphist;
		    release (i, (DG.SNODE { smlinfo = i,
					    localimports = [],
					    globalimports = [] },
				 DE.EMPTY))
		end

	(* do the actual analysis of an ML source and generate the
	 * corresponding node *)
	and analyze (i, history) = let
	    val li = ref []
	    val gi = ref [perv_fsbnode]

	    (* register a local import *)
	    fun localImport n =
		if List.exists (fn n' => DG.seq (n, n')) (!li) then ()
		else li := n :: !li

	    (* register a global import, maintain filter sets *)
	    fun globalImport (s, (f, n)) = let
		fun sameN (_, n') = DG.sbeq (n, n')
	    in
		add_gi_sym s;
		case List.find sameN (!gi) of
		    NONE => gi := (f, n) :: !gi (* brand new *)
		  | SOME (NONE, n') => () (* no filter -> no change *)
		  | SOME (SOME f', n') => let
			(* there is a filter...
			 *  calculate "union", see if there is a change,
			 *  and if so, replace the filter *)
			fun replace filt =
			    gi := (filt, n) ::
				  List.filter (not o sameN) (!gi)
		    in
			case f of
			    NONE => replace NONE
			  | SOME f => if SS.equal (f, f') then ()
				      else replace (SOME (SS.union (f, f')))
		    end
	    end

	    val f = SmlInfo.sourcepath i
	    fun isSelf i' = SmlInfo.eq (i, i')

	    (* lookup function for things not defined in the same ML file.
	     * As a side effect, this function registers local and
	     * global imports. *)
	    fun lookimport s = let
		fun dontcomplain s = DE.EMPTY
		fun lookfar () =
		    case SM.find (imports, s) of
			SOME (farnth, e, _) => (globalImport (s, farnth ());
						look dontcomplain e s)
		      | NONE =>
			    (* We could complain here about an undefined
			     * name.  However, since CM doesn't have the
			     * proper source locations available, it is
			     * better to handle this case silently and
			     * have the compiler catch the problem later. *)
			    DE.EMPTY
	    in
		case SM.find (localdefs, s) of
		    SOME i' =>
			if isSelf i' then lookfar ()
			else let
			    val (n, e) = getResult (i', (s, i) :: history)
			in
			    localImport n;
			    look dontcomplain e s
			end
		  | NONE => lookfar ()
	    end

	    val eval = evalOneSkeleton lookimport

	    val e = case SmlInfo.skeleton gp i of
		SOME sk => eval sk
	      | NONE => DE.EMPTY

	    val n = DG.SNODE { smlinfo = i,
			       localimports = !li,
			       globalimports = !gi }
	in
	    (n, e)
	end

	(* run the analysis on one ML file -- causing the blackboard
	 * to be updated accordingly *)
	fun doSmlFile (i, _) = ignore (getResult (i, []))

	(* run the analysis *)
	val _ = app doSmlFile smlfiles

	(* Invert the "localdefs" map so that each smlinfo is mapped to the
	 * corresponding _set_ of symbols: *)
	local
	    fun add (sy, i, m) =
		case SmlInfoMap.find (m, i) of
		    NONE => SmlInfoMap.insert (m, i, SymbolSet.singleton sy)
		  | SOME ss => SmlInfoMap.insert (m, i, SymbolSet.add (ss, sy))
	in
	    val ilocaldefs = SymbolMap.foldli add SmlInfoMap.empty localdefs
	end

	fun addDummyFilt i = let
	    val (sn, e) = valOf (valOf (fetch i))
	    val sbn = DG.SB_SNODE sn
	    val fsbn = (SmlInfoMap.find (per_file_exports, i), sbn)
	in
	    (* We also thunkify the fsbn so that the result is an impexp. *)
	    (fn () => fsbn, e, valOf (SmlInfoMap.find (ilocaldefs, i)))
	end

	(* First we make a map of all locally defined symbols to
	 * the local "far sb node"
	 * but with only a dummy filter attached.
	 * This makes it consistent with the current state
	 * of "imports" where there can be filters, but
	 * where those filters are not yet strengthened according to
	 * filter *)
	val localmap = SM.map addDummyFilt localdefs

	val exports = let
            (* Strengthening a local export is directly described
             * by filter. *)
	    val local_filter = filter
            (* In contrast, strengthening a re-export must take into
             * account local definitions: anything defined locally
             * must be removed from re-exports. *)
            val reexport_filter =
		SymbolSet.subtractList (filter, SymbolMap.listKeys localdefs)
	    (* We now always have a filter.
	     * We export only the things in the filter.
	     * They can be taken from either localmap or else from
	     * imports.  In either case, it is necessary to strengthen
	     * the filter attached to each node. *)
	    fun strengthen ss (nth, e, allsyms) = let
		val (fopt', sbn) = nth ()
		val new_fopt =
		    case fopt' of
			NONE => SOME ss
		      | SOME ss' => SOME (SS.intersection (ss, ss'))
		fun nth' () = (new_fopt, sbn)
	    in
		(nth', DE.FILTER (ss, e), SS.intersection (allsyms,ss))
	    end
	    fun addNodeFor (s, m) = (
		case SM.find (localmap, s)
		 of SOME n => SM.insert (m, s, strengthen local_filter n)
		  | NONE => (case SM.find (imports, s)
		       of SOME n => (
			    add_gi_sym s;
			    SM.insert (m, s, strengthen reexport_filter n))
			| NONE =>
			 (* This should never happen since we
			  * checked beforehand during
			  * parsing/semantic analysis *)
			    EM.impossible "build: undefined export"
		      (* end case *))
		(* end case *))
	in
	    SS.foldl addNodeFor SM.empty filter
	end
    in
	CheckSharing.check (exports, gp);
	(exports, reqpriv, !gi_syms)
    end
end
