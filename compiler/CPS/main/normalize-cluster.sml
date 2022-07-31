(* normalize-cluster.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Code to normalize a cluster, which means guarantee that it satisfies the
 * following properties:
 *
 *	1) exactly one entry function (i.e., a `CONT` or `ESCAPE` function)
 *
 *	2) there are no internal jumps to the entry function.
 *
 * Note that this pass is run _before_ we do limit-check analysis (see limit.sml)
 *
 * TODO:
 *	- add label as argument to new cluster entry functions, when the function
 *	  will need the base pointer
 *	- reorder parameters to new cluster-entry functions to reduce register
 *	  shuffling; i.e., match the standard-function calling convention.
 *)

structure NormalizeCluster : sig

    type cluster = CPS.function list

  (* `transform cluster` normalizes `cluster`, if necessary, and returns a list of
   * normalized clusters.
   *)
    val transform : cluster * cluster list -> cluster list

  end = struct

    structure C = CPS
    structure LV = LambdaVar
    structure ISet = IntRedBlackSet
    structure LTbl = LV.Tbl

    fun error msg = ErrorMsg.impossible ("NormalizeCluster: " ^ msg)

    fun updateBy f (arr, i) = Array.update(arr, i, f (Array.sub(arr, i)))

    type cluster = C.function list

(*+DEBUG*
val say = Control.Print.say
fun prCluster (fn1::fns) = (
      say "***** CLUSTER START *****\n";
      PPCps.printcps0 fn1;
      List.app (fn f => (say "***** FRAG *****\n"; PPCps.printcps0 f)) fns;
      say "***** CLUSTER END *****\n")
*-DEBUG*)

  (***** bit-vector sets *****)
    structure BVSet :> sig
	type t
	type elem
	val fromInt : int -> elem
	val toInt : elem -> int
	val empty : t
	val singleton : elem -> t
	val add : t * elem -> t
	val member : t * elem -> bool
	val union : t * t -> t
	val same : t * t -> bool
	val compare : t * t -> order
	val map : (elem -> 'a) -> t -> 'a list
	val app : (elem -> unit) -> t -> unit
      end = struct
	datatype t = S of word list	(* least-significant word first *)
	type elem = word
	val fromInt = Word.fromInt
	val toInt = Word.toIntX
	val bitsPerWord = Word.fromInt Word.wordSize
	val empty = S[]
	fun singleton id = let
	      fun lp id = if (id < bitsPerWord)
		    then [Word.<<(0w1, id)]
		    else 0w0 :: lp (id - bitsPerWord)
	      in
		S (lp id)
	      end
	fun add (S bits, id) = let
	      fun lp ([], id) = if (id < bitsPerWord)
		    then [Word.<<(0w1, id)]
		    else 0w0 :: lp ([], id - bitsPerWord)
		| lp (b::bits, id) = if (id < bitsPerWord)
		    then Word.orb(b, Word.<<(0w1, id)) :: bits
		    else b :: lp (bits, id - bitsPerWord)
	      in
		S (lp (bits, id))
	      end
	fun member (S bits, id) = let
	      fun lp ([], _) = false
		| lp (b::bits, id) = if (id < bitsPerWord)
		    then Word.andb(b, Word.<<(0w1, id)) <> 0w0
		    else lp (bits, id - bitsPerWord)
	      in
		lp (bits, id)
	      end
	fun union (S bits1, S bits2) = let
	      fun u ([], bits2) = bits2
		| u (bits1, []) = bits1
		| u (b1::bits1, b2::bits2) = Word.orb(b1, b2) :: u(bits1, bits2)
	      in
		S (u (bits1, bits2))
	      end
	fun same (S bits1, S bits2) = ListPair.allEq (op =) (bits1, bits2)
	fun compare (S bits1, S bits2) = List.collate Word.compare (bits1, bits2)
      (* iterators over the elements of a set *)
	fun map f (S bits) = let
	      fun lp ([], _, elems) = List.rev elems
		| lp (0w0 :: bits, id, elems) = lp (bits, id + bitsPerWord, elems)
		| lp (b :: bits, id, elems) = let
		    fun lp' (0w0, _, elems) = lp (bits, id + bitsPerWord, elems)
		      | lp' (b, id, elems) = if Word.andb(b, 0w1) = 0w0
			  then lp' (Word.>>(b, 0w1), id+0w1, elems)
			  else lp' (Word.>>(b, 0w1), id+0w1, f id :: elems)
		    in
		      lp' (b, id, elems)
		    end
	      in
		lp (bits, 0w0, [])
	      end
	fun app f (S bits) = let
	      fun lp ([], _) = ()
		| lp (0w0 :: bits, id) = lp (bits, id + bitsPerWord)
		| lp (b :: bits, id) = let
		    fun lp' (0w0, _) = lp (bits, id + bitsPerWord)
		      | lp' (b, id) = if Word.andb(b, 0w1) = 0w0
			  then lp' (Word.>>(b, 0w1), id+0w1)
			  else (f id; lp' (Word.>>(b, 0w1), id+0w1))
		    in
		      lp' (b, id)
		    end
	      in
		lp (bits, 0w0)
	      end
      end

  (***** cluster info *****)

    structure Info : sig
      (* information about a group of CPS functions *)
	type t
      (* `mk (entries, frags)` creates the info structure for a cluster with
       * entry functions `entries` and internal functions `frags`.
       *)
	val mk : C.function list * C.function list -> t
      (* return the number of entry functions in the cluster *)
	val nEntries : t -> int
      (* return the number of internal functions in the cluster *)
	val nOther : t -> int
      (* return the total number of functions in the cluster *)
	val nFuncs : t -> int
      (* map a function's name to its integer ID *)
	val idOf : t -> LV.lvar -> int
      (* map a function's ID to its name *)
	val labelOf : t -> int -> LV.lvar
      (* map a function's ID to its definition *)
	val funcOf : t -> int -> C.function
      (* given a function ID, return the list of function IDs that directly call it *)
	val preds : t -> int -> int list
      (* given a function ID, return the list of function IDs that it calls directly *)
	val succs : t -> int -> int list
      (* `removeEdge (info, f, g)` removes the edge from `f` to `g` from the call graph *)
	val removeEdge : t * int * int -> unit
      (* `dfsApp info f id` does a DFS traversal of the graph starting at node `id`
       * and applies the function `f` to each reachable node as it is visited.
       *)
	val dfsApp : t -> (int -> unit) -> int -> unit
(*+DEBUG*
	val idToString : t -> int -> string
	val dump : t -> unit
*-DEBUG*)
      end = struct

      (* a node in the graph is represented with adjacency lists *)
	type nd = {preds : int list, succs : int list}

	type t = {
	    nEntries : int,			(* the number of entries *)
	    nOther : int,			(* the number of non-entry functions *)
	    funcToID : LV.lvar -> int,		(* mapping from lvars to function IDs *)
	    idToFunc : C.function array,	(* mapping from IDs to functions *)
	    callGraph : nd array		(* adjacency-list representation of call graph *)
	  }

	fun mk (entries, frags) : t = let
	      val nEntries = List.length entries
	      val nOther = List.length frags
	      val numFuncs = nEntries + nOther
	      fun isEntry id = (id < nEntries)
	    (* mapping of function names to a dense integer range *)
	      val funcToIdTbl : int LTbl.hash_table = LTbl.mkTable(numFuncs, Fail "func->id")
	      val funcToID = LTbl.lookup funcToIdTbl
	    (* compute the mapping of ids to functions *)
	      val idToFuncTbl = let
		    val tbl = Array.array(numFuncs, hd entries)
		    val add = LTbl.insert funcToIdTbl
		    in
		      List.appi
			(fn (id, func as (_,f,_,_,_)) => (
			    add(f, id); Array.update(tbl, id, func)))
			  entries;
		      List.appi
			(fn (i, func as (_,f,_,_,_)) => let
			    val id = nEntries + i
			    in
			      add(f, id); Array.update(tbl, id, func)
			    end)
			  frags;
		      tbl
		    end
	    (* create a call graph of potentially local control transfers for the functions,
	     * represented as an array `g` of `nd` valus, where `Array.sub(g, id)` is
	     * the predecessor/successor lists for the function with ID `id`.  We do not
	     * include any incoming edges that target entry nodes.
	     *)
	      val graph = Array.array(numFuncs, {preds=[], succs=[]})
	      fun addEdges (gId, (_, g, _, _, body)) = let
		  (* add an edge from `g` to `f`.  If `f` is already in the successors
		   * of `g`, then we do not need to add any information.  Otherwise, we
		   * add `f` to `g`'s successor list and `g` to `f`'s predecessor list.
		   *)
		    fun addEdge (f, succs) = let
			  val id = funcToID f
			  fun add ([], ids) = (
			      (* add `g` to `f`'s predecessors *)
				updateBy
				  (fn {succs, preds} => {succs=succs, preds=gId::preds})
				    (graph, id);
			      (* add `f` to `g`'s successors *)
				id :: ids)
			    | add (id' :: r, ids) = if (id <> id')
				then add(r, id'::ids)
				else succs
			  in
			  (* do not add the edge if `f` is an entry node *)
			    if isEntry id
			      then succs
			      else add (succs, [])
			  end
		    fun calls (e, succs) = (case e
			   of C.APP(C.LABEL l, _) => addEdge (l, succs)
			    | C.APP _ => succs
			    | C.RECORD(_, _, _, e) => calls (e, succs)
			    | C.SELECT(_, _, _, _, e) => calls (e, succs)
			    | C.OFFSET(_, _, _, e) => calls (e, succs)
			    | C.SWITCH(_, _, es) => List.foldl calls succs es
			    | C.BRANCH(_, _, _, e1, e2) => calls (e2, calls (e1, succs))
			    | C.SETTER(_, _, e) => calls (e, succs)
			    | C.LOOKER(_, _, _, _, e) => calls (e, succs)
			    | C.ARITH(_, _, _, _, e) => calls (e, succs)
			    | C.PURE(_, _, _, _, e) => calls (e, succs)
			    | C.RCC(_, _, _, _, _, e) => calls (e, succs)
			    | C.FIX _ => error "unexpected FIX"
			  (* end case *))
		    val nodes = calls (body, [])
		    in
		      updateBy (fn {preds, ...} => {preds=preds, succs=nodes}) (graph, gId)
		    end (* addEdges *)
	      val _ = Array.appi addEdges idToFuncTbl
	      in {
		nEntries = nEntries,
		nOther = nOther,
		funcToID = funcToID,
		idToFunc = idToFuncTbl,
		callGraph = graph
	      } end

	fun nEntries (info : t) = #nEntries info
	fun nOther (info : t) = #nOther info
	fun nFuncs (info : t) = #nEntries info + #nOther info
	fun idOf (info : t) = #funcToID info
	fun funcOf (info : t) id = Array.sub (#idToFunc info, id)
	fun labelOf info id = #2(funcOf info id)
	fun preds (info : t) id = #preds (Array.sub (#callGraph info, id))
	fun succs (info : t) id = #succs (Array.sub (#callGraph info, id))

	fun removeEdge ({callGraph, ...} : t, fId, gId) = let
	      fun rmv ([], _) = error "edge is missing!"
		| rmv (id::ids, id' : int) = if (id = id') then ids else id::rmv(ids, id')
	      in
	      (* remove `g` from `f`'s successor list *)
		updateBy
		  (fn {succs, preds} => {succs=rmv(succs, gId), preds=preds})
		    (callGraph, fId);
	      (* remove `f` from `g`'s predecessor list *)
		updateBy
		  (fn {succs, preds} => {succs=succs, preds=rmv(preds, fId)})
		    (callGraph, gId)
	      end

	fun dfsApp (info : t) f rootId = let
	      val numFuncs = #nEntries info + #nOther info
	      val visited = Array.array(numFuncs, false)
	      fun dfs id = if Array.sub(visited, id)
		    then ()
		    else (
		      Array.update(visited, id, true);
		      f id;
		      List.app dfs (#succs (Array.sub (#callGraph info, id))))
	      in
		dfs rootId
	      end

(*+DEBUG*
	fun idToString ({nEntries, idToFunc, ...} : t) id = let
	      val (_, f, _, _, _) = Array.sub(idToFunc, id)
	      in
		if (id < nEntries) then "*" ^ LV.lvarName f else LV.lvarName f
	      end
	fun dump info = let
	      val id2s = idToString info
	      fun sayNd (id, {preds, succs}) = say(String.concat[
		      "#### ", id2s id, " --> {",
		      String.concatWithMap "," id2s succs,
		      "}\n"
		    ])
	      in
		say (concat[
		    "### CLUSTER INFO (", Int.toString (nEntries info), "/",
		    Int.toString(nFuncs info), " entries)\n"
		  ]);
		Array.appi sayNd (#callGraph info);
		say "###\n"
	      end
*-DEBUG*)

      end (* structure Info *)

  (***** reachability info *****)

  (* we define a "signature" for a fragment to be the set of entry functions
   * that can trace a non-empty path of direct jumps to the fragment.  We represent
   * a signature as a pair `<n, S>`, where `n` is the number of entry nodes that can
   * reach the fragment and `S` is the set of nodes (represented as a bignum
   * bit vector).  Let b_i = 1 if there is a path from the i'th entry to the
   * fragment, and 0 otherwise.  Then n = SUM(b_i) and S =  SUM(b_i * 2^i) for
   * i in [0..nEntries-1].
   *)
    structure FSig : sig
	type t = BVSet.t
      (* singleton signature *)
	val singleton : int -> t
      (* does a signature include the given entry? *)
	val canReach : int -> t -> bool
      (* add a bit to a signature, where we assume that the bit was not previously set *)
	val add : int -> t -> t
      (* map a function over the fragment IDs in a signature *)
	val map : (int -> 'a) -> t -> 'a list
      (* apply a function to the fragment IDs in a signature *)
	val app : (int -> unit) -> t -> unit
      (* finite maps keyed by signatures *)
	structure Map : ORD_MAP where type Key.ord_key = t
      end = struct
        type t = BVSet.t
	fun singleton id = BVSet.singleton(BVSet.fromInt id)
	fun canReach id s = BVSet.member(s, BVSet.fromInt id)
	fun add id s = BVSet.add (s, BVSet.fromInt id)
	fun map f = BVSet.map (fn elem => f (BVSet.toInt elem))
	fun app f = BVSet.app (fn elem => f (BVSet.toInt elem))
	structure Map = RedBlackMapFn (
	  struct
	    type ord_key = t
	    val compare = BVSet.compare
	  end)
      end (* structure FSig *)

  (***** Normalization *****)

  (* check if there is a back edge to the entry function; if so, we split the entry into
   * a trivial header function and a second function that will be the target of the back
   * edges.
   *)
    fun checkForHeader (entry, frags) = let
	  val (fk, entryLab, params, tys, body) = entry
	  fun hasBackEdge e = (case e
		 of C.APP(C.LABEL l, _) => (l = entryLab)
		  | C.APP _ => false
		  | C.RECORD(_, _, _, e) => hasBackEdge e
		  | C.SELECT(_, _, _, _, e) => hasBackEdge e
		  | C.OFFSET(_, _, _, e) => hasBackEdge e
		  | C.SWITCH(_, _, es) => List.exists hasBackEdge es
		  | C.BRANCH(_, _, _, e1, e2) => (hasBackEdge e1 orelse hasBackEdge e2)
		  | C.SETTER(_, _, e) => hasBackEdge e
		  | C.LOOKER(_, _, _, _, e) => hasBackEdge e
		  | C.ARITH(_, _, _, _, e) => hasBackEdge e
		  | C.PURE(_, _, _, _, e) => hasBackEdge e
		  | C.RCC(_, _, _, _, _, e) => hasBackEdge e
		  | C.FIX _ => error "unexpected FIX"
		(* end case *))
	  in
	    if hasBackEdge body
            orelse List.exists (fn (_, _, _, _, e) => hasBackEdge e) frags
	      then let
		val hdrLab = LV.mkLvar()
		val params' = List.map (fn _ => LV.mkLvar()) params
	      (* the new entry function just jumps to the loop header *)
		val entry' =
		     (fk, entryLab, params', tys, C.APP(C.LABEL hdrLab, List.map C.VAR params'))
	      (* rewrite jumps to `entryLab` with jumps to `hdrLab` *)
		fun rewrite e = (case e
		       of C.APP(C.LABEL l, args) => if (l = entryLab)
			    then C.APP(C.LABEL hdrLab, args)
			    else e
			| C.APP _ => e
			| C.RECORD(rk, args, lv, e) => C.RECORD(rk, args, lv, rewrite e)
			| C.SELECT(i, v, lv, ty, e) => C.SELECT(i, v, lv, ty, rewrite e)
			| C.OFFSET(i, v, lv, e) => C.OFFSET(i, v, lv, rewrite e)
			| C.SWITCH(v, id, es) => C.SWITCH(v, id, List.map rewrite es)
			| C.BRANCH(p, vs, id, e1, e2) =>
			    C.BRANCH(p, vs, id, rewrite e1, rewrite e2)
			| C.SETTER(p, vs, e) => C.SETTER(p, vs, rewrite e)
			| C.LOOKER(p, vs, lv, ty, e) => C.LOOKER(p, vs, lv, ty, rewrite e)
			| C.ARITH(p, vs, lv, ty, e) => C.ARITH(p, vs, lv, ty, rewrite e)
			| C.PURE(p, vs, lv, ty, e) => C.PURE(p, vs, lv, ty, rewrite e)
			| C.RCC(b, cc, proto, vs, lvs, e) =>
			    C.RCC(b, cc, proto, vs, lvs, rewrite e)
			| C.FIX _ => error "unexpected FIX"
		      (* end case *))
		val frags' = List.map
		      (fn (fk, lab, xs, tys, e) => (fk, lab, xs, tys, rewrite e))
			frags
		in
		  entry' :: (C.KNOWN, hdrLab, params, tys, rewrite body) :: frags'
		end
	      else entry :: frags
	  end

  (* given a cluster with multiple entry points, split it into a list of clusters, where
   * each resulting cluster has just a single entry point.  We also return a set of the
   * new entry-points fragments.
   *)
    fun splitCluster info = let
	  val nFuncs = Info.nFuncs info
	  val predsOf = Info.preds info
	  val succsOf = Info.succs info
	(* list to record the internal fragments that are going to become entry nodes *)
	  val newEntries = ref ISet.empty
	  fun isEntry id = ISet.member(!newEntries, id)
	(* mark a node as a new entry node and remove all incoming edges *)
	  fun newEntry id = (
		newEntries := ISet.add(!newEntries, id);
		List.app (fn id' => Info.removeEdge (info, id', id)) (predsOf id))
	(* split the cluster; calls itself recursively until everything is normalized *)
	  fun split (entryIds, fragIds) = let
(*+DEBUG*
val _ = say(concat[
"### split: entries = {", String.concatWithMap "," (Info.idToString info) entryIds,
"}, frags = {", String.concatWithMap "," (Info.idToString info) fragIds, "}\n"])
*-DEBUG*)
	      (* a mapping from fragments to their signatures *)
		val idToSig = Array.array(nFuncs, BVSet.empty)
	      (* starting with the given entry function, do a DFS traversal to identify those
	       * fragments that are reachable from the entry and update their signatures.
	       *)
		fun dfs entryId = let
		      val add = FSig.add entryId
		      fun visit id = Array.update(idToSig, id, add(Array.sub(idToSig, id)))
		      in
			Info.dfsApp info visit entryId
		      end
	      (* compute the reachability signatures for the fragments *)
		val _ = List.app dfs entryIds
	      (* set of internal fragments that have not been assigned to a cluster yet *)
		val unassignedFrags = ref (ISet.fromList fragIds)
	      (* for each entry, find the tree of nodes that are uniquely dominated by
	       * the entry (i.e., they are only reachable from the entry).  For any edge
	       * from a node `f` in the tree to some fragment `g` that is not uniquely
	       * dominated by the entry, we mark `g` as a new entry node and remove the
	       * edge from `f` to `g` from the graph.
	       *)
		fun newCluster entryId = let
		      val entrySig = FSig.singleton entryId
		      in
			if BVSet.same(entrySig, Array.sub(idToSig, entryId))
			  then let
			    val visited = ref entrySig
			    fun walk predId (id, frags) =
				  if BVSet.member(!visited, BVSet.fromInt id)
				    then frags (* alread visited *)
				    else let
				      val idSig = Array.sub(idToSig, id)
				      in
					visited := BVSet.add(!visited, BVSet.fromInt id);
					if BVSet.same(entrySig, idSig)
					  then List.foldl (walk id) (id::frags) (succsOf id)
					  else (
					  (* record `id` as a new entry node *)
					    newEntry id;
					    frags)
				      end
			    val frags = List.foldl (walk entryId) [] (succsOf entryId)
			    in
			      unassignedFrags :=
				ISet.subtractList (!unassignedFrags, frags);
			      (entryId, List.rev frags)
			    end
			(* this entry fragment is reachable from other entries, so
			 * we must make it a stand-alone function.
			 *)
			  else (
			  (* for each successor of this node, mark it as a new entry
			   * and remove the edge to it.
			   *)
			    List.app newEntry (succsOf entryId);
			    (entryId, []))
		      end
		val newClusters = List.map newCluster entryIds
		in
		  case ISet.toList(!unassignedFrags)
		   of [] => newClusters
		    | frags' => let
			val (entries', frags') = List.partition isEntry frags'
			in
			  newClusters @ split (entries', frags')
			end
		  (* end case *)
		end
	  val clusters = split (
		List.tabulate(Info.nEntries info, Fn.id),
		List.tabulate(Info.nOther info, fn i => Info.nEntries info + i))
	(* convert function IDs back to CPS functions *)
	  val funcsOf = List.map (Info.funcOf info)
	  fun mkCluster (entry, frags) = funcsOf (entry::frags)
	  in
	    (!newEntries, List.map mkCluster clusters)
	  end (* splitCluster *)

    fun normalize (entries, frags) = let
(*+DEBUG*
val _ = (
      say "## NormalizeCluster.normalize\n";
      prCluster (entries @ frags))
*-DEBUG*)
	  val info = Info.mk (entries, frags)
	  val nEntries = Info.nEntries info
	  val nFuncs = Info.nFuncs info
(*+DEBUG*
val _ = Info.dump info
*-DEBUG*)
	  val (newEntries, clusters) = splitCluster info
	(* rewrite the new entries as necessary *)
	  fun doCluster (cluster as entry::frags) = let
		val (_, f, _, _, _) = entry
		val entryId = Info.idOf info f
		in
		  if ISet.member(newEntries, entryId)
		    then checkForHeader (entry, frags)
		    else cluster
		end
	  in
	    List.map doCluster clusters
	  end

(*+DEBUG*
val normalize = fn arg => let val clusters = normalize arg
in
  say "## NormalizeCluster.normalize result:\n";
  List.app prCluster clusters;
  say "## NormalizeCluster.normalize done\n";
  clusters
end
*-DEBUG*)

  (***** Main function *****)

    fun isEscaping (f : C.function) = (case #1 f
	   of C.CONT => true
	    | C.ESCAPE => true
	    | _ => false
	  (* end case *))
    val partition = List.partition isEscaping

    fun transform ([frag], clusters) = [frag] :: clusters
      | transform (cluster, clusters) = (case partition cluster
	   of ([], _) => error "NormalizeCluster.transform: no entry fragment"
	    | ([entry], frags) => (entry::frags) :: clusters
	    | (ents, frags) => normalize (ents, frags) @ clusters
	  (* end case *))

  end (* NormalizeCluster *)
