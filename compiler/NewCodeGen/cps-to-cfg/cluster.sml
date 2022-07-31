(* cluster.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Cluster : sig

  (* `cluster (singleEntry, fns)` groups the functions `fns` of a compilation
   * unit into "clusters" using a union-find data structure.  A cluster is
   * a maximal graph of CPS functions connects by `APPLY(LABEL lab, ...)`
   * edges.
   *
   * If `singleEntry` is true, then each cluster will have a single entry
   * point.  This restriction may require introducing additional CPS
   * functions to act as join points.
   *
   * The first function in `fns` must be the first function in the first
   * result cluster. This is achieved by ensuring that the first function
   * is mapped to the smallest id in a dense enumeration. This function
   * id will map to the smallest cluster id. The function ids are then
   * iterated in descending order.
   *)
    val cluster : bool * CPS.function list -> CPS.function list list

  end = struct

    structure LV = LambdaVar
    structure LTbl = LV.Tbl
    structure ISet = IntRedBlackSet

    fun error msg = ErrorMsg.impossible ("Cluster." ^ msg)

  (* information about a group of CPS functions *)
    type info = {
	numFuncs : int,				(* the number of functions *)
	funcToID : LambdaVar.lvar -> int,	(* mapping from lvars to function IDs *)
	idToFunc : int -> CPS.function,		(* mapping from IDs to functions *)
	callGraph : int list array		(* representation of call graph *)
      }

    fun mkInfo funcs = let
	  val numFuncs = length funcs
	(* mapping of function names to a dense integer range *)
	  exception FuncId
	  val funcToIdTbl : int LTbl.hash_table = LTbl.mkTable(numFuncs, FuncId)
	  val funcToID = LTbl.lookup funcToIdTbl
	(* mapping of ids to functions *)
	  val idToFuncTbl = let
		val tbl = Array.array(numFuncs, hd funcs)
		val add = LTbl.insert funcToIdTbl
		in
		  List.appi
		    (fn (id, func as (_,f,_,_,_)) => (
			add(f, id); Array.update(tbl, id, func)))
		      funcs;
		  tbl
		end
	(* create a call graph for the functions, represented as an array `g` of integer
	 * IDs, where `Array.sub(g, id)` is the list of function IDs that the function
	 * with ID `id` calls.
	 *)
	  val graph = Array.array(numFuncs, [])
	  fun addEdges (id, (_, _, _, body)) = let
		fun addEdge f = Array.modify (fn succs => funcToID f :: succs) graph
		fun calls (CPS.APP(CPS.LABEL l, _)) = addEdge l
		  | calls (CPS.APP _) = ()
		  | calls (CPS.RECORD(_, _, _, e)) = calls e
		  | calls (CPS.SELECT(_, _, _, _, e)) = calls e
		  | calls (CPS.OFFSET(_, _, _, e)) = calls e
		  | calls (CPS.SWITCH(_, _, es)) = List.app calls es
		  | calls (CPS.BRANCH(_, _, _, e1, e2)) = (calls e1; calls e2)
		  | calls (CPS.SETTER(_, _, e)) = calls e
		  | calls (CPS.LOOKER(_, _, _, _, e)) = calls e
		  | calls (CPS.ARITH(_, _, _, _, e)) = calls e
		  | calls (CPS.PURE(_, _, _, _, e)) = calls e
		  | calls (CPS.RCC(_, _, _, _, _, e)) = calls e
		  | calls (CPS.FIX _) = error "unexpected FIX"
		in
		  calls body
		end (* addEdges *)
	  val _ = List.appi addEdges funcs
	  in {
	    numFuncs = numFuncs,
	    funcToID = funcToID,
	    idToFunc = idToFunc,
	    callGraph = graph
	  } end

  (* group functions into clusters *)
    fun group {numFuncs, funcToID, idToFunc, callGraph} = let
	(* union-find structure -- initially each function in its own cluster *)
	  val trees = Array.tabulate(numFuncs, fn i => i)
	  fun ascend u = let
		val v = Array.sub(trees, u)
		in
		  if v = u then u else ascend(v)
		end
	  fun union (t1, t2) = let
		val r1 = ascend t1
		val r2 = ascend t2
		in
		  if r1 = r2
		    then () (* already in the same set *)
		  else if r1 < r2
		    then Array.update(trees, r2, r1)
		    else Array.update(trees, r1, r2)
		end
	(* build union-find structure *)
	  val _ = let
		fun doFn (caller, callees) =
		      List.app (fn id => union(caller, id)) callees
		in
		  Array.appi doFn callGraph
		end
	(* extract the clusters.
	 * The first func in the funcs list must be the first function
	 * in the first cluster.
	 *)
	  fun extract() = let
		val clusters = Array.array(numFuncs, [])
		fun collect n = let
		      val root = ascend n
		      val func = idToFunc n
		      val cluster = Array.sub(clusters, root)
		      in
			Array.update(clusters, root, func::cluster);
			collect (n-1)
		      end
		fun finish (~1, acc) = acc
		  | finish (n, acc) = (case Array.sub(clusters, n)
		       of [] => finish (n-1, acc)
			| cluster => finish(n-1, cluster::acc)
		      (* end case *))
		in
		  collect (numFuncs-1) handle _ => ();
		  finish (numFuncs-1, [])
		end
	  in
	    build funcs;
	    extract()
	  end (* group *)


  (* if a cluster has multiple entry points, then split it into one function
   * per entry-point, plus additional functions for shared code.  We also
   * guarantee that there are no back edges to an entry function.
   *)
    fun split ({numFuncs, funcToID, idToFunc}, funcs) = let
	  fun isEntry (CONT, _, _, _, _) = true
	    | isEntry (ESCAPE, _, _, _, _) = true
	    | isEntry _ = false
	(* is there a path from `f` to `g`? *)
	  fun reachable (f, g) = let
	      (* depth-first search looking for `g`; visited is the set of IDs that
	       * have been visited.
	       *)
		fun dfs visited h = (h = g)
		      orelse (not ISet.member(visited, h)
			andalso List.exists
			  (dfs (ISet.add(visited, h)))
			    (Array.sub(callGraph, h)))
		in
		  dfs f
		end
	  fun split' (clstr as f1 :: (frest as _::_)) = (
		case List.partition isEntry frest
		 of [] => clstr
		  | entries => ??
		(* end case *))
	    | split' clstr = clstr
	  in
	    split'
	  end (* split *)

  (* print clusters if requested *)
    fun print clusters = let
	  val say = Control.Print.say
	  fun prCluster (fn1::fns) = (
		say "***** CLUSTER START *****\n";
		PPCps.printcps0 fn1;
		List.app (fn f => (say "***** FRAG *****\n"; PPCps.printcps0 f)) fns;
		say "***** CLUSTER END *****\n")
	  in
	    List.app prCluster clusters;
	    clusters
	  end

  (* main function *)
    fun cluster (singleEntry, funcs) = let
	  val tbls = mkTables funcs
	  val callGraph = mkCallGraph (tbls, funcs)
	  val clusters = group (tbls, callGraph)
	  val clusters = if singleEntry
		then let
		  val split = List.foldr
		  (fn (clstr, clstrs) => split clstr @ clstrs)
		    [] clusters
		else clusters
	  in
	    if !Control.CG.printClusters then print clusters else ();
	    clusters
	  end

  end
