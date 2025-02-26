(* cluster.sml
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Cluster : sig

    type cluster = CPS.function list

  (* `cluster fns` groups the functions `fns` into a list of *clusters*,
   * where a cluster is a maximal graph where each node is a CPS function (from
   * the list `fns`) and the edges are defined by applications of known labels.
   * The first function in the first cluster will be the first function in `fns`.
   *
   * Functions that are not reachable from an entry-point (i.e., functions
   * that have kind `CONT` or `ESCAPE`) are removed from the program.
   *)
    val cluster : CPS.function list -> cluster list

  (* utility functions on clusters *)
    val map : (CPS.function -> CPS.function) -> cluster list -> cluster list
    val app : (CPS.function -> unit) -> cluster list -> unit

(*+DEBUG*)
val print : cluster list -> cluster list
(*-DEBUG*)
  end = struct

    fun error msg = ErrorMsg.impossible ("Cluster." ^ msg)

    type cluster = CPS.function list

    val say = Control.Print.say

  (* print clusters if requested *)
    fun print clusters = let
	  fun prCluster (fn1::fns) = (
		say "***** CLUSTER START *****\n";
		PPCps.printcps0 fn1;
		List.app (fn f => (say "***** FRAG *****\n"; PPCps.printcps0 f)) fns;
		say "***** CLUSTER END *****\n")
	  in
	    List.app prCluster clusters;
	    clusters
	  end

    val normalizeCluster = NormalizeCluster.transform

    (* is a function an entry? *)
    fun isEntry (CPS.CONT, _, _, _, _) = true
      | isEntry (CPS.ESCAPE, _, _, _, _) = true
      | isEntry _ = false

(*+DEBUG*
  (* returns true if there is an error *)
    fun checkCluster [] = raise Empty
      | checkCluster (entry::frags) = List.exists isEntry frags
*-DEBUG*)

    fun cluster [] = raise List.Empty
      | cluster funcs = let
	(* We guarantee that the first function in `funcs` is the first
	 * function in the first cluster by ensuring that the first
	 * function is mapped to the smallest id in a dense enumeration.
	 * This function id will map to the smallest cluster id.
	 * The function ids are then iterated in descending order.
	 *)
	  val numOfFuncs = length funcs
	(* mapping of function names to a dense integer range *)
	  exception FuncId
	  val funcToIdTbl : int LambdaVar.Tbl.hash_table =
		LambdaVar.Tbl.mkTable(numOfFuncs, FuncId)
	  val lookup = LambdaVar.Tbl.lookup funcToIdTbl
	(* mapping of ids to functions *)
	  val idToFuncTbl = let
		val tbl = Array.array(numOfFuncs, hd funcs)
		val add = LambdaVar.Tbl.insert funcToIdTbl
		in
		  List.appi
		    (fn (id, func as (_,f,_,_,_)) => (
                        add(f, id); Array.update(tbl, id, func)))
		      funcs;
		  tbl
		end
	(* union-find structure -- initially each function in its own cluster *)
	  val trees = Array.tabulate(numOfFuncs, fn i => i)
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
	(* work-list algorithm to build union-find structure.  We start with
         * with the escaping functions and continuations.  We add known functions
         * to the work list as they are referenced by direct calls.  We are done
         * when the worklist is empty.
         * Known functions that are never referenced are identified and removed
         * from the result.
         *)
          (* boolean flags for functions that have been added to the work list *)
          val visitMarks = Array.array(numOfFuncs, false)
          fun visit id = Array.update(visitMarks, id, true)
          fun visited id = Array.sub(visitMarks, id)
          (* the initial work list are the entry functions *)
          val initialWL = let
                fun f (id, func, wl) = if isEntry func
                      then (visit id; func::wl)
                      else wl
                in
                  List.rev (List.foldli f [] funcs)
                end
          (* the worklist algorithm *)
          fun build [] = ()
            | build ((_,f,_,_,body)::rest) = let
		val fId = lookup f
		fun calls (CPS.APP(CPS.LABEL l,_), wl) = let
                      val lId = lookup l
                      in
                        union(fId, lId);
                        if visited lId
                          then wl
                          else (
                            visit lId;
                            Array.sub(idToFuncTbl, lId) :: wl)
                      end
		  | calls (CPS.APP _, wl) = wl
		  | calls (CPS.RECORD(_,_,_,e), wl) = calls (e, wl)
		  | calls (CPS.SELECT(_,_,_,_,e), wl) = calls (e, wl)
		  | calls (CPS.OFFSET(_,_,_,e), wl) = calls (e, wl)
		  | calls (CPS.SWITCH(_,_,es), wl) = List.foldl calls wl es
		  | calls (CPS.BRANCH(_,_,_,e1,e2), wl) = calls (e2, calls (e1, wl))
		  | calls (CPS.SETTER(_,_,e), wl) = calls (e, wl)
		  | calls (CPS.LOOKER(_,_,_,_,e), wl) = calls (e, wl)
		  | calls (CPS.ARITH(_,_,_,_,e), wl) = calls (e, wl)
		  | calls (CPS.PURE(_,_,_,_,e), wl) = calls (e, wl)
		  | calls (CPS.RCC(_,_,_,_,_,e), wl) = calls (e, wl)
		  | calls (CPS.FIX _, _) = error "calls.f:FIX"
		in
		  build (calls (body, rest))
		end (* build *)
	(* extract the clusters.
	 * The first func in the funcs list must be the first function
	 * in the first cluster.
	 *)
	  fun extract () = let
		val clusters = Array.array(numOfFuncs, [])
	      (* group functions into clusters *)
		fun collect n = if (n < 0)
                      then ()
                      else (
                        if visited n
                          then let
                            val root = ascend n
                            val func = Array.sub(idToFuncTbl, n)
                            val cluster = Array.sub(clusters, root)
                            in
                              Array.update(clusters, root, func::cluster)
                            end
                          else (); (* skip functions that were not visited *)
                        collect (n-1))
		val _ = collect (numOfFuncs-1)
	      (* collect the clusters *)
		fun finish (~1, acc) = acc
		  | finish (n, acc) = (case Array.sub(clusters, n)
		       of [] => finish (n-1, acc)
			| cl => finish (n-1, cl::acc)
		      (* end case *))
                (* the resulting clusters *)
                val clusters = finish (numOfFuncs-1, [])
		in
		  clusters
		end
	  in
	    build initialWL;
	    if !Control.CG.printClusters
	      then print (extract())
	      else extract()
	  end (* cluster *)

  (* map a function over a list of clusters *)
    fun map (f : CPS.function -> CPS.function) = List.map (List.map f)
  (* apply a function to a list of clusters *)
    fun app (f : CPS.function -> unit) = List.app (List.app f)

  end
