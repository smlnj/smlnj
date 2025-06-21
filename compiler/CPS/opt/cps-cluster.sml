(* cps-cluster.sml
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

structure CPSCluster : sig

    type cluster = CPS.function list

    type t

  end = struct

    structure C = CPS
    structure LV = LambdaVar
    structure Tbl = LV.Tbl

    type cluster = C.function list

    type t = cluster list

    fun mkTables func = let
          fun walkFB (func as (_, _, _, _, body), fns) = walkExp (body, func::fns)
          and walkExp (C.RECORD(_, _, _, ce), fns) = walkExp (ce, fns)
            | walkExp (C.SELEVT(_, _, _, _, ce), fns) = walkExp (ce, fns)
            | walkExp (C.OFFSET _, fns) = raise Fail "unexpected OFFSET"
            | walkExp (C.APP _, fns) = fns
            | walkExp (C.FIX(fbs, ce), fns) = walkExp (ce, List.foldl walkFB fns fbs)
            | walkExp (C.SWITCH(_, _, ces), fns) = List.foldl walkExp fns ces
            | walkExp (C.BRANCH(_, _, _, ce1, ce2), fns) = walkExp (ce2, walkExp (ce2, fns))
            | walkExp (C.SETTER(_, _, _, _, ce), fns) = walkExp (ce, fns)
            | walkExp (C.LOOKER(_, _, _, _, ce), fns) = walkExp (ce, fns)
            | walkExp (C.ARITH(_, _, _, _, ce), fns) = walkExp (ce, fns)
            | walkExp (C.PURE(_, _, _, _, ce), fns) = walkExp (ce, fns)
            | walkExp (C.RCC(_, _, _, _, _, ce), fns) = walkExp (ce, fns)
          (* the list of functions in right-to-left post order *)
          val funcs = List.walkFB (func, [])
          (* count the number of fucntions while reversing the list *)
          val (numFuncs, funcs) = let
                fun count (n, [], xs) = (n, xs)
                  | count (n, x::xr, xs) = count (n+1, xr, x::xs)
                in
                  count (0, funcs, [])
                end
          (* hash table mapping function names to a dense range of integer IDs *)
	  exception FuncId
	  val funcToIdTbl : int Tbl.hash_table = Tbl.mkTable(numFuncs, FuncId)
          val ins = Tbl.insert funcToIdTbl
          (* array mapping IDs to functions *)
          val idToFuncTbl = Array.array(numFuncs, func)
          (* add a function to the tables *)
          fun add (id, func as (_, f, _, _, _)) = (
                ins (f, id);
                Array.update (idToFuncTbl, id, func))
          in
            (* initialize the two tables *)
            List.appi add funcs;
            {numFuncs = numFuncs, funcToIdTbl = funcToIdTbl, idToFuncTbl = idToFuncTbl}
          end

    fun cluster func = let
          (* *)
          val {numFuncs, funcToIdTbl, idToFuncTbl} = mkTables func
	  val find = Tbl.find funcToIdTbl
	  (* union-find structure -- initially each function in its own cluster *)
	  val trees = Array.tabulate(numFuncs, fn i => i)
	  fun ascend u = let
		val v = Array.sub(trees, u)
		in
		  if v = u then u else ascend(v)
		end
	  fun union(t1, t2) = let
		val r1 = ascend t1
		val r2 = ascend t2
		in
		  if r1 = r2
		    then () (* already in the same set *)
		  else if r1 < r2
		    then Array.update(trees, r2, r1)
		    else Array.update(trees, r1, r2)
		end
	  (* build the union-find structure by walking the tree *)
          fun build (_, f, _, _, body) = let
		val SOME fId = find f
		fun calls (C.RECORD(_, _, _, ce)) = calls ce
		  | calls (C.SELECT(_, _, _, _, ce)) = calls ce
		  | calls (C.OFFSET(_, _, _, ce)) = raise Fail "unexpected OFFSET"
                  | calls (C.APP(CPS.VAR g, _)) = (case find g
                       of SOME id => union(fId, id)
                        | NONE => ()
                      (* end case *))
                  | calls (C.APP _) = ()
                  | calls (C.FIX(fbs, ce)) = (List.app build fbs; calls ce)
		  | calls (C.SWITCH(_, _, ces)) = List.app calls ces
		  | calls (C.BRANCH(_, _, _, ce1, ce2)) = (calls ce1; calls ce2)
		  | calls (C.SETTER(_, _, ce)) = calls ce
		  | calls (C.LOOKER(_, _, _, _, ce)) = calls ce
		  | calls (C.ARITH(_, _, _, _, ce)) = calls ce
		  | calls (C.PURE(_, _, _, _, ce)) = calls ce
		  | calls (C.RCC(_, _, _, _, _, ce)) = calls ce
                in
                end (* build *)
          (* extract the clusters.
           * The first func in the funcs list must be the first function
           * in the first cluster.
           *)
	  fun extract() = let
		val clusters = Array.array(numFuncs, [])
		fun collect n = if (n < 0)
                      then ()
                      else let
                        val root = ascend n
                        val func = Array.sub(idToFuncTbl, n)
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
		  collect (numFuncs-1);
		  finish (numFuncs-1, [])
		end
	  in
	    build func;
	    extract()
	  end (* cluster *)

  end
