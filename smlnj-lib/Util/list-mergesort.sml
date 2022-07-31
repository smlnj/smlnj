(* list-mergesort.sml
 *
 * COPYRIGHT (c) 2014 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure ListMergeSort : LIST_SORT =
  struct

  (* Given a ">" relation, sort the list into increasing order.  This sort
   * detects initial increasing and decreasing runs and thus is linear
   * time on ordered input.  It is also stable.
   *)
    fun sort gt = let
	  fun revAppend ([], ys) = ys
	    | revAppend (x::xs, ys) = revAppend(xs, x::ys)
	  fun reverse (x, xs) = revAppend (xs, [x])
	(* merge two sorted lists, where we assume that the elements of the first list
	 * appeared before the elements of the second list.
	 *)
	  fun merge ([], ys, acc) = revAppend(acc, ys)
	    | merge (xs, [], acc) = revAppend(acc, xs)
	    | merge (xs as (x::xr), ys as (y::yr), acc) =
	      (* note that if `x` and `y` are equal, then we want `x` first in the result *)
		if gt(y, x)
		  then merge (xr, ys, x::acc)
		  else merge (xs, yr, y::acc)
	(* given a list of lists, where the order of the sublists corresponds to the original
	 * list order, merge neighboring pairs of sublists.
	 *)
	  fun mergeNeighbors ([], yss) = finishPass yss
	    | mergeNeighbors ([xs], yss) = finishPass (xs::yss)
	    | mergeNeighbors (xs1::xs2::xss, yss) =
		mergeNeighbors (xss, merge(xs1, xs2, [])::yss)
	(* finish a mergeNeighbors pass *)
	  and finishPass [] = []
	    | finishPass [xs] = xs
	    | finishPass xss = mergeNeighborsRev (xss, [])
	(* given a list of lists, where the order of the sublists is the reverse of the
	 * original list order, merge neighboring pairs of sublists.
	 *)
	  and mergeNeighborsRev ([], yss) = finishPassRev yss
	    | mergeNeighborsRev ([xs], yss) = finishPassRev (xs::yss)
	    | mergeNeighborsRev (xs1::xs2::xss, yss) =
		mergeNeighborsRev (xss, merge(xs2, xs1, [])::yss)
	(* finish a mergeNeighborsRev pass *)
	  and finishPassRev [] = []
	    | finishPassRev [xs] = xs
	    | finishPassRev xss = mergeNeighbors (xss, [])
        (* the initialization pass computes an initial list of lists, where the
	 * elements of each sub list are either equal or ordered in decreasing
	 * order.
	 *)
	  fun init (prev, [], yss) = mergeNeighbors ([prev]::yss, [])
	    | init (prev, x::xs, yss) = if gt(prev, x)
		  then runDn (x, xs, [prev], yss)
		else if gt(x, prev)
		  then runUp (x, xs, [prev], yss)
		  else runEq (x, xs, [prev], yss)
	(* identify a run of strictly increasing values; we know that `prev::run`
	 * is a strictly increasing run.
	 *)
	  and runUp (prev, [], run, yss) = mergeNeighbors (reverse(prev, run)::yss, [])
	    | runUp (prev, x::xr, run, yss) =
		if gt(x, prev)
		  then runUp (x, xr, prev::run, yss)
		  else init (x, xr, reverse(prev, run)::yss)
	(* identify a run of strictly decreasing values; we know that `prev::run`
	 * is a strictly decreasing run.
	 *)
	  and runDn (prev, [], run, yss) = mergeNeighbors ((prev::run)::yss, [])
	    | runDn (prev, x::xr, run, yss) =
		if gt(prev, x)
		  then runDn (x, xr, prev::run, yss)
		  else init (x, xr, (prev::run)::yss)
	(* identify a run of equal values; note that to preserve stability of the
	 * sort, we need to reverse the order of the run when it is finished.
	 *)
	  and runEq (prev, [], run, yss) = mergeNeighbors (reverse(prev, run)::yss, [])
	    | runEq (prev, x::xr, run, yss) =
		if gt(prev, x) orelse gt(x, prev)
		  then init (x, xr, reverse(prev, run)::yss)
		  else runEq (x, xr, prev::run, yss)
	  in
	    fn [] => [] | (x::xs) => init(x, xs, [])
	  end

  (* Given a comparison function, sort the sequence in ascending order while eliminating
   * duplicates.  This sort detects initial increasing and decreasing runs and thus is linear
   * time on ordered input.
   *)
    fun uniqueSort cmp = let
	  fun revAppend ([], ys) = ys
	    | revAppend (x::xs, ys) = revAppend(xs, x::ys)
	  fun reverse (x, xs) = revAppend (xs, [x])
	  fun merge ([], ys, acc) = revAppend(acc, ys)
	    | merge (xs, [], acc) = revAppend(acc, xs)
	    | merge (xs as (x::xr), ys as (y::yr), acc) = (
		case cmp (x, y)
		 of LESS => merge (xr, ys, x::acc)
		  | EQUAL => merge (xr, yr, x::acc)  (* discard duplicate *)
		  | GREATER => merge (xs, yr, y::acc)
		(* end case *))
	  fun mergeNeighbors ([], yss) = finishPass yss
	    | mergeNeighbors ([xs], yss) = finishPass (xs::yss)
	    | mergeNeighbors (xs1::xs2::xss, yss) =
		mergeNeighbors (xss, merge(xs1, xs2, [])::yss)
	  and finishPass [] = []
	    | finishPass [xs] = xs
	    | finishPass xss = mergeNeighbors (xss, [])
	  fun init (prev, [], yss) = mergeNeighbors ([prev]::yss, [])
	    | init (prev, x::xs, yss) = (case cmp(prev, x)
		 of LESS => runUp (x, xs, [prev], yss)
		  | EQUAL => init (prev, xs, yss) (* discard duplicate *)
		  | GREATER => runDn (x, xs, [prev], yss)
		(* end case *))
	  and runUp (prev, [], run, yss) = mergeNeighbors (reverse(prev, run)::yss, [])
	    | runUp (prev, x::xr, run, yss) = (case cmp (prev, x)
		 of LESS => runUp (x, xr, prev::run, yss)
		  | EQUAL => runUp (prev, xr, run, yss) (* discard duplicate *)
		  | GREATER => init (x, xr, reverse(prev, run)::yss)
		(* end case *))
	  and runDn (prev, [], run, yss) = mergeNeighbors ((prev::run)::yss, [])
	    | runDn (prev, x::xr, run, yss) = (case cmp (prev, x)
		 of LESS => init (x, xr, (prev::run)::yss)
		  | EQUAL => runDn (prev, xr, run, yss) (* discard duplicate *)
		  | GREATER => runDn (x, xr, prev::run, yss)
		(* end case *))
	  in
	    fn [] => [] | (x::xs) => init(x, xs, [])
	  end

  (* is the list sorted in ascending order according to the given ">" relation? *)
    fun sorted (op >) = let
	  fun chk (_, []) = true
	    | chk (x1, x2::xs) = not(x1>x2) andalso chk(x2, xs)
	  in
	    fn [] => true
	     | (x::xs) => chk(x, xs)
	  end

  end (* ListMergeSort *)
