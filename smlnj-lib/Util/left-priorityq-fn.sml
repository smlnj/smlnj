(* left-priorityq-fn.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * An implementation of priority queues based on leaftist heaps (see
 * Purely Functional Data Structures by Chris Okasaki).
 *)

functor LeftPriorityQFn (P : PRIORITY) : MONO_PRIORITYQ =
  struct

    type item = P.item

    datatype queue = Q of (int * heap)
    and heap = EMPTY | ND of (int * item * heap * heap)

    val empty  = Q(0, EMPTY)

    fun singletonHeap x = ND(1, x, EMPTY, EMPTY)
    fun singleton x = Q(1, singletonHeap x)

    fun rank EMPTY = 0
      | rank (ND(r, _, _, _)) = r

    fun mkNode (x, a, b) = if (rank a >= rank b)
	  then ND(rank b + 1, x, a, b)
	  else ND(rank a + 1, x, b, a)

    fun mergeHeap (h, EMPTY) = h
      | mergeHeap (EMPTY, h) = h
      | mergeHeap (h1 as ND(_, x, h11, h12), h2 as ND(_, y, h21, h22)) = (
	  case P.compare(P.priority x, P.priority y)
	   of GREATER => mkNode(x, h11, mergeHeap(h12, h2))
	    | _ => mkNode(y, h21, mergeHeap(h1, h22))
	  (* end case *))

    fun insertHeap (h, x) = mergeHeap(singletonHeap x, h)

    fun insert (x, Q(n, h)) = Q(n+1, insertHeap (h, x))

    fun next (Q(_, EMPTY)) = NONE
      | next (Q(n, ND(_, x, h1, h2))) = SOME(x, Q(n-1, mergeHeap(h1, h2)))

    fun remove (Q(_, EMPTY)) = raise List.Empty
      | remove (Q(n, ND(_, x, h1, h2))) = (x, Q(n-1, mergeHeap(h1, h2)))

  (* this is a somewhat brute force implementation that probably could be improved *)
    fun findAndRemove (Q(n, heap), pred) = let
	  fun find (EMPTY, rejects) = NONE
	    | find (ND(_, x, h1, h2), rejects) = if pred x
		then SOME(x, Q(n-1, mergeHeap(rejects, mergeHeap(h1, h2))))
		else find (mergeHeap(h1, h2), insertHeap (rejects, x))
	  in
	    find (heap, EMPTY)
	  end

    fun delete (Q(n, heap), pred) = let
	  fun filter (EMPTY, (n, residual)) = (n, residual)
	    | filter (ND(_, x, h1, h2), (n, residual)) = if pred x
		then filter (h2, filter (h1, (n-1, residual)))
		else filter (h2, filter (h1, (n, insertHeap (residual, x))))
	  in
	    Q (filter (heap, (n, EMPTY)))
	  end

    fun merge (Q(n1, h1), Q(n2, h2)) = Q(n1+n2, mergeHeap(h1, h2))

    fun numItems (Q(n, _)) = n

    fun isEmpty (Q(_, EMPTY)) = true
      | isEmpty _ = false

    fun fromList [] = empty
      | fromList [x] = Q(1, singletonHeap x)
      | fromList l = let
	  fun init ([], n, items) = (n, items)
	    | init (x::r, n, items) = init (r, n+1, singletonHeap x :: items)
	  fun merge ([], [h]) = h
	    | merge ([], hl) = merge (hl, [])
	    | merge ([h], hl) = merge (h::hl, [])
	    | merge (h1::h2::r, l) = merge (r, mergeHeap(h1, h2) :: l)
	  val (len, hs) = init (l, 0, [])
	  in
	    Q(len, merge (hs, []))
	  end

  end;
