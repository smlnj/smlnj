(* rope.sml
 *
 * COPYRIGHT (c) 2009 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Ropes are a balanced-tree representation of immutable vectors.  This
 * implementation is based on
 *
 *	Ropes: An Alternative to Strings, by Hans-J. Boehm,
 *	Russ Atkinson, and Michael Plass.  Software --- Practice &
 *	Experience, 1995.
 *
 * The original implementation was by Mike Rainey and Adam Shaw and has
 * been modified to match the VECTOR signature.
 *)

structure Rope :> VECTOR =
  struct

    val maxLeafSize = 128

    structure V = Vector
    structure Slice = VectorSlice
    structure W = Word

    val get = Unsafe.Vector.sub

    datatype 'a vector
      = CAT of {depth : int, len : word, l : 'a vector, r : 'a vector}
      | LF of 'a V.vector

    val maxLen : int = 268435456 (* 256 Meg *)

  (* is an index in bounds? *)
    fun inBounds (len, i) = W.<(i, len)

    fun length (CAT{len, ...}) = W.toIntX len
      | length (LF v) = V.length v

    fun size (CAT{len, ...}) = len
      | size (LF v) = W.fromInt(V.length v)

    fun isEmpty rope = (length rope = 0)

    fun sub (rope, i) = let
	  fun sub' (CAT{l, r, ...}, i) = let
		val n = size l
		in
		  if (i < size l) then sub' (l, i) else sub' (r, i - n)
		end
	    | sub' (LF v, i) = get(v, Word.toIntX i)
	  val j = W.fromInt i
	  in
	    if inBounds(size rope, j) then sub' (rope, j) else raise Subscript
	  end

  (* rope construction *)
    local
    (* Compute the nth Fibonacci number, where
     *   fib 0 = 0, fib 1 = 1, fib 2 = 1, etc.
     * Returns 0 for negative args, so be careful.
     *)
      fun fib n = let
	    fun ff (0, u, p) = u
	      | ff (n, u, p) = ff (n-1, u+p, u)
	    in
	      if n <= 0 then 0
	      else if n = 1 then 1
	      else ff (n, 0, 1)
	    end
    (* split a vector into two pieces *)
      fun splitVec (v, n) = let
	    val s1 = Slice.slice(v, 0, SOME n)
	    val s2 = Slice.slice(v, n, NONE)
	    in
	      (Slice.vector s1, Slice.vector s2)
	    end
    (* Split a list into two pieces.  This function is basically
     * take and drop at the same time.  It does not raise an exn if
     * there are not enough elements.
     * ex: split ([1,2,3], 0) => ([],[1,2,3])
     * ex: split ([1,2,3], 1) => ([1],[2,3])
     * ex: split ([1,2,3], 2) => ([1,2],[3])
     * ex: split ([1,2,3], 4) => ([1,2,3],[])
     *)
      fun splitList (xs, n) = let
	    fun loop (_, taken, []) = (List.rev taken, [])
	      | loop (n, taken, L as h::t) = 
		  if n = 0 then (List.rev taken, L)
		  else loop (n-1, h::taken, t)
	    in
	      if n <= 0 then ([], xs)
	      else loop (n, [], xs)
	    end
    (* chop a list into pieces of the given length *)
      fun chop (xs, n) = let
	    fun lp [] = []
	      | lp xs = (case splitList (xs, n)
		   of (xs, []) => [xs]
		    | (xs, ys) => xs :: lp ys
		  (* end case *))
	    in
	      lp xs
	    end
    (* absorb the given sequence in the rightmost leaf of the rope *)
      fun absorbLeft (v, r) = let
	    val n = Word.fromInt(V.length v)
	    fun build (CAT{depth, len, l, r}) = CAT{depth=depth, len=len+n, l=build l, r=r}
	      | build (LF v') = LF(V.concat[v, v'])
	    in
	      build r
	    end
    (* absorb the given sequence in the leftmost leaf of the rope *)
      fun absorbRight (r, v) = let
	    val n = Word.fromInt(V.length v)
	    fun build (CAT{depth, len, l, r}) = CAT{depth=depth, len=len+n, l=l, r=build r}
	      | build (LF v') = LF(V.concat[v', v])
	    in
	      build r
	    end
  (* Concatenates two ropes without balancing.  That is, if the resulting rope is
   * unbalanced, so be it.  Concatenates naturally, but handles the following special
   * cases:
   *   - if either rope is empty, the other rope is returned as-is
   *   - if the ropes are both leaves, and they can be fit in a single leaf, they are
   *   - if the ropes are both leaves, and they can't be fit in a single leaf, they're
   *     packed to the left in a pair of leaves
   *   - if the left rope is a cat and the right is a leaf, and the right leaf can be
   *     packed into the rightmost leaf of the left, it is
   *   - symm. case to previous
   *)
    fun concat2 (r1, r2) =
	  if isEmpty r1 then r2
	  else if isEmpty r2 then r1
	  else (case (r1, r2)
	     of (LF v1, LF v2) => let
		  val len1 = V.length v1
		  val len2 = V.length v2
		  val len = len1 + len2
		  in
		    if (len <= maxLeafSize) then LF(V.concat[v1, v2])
		    else let
		      val df  = maxLeafSize - len1
		      val (v21, v22) = splitVec (v2, df)
		      val lf1 = LF(V.concat[v1, v21])
		      val lf2 = LF v22
		      in
			CAT{depth=1, len=Word.fromInt len, l=lf1, r=lf2}
		      end
		  end
	      | (right as CAT{depth=d, len, l, r}, left as LF v) => let
		  val rightLen = V.length v
		  fun rightmostLeafLen (LF v) = V.length v
		    | rightmostLeafLen (CAT{r, ...}) = rightmostLeafLen r
		  val len = len + Word.fromInt rightLen
		  in
		    if rightmostLeafLen r + rightLen <= maxLeafSize
		      then CAT{depth=d, len=len, l=l, r=absorbRight(r, v)}
		      else CAT{depth=d+1, len=len, l=left, r=right}
		  end
	      | (left as LF v, right as CAT{depth=d, len, l, r}) => let
		  val leftLen = V.length v
		  fun leftmostLeafLen (LF v) = V.length v
		    | leftmostLeafLen (CAT{l, ...}) = leftmostLeafLen l
		  val len = len + Word.fromInt leftLen
		  in
		    if leftmostLeafLen l + leftLen <= maxLeafSize
		      then CAT{depth=d, len=len, l=absorbLeft(v, l), r=r}
		      else CAT{depth=d+1, len=len, l=left, r=right}
		  end 
	      | (left as CAT{depth=d1, len=n1, ...}, right as CAT{depth=d2, len=n2, ...}) =>
		  CAT{depth = 1 + Int.max(d1, d2), len=n1+n2, l=left, r=right}
		(* end case *))
  (* convert a list of leaves into a balanced rope.  We assume that the leaves are
   * all maxLeafSize (except the rightmost one).
   *)
    fun fromLeafList [] = LF(Vector.fromList [])
      | fromLeafList [lf] = lf
      | fromLeafList leaves = let
	(* iteratively concatenate adjacent pairs until we have reduced the list
	 * to a single rope.
	 *)
	  fun reduce (_, [r]) = r
	    | reduce (d, ropes) = let
		fun cat [] = []
		  | cat [r] = [r]
		  | cat (r1::r2::rest) = let
		      val len = size r1 + size r2
		      val r = CAT{depth=d, len=len, l=r1, r=r2}
		      in
			r :: cat rest
		      end
		in
		  reduce (d+1, cat ropes)
		end
	  in
	    reduce (1, leaves)
	  end
(*
    fun balanceIfNecessary r = if isBalanced r then r else balance r
    fun appendWithBalancing (r1, r2) =
	  balanceIfNecessary (concat2 (r1, r2))
*)
    in
(*
    val update : 'a vector * int * 'a -> 'a vector

    fun concat [] = LF(V.fromList [])
      | concat [v] = v
      | concat l = let
	  val n = List.length v
	  in
	    ??
	  end
*)

    fun tabulate (0, f) = LF(V.fromList [])
      | tabulate (n, f) = let
	  fun toLeafList (i, rl) =
		if (i < n)
		  then let
		    val len = Int.min(n - i, maxLeafSize)
		    val lf = LF(V.tabulate (len, fn j => f(i+j)))
		    in
		      toLeafList (i + len, lf :: rl)
		    end
		  else List.rev rl
	  in
	    fromLeafList (toLeafList (0, []))
	  end

  (* create a rope from a list of elements *)
    fun fromList l = let
	  fun toLeafList ([], rl) = List.rev rl
	    | toLeafList (l, rl) = let
		val (l, rest) = splitList (l, maxLeafSize)
		in
		  toLeafList (rest, LF(V.fromList l) :: rl)
		end
	  in
	    fromLeafList (toLeafList (l, []))
	  end
    end

  (* apply a function to the elements of the rope with an index *)
    fun appi f = let
	  fun app' (CAT{l, r, ...}, i) = app' (r, app' (l, i))
	    | app' (LF v, i) = (
		V.appi (fn (j, x) => f(i+j, x)) v;
		i + V.length v)
	  in
	    fn rope => ignore (app' (rope, 0))
	  end

  (* apply a function to the elements of the rope *)
    fun app f = let
	  fun app' (CAT{l, r, ...}) = (app' l; app' r)
	    | app' (LF v) = V.app f v
	  in
	    app'
	  end

    fun mapi f = let
	  fun mapf (CAT{l, r, depth, len}, i) = let
		val l = mapf (l, i)
		val r = mapf (r, i + W.toIntX len)
		in
		  CAT{l=l, r=r, depth=depth, len=len}
		end
	    | mapf (LF v, i) = LF(V.mapi (fn (j, x) => f (i+j, x)) v)
	  in
	    fn rope => mapf(rope, 0)
	  end

    fun map f = let
	  fun mapf (CAT{l, r, depth, len}) = let
		val l = mapf l
		val r = mapf r
		in
		  CAT{l=l, r=r, depth=depth, len=len}
		end
	    | mapf (LF v) = LF(V.map f v)
	  in
	    mapf
	  end

  (* fold left-to-right with index *)
    fun foldli f = let
	  fun fold (CAT{l, r, ...}, i, acc) = let
		val (i, acc) = fold (l, i, acc)
		in
		  fold (r, i, acc)
		end
	    | fold (LF v, i, acc) = let
		val acc = V.foldli (fn (j, x, acc) => f(i+j, x, acc)) acc v
		in
		  (i + V.length v, acc)
		end
	  in
	    fn init => fn rope => #2 (fold (rope, 0, init))
	  end
  (* fold right-to-left with index *)
    fun foldri f = let
	  fun fold (CAT{l, r, ...}, i, acc) = let
		val (i, acc) = fold (r, i, acc)
		in
		  fold (l, i, acc)
		end
	    | fold (LF v, i, acc) = let
		val i = i - V.length v
		val acc = V.foldri (fn (j, x, acc) => f(i+j, x, acc)) acc v
		in
		  (i, acc)
		end
	  in
	    fn init => fn rope => #2 (fold (rope, length rope, init))
	  end
  (* fold left-to-right *)
    fun foldl f = let
	  val foldl' = V.foldl f
	  fun fold (CAT{l, r, ...}, acc) = fold (r, fold (l, acc))
	    | fold (LF v, acc) = foldl' acc v
	  in
	    fn init => fn rope => fold (rope, init)
	  end
  (* fold right-to-left *)
    fun foldr f = let
	  val foldr' = V.foldr f
	  fun fold (CAT{l, r, ...}, acc) = fold (l, fold (r, acc))
	    | fold (LF v, acc) = foldr' acc v
	  in
	    fn init => fn rope => fold (rope, init)
	  end

  (* find leftmost element that satisfies the predicate *)
    fun findi pred = let
	  fun find (CAT{l, r, ...}, i) = (case find (l, i)
		 of NONE => find (r, i + length l)
		  | someItem => someItem
		(* end case *))
	    | find (LF v, i) = (case V.findi (fn (j, x) => pred(i+j, x)) v
		 of NONE => NONE
		  | SOME(j, x) => SOME(i+j, x)
		(* end case *))
	  in
	    fn rope => find (rope, 0)
	  end
    fun find pred = let
	  val find' = V.find pred
	  fun findP (CAT{l, r, ...}) = (case findP l
		 of NONE => findP r
		  | someItem => someItem
		(* end case *))
	    | findP (LF v) = find' v
	  in
	    findP
	  end

  (* return true if there exists an element satisfying the predicate *)
    fun exists pred = let
	  val exists' = V.exists pred
	  fun existsP (CAT{l, r, ...}) = existsP l orelse existsP r
	    | existsP (LF v) = exists' v
	  in
	    existsP
	  end
  (* return true if all elements satisfy the predicate *)
    fun all pred = let
	  val all' = V.all pred
	  fun allP (CAT{l, r, ...}) = allP l andalso allP r
	    | allP (LF v) = all' v
	  in
	    allP
	  end

  (* an iterator over ropes *)
    type 'a iter = int * 'a V.vector * 'a vector list

    fun first (LF v, spine) = (0, v, spine)
      | first (CAT{l, r, ...}, spine) = first(l, r :: spine)

    fun next (i, v, r) = if i < V.length v
	  then SOME(get(v, i), (i+1, v, r))
	  else (case r
	     of [] => NONE
	      | (a::r) => next (first(a, r))
	    (* end case *))

    fun collate cmp = let
	  fun collate' (iter1, iter2) = (case (next iter1, next iter2)
		 of (NONE, NONE) => EQUAL
		  | (NONE, _) => LESS
		  | (_, NONE) => GREATER
		  | (SOME(a, iter1'), SOME(b, iter2')) => (
		      case cmp (a, b)
		       of EQUAL => collate' (iter1', iter2')
			| order => order
		      (* end case *))
		(* end case *))
	  in
	    fn (v1, v2) => collate' (first(v1, []), first(v2, []))
	  end

  end
