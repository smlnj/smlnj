(* vector-slice.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Author: Matthias Blume (blume@tti-c.org)
 *)

structure VectorSlice :> VECTOR_SLICE =
  struct

  (* `SL(base, start, len)` with the invariant that
   *	0 <= start <= start+len <= length base
   *)
    datatype 'a slice = SL of ('a vector * int * int)

  (* fast add/subtract avoiding the overflow test *)
    infix 6 -- ++
    fun x -- y = InlineT.Int.fast_sub(x, y)
    fun x ++ y = InlineT.Int.fast_add(x, y)

  (* unchecked vector access functions *)
    val usub = InlineT.PolyVector.sub
    val vlength = InlineT.PolyVector.length

  (* empty vector *)
    val vector0 : 'a vector = Assembly.vector0

    fun length (SL(_, _, len)) = len

    fun sub (SL(base, start, len), i) =
	(* check that 0 <= i < len *)
	  if InlineT.Int.geu(i, len)
	    then raise Subscript
	    else usub (base, start ++ i)

    fun full vec = SL(vec, 0, vlength vec)

    fun base (SL arg) = arg

    fun isEmpty (SL(_, _, 0)) = true
      | isEmpty _ = false

    fun slice (vec, start, olen) = let
	  val vl = vlength vec
	(* check that 0 <= start <= length vec *)
	  val _ = if InlineT.Int.ltu(vl, start) then raise Subscript else ()
	  val avail = vl -- start
	  val len = (case olen
		 of NONE => avail
		  | SOME n => if InlineT.Int.ltu(avail, n) (* check: 0 <= n <= avail *)
		      then raise Subscript
		      else n
		(* end case *))
	  in
	    SL(vec, start, len)
	  end

    fun subslice (SL(base, start, len), i, olen) = let
	(* check that 0 <= i <= len *)
	  val _ = if InlineT.Int.ltu(len, i) then raise Subscript else ()
	  val start' = start ++ i
	  val avail = len -- i
	  val len' = (case olen
		 of NONE => avail
		  | SOME n => if InlineT.Int.ltu(avail, n) (* check: 0 <= n <= avail *)
		      then raise Subscript
		      else n
		(* end case *))
	  in
	    SL(base, start', len')
	  end

    fun vector (SL(base, start, len)) =
	  Vector.tabulate (len, fn i => usub (base, start ++ i))

    fun getItem (SL(_, _, 0)) = NONE
      | getItem (SL(base, start, len)) =
	  SOME (usub (base, start), SL(base, start ++ 1, len -- 1))

    fun appi f (SL(base, start, len)) = let
	  fun appf i = if (i < len)
		then (f (i, usub (base, start ++ i)); appf (i ++ 1))
		else ()
	  in
	    appf 0
	  end

    fun app f (SL(base, start, len)) = let
	  val stop = start ++ len
	  fun appf i = if (i < stop)
		then (f (usub (base, i)); appf (i ++ 1))
		else ()
	  in
	    appf start
	  end

    fun mapi f (SL(base, start, len)) =
	  Vector.tabulate (len, fn i => f (i, usub (base, start ++ i)))

    fun map f (SL(base, start, len)) =
	  Vector.tabulate (len, fn i => f (usub (base, start ++ i)))

    fun foldli f init (SL(base, start, len)) = let
	  fun fold (i, acc) = if (i < len)
		then fold (i ++ 1, f (i, usub (base, start ++ i), acc))
		else acc
	  in
	    fold (0, init)
	  end

    fun foldl f init (SL(base, start, len)) = let
	  val stop = start ++ len
	  fun fold (i, acc) = if (i < stop)
		then fold (i ++ 1, f (usub (base, i), acc))
		else acc
	  in
	    fold (start, init)
	  end

    fun foldri f init (SL(base, start, len)) = let
	  fun fold (i, acc) = if (0 <= i)
		then fold (i -- 1, f (i, usub (base, start ++ i), acc))
		else acc
	  in
	    fold (len -- 1, init)
	  end

    fun foldr f init (SL(base, start, len)) = let
	  fun fold (i, acc) = if (start <= i)
		then fold (i -- 1, f (usub (base, i), acc))
		else acc
	  in
	    fold (start ++ len -- 1, init)
	  end

    fun findi pred (SL(base, start, len)) = let
	  fun fnd i = if (i < len)
		then let
		  val x = usub (base, start ++ i)
		  in
		    if pred(i, x) then SOME(i, x) else fnd (i ++ 1)
		  end
		else NONE
	  in
	    fnd 0
	  end

    fun find pred (SL(base, start, len)) = let
	  val stop = start ++ len
	  fun fnd i = if (i < stop)
		then let
		  val x = usub (base, i)
		  in
		    if pred x then SOME x else fnd (i ++ 1)
		  end
		else NONE
	  in
	    fnd start
	  end

    fun exists pred (SL(base, start, len)) = let
	  val stop = start ++ len
	  fun ex i = (i < stop) andalso (pred (usub (base, i)) orelse ex (i ++ 1))
	  in
	    ex start
	  end

    fun all pred (SL(base, start, len)) = let
	  val stop = start ++ len
	  fun ex i = (i < stop) andalso (pred (usub (base, i)) orelse ex (i ++ 1))
	  in
	    ex start
	  end

    fun concat sll =
	  Vector.fromList (List.foldr (fn (sl, l) => foldr op :: l sl) [] sll)

    fun collate cmp (SL(b1, s1, l1), SL(b2, s2, l2)) = let
	  val len = if (l1 < l2) then l1 else l2
	  fun compare i = if (i < len)
		  then (case cmp (usub (b1, s1 ++ i), usub (b2, s2 ++ i))
		     of EQUAL => compare (i ++ 1)
		      | order => order
		    (* end case *))
		else if (l1 < l2)
		  then LESS
		else if (l1 = l2)
		  then EQUAL
		  else GREATER
	  in
	    compare 0
	  end

  (* added for Basis Library proposal 2018-002 *)

    fun triml n (SL(base, start, len)) = if (n < 0)
	    then raise Subscript
	  else if (n < len)
	    then SL(base, start ++ n, len -- n)
	    else SL(base, start ++ len, 0)

    fun trimr n (SL(base, start, len)) = if (n < 0)
	    then raise Subscript
	  else if (n < len)
	    then SL(base, start, len -- n)
	    else SL(base, start, 0)

    fun splitAt (slice as SL(base, start, len), 0) =
	  (SL(base, start, 0), slice)
      | splitAt (SL(base, start, len), i) = let
	(* check that 0 <= i <= len *)
	  val _ = if InlineT.Int.ltu(len, i) then raise Subscript else ()
	  in
	    (SL(base, start, i), SL(base, start ++ i, len -- i))
	  end

    fun getVec (slice, 0) = SOME(vector0, slice)
      | getVec (SL(base, start, len), n) = if (n < 0)
	    then raise Subscript
	  else if (len < n)
	    then NONE
	    else let
	      val start' = start ++ n
	      fun mkVec (i, items) =
		    if i < start
		      then Assembly.A.create_v (n, items)
		      else mkVec (i -- 1, usub (base, i) :: items)
	      in
		SOME(mkVec(start' -- 1, []), SL(base, start', len -- n))
	      end

  end
