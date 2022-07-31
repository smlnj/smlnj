(* word8-array-slice.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Word8ArraySlice :> MONO_ARRAY_SLICE
			where type elem = Word8.word
			where type array = Word8Array.array
			where type vector = Word8Vector.vector
			where type vector_slice = Word8VectorSlice.slice
  = struct

    type elem = Word8.word
    type array = Word8Array.array
    type vector = Word8Vector.vector
    type vector_slice = Word8VectorSlice.slice

  (* `SL(base, start, len)` with the invariant that
   *	0 <= start <= start+len <= length base
   *)
    datatype slice = SL of (array * int * int)

  (* fast add/subtract avoiding the overflow test *)
    infix 6 -- ++
    fun x -- y = InlineT.Int.fast_sub(x, y)
    fun x ++ y = InlineT.Int.fast_add(x, y)

  (* unchecked array/vector access functions *)
    val usub = InlineT.Word8Array.sub
    val uupd = InlineT.Word8Array.update
    val vusub = InlineT.Word8Vector.sub
    val vuupd = InlineT.Word8Vector.update
    val alength = InlineT.Word8Array.length
    val vlength = InlineT.Word8Vector.length

  (* empty vector *)
    val vector0 : vector = InlineT.cast ""

  (* create an uninitialized vector of known length *)
    val create : int -> vector = InlineT.cast Assembly.A.create_s

    fun length (SL(_, _, len)) = len

    fun sub (SL(base, start, len), i) =
	(* check that 0 <= i < len *)
	  if InlineT.Int.geu(i, len)
	    then raise Subscript
	    else usub (base, start ++ i)

    fun update (SL(base, start, len), i, x) =
	  if InlineT.Int.geu(i, len)
	    then raise Subscript
	    else uupd (base, start ++ i, x)

    fun full arr = SL(arr, 0, alength arr)

    fun base (SL arg) = arg

    fun isEmpty (SL(_, _, 0)) = true
      | isEmpty _ = false

    fun slice (arr, start, olen) = let
	  val al = alength arr
	(* check that 0 <= start <= length arr *)
	  val _ = if InlineT.Int.ltu(al, start) then raise Subscript else ()
	  val avail = al -- start
	  val len = (case olen
		 of NONE => avail
		  | SOME n => if InlineT.Int.ltu(avail, n) (* check: 0 <= n <= avail *)
		      then raise Subscript
		      else n
		(* end case *))
	  in
	    SL(arr, start, len)
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

    fun vector (SL(_, _, 0)) = vector0
      | vector (SL(base, start, len)) = let
	  val s = create len
	  fun fill (i, j) = if i >= len
		then s
		else (
		  vuupd (s, i, usub (base, j));
		  fill (i ++ 1, j ++ 1))
	  in
	    fill (0, start)
	  end

    fun copy { src = SL(base, start, len), dst, di } = let
	(* check that 0 <= di andalso di+len <= length dst *)
	  val _ = if (di < 0) orelse (di > (alength dst -- len))
		then raise Subscript
		else ()
	  in
	  (* need to be careful about the direction of copying for the case
	   * where base = dst!
	   *)
	    if (di >= start)
	      then let (* copy from di+len-1 to di *)
		fun copy (si, di) = if (si >= start)
		      then (
			uupd (dst, di, usub (base, si));
			copy (si -- 1, di -- 1))
		      else ()
		in
		  copy (start ++ len -- 1, di ++ len -- 1)
		end
	      else let (* copy from di to di+len-1 to di *)
		val stop = start ++ len
		fun copy (si, di) = if (si < stop)
		      then (
			uupd (dst, di, usub (base, si));
			copy  (si ++ 1, di ++ 1))
		      else ()
		in
		  copy (start, di)
		end
	  end

    fun copyVec { src = vsl, dst, di } = let
	  val (base, start, vlen) = Word8VectorSlice.base vsl
	(* check that 0 <= di andalso di+vlen <= length dst *)
	  val _ = if (di < 0) orelse (di > (alength dst -- vlen))
		then raise Subscript
		else ()
	  val stop = start ++ vlen
	  fun copy (si, di) = if (si < stop)
		then (
		  uupd (dst, di, vusub (base, si));
		  copy (si ++ 1, di ++ 1))
		else ()
	  in
	    copy (start, di)
	  end

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

    fun modifyi f (SL(base, start, len)) = let
	  val stop = start ++ len
	  fun modifyf i = if (i < stop)
	        then (
		  uupd (base, i, f (i -- start, usub (base, i)));
		  modifyf (i ++ 1))
		else ()
	  in
	    modifyf start
	  end

    fun modify f (SL(base, start, len)) = let
	  val stop = start ++ len
	  fun modifyf i = if (i < stop)
	        then (
		  uupd (base, i, f (usub (base, i)));
		  modifyf (i ++ 1))
		else ()
	  in
	    modifyf start
	  end

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
	      val vec = create n
	      fun copy i = if (i < n)
		    then (
		      vuupd(vec, i, usub(base, start ++ i));
		      copy (i ++ 1))
		    else ()
	      in
		copy 0;
		SOME(vec, SL(base, start ++ n, len -- n))
	      end

  end (* structure Word8ArraySlice *)
