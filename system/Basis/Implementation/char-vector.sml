(* char-vector.sml
 *
 * COPYRIGHT (c) 2015 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Vectors of characters (aka strings).
 *)

structure CharVector : MONO_VECTOR =
  struct

    structure String = StringImp

    (* fast add/subtract avoiding the overflow test *)
    infix -- ++
    fun x -- y = InlineT.Int.fast_sub(x, y)
    fun x ++ y = InlineT.Int.fast_add(x, y)

    val usub = InlineT.CharVector.sub
    val uupd = InlineT.CharVector.update

    type elem = char
    type vector = string

    val maxLen = String.maxSize

    fun checkLen n =
	if InlineT.Int.ltu(maxLen, n) then raise General.Size else ()

    val fromList = String.implode

    fun tabulate (0, _) = ""
      | tabulate (n, f) = let
	  val _ = checkLen n
	  val ss = Assembly.A.create_s n
	  fun fill i =
	      if i < n then (uupd (ss, i, f i); fill (i ++ 1))
	      else ()
	  in
	    fill 0; ss
	  end

    val length   = InlineT.CharVector.length
    val sub      = InlineT.CharVector.chkSub
    val concat   = String.concat

    fun update (v, i, x) = tabulate (length v,
				     fn i' => if i = i' then x
					      else usub (v, i'))

    fun appi f vec = let
	val len = length vec
	fun app i =
	    if i >= len then () else (f (i, usub (vec, i)); app (i ++ 1))
    in
	app 0
    end

    fun app f vec = let
	val len = length vec
	fun app i =
	    if i >= len then () else (f (usub (vec, i)); app (i ++ 1))
    in
	app 0
    end

    fun mapi f vec = tabulate (length vec, fn i => f (i, usub (vec, i)))

    val map = String.map

    fun foldli f init vec = let
	val len = length vec
	fun fold (i, a) =
	    if i >= len then a else fold (i ++ 1, f (i, usub (vec, i), a))
    in
	fold (0, init)
    end

    fun foldri f init vec = let
	fun fold (i, a) =
	    if i < 0 then a else fold (i -- 1, f (i, usub (vec, i), a))
    in
	fold (length vec -- 1, init)
    end

    fun foldl f init vec = let
	val len = length vec
	fun fold (i, a) =
	    if i >= len then a else fold (i ++ 1, f (usub (vec, i), a))
    in
	fold (0, init)
    end

    fun foldr f init vec = let
	fun fold (i, a) =
	    if i < 0 then a else fold (i -- 1, f (usub (vec, i), a))
    in
	fold (length vec -- 1, init)
    end

    fun findi p vec = let
	val len = length vec
	fun fnd i =
	    if i >= len then NONE
	    else let val x = usub (vec, i)
		 in
		     if p (i, x) then SOME (i, x) else fnd (i ++ 1)
		 end
    in
	fnd 0
    end

    fun find p vec = let
	val len = length vec
	fun fnd i =
	    if i >= len then NONE
	    else let val x = usub (vec, i)
		 in
		     if p x then SOME x else fnd (i ++ 1)
		 end
    in
	fnd 0
    end

    fun exists p vec = let
	val len = length vec
	fun ex i = i < len andalso (p (usub (vec, i)) orelse ex (i ++ 1))
    in
	ex 0
    end

    fun all p vec = let
	val len = length vec
	fun al i = i >= len orelse (p (usub (vec, i)) andalso al (i ++ 1))
    in
	al 0
    end

    fun collate c (v1, v2) = let
	val l1 = length v1
	val l2 = length v2
	val l12 = InlineT.Int.min (l1, l2)
	fun col i =
	    if i >= l12 then IntImp.compare (l1, l2)
	    else case c (usub (v1, i), usub (v2, i)) of
		     EQUAL => col (i ++ 1)
		   | unequal => unequal
    in
	col 0
    end

  (* added for Basis Library proposal 2015-003 *)
    local
    (* utility function for extracting the elements of a vector as a list *)
      fun getList (_, 0, l) = l
	| getList (vec, i, l) = let val i = i -- 1
	    in
	      getList (vec, i, usub(vec, i) :: l)
	    end
    in

    fun toList vec = let
	  val n = length vec
	  in
	    getList (vec, n, [])
	  end

    fun append (vec, x) = let
	  val n = length vec
	  val n' = n ++ 1
	  val _ = checkLen n'
	  val ss = Assembly.A.create_s n'
	  fun fill i = if i < n
		then (uupd (ss, i, usub(vec, i)); fill (i ++ 1))
	        else ()
	  in
	    fill 0; uupd (ss, n, x);
	    ss
	  end

    fun prepend (x, vec) = let
	  val n = length vec
	  val n' = n ++ 1
	  val _ = checkLen n'
	  val ss = Assembly.A.create_s n'
	  fun fill i = if i < n
		then (uupd (ss, i ++ 1, usub(vec, i)); fill (i ++ 1))
	        else ()
	  in
	    uupd (ss, 0, x); fill 0;
	    ss
	  end

    end (* local *)

  end (* CharVector *)
