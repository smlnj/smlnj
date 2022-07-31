(* char-array.sml
 *
 * COPYRIGHT (c) 2015 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure CharArray : MONO_ARRAY =
  struct
    structure String = StringImp
    structure A = InlineT.CharArray

    (* fast add/subtract avoiding the overflow test *)
    infix -- ++
    fun x -- y = InlineT.Int.fast_sub(x, y)
    fun x ++ y = InlineT.Int.fast_add(x, y)

  (* unchecked access operations *)
    val uupd = A.update
    val usub = A.sub
    val vuupd = InlineT.CharVector.update
    val vusub = InlineT.CharVector.sub
    val vlength = InlineT.CharVector.length

    type elem = char
    type vector = string
    type array = A.array

    val maxLen = Core.max_length

    fun array (0, c) = A.newArray0()
      | array (len, c) = if (InlineT.Int.ltu(maxLen, len))
	    then raise General.Size
	    else let
	      val arr = A.create len
	      fun init i = if (i < len)
		    then (uupd(arr, i, c); init(i+1))
		    else ()
	      in
		init 0; arr
	      end

    fun tabulate (0, _) = A.newArray0()
      | tabulate (len, f) = if (InlineT.Int.ltu(maxLen, len))
	    then raise General.Size
	    else let
	      val arr = A.create len
	      fun init i = if (i < len)
		    then (uupd(arr, i, f i); init(i+1))
		    else ()
	      in
		init 0; arr
	      end

    fun fromList [] = A.newArray0()
      | fromList l = let
	  fun length ([], n) = n
	    | length (_::r, n) = length (r, n+1)
	  val len = length (l, 0)
	  val _ = if (maxLen < len) then raise General.Size else ()
	  val arr = A.create len
	  fun init ([], _) = ()
	    | init (c::r, i) = (uupd(arr, i, c); init(r, i+1))
	  in
	    init (l, 0); arr
	  end

    val length      : array -> int		  = InlineT.CharArray.length
    val sub         : (array * int) -> elem	  = InlineT.CharArray.chkSub
    val update      : (array * int * elem) -> unit
                                               = InlineT.CharArray.chkUpdate

    fun vector a =
	case length a of
	    0 => ""
	  | len =>  let
		val s = Assembly.A.create_s len
		fun fill i =
		    if i >= len then ()
		    else (vuupd (s, i, usub (a, i)); fill (i ++ 1))
	    in
		fill 0;
		s
	    end

    fun copy { src, dst, di } = let
	val sl = length src
	val de = sl + di
	fun copyDn (s, d) =
	    if s < 0 then () else (uupd (dst, d, usub (src, s));
				   copyDn (s -- 1, d -- 1))
    in
	if di < 0 orelse de > length dst then raise Subscript
	else copyDn (sl -- 1, de -- 1)
    end

    fun copyVec { src, dst, di } = let
	val sl = vlength src
	val de = sl + di
	fun copyDn (s, d) =
	    if s < 0 then () else (uupd (dst, d, vusub (src, s));
				   copyDn (s -- 1, d -- 1))
    in
	if di < 0 orelse de > length dst then raise Subscript
	else copyDn (sl -- 1, de -- 1)
    end

    fun appi f arr = let
	val len = length arr
	fun app i =
	    if i >= len then () else (f (i, usub (arr, i)); app (i ++ 1))
    in
	app 0
    end

    fun app f arr = let
	val len = length arr
	fun app i =
	    if i >= len then () else (f (usub (arr, i)); app (i ++ 1))
    in
	app 0
    end

    fun modifyi f arr = let
	val len = length arr
	fun mdf i =
	    if i >= len then ()
	    else (uupd (arr, i, f (i, usub (arr, i))); mdf (i ++ 1))
    in
	mdf 0
    end

    fun modify f arr = let
	val len = length arr
	fun mdf i =
	    if i >= len then ()
	    else (uupd (arr, i, f (usub (arr, i))); mdf (i ++ 1))
    in
	mdf 0
    end

    fun foldli f init arr = let
	val len = length arr
	fun fold (i, a) =
	    if i >= len then a else fold (i ++ 1, f (i, usub (arr, i), a))
    in
	fold (0, init)
    end

    fun foldl f init arr = let
	val len = length arr
	fun fold (i, a) =
	    if i >= len then a else fold (i ++ 1, f (usub (arr, i), a))
    in
	fold (0, init)
    end

    fun foldri f init arr = let
	fun fold (i, a) =
	    if i < 0 then a else fold (i -- 1, f (i, usub (arr, i), a))
    in
	fold (length arr -- 1, init)
    end

    fun foldr f init arr = let
	fun fold (i, a) =
	    if i < 0 then a else fold (i -- 1, f (usub (arr, i), a))
    in
	fold (length arr -- 1, init)
    end

    fun findi p arr = let
	val len = length arr
	fun fnd i =
	    if i >= len then NONE
	    else let val x = usub (arr, i)
		 in
		     if p (i, x) then SOME (i, x) else fnd (i ++ 1)
		 end
    in
	fnd 0
    end

    fun find p arr = let
	val len = length arr
	fun fnd i =
	    if i >= len then NONE
	    else let val x = usub (arr, i)
		 in
		     if p x then SOME x else fnd (i ++ 1)
		 end
    in
	fnd 0
    end

    fun exists p arr = let
	val len = length arr
	fun ex i = i < len andalso (p (usub (arr, i)) orelse ex (i ++ 1))
    in
	ex 0
    end

    fun all p arr = let
	val len = length arr
	fun al i = i >= len orelse (p (usub (arr, i)) andalso al (i ++ 1))
    in
	al 0
    end

    fun collate c (a1, a2) = let
	val l1 = length a1
	val l2 = length a2
	val l12 = InlineT.Int.min (l1, l2)
	fun coll i =
	    if i >= l12 then IntImp.compare (l1, l2)
	    else case c (usub (a1, i), usub (a2, i)) of
		     EQUAL => coll (i ++ 1)
		   | unequal => unequal
    in
	coll 0
    end

  (* added for Basis Library proposal 2015-003 *)
    fun toList arr = foldr op :: [] arr

    fun fromVector v = let
	  val n = vlength v
	  in
	    if (n = 0)
	      then A.newArray0()
	      else let
		val arr = A.create n
		fun fill i = if (i < n)
		      then (uupd(arr, i, vusub(v, i)); fill(i ++ 1))
		      else arr
		in
		  fill 0
		end
	  end

    val toVector = vector

  end (* CharArray *)
