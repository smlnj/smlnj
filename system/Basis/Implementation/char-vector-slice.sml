(* char-vector-slice.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure CharVectorSlice :> MONO_VECTOR_SLICE
				 where type elem = char
				 where type vector = CharVector.vector
				 where type slice = Substring.substring
  = struct

    structure SS = Substring

    type elem = char
    type vector = CharVector.vector
    type slice = SS.substring

  (* fast add/subtract avoiding the overflow test *)
    infix 6 -- ++
    fun x -- y = InlineT.Int.fast_sub(x, y)
    fun x ++ y = InlineT.Int.fast_add(x, y)

  (* unchecked vector access functions *)
    val usub = InlineT.CharVector.sub
    val vuupd = InlineT.CharVector.update
    val vlength = InlineT.CharVector.length

  (* inherit operations from Substring *)
    val length = SS.size
    val sub = SS.sub
    val full = SS.full
    val slice = SS.extract
    val subslice = SS.slice
    val base = SS.base
    val vector = SS.string
    val isEmpty = SS.isEmpty
    val getItem = SS.getc
    val app = SS.app
    val foldl = SS.foldl
    val foldr = SS.foldr
    val concat = SS.concat
    val collate = SS.collate

    fun appi f vs = let
	  val (base, start, len) = SS.base vs
	  fun appf i = if (i < len)
		then (f (i, usub (base, start ++ i)); appf (i ++ 1))
		else ()
	  in
	    appf 0
	  end

    fun foldli f init vs = let
	  val (base, start, len) = SS.base vs
	  fun fold (i, acc) = if (i < len)
		then fold (i ++ 1, f (i, usub (base, start ++ i), acc))
		else acc
	  in
	    fold (0, init)
	  end

    fun foldri f init vs = let
	  val (base, start, len) = SS.base vs
	  fun fold (i, acc) = if (0 <= i)
		then fold (i -- 1, f (i, usub (base, start ++ i), acc))
		else acc
	  in
	    fold (len -- 1, init)
	  end

    fun map f sl = (case SS.base sl
	   of (_, _, 0) => ""
	    | (base, start, len) => let
		val v = Assembly.A.create_s len
		fun mapf i = if (i < len)
		      then (
			vuupd(v, i, f(usub(base, start ++ i)));
			mapf (i ++ 1))
		      else v
		in
		  mapf 0
		end
	  (* end case *))

    fun mapi f sl = (case SS.base sl
	   of (_, _, 0) => ""
	    | (base, start, len) => let
		val v = Assembly.A.create_s len
		fun mapf i = if (i < len)
		      then (
			vuupd(v, i, f(i, usub(base, start ++ i)));
			mapf (i ++ 1))
		      else v
		in
		  mapf 0
		end
	  (* end case *))

    fun findi pred vs = let
	  val (base, start, len) = SS.base vs
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

    fun find pred vs = let
	  val (base, start, len) = SS.base vs
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

    fun exists pred vs = let
	  val (base, start, len) = SS.base vs
	  val stop = start ++ len
	  fun ex i = (i < stop) andalso (pred (usub (base, i)) orelse ex (i ++ 1))
	  in
	    ex start
	  end

    fun all pred vs = let
	  val (base, start, len) = SS.base vs
	  val stop = start ++ len
	  fun ex i = (i < stop) andalso (pred (usub (base, i)) orelse ex (i ++ 1))
	  in
	    ex start
	  end

  (* added for Basis Library proposal 2018-002 *)

    val triml = SS.triml
    val trimr = SS.trimr
    val splitAt = SS.splitAt

    fun getVec (slice, 0) = SOME("", slice)
      | getVec (slice, n) = if (n < 0)
	  then raise Subscript
	  else let
	    val (base, start, len) = SS.base slice
	    val start' = start ++ n
	    fun mkVec () = let
		  val vec = Assembly.A.create_s n
		  fun copy i = if (i < n)
			then (
			  vuupd(vec, i, usub(base, start ++ i));
			  copy (i ++ 1))
			else vec
		  in
		    copy 0
		  end
	    in
	      if (n <= len)
		then SOME(mkVec(), SS.extract(base, start', NONE))
		else NONE
	    end

  end
