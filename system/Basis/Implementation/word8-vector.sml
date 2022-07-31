(* word8-vector.sml
 *
 * COPYRIGHT (c) 2015 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Word8Vector : MONO_VECTOR =
  struct

    structure V = InlineT.Word8Vector

    (* fast add/subtract avoiding the overflow test *)
    infix -- ++
    fun x -- y = InlineT.Int.fast_sub(x, y)
    fun x ++ y = InlineT.Int.fast_add(x, y)

  (* unchecked access operations *)
    val usub = V.sub
    val uupd = V.update

    type vector = V.vector
    type elem = Word8.word

    val vector0 : vector = InlineT.cast ""
    val createVec : int -> vector = InlineT.cast Assembly.A.create_s

    val maxLen = Core.max_length

    val fromList : elem list -> vector
	  = InlineT.cast CharVector.fromList
    val tabulate : (int * (int -> elem)) -> vector
	  = InlineT.cast CharVector.tabulate

    val length   = V.length
    val sub      = V.chkSub
    val concat : vector list -> vector
          = InlineT.cast CharVector.concat
    val appi : (int * elem -> unit) -> vector -> unit
          = InlineT.cast CharVector.appi
    val app : (elem -> unit) -> vector -> unit
          = InlineT.cast CharVector.app

    val update : (vector * int * elem -> vector)
          = InlineT.cast CharVector.update

    val mapi : (int * elem -> elem) -> vector -> vector
          = InlineT.cast CharVector.mapi
    val map : (elem -> elem) -> vector -> vector
          = InlineT.cast CharVector.map

    val v2cv : vector -> CharVector.vector = InlineT.cast

    fun foldli f init vec = let
	val len = length vec
	fun fold (i, a) =
	    if i >= len then a else fold (i ++ 1, f (i, usub (vec, i), a))
    in
	fold (0, init)
    end

    fun foldl f init vec = let
	val len = length vec
	fun fold (i, a) =
	    if i >= len then a else fold (i ++ 1, f (usub (vec, i), a))
    in
	fold (0, init)
    end

    fun foldri f init vec = let
	fun fold (i, a) =
	    if i < 0 then a else fold (i --1, f (i, usub (vec, i), a))
    in
	fold (length vec -- 1, init)
    end

    fun foldr f init vec = let
	fun fold (i, a) =
	    if i < 0 then a else fold (i --1, f (usub (vec, i), a))
    in
	fold (length vec -- 1, init)
    end

    val findi : (int * elem -> bool) -> vector -> (int * elem) option
          = InlineT.cast CharVector.findi
    val find : (elem -> bool) -> vector -> elem option
          = InlineT.cast CharVector.find
    val exists : (elem -> bool) -> vector -> bool
          = InlineT.cast CharVector.exists
    val all : (elem -> bool) -> vector -> bool
          = InlineT.cast CharVector.all
    val collate : (elem * elem -> order) -> vector * vector -> order
          = InlineT.cast CharVector.collate

  (* added for Basis Library proposal 2015-003 *)
    val toList : vector -> elem list = InlineT.cast CharVector.toList
    val append : vector * elem -> vector = InlineT.cast CharVector.append
    val prepend : elem * vector -> vector = InlineT.cast CharVector.prepend

  end
