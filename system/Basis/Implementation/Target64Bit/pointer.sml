(* pointer.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Implementation of raw runtime-system pointers for 32-bit targets.
 *)

structure PointerImp =
  struct

    structure AddrWord = Word64Imp

    type c_pointer = InlineT.Pointer.t

    val sizeofPointer = 0w8

    val fromWord = InlineT.Pointer.fromWord64

  (* convert a pointer to its bit representation *)
    val toWord = InlineT.Pointer.toWord64

  (* convert a pointer to its bit representation in the LargeWord type *)
    val toLargeWord = AddrWord.toLarge o InlineT.Pointer.toWord64

  (* compare two pointers *)
    fun compare (p, q) = if (p = q) then EQUAL
	  else if (InlineT.Pointer.toWord64 p < InlineT.Pointer.toWord64 q) then LESS
	  else GREATER

  (* return hash value *)
    fun hash cp = InlineT.Word.fromLarge(AddrWord.>>(InlineT.Pointer.toWord64 cp, 0w3))

  end
