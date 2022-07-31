(* pointer.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Implementation of raw runtime-system pointers for 32-bit targets.
 *)

structure PointerImp =
  struct

    structure AddrWord = Word32Imp

    type c_pointer = InlineT.Pointer.t

    val sizeofPointer = 0w4

    val fromWord = InlineT.Pointer.fromWord32

  (* convert a pointer to its bit representation *)
    val toWord = InlineT.Pointer.toWord32

  (* convert a pointer to its bit representation in the LargeWord type *)
    val toLargeWord = AddrWord.toLarge o InlineT.Pointer.toWord32

  (* compare two pointers *)
    fun compare (p, q) = if (p = q) then EQUAL
	  else if (InlineT.Pointer.toWord32 p < InlineT.Pointer.toWord32 q) then LESS
	  else GREATER

  (* return hash value *)
    fun hash cp = InlineT.Word.fromWord32(AddrWord.>>(InlineT.Pointer.toWord32 cp, 0w2))

  end
