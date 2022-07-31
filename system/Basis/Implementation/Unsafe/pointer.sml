(* pointer.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Implementation of the UNSAFE_POINTER signature for 32-bit targets.
 *)

structure UnsafePointer : UNSAFE_POINTER =
  struct

    structure AddrWord = PointerImp.AddrWord

    type t = PointerImp.c_pointer

  (* null pointer *)
    val null = PointerImp.fromWord 0w0

  (* size of pointer in bytes *)
    val sizeofPointer = PointerImp.sizeofPointer

  (* convert a pointer to its bit representation *)
    val toWord = PointerImp.toWord

  (* compare two pointers *)
    val compare = PointerImp.compare

  (* return a hash key for an address *)
    val hash = PointerImp.hash

  end
