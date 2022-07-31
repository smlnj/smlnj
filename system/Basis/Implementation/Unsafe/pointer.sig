(* pointer.sig
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * An interface to raw runtime-system pointers.
 *
 * TODO: eventually, we should flesh this API out to match the MLton `MLTON_POINTER`
 * signature.
 *)

signature UNSAFE_POINTER =
  sig

    structure AddrWord : WORD	(* address-sized word type *)

    type t = PointerImp.c_pointer (* = PrimTypes.c_pointer *)

  (* null pointer *)
    val null : t

  (* size of pointer in bytes *)
    val sizeofPointer : word

  (* convert a pointer to its bit representation *)
    val toWord : t -> AddrWord.word

  (* compare two pointers *)
    val compare : t * t -> order

  (* return a hash key for an address *)
    val hash : t -> word

  end
