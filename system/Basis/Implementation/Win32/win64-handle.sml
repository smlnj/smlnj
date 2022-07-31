(* win64-handle.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * An SML interface to the HANDLE type for 64-bit platforms.
 *)

structure Handle : sig

    eqtype t

    val compare : t * t -> order

    val hash : t -> word

    val isValid : t -> bool

    val toString : t -> string

  end = struct

    type t = Word64.word

    val compare = Word64Imp.compare

    fun hash h = WordImp.fromLarge h

    fun isValid (0wxFFFFFFFFFFFFFFFF : t) = false	(* INVALID_HANDLE_VALUE *)
      | isValid _ = true

    fun toString h = "0x" ^ Word64Imp.toString h

  end
