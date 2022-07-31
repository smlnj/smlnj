(* win32-handle.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * An SML interface to the HANDLE type for 32-bit platforms.
 *)

structure Handle : sig

    eqtype t

    val compare : t * t -> order

    val hash : t -> word

    val isValid : t -> bool

    val toString : t -> string

  end = struct

    type t = Word32.word

    val compare = Word32Imp.compare

    fun hash h = InlineT.Word.fromWord32 h

    fun isValid (0wxFFFFFFFF : t) = false	(* INVALID_HANDLE_VALUE *)
      | isValid _ = true

    fun toString h = "0x" ^ Word32Imp.toString h

  end
