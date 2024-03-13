(* unsafe-real64.sig
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature UNSAFE_REAL64 =
  sig

    val castToWord : Real64.real -> Word64.word
    val castFromWord : Word64.word -> Real64.real

  end
