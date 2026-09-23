(* numkind.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

structure NumKind =
  struct

    (* the numeric types are divided into signed integers, unsigned
     * words, and reals with varying precisions specified in bits.
     * We use `INT ~1` to represent arbitraty precision integers.
     *)
    datatype t
      = INT of int
      | UINT of int
      | FLOAT of int

    val intInfKind = INT ~1
    val dfltIntKind = INT Target.defaultIntSz
    val dfltWordKind = UINT Target.defaultIntSz
    val dfltRealKind = FLOAT Target.defaultRealSz

    fun same (INT n1, INT n2) = (n1 = n2)
      | same (UINT n1, UINT n2) = (n1 = n2)
      | same (FLOAT n1, FLOAT n2) = (n1 = n2)
      | same _ = false

    fun toString (INT bits) = "i" ^ Int.toString bits
      | toString (UINT bits) = "u" ^ Int.toString bits
      | toString (FLOAT bits) = "f" ^ Int.toString bits

  end
