(* real64-values.sml
 *
 * Specific Real64 values that we need both in the real64.sml and num-scan.sml
 * files.
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

structure Real64Values : sig

    (* maximum finite 64-bit real value *)
    val maxFinite : real
    (* minimum normalized positive real value *)
    val minNormalPos : real
    (* minimum positive real value (denormalized) *)
    val minPos : real
    (* positive infinity *)
    val posInf : real
    (* negative infinity *)
    val negInf : real
    (* nan with zero sign bit *)
    val posNaN : real
    (* nan with one sign bit *)
    val negNaN : real

  end = struct

    (* bitcast a Word64.word to a Real64.real *)
    val fromBits = InlineT.Real64.fromBits

    val maxFinite       = fromBits 0wx7FEFFFFFFFFFFFFF
    val minNormalPos    = fromBits 0wx0010000000000000
    val minPos          = fromBits 0wx0000000000000001
    val posInf          = fromBits 0wx7FF0000000000000
    val negInf          = fromBits 0wxFFF0000000000000
    val posNaN          = fromBits 0wx7FF8000000000000
    val negNaN          = fromBits 0wxFFF8000000000000

  end
