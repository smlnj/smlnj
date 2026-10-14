(* target-amd64.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 * Target information for the amd64 (aka "x86-64" processor)
 *)

structure Target : TARGET =
  struct

    val name = "amd64"

    (* generic 64-bit features *)
    val pointerSz = 64
    val mlValueSz = 64
    val defaultIntSz = 63
    val fixedIntSz = 64
    val defaultRealSz = 64
    val is64 = true
    val alignInBytes = 8

    (* AMD64 features *)
    val bigEndian = false
    val features = [
          (* hardware square root operation *)
          "builtin_sqrt",
          (* floating-point to floating-point hardware rounding; if these
           * are enabled, then we can use the LLVM intrinsics.
           *)
          "builtin_real_ceil",
          "builtin_real_floor",
          "builtin_real_trunc",
          "builtin_real_round"
        ]

  end
