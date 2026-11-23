(* target.sig
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

signature TARGET =
  sig

    (* the target architecture name *)
    val name : string

    (* the size of a machine address in bits *)
    val pointerSz : int

    (* the size of an ML value (aka "word") in bits on the target; usually this
     * is the natural pointer size, but some 64-bit targets (e.g., the Alpha)
     * have used 32-bits for ML words in the past.
     *)
    val mlValueSz : int

    (* the size of the default "int" type *)
    val defaultIntSz : int

    (* the size of the FixedInt.int type *)
    val fixedIntSz : int

    (* the size of the default "real" type (usually 64-bits) *)
    val defaultRealSz : int

    (* true for 64-bit targets *)
    val is64 : bool

    (* standard alignment in bytes (= mlValueSz div 8) *)
    val alignInBytes : int

    (* true for big-endian targets (SPARC, PPC) *)
    val bigEndian : bool

    (* a list of hardware supported features.  These should be a subset of
     *
     *          "builtin_sqrt",
     *          "builtin_real_ceil",
     *          "builtin_real_floor",
     *          "builtin_real_trunc",
     *          "builtin_real_round"
     *
     * The Basis implementation can test for these using the `InlineT.hasFeature`
     * primitive operator.
     *)
    val features : string list

  end
