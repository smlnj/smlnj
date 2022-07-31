(* target.sig
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature TARGET =
  sig

  (* the size of a machine address in bits *)
    val pointerSz : int

  (* the size of an ML value (aka "word") in bits on the target; usually this
   * is the natural pointer size, but some 64-bit targets (e.g., the Alpha)
   * have used 32-bits for ML words in the past.
   *)
    val mlValueSz : int

  (* the size of the default "int" type (== mlValueSz-1) *)
    val defaultIntSz : int

  (* the size of the FixedInt.int type *)
    val fixedIntSz : int

  (* the default size of the "real" type (usually 64-bits) *)
    val defaultRealSz : int

  (* true for 64-bit targets *)
    val is64 : bool

  (* true for big-endian targets (SPARC, PPC) *)
    val bigEndian : bool

  (* standard alignment in bytes (= mlValueSz div 8) *)
    val alignInBytes : int

  end
