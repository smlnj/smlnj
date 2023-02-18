(* endianess-big.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Endianess flag for big-endian targets (ppc, sparc)
 *)

structure Endianess : sig

    val bigEndian : bool

  end = struct

    val bigEndian = true

  end;
