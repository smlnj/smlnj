(* endianess-little.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Endianess flag for little-endian targets (x86, amd64)
 *)

structure Endianess : sig

    val bigEndian : bool

  end = struct

    val bigEndian = false

  end;
