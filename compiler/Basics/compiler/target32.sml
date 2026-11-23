(* target32.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 * Definition of TARGET for 32-bit targets
 *)

structure Target : TARGET =
  struct

    val name = "generic32"

    val pointerSz = 32
    val mlValueSz = 32
    val defaultIntSz = 31
    val fixedIntSz = 32
    val defaultRealSz = 64
    val is64 = false
    val alignInBytes = 4
    val bigEndian = Endianess.bigEndian

    val features = []

  end
