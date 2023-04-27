(* target32.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Definition of TARGET for 32-bit targets
 *)

structure Target : TARGET =
  struct

    val pointerSz = 32
    val mlValueSz = 32
    val defaultIntSz = 31
    val fixedIntSz = 32
    val defaultRealSz = 64
    val is64 = false
    val bigEndian = Endianess.bigEndian
    val alignInBytes = 4

  end
