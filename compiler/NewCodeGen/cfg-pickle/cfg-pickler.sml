(* cfg-pickler.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure CFGPickler : sig

    val toFile : string * CFG.comp_unit -> unit

    val toBytes : CFG.comp_unit -> Word8Vector.vector

  end = struct

    val toFile = ASDLFilePickle.toFile CFGFilePickle.write_comp_unit

    val toBytes = ASDLMemoryPickle.toVector CFGMemoryPickle.write_comp_unit

  end
