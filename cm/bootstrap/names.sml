(* names.sml
 *
 * COPYRIGHT (c) 2022 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 * Some hard-wired file names used by the bootstrap compiler.
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)

structure BtNames =
  struct
    val initgspec = "$smlnj/init/init.cmi"
    val maingspec = "$/root.cm"
    val dirbaseDefault = "sml"
    val penvspec = "pathconfig"
    val bare_preloads = "preloads.bare"
    val standard_preloads = "preloads.standard"
    val bootlist = "BOOTLIST"
    val pidmap = "PIDMAP"
    val bin_infix = ".bin."
    val boot_infix = ".boot."
  end
