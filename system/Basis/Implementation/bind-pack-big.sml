(* bind-pack-big.sml
 *
 * COPYRIGHT (c) 2015 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Bind Pack* structures for big-endian architectures.
 *)

structure PackReal64Big =
  struct
    open PackReal64Native
    val isBigEndian : bool = true
  end

structure PackReal64Little =
  struct
    open PackReal64Swap
    val isBigEndian : bool = false
  end
