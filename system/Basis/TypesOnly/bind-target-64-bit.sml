(* bind-target-64-bit.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * NOTE: if we make SysWord be Word64 on 32-bit machines, then this
 * binding can be moved to TypesOnly/bind-structs.sml
 *)

structure SysWord = Word64
