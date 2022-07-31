(* debindex.sig *)
(*
 * COPYRIGHT (c) 2021 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(* I moved this into the elaborator library.  It may be moved
 * back to FLINT if the elaborator gets "cleaned up", i.e., if
 * it is made to be unaware of such middle-end specifics.
 * (08/2001 Blume)
 * When front end is revised to use "named" bound type variables
 * in expressions, this will no longer be relevant to the front
 * end, and probably not to FLINT either. (DBM, 2021.10)
 *)

signature DEB_INDEX =
sig

  eqtype depth
  eqtype index

  val top  : depth
  val next : depth -> depth
  val prev : depth -> depth
  val eq   : depth * depth -> bool
  val getIndex : depth * depth -> index
  val cmp  : depth * depth -> order

  val dp_print : depth -> string
  val dp_key : depth -> int
  val dp_toint: depth -> int
  val dp_fromint: int -> depth

  val di_print : index -> string
  val di_key : index -> int
  val di_toint: index -> int
  val di_fromint: int -> index

  val innermost : index
  val innersnd : index

end (* signature DEB_INDEX *)
