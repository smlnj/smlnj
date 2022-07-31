(* array-sort-sig.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Signature for in-place sorting of polymorphic arrays
 *)

signature ARRAY_SORT =
  sig

    val sort   : ('a * 'a -> order) -> 'a array -> unit
    val sorted : ('a * 'a -> order) -> 'a array -> bool

  end (* ARRAY_SORT *)

