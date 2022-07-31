(* mono-array-sort-sig.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * Signature for in-place sorting of monomorphic arrays
 *
 *)

signature MONO_ARRAY_SORT =
  sig

    structure A : MONO_ARRAY

    val sort : (A.elem * A.elem -> order) -> A.array -> unit

    val sorted : (A.elem * A.elem -> order) -> A.array -> bool

  end; (* MONO_ARRAY_SORT *)

