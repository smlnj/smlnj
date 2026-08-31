(* unsafe-mono-vector.sig
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

signature UNSAFE_MONO_VECTOR =
  sig

    type vector
    type elem

    val sub : (vector * int) -> elem
    val update : (vector * int * elem) -> unit
    val create : int -> vector

  end;
