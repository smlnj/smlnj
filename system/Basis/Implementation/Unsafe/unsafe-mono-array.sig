(* unsafe-mono-array.sig
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

signature UNSAFE_MONO_ARRAY =
  sig

    type array
    type elem

    val sub : (array * int) -> elem
    val update : (array * int * elem) -> unit
    val create : int -> array

  end;
