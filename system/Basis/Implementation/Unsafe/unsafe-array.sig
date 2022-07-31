(* unsafe-array.sig
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature UNSAFE_ARRAY =
  sig

    val sub : ('a array * int) -> 'a
    val update : ('a array * int * 'a) -> unit
    val create : (int * 'a) -> 'a array

  end;
