(* unsafe-vector.sig
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature UNSAFE_VECTOR =
  sig

    val sub : ('a vector * int) -> 'a
    val create : (int * 'a list) -> 'a vector

  end;
