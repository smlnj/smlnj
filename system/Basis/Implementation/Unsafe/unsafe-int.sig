(* unsafe-int.sig
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

signature UNSAFE_INT =
  sig

    (* add/subtract w/o overflow checking *)
    val add : int * int -> int
    val sub : int * int -> int

  end
