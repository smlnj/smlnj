(* hash-cons-bool.sml
 *
 * Implementation of hash-consed booleans.
 *
 * COPYRIGHT (c) 2023 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure HashConsBool : sig

    type hash_key = bool
    type obj = hash_key HashCons.obj

    val mk : hash_key -> obj

    (* the hash-consed boolean values *)
    val hcFalse : obj
    val hcTrue : obj

  end = struct

    type hash_key = bool
    type obj = hash_key HashCons.obj

    val hcFalse = {nd = false, tag = 0w0, hash = 0w17}
    val hcTrue = {nd = true, tag = 0w1, hash = 0w13}

    fun mk false = hcFalse
      | mk true = hcTrue

  end
