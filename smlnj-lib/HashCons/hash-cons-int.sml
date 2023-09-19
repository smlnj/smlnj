(* hash-cons-int.sml
 *
 * Hash-cons wrapper for `int` values; this directly uses the value
 * as its representation w/o a tabke.
 *
 * COPYRIGHT (c) 2023 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure HashConsInt : sig

    type hash_key = int
    type obj = hash_key HashCons.obj

    val mk : hash_key -> obj

  end = struct

    type hash_key = int
    type obj = hash_key HashCons.obj

    fun mk n = {nd = n, tag = Word.fromInt n, hash = Word.fromInt n}

  end
