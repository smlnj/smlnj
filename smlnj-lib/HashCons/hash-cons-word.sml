(* hash-cons-word.sml
 *
 * Hash-cons wrapper for `word` values; this directly uses the value
 * as its representation w/o a tabke.
 *
 * COPYRIGHT (c) 2023 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure HashConsWord : sig

    type hash_key = word
    type obj = hash_key HashCons.obj

    val mk : hash_key -> obj

  end = struct

    type hash_key = word
    type obj = hash_key HashCons.obj

    fun mk w = {nd = w, tag = w, hash = w}

  end
