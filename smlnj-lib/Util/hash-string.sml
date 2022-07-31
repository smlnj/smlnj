(* hash-string.sml
 *
 * COPYRIGHT (c) 2020
 * All rights reserved.
 *)

structure HashString : sig

    val hashString  : string -> word

    val hashSubstring : substring -> word

  end = FNVHash

