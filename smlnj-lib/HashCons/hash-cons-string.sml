(* hash-cons-string.sml
 *
 * COPYRIGHT (c) 2011 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure HashConsString = HashConsGroundFn (
  struct
    type hash_key = string
    val sameKey = (op = : string * string -> bool)
    val hashVal = HashString.hashString
  end)
