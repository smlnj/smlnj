(* prot-db.sig
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

signature NET_PROT_DB =
  sig
    type entry
    val name : entry -> string
    val aliases : entry -> string list
    val protocol : entry -> int
    val getByName   : string -> entry option
    val getByNumber : int -> entry option
  end

