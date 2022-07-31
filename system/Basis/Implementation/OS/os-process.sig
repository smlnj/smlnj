(* os-process.sig
 *
 * COPYRIGHT (c) 2007 Fellowship of SML/NJ
 * All rights reserved.
 *
 * The generic process control interface.
 *
 *)

signature OS_PROCESS =
  sig

    eqtype status

    val success   : status
    val failure   : status

    val isSuccess : status -> bool

    val system    : string -> status

    val atExit    : (unit -> unit) -> unit

    val exit      : status -> 'a
    val terminate : status -> 'a

    val getEnv : string -> string option

    val sleep : Time.time -> unit

  end
