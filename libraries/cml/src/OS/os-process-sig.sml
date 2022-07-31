(* os-process-sig.sml
 *
 * COPYRIGHT (c) 2008 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * The CML version of the generic process control interface.
 *)

signature CML_OS_PROCESS =
  sig

    type status

    val success   : status
    val failure   : status
    val isSuccess : status -> bool

    val system    : string -> status
    val systemEvt : string -> status Event.event

    val atExit    : (unit -> unit) -> unit

    val exit      : status -> 'a
    val terminate : status -> 'a

    val getEnv    : string -> string option
    val sleep     : Time.time -> unit

  end
