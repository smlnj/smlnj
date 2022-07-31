(* timeout-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 * COPYRIGHT (c) 1989-1991 John H. Reppy
 *
 * Exported interface for timeout synchronization.
 *)

signature TIME_OUT =
  sig

    type 'a event

    val timeOutEvt : Time.time -> unit event
    val atTimeEvt : Time.time -> unit event

  end;

