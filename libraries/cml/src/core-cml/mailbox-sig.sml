(* mailbox-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories
 * COPYRIGHT (c) 1989-1991 John H. Reppy
 *
 * Asynchronous channels (called mailboxes).
 *)

signature MAILBOX =
  sig

    type 'a mbox

    val mailbox : unit -> 'a mbox

    val sameMailbox : ('a mbox * 'a mbox) -> bool

    val send     : ('a mbox * 'a) -> unit
    val recv     : 'a mbox -> 'a
    val recvEvt  : 'a mbox -> 'a Event.event
    val recvPoll : 'a mbox -> 'a option

  end (* MAILBOX *)

