(* channel-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 * COPYRIGHT (c) 1989-1991 John H. Reppy
 *
 * The representation of synchronous channels.
 *)

signature CHANNEL =
  sig

    type 'a chan
    type 'a event

    val channel : unit -> 'a chan

    val sameChannel : ('a chan * 'a chan) -> bool

    val send : ('a chan * 'a) -> unit
    val recv : 'a chan -> 'a

    val sendEvt  : ('a chan * 'a) -> unit event
    val recvEvt  : 'a chan -> 'a event

    val sendPoll : ('a chan * 'a) -> bool
    val recvPoll : 'a chan -> 'a option

  end

