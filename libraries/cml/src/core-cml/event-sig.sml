(* events-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 * COPYRIGHT (c) 1989-1991 John H. Reppy
 *
 * The representation of event values and the event combinators.
 *)

signature EVENT =
  sig

    type 'a event

    val never     : 'a event
    val alwaysEvt : 'a -> 'a event

    val wrap        : ('a event * ('a -> 'b)) -> 'b event
    val wrapHandler : ('a event * (exn -> 'a)) -> 'a event

    val guard    : (unit -> 'a event) -> 'a event
    val withNack : (unit event -> 'a event) -> 'a event

    val choose : 'a event list -> 'a event

    val sync : 'a event -> 'a

    val select : 'a event list -> 'a

  end

