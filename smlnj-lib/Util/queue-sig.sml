(* queue-sig.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * Imperative fifos
 *
 *)

signature QUEUE =
  sig
    type 'a queue

    exception Dequeue

    val mkQueue : unit -> 'a queue
	(* make a new queue *)
    val clear : 'a queue -> unit
	(* remove all elements *)
    val isEmpty : 'a queue -> bool
	(* test for empty queue *)
    val enqueue : 'a queue * 'a -> unit
	(* enqueue an element at the rear *)
    val dequeue : 'a queue -> 'a
	(* remove the front element (raise Dequeue if empty) *)
    val next : 'a queue -> 'a option
	(* remove the first element; return NONE if the queue is empty *)
    val delete : ('a queue * ('a -> bool)) -> unit
	(* delete all elements satisfying the given predicate *)
    val head : 'a queue -> 'a
	(* return the first queue element without removing it *)
    val peek : 'a queue -> 'a option
	(* peek at the first queue element without removing it *)
    val length : 'a queue -> int
    val contents : 'a queue -> 'a list
    val app : ('a -> unit) -> 'a queue -> unit
    val map : ('a -> 'b) -> 'a queue -> 'b queue
    val foldl : ('a * 'b -> 'b) -> 'b -> 'a queue -> 'b
    val foldr : ('a * 'b -> 'b) -> 'b -> 'a queue -> 'b

  end
