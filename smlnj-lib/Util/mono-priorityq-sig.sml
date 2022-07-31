(* mono-priorityq-sig.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This signature describes the interface to monomorphic functional
 * priority queues.
 *)

signature MONO_PRIORITYQ =
  sig

    type item
    type queue

    val empty : queue

    val singleton : item -> queue
	(* create a queue from a single item *)

    val fromList : item list -> queue
	(* build a queue from a list of items *)

    val insert : (item * queue) -> queue
	(* insert an item *)

    val remove : queue -> (item * queue)
	(* remove the highest priority item from the queue; raise List.Empty
	 * if the queue is empty.
 	 *)

    val next : queue -> (item * queue) option
	(* remove the highest priority item from the queue; return NONE
	 * if the queue is empty.
	 *)

    val findAndRemove : queue * (item -> bool) -> (item * queue) option
	(* find the item with the highest priority that satisfies the predicate *)

    val delete : queue * (item -> bool) -> queue
	(* delete all elements satisfying the given predicate *)

    val merge : (queue * queue) -> queue
	(* Merge two queues. *)

    val numItems : queue -> int
	(* return the number of items in the queue *)

    val isEmpty : queue -> bool
	(* return true, if the queue is empty *)

  end;
