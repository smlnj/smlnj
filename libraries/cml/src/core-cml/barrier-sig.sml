(* barrier-sig.sml
 *
 * COPYRIGHT (c) 2011 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Support for barrier synchronization with global state.  This mechanism
 * is inspired by the similar CHP mechanism (http://www.cs.kent.ac.uk/projects/ofa/chp/).
 *)

signature BARRIER =
  sig

    type 'a barrier
    type 'a enrollment

  (* create a new barrier.  The first argument is the update function that
   * is applied to the global state whenever a barrier synchronization occurs.
   * The second argument is the initial global state.
   *)
    val barrier : ('a -> 'a) -> 'a -> 'a barrier

  (* enroll in a barrier *)
    val enroll : 'a barrier -> 'a enrollment

  (* synchronize on a barrier *)
    val wait : 'a enrollment -> 'a

  (* resign from an enrolled barrier *)
    val resign : 'a enrollment -> unit

  (* get the current state of the barrier *)
    val value : 'a enrollment -> 'a

  end

