(* priority-sig.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 * Argument signature for functors that implement priority queues.
 *)

signature PRIORITY =
  sig
    type priority
    val compare : (priority * priority) -> order
    type item
    val priority : item -> priority
  end;
