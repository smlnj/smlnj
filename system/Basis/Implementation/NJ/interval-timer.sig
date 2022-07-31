(* interval-timer.sig
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * An interface to the runtime-system interval timer.
 *)

signature INTERVAL_TIMER =
  sig

    val minInterval : unit -> Time.time
	(* the minimum interval that the interval timers support *)

    val setIntTimer : Time.time option -> unit
	(* set the interval timer; NONE means to disable the timer. *)

  end;
