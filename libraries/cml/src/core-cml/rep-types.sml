(* rep-types.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 * COPYRIGHT (c) 1989-1991 John H. Reppy
 *
 * These are the concrete representations of the various CML types.
 * These types are abstract (or not even visible) outside this library.
 *)

structure RepTypes =
  struct

  (* queues --- see queue.sml *)
    datatype 'a queue = Q of {
	front : 'a list ref,
	rear : 'a list ref
      }

  (** thread IDs --- see threads.sml **)
    datatype thread_id = TID of {  (* thread ids *)
	id	   : int,	(* an unique ID *)
	alert	   : bool ref,	(* true, if there is a pending alert on this *)
				(* thread *)
	done_comm  : bool ref,	(* set this whenever this thread does some *)
				(* concurrency operation. *)
	exnHandler : (exn -> unit) ref,	(* root-level exception handler hook *)
	props	   : exn list ref, (* holds thread-local properties *)
	dead       : cvar	(* the cvar that becomes set when the thread *)
				(* dies *)
      }

  (* transaction IDs are used to mark blocked threads in the various waiting
   * queues.  They are "cancelled" when some other event is selected.
   *)
    and trans_id
      = CANCEL
      | TRANS of thread_id

  (* Condition variables --- see events.sml.
   * These are essentially unit valued ivars, and are used for various
   * internal synchronization conditions (e.g., nack events, I/O
   * synchronization, and thread termination).
   *)
    and cvar = CVAR of cvar_state ref
    and cvar_state
      = CVAR_unset of {
	    transId : trans_id ref,
	    cleanUp : unit -> unit,
	    kont : unit SMLofNJ.Cont.cont
	  } list
      | CVAR_set of int

  (** events --- see events.sml **)
    datatype 'a event_status
      = ENABLED of {prio : int, doFn : unit -> 'a}
      | BLOCKED of {
	    transId : trans_id ref, cleanUp : unit -> unit, next : unit -> unit
	  } -> 'a

    type 'a base_evt = unit -> 'a event_status

    datatype 'a event
      = BEVT of 'a base_evt list
      | CHOOSE of 'a event list
      | GUARD of unit -> 'a event
      | W_NACK of unit event -> 'a event

  (* we put this function here, because it is useful when debugging the
   * CML internals.
   *)
    fun tidToString (TID{id, ...}) =
	  concat["[", StringCvt.padLeft #"0" 6 (Int.toString id), "]"]

  end
