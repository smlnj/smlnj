(* threads-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 * COPYRIGHT (c) 1989-1991 John H. Reppy
 *)

signature THREAD =
  sig
    type 'a event
    type thread_id

    val getTid : unit -> thread_id

    val sameTid    : (thread_id * thread_id) -> bool
    val compareTid : (thread_id * thread_id) -> order
    val hashTid    : thread_id -> word

    val tidToString : thread_id -> string

    val spawnc : ('a -> unit) -> 'a -> thread_id
    val spawn  : (unit -> unit) -> thread_id

    val exit : unit -> 'a

    val joinEvt : thread_id -> unit event

    val yield : unit -> unit	(* mostly for benchmarking *)

  (* thread-local data *)
    val newThreadProp : (unit -> 'a) -> {
	    clrFn : unit -> unit,	(* clear's current thread's property *)
	    getFn : unit -> 'a,		(* get current thread's property; if *)
					(* the property is not defined, then *)
					(* it sets it using the initialization *)
					(* function. *)
	    peekFn : unit -> 'a option,	(* return the property's value, if any *)
	    setFn : 'a -> unit		(* set the property's value for the *)
					(* current thread. *)
	  }

    val newThreadFlag : unit -> {getFn : unit -> bool, setFn : bool -> unit}

  end;

