(* os-glue-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 * COPYRIGHT (c) 1989-1991 John H. Reppy
 *
 * This is the interface to an OS specific module that glues the various
 * OS-specific scheduling operations together (i.e., timeouts, I/O, signals,
 * etc...).
 *)

signature OS_GLUE =
  sig

    val init : unit -> unit
	(* this function is called at start-up time *)

    val pollOS : unit -> unit
	(* this function is called at pre-emption points *)

    val pause : unit -> bool
	(* this function is called when there is nothing else to do.  It returns
	 * false if there are no threads blocked on OS conditions.
	 *)

    val shutdown : unit -> unit
	(* this function is called when the system is shuting down *)

  end
