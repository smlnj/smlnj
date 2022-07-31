(* unix-glue.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 * COPYRIGHT (c) 1989-1991 John H. Reppy
 *
 * The Glue for the UNIX version of CML.
 *)

structure UnixGlue : OS_GLUE =
  struct

    fun init () = TimeOut.reset()

    fun pollOS () = (
	  TimeOut.pollTime();
	  IOManager.pollIO();
	  ProcManager.pollProcs())

    fun pause () = (case TimeOut.anyWaiting()
	   of NONE => if (IOManager.anyWaiting() orelse ProcManager.anyWaiting())
		then (Signals.pause(); true)
		else false
(** NOTE: eventually, we should just go to sleep for the specified time **)
	    | (SOME t) => (Signals.pause(); true)
	  (* end case *))

    fun shutdown () = TimeOut.reset()

  end;

