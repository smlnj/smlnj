(* proc-manager.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 * COPYRIGHT (c) 1989-1991 John H. Reppy
 *
 * Unix process management.
 *
 *)

structure ProcManager : sig

    val addPid : Posix.Process.pid -> Posix.Process.exit_status Event.event

    val pollProcs : unit -> unit

    val anyWaiting : unit -> bool

  end = struct

    structure S = Scheduler
    structure P_Proc = Posix.Process
    structure R = Result

    datatype pid = PID of {
	wait : P_Proc.exit_status R.result,
	pid : P_Proc.pid
      }

    val waiting = ref ([] : pid list)

    fun addPid pid = let
	  val rv = Result.result()
	  in
	    waiting := PID{wait = rv, pid = pid} :: !waiting;
	    Result.getEvt rv
	  end

    fun pollProcs () = let
(** NOTE: it would be more efficient to poll for any zombie process,
 ** until there are no more.
 **)
	  fun pollPid pid = P_Proc.waitpid_nh (P_Proc.W_CHILD pid, [])
	  fun pollItem (item as PID{wait, pid}) = (
		case (pollPid pid)
		 of SOME(_, sts) => (
		      S.enqueueTmpThread (fn () => R.put(wait, sts));
		      false)
		  | NONE => true
		(* end case *))
		  handle ex => (
		    S.enqueueTmpThread (fn () => R.putExn (wait, ex));
		    false)
	  in
	    waiting := List.filter pollItem (! waiting)
	  end

    fun anyWaiting () = (case !waiting of [] => false | _ => true)

  end

