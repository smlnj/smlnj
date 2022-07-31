(* os-process.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * The generic process control interface.
 * Modified to work for Win32 (no reliance on Posix.Process)
 *)

structure OS_Process : OS_PROCESS =
  struct

    structure S = Scheduler
    structure PM = ProcManager
    structure CC = SMLofNJ.Cont

    structure P = OS.Process
    structure WP = Win32Process

    type status = P.status

    val success = P.success
    val failure = P.failure
    val isSuccess = P.isSuccess

(** NOTE: we probably need to disable timer signals here **)
    fun system' cmd = let
	  val _ = S.stopTimer ()
	  val pid = WP.createProcess (cmd)
	  val _ = S.restartTimer ()
	  in
	    pid
	  end

    fun systemEvt cmd = let
	  val pid = system' cmd
	  val evt = (S.atomicBegin(); PM.addPid pid before S.atomicEnd())
	  in
	    Event.wrap (evt,
	      fn WP.SUCCESS => P.success
	       | _ => P.failure)
	  end

    val system = Event.sync o systemEvt

    fun atExit _ = raise Fail "OS.Process.atExit unimplemented"
    fun exit sts = (S.atomicBegin(); CC.throw (!S.shutdownHook) (true, sts))
    fun terminate sts = (S.atomicBegin(); CC.throw (!S.shutdownHook) (false, sts))

    val getEnv = P.getEnv

  (* should sleep be per-thread or for the whole system? *)
    val sleep = Event.sync o TimeOut.timeOutEvt

  end
