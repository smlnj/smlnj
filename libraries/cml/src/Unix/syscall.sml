(* syscall.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *
 * Some system calls may take a long time to complete and may
 * be interrupted by timer signals before they complete.  This
 * module implements mechanisms to protect against this problem.
 *)

structure Syscall : sig

    val isIntr : OS.syserror -> bool

    val doSyscall : ('a -> 'b) -> 'a -> 'b
	(* do a system call, and restart if it is interrupted *)

    val doAtomicSyscall : ('a -> 'b) -> 'a -> 'b
	(* do a system call with timer signals masked *)

  end = struct

    structure S = Scheduler

    fun isIntr err = (err = Posix.Error.intr)

    fun doAtomicSyscall f x = let
	  val _ = S.stopTimer()
	  val y = (f x) handle ex => (S.restartTimer(); raise ex)
	  in
	    S.restartTimer(); y
	  end

    fun doSyscall f x = let
	  fun try 0 = doAtomicSyscall f x
	    | try n = ((f x)
		handle (ex as OS.SysErr(_, SOME err)) =>
		  if isIntr err then try(n-1) else raise ex)
	  in
	    try 3
	  end

  end;

