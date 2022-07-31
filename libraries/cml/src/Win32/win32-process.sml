(* win32-process.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Simulate some of the Posix.Process structure on Win32 machines
 *)

structure Win32Process :
  sig

    type pid
    datatype exit_status = SUCCESS | FAIL
    val createProcess : string -> pid
    val waitForSingleObject : pid -> exit_status option

  end = struct

    type pid = Word32.word  (* actually, a handle *)
    datatype exit_status = SUCCESS | FAIL

    fun cfun x = Unsafe.CInterface.c_function "WIN32-PROCESS" x

    val createProcess : string -> pid = cfun "create_process"

    val wait_for_single_object : pid -> pid option = cfun "wait_for_single_object"
    fun waitForSingleObject (p : pid) = (case (wait_for_single_object p)
	   of NONE => NONE
	    | SOME (v) => if v=0w0 then SOME FAIL else SOME SUCCESS
	  (* end of case *))

  end
