(* debug.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 * COPYRIGHT (c) 1989-1991 John H. Reppy
 *
 * Debugging support for the CML core.
 *)

structure Debug : sig

    val sayDebug : string -> unit
    val sayDebugTS : string -> unit
    val sayDebugId : string -> unit

  end = struct

    val sayDebug : string -> unit =
	  Unsafe.CInterface.c_function "SMLNJ-RunT" "debug"

    fun sayDebugTS msg = sayDebug(concat["[", Time.fmt 3 (Time.now()), "] ", msg])

    val getCurThread : unit -> RepTypes.thread_id = Unsafe.getVar

    fun sayDebugId msg = sayDebug(concat[
	    RepTypes.tidToString(getCurThread()), " ", msg
	  ])

  end

