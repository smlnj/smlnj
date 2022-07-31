(* win32-details.sml
 *
 *   Check for would-block error condition on WinSockets.
 *
 * Copyright (c) 2003 by The Fellowship of SML/NJ
 *
 * Author: Matthias Blume (blume@tti-c.org)
 *)

structure OpsysDetails : sig

    val mkIODesc : int -> PreOS.IO.iodesc
    val wrapNB_o : ('a -> 'b) -> ('a -> 'b option)
    val wrapNB_b : ('a -> unit) -> ('a -> bool)

  end = struct

    val mkIODesc = PreOS.IO.SockDesc

    (* This is a placeholder for a correct implementation that actually
     * checks the error condition... *)
    fun blockErr (OSImp.SysErr _) = true
      | blockErr _ = false

    fun wrapNB_o f x = SOME (f x)
	handle ex => if blockErr ex then NONE else raise ex

    fun wrapNB_b f x = (f x; true)
	handle ex => if blockErr ex then false else raise ex

  end
