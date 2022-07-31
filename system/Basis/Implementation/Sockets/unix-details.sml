(* unix-details.sml
 *
 *   Check for would-block error condition on Unix sockets.
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

    val mkIODesc = PreOS.IO.IODesc

 (* The following is essentially lifted from the CML implementation's
  * handling of non-blocking socket I/O.  I am not sure whether
  * conflating EINPROGRESS, EAGAIN, and EWOULDBLOCK is exactly
  * the right thing, though...
  *)
    val blockErrors = (case Posix.Error.syserror "wouldblock"
	   of NONE => [Posix.Error.again, Posix.Error.inprogress]
	    | SOME e => [e, Posix.Error.again, Posix.Error.inprogress]
	  (* end case *))

    fun blockErr (OSImp.SysErr(_, SOME err)) =
	List.exists (fn err' => err = err') blockErrors
      | blockErr _ = false

    fun wrapNB_o f x = SOME (f x)
	handle ex => if blockErr ex then NONE else raise ex

    fun wrapNB_b f x = (f x; true)
	handle ex => if blockErr ex then false else raise ex

  end
