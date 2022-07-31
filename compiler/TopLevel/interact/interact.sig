(* interact.sig
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature INTERACT =
  sig
    exception Interrupt

    val interact : unit -> unit

  (* compile and execute the contents of a file.  This function, which is used
   * to implement the interactive "use" function, will return true if everything
   * worked without error and will return false if there was a compile-time
   * error.  If an exception is raised during execution of the code, then
   * it is propagated from this function.  Any changes to the environment that
   * happen before a compiler or runtime error is detected will _not_ be rolled
   * back.
   *)
    val use : string -> bool

  (* compile a file; returns true if okay and false on either compile-time
   * or run-time error.  This version of "use" is used to execute files
   * specified on the command line (see system/smlnj/internal/boot-env-fn.sml)
   *)
    val useFile : string -> bool

    val useStream : TextIO.instream -> unit
    val evalStream : TextIO.instream * Environment.environment -> Environment.environment

    val withErrorHandling : bool -> (* true: treat all exns like usercode exns *)
	{ thunk: unit -> unit, flush: unit -> unit, cont: exn -> unit } -> unit

    val installCompManagers:
	{ manageImport : Ast.dec * EnvRef.envref -> unit,
	  managePrint : Symbol.symbol * EnvRef.envref -> unit,
	  getPending : unit -> Symbol.symbol list } -> unit

    val redump_heap_cont : string SMLofNJ.Cont.cont ref

  end  (* signature INTERACT *)
