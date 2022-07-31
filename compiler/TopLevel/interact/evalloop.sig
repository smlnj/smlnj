(* evalloop.sig
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature EVALLOOP =
  sig
    exception Interrupt

    val interact    : unit -> unit
    val evalStream  : string * TextIO.instream -> unit

  (* to wrap exceptions that are raised during the execution of a top-level transaction *)
    exception ExnDuringExecution of exn

  (* true: treat all exns like usercode exns *)
    val withErrorHandling : bool -> {
	    thunk: unit -> unit,
	    flush: unit -> unit,
            cont: exn -> unit
	  } -> unit

    val installCompManagers : {
	    manageImport : Ast.dec * EnvRef.envref -> unit,
	    managePrint : Symbol.symbol * EnvRef.envref -> unit,
	    getPending : unit -> Symbol.symbol list
	  } -> unit

  (* print a message for uncaught exceptions using Control.Print.say *)
    val uncaughtExnMessage : exn -> unit

  end (* signature EVALLOOP *)
