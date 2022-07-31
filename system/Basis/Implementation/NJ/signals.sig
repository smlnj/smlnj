(* signals.sig
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * These are the two basic interfaces to the run-time system signals support.
 * The interface covers the basic signals operations, plus a small collection
 * of standard signals that should be portable to non-UNIX systems.
 *
 *)

signature SIGNALS =
  sig

    eqtype signal

    datatype sig_action
      = IGNORE
      | DEFAULT
      | HANDLER of (signal * int * unit Cont.cont) -> unit Cont.cont

    val listSignals : unit -> signal list
	(* list the signals supported by this version *)

    val toString : signal -> string
	(* return the name of a signal *)

    val fromString : string -> signal option
	(* return the signal with the corresponding name; returns NONE, if
	 * no such signal exists.
	 *)

    val setHandler : (signal * sig_action) -> sig_action
	(* set the handler for a signal, returning the previous action. *)
    val overrideHandler : (signal * sig_action) -> sig_action
	(* if a signal is not being ignored, then set the handler.  This
	 * returns the previous handler (if IGNORE, then the current handler
	 * is still IGNORE).
	 *)

    val inqHandler : signal -> sig_action
	(* get the current action for the given signal *)

    datatype sigmask
      = MASKALL
      | MASK of signal list

    val maskSignals : sigmask -> unit
	(* mask the specified set of signals: signals that are not IGNORED
	 * will be delivered when unmasked.  Calls to maskSignals nest on a
	 * per signal basis.
	 *)
    val unmaskSignals : sigmask -> unit
	(* Unmask the specified signals.  The unmasking of a signal that is
	 * not masked has no effect.
	 *)

    val masked : unit -> sigmask
	(* return the set of masked signals; the value MASK[] means that
	 * no signals are masked.
	 *)

    val pause : unit -> unit
	(* sleep until the next signal; if called when signals are masked,
	 * then signals will still be masked when pause returns.
	 *)

  (* these signals should be supported even on non-UNIX platforms. *)
    val sigINT : signal		(* interactive interrupt *)
    val sigALRM : signal	(* interval timer signal *)
    val sigTERM : signal	(* termination *)
    val sigGC : signal		(* garbage collection *)

  end; (* SIGNALS *)


