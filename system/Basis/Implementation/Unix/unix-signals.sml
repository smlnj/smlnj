(* unix-signals.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * This extends the generic SIGNALS interface to POSIX signals.
 *
 *)

structure UnixSignals : UNIX_SIGNALS =
  struct
    open Signals

  (** The following signals are already defined in SIGNALS:
   **
   **  val sigHUP  : signal	(* hangup *)
   **  val sigINT  : signal	(* interactive interrupt *)
   **  val sigALRM : signal	(* interval timer signal *)
   **  val sigTERM : signal	(* termination *)
   **  val sigGC   : signal	(* garbage collection *)
   **)

  (* required Posix signals *)
    val sigPIPE : signal = Option.valOf(fromString "PIPE")
    val sigQUIT : signal = Option.valOf(fromString "QUIT")
    val sigUSR1 : signal = Option.valOf(fromString "USR1")
    val sigUSR2 : signal = Option.valOf(fromString "USR2")

  (* job-control signals *)
    val sigCHLD : signal = Option.valOf(fromString "CHLD")
    val sigCONT : signal = Option.valOf(fromString "CONT")
    val sigTSTP : signal = Option.valOf(fromString "TSTP")
    val sigTTIN : signal = Option.valOf(fromString "TTIN")
    val sigTTOU : signal = Option.valOf(fromString "TTOU")

  (** other UNIX signals that may be available (depending on the OS):
   **
   ** val sigWINCH  : signal
   ** val sigURG    : signal
   ** val sigIO     : signal
   ** val sigPOLL   : signal
   ** val sigVTALRM : signal
   **)

   end (* UnixSignals *)


