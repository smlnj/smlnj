(* unix-signals.sig
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * This extends the generic SIGNALS interface to POSIX signals.
 *
 *)

signature UNIX_SIGNALS =
  sig
    include SIGNALS

  (** The following signals are already defined in SIGNALS:
   **
   **  val sigHUP  : signal	(* hangup *)
   **  val sigINT  : signal	(* interactive interrupt *)
   **  val sigALRM : signal	(* interval timer signal *)
   **  val sigTERM : signal	(* termination *)
   **  val sigGC   : signal	(* garbage collection *)
   **)

  (* required Posix signals *)
    val sigPIPE : signal
    val sigQUIT : signal
    val sigUSR1 : signal
    val sigUSR2 : signal

  (* job-control signals *)
    val sigCHLD : signal
    val sigCONT : signal
    val sigTSTP : signal
    val sigTTIN : signal
    val sigTTOU : signal

  (** other UNIX signals that may be available (depending on the OS):
   **
   ** val sigWINCH  : signal
   ** val sigURG    : signal
   ** val sigIO     : signal
   ** val sigPOLL   : signal
   ** val sigVTALRM : signal
   **)

   end


