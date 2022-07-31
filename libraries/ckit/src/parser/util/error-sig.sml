(*
 * Copyright (c) 1996 by Satish Chandra, Brad Richards, Mark D. Hill, 
 * James R. Larus, and David A. Wood.
 *
 * Teapot is distributed under the following conditions:
 * 
 *     You may make copies of Teapot for your own use and modify those copies.
 * 
 *     All copies of Teapot must retain our names and copyright notice.
 * 
 *     You may not sell Teapot or distributed Teapot in conjunction with a
 *     commercial product or service without the expressed written consent of
 *     the copyright holders.
 * 
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 * 
 *)

(* error-sig.sml
 *
 * CS703 --- Project --- Spring '94
 *
 * COPYRIGHT (c) 1992 AT&T Bell Laboratories
 *)

signature ERROR =
sig

  type errorState
    (* the information related to error reporting, including counters
     * for errors and warnings and upper bounds thereon.*)

(* global limit variables *)

  val errorsLimit : int ref
    (* caps number of errors reported on an error state (see mkErrState) *)

  val warningsLimit : int ref
    (* caps number of warnings reported on an error state (see mkErrState) *)


(* creating errorStates *)

  val mkErrState : TextIO.outstream -> errorState
    (* mkErrorState(os): make an error state with destination outstream os.
     * Uses the current values of errorsLimit and warningsLimit as upper bounds
     * on numbers of errors and warnings reported via the resulting errorState.
     *)


(* reporting internal bugs *)

  val bug : errorState -> string -> unit
    (* for reporting internal bugs *)


(* generating warning messages *)

  val warning : (errorState * SourceMap.location * string) -> unit
    (* warning(es,loc,message): the message and location loc will be printed 
     * to the destination outstream component of es *)

  val warningf : (errorState * SourceMap.location * string * Format.fmt_item list)
                 -> unit
    (* warning(es,loc,message,items): the message and location loc and
     * formated representation of items will be printed to the destination
     * outstream component of es *)

  val noMoreWarnings : errorState -> unit
    (* turns off printing of warning messages for the given errorState *)


(* generating error messages *)

  val hint: string -> unit
    (* MAGIC (i.e. really gross hack) that allows you to insert hints
     * that will be utilized by the next call to error.  This was introduced
     * to support better parser error messages.  The next call to error will
     * consume the hint, so it only applies to the next error.  Typically
     * it is a hint as to why the error occurred. *)

  val error : (errorState * SourceMap.location * string) -> unit
    (* warning(es,loc,message): the message and location loc will be printed 
     * to the destination outstream component of es *)

  val errorf : (errorState * SourceMap.location * string * Format.fmt_item list)
    (* warning(es,loc,message,items): the message and location loc and
     * formated representation of items will be printed to the destination
     * outstream component of es *)
               -> unit
  val noMoreErrors : errorState -> unit
    (* turns off printing of warning messages for the given errorState *)

  val ppError :
	(errorState * SourceMap.location * (OldPrettyPrint.ppstream -> unit))
	-> unit
      (* pretty-print an error message on the error stream *)

  val errStream : errorState -> TextIO.outstream
    (* returns the destination outstream of the errorState *)

  val errorCount : errorState -> int
    (* returns n, if there have been n>0 errors reported on the state since
     * it was initialized or last reset *)

  val warningCount : errorState -> int
    (* returns n, if there have been n>0 warnings reported on the state since
     * it was initialized or last reset *)

  val reset : errorState -> unit
    (* clears the error and warnings counts *)

end (* signature ERROR *)
