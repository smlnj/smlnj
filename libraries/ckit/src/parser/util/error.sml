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

(* error.sml
 *
 * CS703 --- Project --- Spring '94
 *
 * COPYRIGHT (c) 1992 AT&T Bell Laboratories
 *)

structure Error : ERROR =
struct

  structure F = Format
  structure PP = OldPrettyPrint
  structure SM = SourceMap

  datatype errorState
    = ES of
        {outStrm   : TextIO.outstream,
	 numErrors : int ref,
	 numWarnings : int ref,
	 warningsEnabled : bool ref,
	 errorsEnabled : bool ref,
	 errorsLimit : int,
	 warningsLimit : int}

  (* global error and warning count limits *)
  val errorsLimit = ref 10  (* flag for suppressing error messages *)
  val warningsLimit = ref 10  (* flag for suppressing warning messages *)

(* make an error state.  src is the source file name, dst is the
 * output state to report errors on, lnum and lpos are references
 * used to keep track of the current line number and starting
 * character positions of the scanned lines.
 *)
  fun mkErrState (dst: TextIO.outstream) = 
	ES {outStrm   = dst,
	    numErrors = ref 0,
	    numWarnings = ref 0,
	    warningsEnabled = ref true,
	    errorsEnabled = ref true,
	    errorsLimit = !errorsLimit,
	    warningsLimit = !warningsLimit
	   }

  fun inc (i: int ref) = (i := !i + 1; ())
  fun dec (i: int ref) = (i := !i - 1; ())

(* curried version of TextIO.output *)
  fun outputc outstrm strng = TextIO.output(outstrm, strng)

(* for reporting internal bugs *)
  fun bug (ES{outStrm,...}) (msg: string) : unit=
      TextIO.output(outStrm,("Compiler bug: " ^ msg ^ "\n"))

(* output a warning/error message with location info *)
  fun sayError (es as ES{outStrm, ...}, loc, kind, msg) =
	F.formatf "%s: %s%s\n" (outputc outStrm) [
	    F.STR(SM.locToString loc), F.STR kind, F.STR msg
	  ]

(* output a formatted warning/error message with location info *)
  fun fmtError (es as ES{outStrm, ...}, loc, kind, fmt, items) =
	F.formatf ("%s: %s" ^ fmt ^ "\n") (outputc outStrm)
	  ((F.STR(SM.locToString loc))::(F.STR kind)::items)

(* generate warning messages to the error stream *)
  fun warning (es as ES{numWarnings,warningsLimit,warningsEnabled,...}, loc, msg) =
    if !warningsEnabled then
      (sayError(es, loc, "warning: ", msg);
       inc numWarnings;
       if !numWarnings > warningsLimit then
	 (warningsEnabled := false;
	  sayError(es, loc, "warning: ", "additional warnings suppressed"))
       else ())
    else ()

  fun warningf (es as ES{numWarnings,warningsLimit,warningsEnabled,...},
		loc, fmt, items) = 
    if !warningsEnabled then
      (fmtError(es, loc, "warning: ", fmt, items);
       inc numWarnings;
       if !numWarnings > warningsLimit then
	 (warningsEnabled := false;
	  sayError(es, loc, "warning: ", "additional warnings suppressed"))
       else ())	 
    else ()

  fun noMoreWarnings (es as ES{warningsEnabled,...}) = 
    (warningsEnabled := false;
     sayError(es, SM.UNKNOWN, "warning: ", "additional warnings suppressed."))

(* hints - heuristic help for error messages;
   Note: must be called before error call is generated. *)
  val lastHint = ref (NONE : string option)
  fun hint s = (lastHint := SOME s)

(* generate error messages to the error stream *)
  fun error (es as ES{numErrors, errorsLimit, errorsEnabled,...}, loc, msg) = 
      if !errorsEnabled then
	(case !lastHint of
	   SOME s => (sayError(es, loc, "error: ", msg ^ "\n" ^ s);
		      lastHint := NONE)
	 | NONE => sayError(es, loc, "error: ", msg);
	 inc numErrors;
	 if !numErrors > errorsLimit then
	    (errorsEnabled := false;
	     sayError(es, loc, "warning: ", "additional errors suppressed."))
	 else ())
      else ()

  fun errorf (es as ES{numErrors,errorsLimit,errorsEnabled,...}, loc, fmt, items) =
      if !errorsEnabled then
	(fmtError(es, loc, "error: ", fmt, items);
	 inc numErrors;
	 if !numErrors > errorsLimit then
	    (errorsEnabled := false;
	     sayError(es, loc, "warning: ", "additional errors suppressed."))
	 else ())
      else ()

  fun noMoreErrors(es as ES{errorsEnabled,...}) =
    (errorsEnabled := false;
     sayError(es, SM.UNKNOWN, "warning: ", "additional errors suppressed."))

(* pretty-print an error message on the error stream *)
  fun ppError (es as ES{outStrm, numErrors, ...}, loc, pp) = let
	val ppStrm = PP.mk_ppstream {
		consumer = outputc outStrm,
		flush = fn () => TextIO.flushOut outStrm,
		linewidth = 80
	      }
	in
	  inc numErrors;
	  PP.begin_block ppStrm PP.INCONSISTENT 0;
	    PP.add_string ppStrm
	      (F.format "Error %s: " [F.STR(SM.locToString loc)]);
	    pp ppStrm;
	    PP.add_newline ppStrm;
	  PP.end_block ppStrm;
	  PP.flush_ppstream ppStrm
	end

  fun errStream (ES{outStrm, ...}) = outStrm

(* returns count of errors reported on the state (since last reset) *)
  fun errorCount (ES{numErrors, ...}) =
      !numErrors
(* returns count of warnings reported on the state (since last reset) *)
  fun warningCount (ES{numWarnings, ...}) =
      !numWarnings

(* clears the error and warning counts, so that errorCount and
 * warningCount will return 0. *)
  fun reset (ES{numErrors, numWarnings,...}) =
      (numErrors := 0; numWarnings := 0)

end (* Error *)
