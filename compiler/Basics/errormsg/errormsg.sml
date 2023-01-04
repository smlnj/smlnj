(* Basics/errormsg/errormsg.sml
 *
 * COPYRIGHT (c) 2022 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure ErrorMsg : ERRORMSG =
struct

local

  structure PP = NewPrettyPrint
  structure PPS = PPSourceMap  (* fmtRegion, fmtSourceRegion *)
  structure SR = Source
  structure SM = SourceMap

in
 (* error reporting *)

  exception Error

  type output = string -> unit

  datatype severity = WARN | COMPLAIN

  type complainer = severity -> string -> PP.format -> unit

  type errorFn = SM.region -> complainer
  (* the four arguments of errorFn are: region, severity, string, format *)

  type errors = {error: errorFn,
                 anyErrors: bool ref,
                 fmtRegion: SM.region -> PP.format}

  val say = Control_Print.say  (* == (fn s => TextIO.output (TextIO.stdOut, s)) *)

  (* Default output is currently fixed (= Control.Print.say), but we could make default
   * output "settable" with a function "setOutput: (string -> unit) -> unit".
   * The error reporting functions use defaultOutput for printing/rendering.
   * Could, for instance, use TextIO.stdErr instead of TextIO.stdOut for printing error messages. *)

  (* defaultOutput : unit -> (string -> unit) *)
  fun defaultOutput () = say

  (* nullErrorBody : PP.format
   * default body format *)
  val nullErrorBody = PP.empty

  val lineWidth : int ref = Control_Print.lineWidth

  (* recordError : severity * bool ref -> unit *)
  fun recordError (COMPLAIN, anyErrors) = anyErrors := true
    | recordError (WARN,_) = ()

  (* impossible : string -> 'a *)
  fun impossible (msg: string) =
      (app say ["Error: Compiler bug: ", msg, "\n"];
       Control_Print.flush();
       raise Error)

  (* impossibleWithBody : string -> PP.format -> 'a *)
  fun impossibleWithBody (msg: string) (body: PP.format) =
      (PP.printFormatNL
         (PP.vcat (PP.hcat (PP.text "Error: Compiler bug:", PP.text msg), body));
       raise Error)

  (* warn : string -> unit *)
  fun warn (msg: string) =
      (app Control_Print.say ["Warning: ", msg, "\n"];
       Control_Print.flush())

  fun fmtSeverity WARN = PP.text "Warning:"
    | fmtSeverity COMPLAIN = PP.text "Error:"

  (* fmtMessage : PP.format * severity * string * PP.format -> unit *)
  fun fmtMessage (location: PP.format, severity: severity, msg: string, body: PP.format) =
      case (!BasicControl.printWarnings, severity)
	of (false, WARN) => PP.empty  (* no Warning messages if suppressed *)
	 | _ => PP.appendNewLine
		  (PP.hblock
		     [location,
		      fmtSeverity severity,
		      PP.text msg,
		      PP.breakIndent 2 body])

  (* error : SR.source -> errorFn *)
  fun error (source: SR.source) (region: SM.region) (severity: severity) (msg: string) (body : PP.format) =
      let val errorFmt = fmtMessage (PPS.fmtSourceRegion (source, region), severity, msg, body)
       in recordError (severity, #anyErrors source);
	  PP.render (errorFmt, defaultOutput (), !lineWidth)
      end

  (* errorNoSource : errorFn
   *   2 Uses: compiler: TopLevel/print/pptable.sml, cm: cm/stable/stabilize.sml *)
  val errorNoSource =
      let val dummySource = SR.newSource ("dummy", TextIO.stdIn, false)
          (* dummySource is not "relevant", only field referenced is anyErrors, which
	     may be set to true, but is not accessed elsewhere so it has no effect. *)
       in error dummySource
      end

  (* errors : SR.source -> errors *)
  fun errors (source: SR.source) : errors =
      {error = error source,
       anyErrors = #anyErrors source,
       fmtRegion = (fn region => PPS.fmtSourceRegion (source, region))}

  (* anyErrors : errors -> bool *)
  fun anyErrors ({anyErrors, ...}: errors) = !anyErrors

end (* top local *)
end (* structure ErrorMsg *)

(* [DBM, 2022.09]
    (0) Using formats rather than strings in composing error messages (complainer type).
        This will lead to lots of revised error messages!
    (1) Ramsey's NoWeb hasn't been supported for a long while, so we have
        removed the associated complications!
    (2) The ERRORMSG interface is still a mess, and needs to be revised.
        -- the output (errConsumer) function no longer comes from SR.source
        -- anyErrors (bool ref) still comes from SR.source. Why?
           Where do the anyErrors values come from when calling errorNoFile, errorNoSource,
           etc?
    (3) Is there any problem with having a default output (defaultOutput = say)?

  [DBM: 2022.10.10]
    (1) Revised Source and SourceMap and PPSourceMap.
    (2) Changed field name errorMatch to fmtRegion in errors type record.
    (3) errorNoFile was removed because it was the same as errorNoSource. Two uses of errorNoFile
        in TopLevel/print/pptable.sml were replaced.
    (4) Renamed errorsNoFile to errorsNoSource.
*)
