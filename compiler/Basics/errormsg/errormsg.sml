(* Basics/errormsg/errormsg.sml
 *
 * COPYRIGHT (c) 2022 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure ErrorMsg : ERRORMSG =
struct

local

  structure PP = NewPP
  structure PPU = NewPPUtil
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
                 errorMatch: SM.region -> PP.format,
                 anyErrors: bool ref}

  val say = Control_Print.say

  (* defaultOutput : unit -> (string -> unit) *)
  fun defaultOutput () = say

  (* nullErrorBody : PP.format
   * default body format *)
  val nullErrorBody = PP.empty

  val lineWidth : int ref = Control_Print.lineWidth

  (* printMessage : output * PP.format * severity * string * PP.format -> unit *)
  fun printMessage (output: output, location: PP.format, severity: severity,
		    msg: string, body: PP.format) =
      case (!BasicControl.printWarnings, severity)
	of (false, WARN) => ()  (* no Warning messages if suppressed *)
	 | _ =>
	   let val msgFormat =
		   (PP.appendNewLine
		      (PP.hblock
			[location,
			 PP.text (* print error label *)
			   (case severity
			      of WARN => "Warning:"
			       | COMPLAIN => "Error:"),
			 PP.text msg, 
			 PP.hardIndent 2 body]))
	    in PP.render (msgFormat, output, !lineWidth)
	   end

  (* record : severity * bool ref -> unit *)
  fun record (COMPLAIN, anyErrors) = anyErrors := true
    | record (WARN,_) = ()

  (* impossible : string -> 'a *)
  fun impossible msg =
      (app say ["Error: Compiler bug: ", msg, "\n"];
       Control_Print.flush();
       raise Error)

  (* warn : string -> unit *)
  fun warn msg =
      (app Control_Print.say ["Warning: ", msg, "\n"];
       Control_Print.flush())

  (* fmtSourceloc : SM.sourceloc -> PP.format *)
  fun fmtSourceloc ({line, column, ...}: SM.sourceloc) = 
        PP.cblock [PP.integer line, PP.period, PP.integer column]

  (* fmtRegion : SR.source option -> SM.region -> PP.format *)
  fun fmtRegion (sourceOp : SR.source option) (SM.REGION (p1,p2)) : PP.format =
      (case sourceOp
         of SOME({sourceMap, fileOpened, ...}) =>
		   let val (lo, hi) = SM.fileregion sourceMap (p1, p2)
		    in PP.hcat (PP.ccat (PP.text fileOpened, PP.colon),
				PP.hblock [fmtSourceloc lo,  PP.text "-", fmtSourceloc hi])
		   end
	       | NONE => PP.cblock [PP.integer p1, PP.text "-", PP.integer p2])

  (* error : SR.source -> SM.region -> complainer *)
  fun error (source: SR.source) ((p1,p2): SM.region) (severity: severity)
            (msg: string) (body : PP.format) =
      (printMessage (defaultOutput (), (fmtRegion (SOME source) (p1,p2)), severity, msg, body);
       record(severity, #anyErrors source))

  (* errorNoSource : output * bool ref -> PP.format -> complainer *)
  fun errorNoSource (output: output, anyErrors: bool ref) loc severity msg body =
      (printMessage (output, loc, severity, msg, body);
       record (severity, anyErrors))

  (* errorNoFile : output * bool ref -> SM.region -> complainer *)
  fun errorNoFile (output, anyErrors) ((p1,p2): SM.region) severity msg body =
      (printMessage (output, fmtRegion NONE (p1, p2), severity, msg, body);
       record(severity, anyErrors))

  (* impossibleWithBody : string -> PP.format -> 'a *)
  fun impossibleWithBody (msg: string) (body: PP.format) =
      (PP.printFormatNL
        (PP.vcat (PP.hcat (PP.text "Error: Compiler bug:", PP.text msg), body));
       raise Error)

  (* errors : SR.source -> errors *)
  fun errors (source: SR.source) : errors =
      {error = error source,
       errorMatch = fmtRegion (SOME source),
       anyErrors = #anyErrors source}

  (* anyErrors : errors -> bool *)
  fun anyErrors ({anyErrors, error, errorMatch}: errors) = !anyErrors

  (* errorsNoFile : output * bool ref -> errors *)
  fun errorsNoFile (output : output, anyErrors : bool ref) =
      {error = errorNoFile (output, anyErrors),
       errorMatch = fmtRegion NONE,
       anyErrors = anyErrors}

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
*)
