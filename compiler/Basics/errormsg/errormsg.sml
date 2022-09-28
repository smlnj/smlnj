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
  structure SM = SourceMap

in
 (* error reporting *)

  exception Error

  datatype severity = WARN | COMPLAIN

  type output = string -> unit

  type complainer = severity -> string -> PP.format -> unit

  type errorFn = region -> complainer
  (* the four arguments of errorFn are: region, severity, string, format *)

  type errors = {error: errorFn,
                 errorMatch: region->string,
                 anyErrors: bool ref}

  (* defaultOutput : unit -> (string -> unit) *)
  fun defaultOutput () = Control.Print.say

  (* nullErrorBody : PP.format *)
  val nullErrorBody = PP.empty  (* default format argument *)

  val lineWidth : int ref = Control.Print.lineWidth

  fun printMessage (output: output, location: PP.format, severity: severity,
		    msg: string, body: PP.format) =
      case (!BasicControl.printWarnings, severity)
	of (false, WARN) => ()  (* no Warning messages if suppressed *)
	 | _ =>
	   let val msgFormat =
		   (PP.appendNewline
		      (PP.hblock
			[location,
			 PP.text (* print error label *)
			   (case severity
			      of WARN => "Warning:"
			       | COMPLAIN => "Error:"),
			 PP.text msg, 
			 PP.hardIndent 2 body]))
	   in PP.render output msgFormat (!lineWidth)
	   end

  fun record (COMPLAIN, anyErrors) = anyErrors := true
    | record (WARN,_) = ()

  fun impossible msg =
      (app Control_Print.say ["Error: Compiler bug: ", msg, "\n"];
       Control_Print.flush();
       raise Error)

  fun warn msg =
      (app Control_Print.say ["Warning: ", msg, "\n"];
       Control_Print.flush())

  fun fmtSourceloc ({line, column, ...}: SM.sourceloc) = 
        PP.parens (PP.cblock [PP.integer line, PP.period, PP.integer column])

  (* fmtRegion : Source.inputSource option -> SourceMap.region -> string *)
  fun fmtRegion (sourceOp : Source.inputSource option)
                 ((p1,p2): SourceMap.region) : PP.format =
      in if (p1 + 1 >= p2)
	 then PP.text "<bad region>"
	 else (case sourceOp
		 of SOME({sourceMap, fileOpened, ...}) =>
		      let val (lo, hi) = fileregion sourceMap (p1, p2)
		       in PP.hcat (PP.ccat (PP.text fileOpened, PP.colon),
				   PP.hblock [fmtSourceloc lo,  PP.text "-", fmtSourceloc hi])
		      end
		  | NONE => PPU.fmtRegion (p1, p2))
      end

  fun error (source: Source.inputSource) ((p1,p2): SourceMap.region) (severity: severity)
            (msg: string) (body : PP.format) =
      (printMessage (defaultOutput, (fmtRegion (SOME source) (p1,p2)), severity, msg, body);
       record(severity, #anyErrors source))

  fun errorNoSource (output, source) locs severity msg body =
      (printMessage (output, locs, sev, msg, body); record (severity, #anyErrors source))

  fun errorNoFile (output, anyErrors) ((p1,p2): region) severity msg body =
      (printMessage (output, fmtRegion NONE (p1, p2), severity, msg, body);
       record(severity, anyErrors))

  fun impossibleWithBody (msg: string) (body: PP.format) =
      (PP.printFormatNL
        (PP.vcat (PP.hcat (PP.text "Error: Compiler bug:", PP.text msg), body);
       raise Error)

  val matchErrorFormat = fmtRegion

  fun errors source =
      {error = error source,
       errorMatch = matchErrorFormat source,
       anyErrors = #anyErrors source}

  (* anyErrors : errors -> bool *)
  fun anyErrors ({anyErrors, error, errorMatch}: errors) = !anyErrors

  fun errorsNoFile (output : output, any : bool ref) =
      {error = errorNoFile (output, any),
       errorMatch = fn _ => "Match",
       anyErrors = any}

end (* top local *)
end (* structure ErrorMsg *)

(* [DBM, 2022.09]
    (0) Using formats rather than strings in composing error messages (complainer type).
        This will lead to lots of revised error messages!
    (1) Ramsey's NoWeb hasn't been supported for a long while, so we have
        removed the associated complications!
    (2) The ERRORMSG interface is still a mess, and needs to be revised.
        -- the output (errConsumer) function no longer comes from Source.inputSource
        -- anyErrors (bool ref) still comes from Source.inputSource. Why?
           Where do the anyErrors values come from when calling errorNoFile, errorNoSource,
           etc?
    (3) Is there any problem with having a default output (defaultOutput = Control.Print.say)?
*)
