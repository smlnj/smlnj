(* errormsg.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure ErrorMsg : ERRORMSG =
struct

  structure PP = NewPP
  structure SM = SourceMap

 (* error reporting *)

  exception Error  (* was Syntax, changed to Error in 0.92 *)

  datatype severity = WARN | COMPLAIN

  type output = string -> unit

  type complainer = severity -> string -> (* output -> *) -> unit

  type errorFn = region -> complainer

  type errors = {error: errorFn,
                 errorMatch: region->string,
                 anyErrors: bool ref}

  fun defaultOutput () = Control.Print.say

  val nullErrorBody = PP.empty

  fun printMessage (output: output, location: string, severity: severity,
		    msg: string, body: PP.format) =
      case (!BasicControl.printWarnings, severity)
	of (false, WARN) => ()
	 | _ =>
	    PP.printFormatNL output
	      (PP.hblock
		[PP.text location,
	         PP.text (* print error label *)
		   (case severity
		      of WARN => "Warning:"
		       | COMPLAIN => "Error:"),
		 PP.text msg, 
		 body])


  fun record (COMPLAIN, anyErrors) = anyErrors := true
    | record (WARN,_) = ()

  fun impossible msg =
      (app Control_Print.say ["Error: Compiler bug: ", msg, "\n"];
       Control_Print.flush();
       raise Error)

  fun warn msg =
      (app Control_Print.say ["Warning: ", msg, "\n"];
       Control_Print.flush())

(* [Ramsey] With the advent of source-map resynchronization (a.k.a
 * ( *#line...* ) comments), a contiguous region as seen by the compiler
 * can correspond to one or more contiguous segments in source code, with
 * different segments possibly lying in different source files.
 * We can imagine myriad ways of displaying such information, but we
 * confine ourselves to two:
 *  * When there's just one source region, we have what we had in the old
 *    compiler, and we display it the same way:
 *
 *      name:line.col or
 *      name:line1.col1-line2.col2
 *
 *  * When the region spans two or more source segments, we use an ellipsis instead
 *    of a dash, and if not all regions are from the same file, we provide
 *    the file names of both endpoints (even if the endpoints are the same
 *    file).
 *)
(* [DBM] hasn't been supported for along while. get rid of the associated complications! *)

  (* locationTostring : Source.inputSource -> SourceMap.region -> string *)
  fun locationTostring ({sourceMap,fileOpened,...}:Source.inputSource)
                      ((p1,p2): SourceMap.region) : string =
      let fun shortpoint ({line, column,...}:sourceloc, l) =
             Int.toString line :: "." :: Int.toString column :: l
          fun showpoint (p as {fileName,...}:sourceloc, l) =
             Pathnames.trim fileName :: ":" :: shortpoint (p, l)
          fun allfiles(f, (src:sourceloc, _)::l) =
                f = #fileName src andalso allfiles(f, l)
            | allfiles(f, []) = true
          fun lastpos [(_, hi)] = hi
            | lastpos (h::t) = lastpos t
            | lastpos [] = impossible "lastpos botch in ErrorMsg.locationToString"
      in  concat (
            case fileregion sourceMap (p1, p2)
              of [(lo, hi)] =>
                    if p1+1 >= p2 then showpoint (lo, [])
                    else showpoint(lo, "-" :: shortpoint(hi, []))
               | (lo, _) :: rest =>
                    if allfiles(#fileName lo, rest) then
                      showpoint(lo, "..." :: shortpoint(lastpos rest, []))
                    else
                      showpoint(lo, "..." :: showpoint (lastpos rest, []))
               | [] => [Pathnames.trim fileOpened, ":<nullRegion>"]
          )
      end

  fun error (source as {anyErrors, errConsumer,...}: Source.inputSource)
            ((p1,p2): SourceMap.region) (severity: severity)
            (msg: string) (body : PP.format) =
      (printMessage (errConsumer, (locationToString source (p1,p2)), severity, msg, body);
       record(severity,anyErrors))

  fun errorNoSource (output, anyE) locs sev msg body =
      (printMessage (output, locs, sev, msg, body); record (sev, anyE))

  fun errorNoFile (errConsumer,anyErrors) ((p1,p2): region) severity msg body =
      (ppmsg(errConsumer,
             if p2>0 then concat[Int.toString p1, "-", Int.toString p2]
                     else "",
             severity, msg, body);
       record(severity,anyErrors))

  fun impossibleWithBody (msg: string) (body: PP.format) =
      (PP.printFormatNL
        (PP.vcat
	   (PP.hcat (PP.text "Error: Compiler bug:", PP.text msg),
            body);
       raise Error)

  val matchErrorString = locationToString

  fun errors source =
      {error = error source,
       errorMatch = matchErrorString source,
       anyErrors = #anyErrors source}

  fun anyErrors {anyErrors, error, errorMatch} = !anyErrors

  fun errorsNoFile (output : output, any : bool ref) =
      {error = errorNoFile (output, any),
       errorMatch = fn _ => "Match",
       anyErrors = any}

end  (* structure ErrorMsg *)
