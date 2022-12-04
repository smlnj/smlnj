(* Basics/errormsg/errormsg.sig
 *
 * COPYRIGHT (c) 2022 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature ERRORMSG =
sig

    type output = string -> unit

    datatype severity = WARN | COMPLAIN

    type complainer = severity -> string -> NewPrettyPrint.format -> unit
    type errorFn = SourceMap.region -> complainer

    type errors (* = {error: errorFn,
                      fmtRegion: region -> NewPrettyPrint.format,
                      anyErrors : bool ref} *)

    exception Error
    (* Raised by terminal errors (impossible and impossibleWithBody).
     * Should not be handled! *)

    val defaultOutput : unit -> output
    (* Is the default output the responsibility of ErrorMsg?
     * Or should it be in Control.Print? Or compInfo? Or ?
     * Should it be settable? *)

    val errors : Source.source -> errors
    val anyErrors : errors -> bool

    val nullErrorBody : NewPrettyPrint.format  (* == NewPrettyPrint.empty *)

    val error         : Source.source -> errorFn
    val errorNoSource : errorFn  (* == error Source.dummySource, only 2 uses *)

    val impossible : string -> 'a  (* raises Error exception *)
    val impossibleWithBody : string -> NewPrettyPrint.format -> 'a  (* raises Error exception *)
    val warn : string -> unit  (* prints warning message, no exception raised *)

end (* signature ERRORMSG *)
