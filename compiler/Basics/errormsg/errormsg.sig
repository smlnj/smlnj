(* Basics/errormsg/errormsg.sig
 *
 * COPYRIGHT (c) 2022 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature ERRORMSG =
sig

    datatype severity = WARN | COMPLAIN
    type complainer = severity -> string -> NewPP.format -> unit
    type output = string -> unit
    type errorFn = SourceMap.region -> complainer
    type errors (* = {error: errorFn,
                      errorMatch: region -> string,
                      anyErrors : bool ref} *)

    exception Error

    val anyErrors : errors -> bool
    val defaultOutput : unit -> (string -> unit)
    val nullErrorBody : NewPP.format

    val error : Source.inputSource -> SourceMap.region -> complainer
    (* with a known location string but without access to the actual source: *)
    val errors : Source.inputSource -> errors
    val errorNoSource : output * bool ref -> string -> complainer
    val errorNoFile : output * bool ref -> SourceMap.region -> complainer
    val errorsNoFile : output * bool ref -> errors

    val matchErrorFormat : Source.inputSource option -> SourceMap.region -> NewPP.format

    val impossible : string -> 'a
    val warn : string -> unit
    val impossibleWithBody : string -> NewPP.format -> 'a

end (* signature ERRORMSG *)
