(* Basics/errormsg/errormsg.sig
 *
 * COPYRIGHT (c) 2022 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature ERRORMSG =
sig

    type output = string -> unit

    datatype severity = WARN | COMPLAIN
    type complainer = severity -> string -> NewPP.format -> unit
    type errorFn = SourceMap.region -> complainer

    type errors (* = {error: errorFn,
                      errorMatch: region -> NewPP.format,
                      anyErrors : bool ref} *)

    val defaultOutput : unit -> output

    exception Error

    val errors : Source.inputSource -> errors
    val errorsNoFile  : output * bool ref -> errors
    val anyErrors : errors -> bool

    val nullErrorBody : NewPP.format

    val error         : Source.inputSource -> SourceMap.region -> complainer
    val errorNoSource : output * bool ref  -> NewPP.format     -> complainer  (* location format *)
    val errorNoFile   : output * bool ref  -> SourceMap.region -> complainer

    val impossible : string -> 'a
    val warn : string -> unit
    val impossibleWithBody : string -> NewPP.format -> 'a

end (* signature ERRORMSG *)
