(* errormsg.sig
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature ERRORMSG =
  sig
    datatype severity = WARN | COMPLAIN
    type complainer = severity -> string -> (PrettyPrint.stream -> unit) -> unit
    type errorFn = SourceMap.region -> complainer
    type errors (* = {error: errorFn,
                      errorMatch: region->string,
                      anyErrors : bool ref} *)
    val anyErrors : errors -> bool
    exception Error
    val defaultConsumer : unit -> PrettyPrint.device
    val nullErrorBody : PrettyPrint.stream -> unit
    val error : Source.inputSource -> SourceMap.region -> complainer
    (* with a known location string but without access to the actual source: *)
    val errorNoSource :
	PrettyPrint.device * bool ref -> string -> complainer
    val errorNoFile : PrettyPrint.device * bool ref -> SourceMap.region
                      -> complainer

    val matchErrorString : Source.inputSource -> SourceMap.region -> string
    val errors : Source.inputSource -> errors
    val errorsNoFile : PrettyPrint.device * bool ref -> errors

    val impossible : string -> 'a
    val warn : string -> unit
    val impossibleWithBody : string -> (PrettyPrint.stream -> unit) -> 'a

  end (* signature ERRORMSG *)
