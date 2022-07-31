(* pp-desc-sig.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This interface provides a declarative way to specify pretty-printing.
 *)

signature PP_DESC =
  sig
    structure PPS : PP_STREAM

    type pp_desc

    val hBox    : pp_desc list -> pp_desc
    val vBox    : (PPS.indent * pp_desc list) -> pp_desc
    val hvBox   : (PPS.indent * pp_desc list) -> pp_desc
    val hovBox  : (PPS.indent * pp_desc list) -> pp_desc
    val box     : (PPS.indent * pp_desc list) -> pp_desc

    val token   : PPS.token -> pp_desc
    val string  : string -> pp_desc

    val style   : (PPS.style * pp_desc list) -> pp_desc

    val break   : {nsp : int, offset : int} -> pp_desc
    val space   : int -> pp_desc
    val cut     : pp_desc
    val newline : pp_desc
    val nbSpace : int -> pp_desc

    val control : (PPS.device -> unit) -> pp_desc

    val description : PPS.stream * pp_desc -> unit

  end

