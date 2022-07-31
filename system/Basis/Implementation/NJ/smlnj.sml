(* smlnj.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

structure SMLofNJ (* : SML_OF_NJ *) =
  struct

  (* command-line arguments *)
    val getCmdName : unit -> string =
	  CInterface.c_function "SMLNJ-RunT" "cmdName"
    val getArgs : unit -> string list =
	  CInterface.c_function "SMLNJ-RunT" "argv"
    val getAllArgs : unit -> string list =
	  CInterface.c_function "SMLNJ-RunT" "rawArgv"
    val shiftArgs : unit -> unit =
	  CInterface.c_function "SMLNJ-RunT" "shiftArgv"

(** How do we define this here???
    val use = Compiler.Interact.use_file
**)

(*
    datatype 'a frag = QUOTE of string | ANTIQUOTE of 'a
*)
    datatype frag = datatype PrimTypes.frag

    val exnHistory : exn -> string list =
	   InlineT.cast(fn (_,_,hist: string list) => hist)

  end;
