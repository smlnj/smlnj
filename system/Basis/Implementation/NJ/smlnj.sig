(* smlnj.sig
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

signature SML_OF_NJ =
  sig

    structure Cont : CONT
    structure IntervalTimer : INTERVAL_TIMER
    structure Internals : INTERNALS
    structure SysInfo : SYS_INFO
    structure Weak : WEAK
    structure Susp : SUSP

    val exportML : string -> bool
    val exportFn : (string * ((string * string list) -> OS.Process.status)) -> unit

  (* command-line arguments *)
    val getCmdName : unit -> string
    val getArgs    : unit -> string list
    val getAllArgs : unit -> string list
    val shiftArgs : unit -> unit (* like /bin/sh's "shift" command *)

(** can't handle this yet **
    val use : string -> unit
**)

    datatype 'a frag = QUOTE of string | ANTIQUOTE of 'a

    val exnHistory : exn -> string list

  end;


