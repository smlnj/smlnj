(* export.sig
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

signature EXPORT = 
  sig
    val exportML : string -> bool
    val exportFn : (string * ((string * string list) -> OS.Process.status)) -> unit
  end


