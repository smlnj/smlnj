(* command-line.sig
 *
 * COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies.
 *)

signature COMMAND_LINE =
  sig

    val name : unit -> string
    val arguments : unit -> string list

  end;

