(* command-line.sml
 *
 * COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies.
 *)

structure CommandLine : COMMAND_LINE =
  struct
    val name = SMLofNJ.getCmdName
    val arguments = SMLofNJ.getArgs
  end;

