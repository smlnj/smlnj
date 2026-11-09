(* mlton-main.sml
 *
 * COPYRIGHT (c) 2009 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

val _ = OS.Process.exit (Gen.main (CommandLine.name (), CommandLine.arguments ()))
