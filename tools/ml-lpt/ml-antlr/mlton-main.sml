(* mlton-main.sml
 *
 * COPYRIGHT (c) 2009 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

val _ = OS.Process.exit (Main.main (CommandLine.name (), CommandLine.arguments ()))
