(* output-sig.sml
 *
 * COPYRIGHT (c) 2005 
 * John Reppy (http://www.cs.uchicago.edu/~jhr)
 * Aaron Turon (adrassi@gmail.com)
 * All rights reserved.
 *
 * The expected signature for any "output" (backend) module.
 *)

signature OUTPUT = 
  sig

    val output : LexOutputSpec.spec * string -> unit

  end