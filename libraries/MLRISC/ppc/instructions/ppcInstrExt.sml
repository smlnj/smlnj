(* ppcInstrExt.sml
 *
 * COPYRIGHT (c) 2004 John Reppy (http://www.cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure PPCInstrExt =
  struct

    datatype ('s, 'r, 'f, 'c) sext
      = STWU of {src : 'r, ea : 'r}	(* store word and update *)

  end
