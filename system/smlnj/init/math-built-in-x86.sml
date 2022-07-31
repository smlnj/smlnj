(* math-built-in-x86.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Built-in math functions where there is hardware support (i.e., x86).
 *)

(* REAL32: FIXME *)
structure MathInlineT =
  struct

    val sqrt   : real -> real = InLine.real64_sqrt
    val sine   : real -> real = InLine.real64_sin
    val cosine : real -> real = InLine.real64_cos
    val tangent: real -> real = InLine.real64_tan

  end