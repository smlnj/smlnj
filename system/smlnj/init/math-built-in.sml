(* math-built-in.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Built-in math functions where there is hardware support for sqrt.
 *)

(* REAL32: FIXME *)
structure MathInlineT =
  struct

    val sqrt : real -> real = InLine.real64_sqrt

  end
