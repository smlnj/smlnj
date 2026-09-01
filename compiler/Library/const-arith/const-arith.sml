(* const-arith.sml
 *
 * COPYRIGHT (c) 2025 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure ConstArith = ConstArithGlueFn (
    structure B = BitwiseConstArith
    structure S = SignedTrappingArith
    structure U = UnsignedWrappingArith)
