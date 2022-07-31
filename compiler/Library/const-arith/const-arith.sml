(* const-arith.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure ConstArith = ConstArithGlueFn (
    structure B = BitwiseConstArith
    structure S = SignedTrappingArith
    structure U = UnsignedWrappingArith)
