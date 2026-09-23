(* pure-ops.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 * Representations of the "pure" arithmetic primitive operators.
 *)

structure PureOps =
  struct

  (* arithmetic operations that do not overflow; for the division operators,
   * we assume that the second argument is never zero (i.e., an explicit
   * test for zero is done before the operation).
   *)
    datatype t
      = ADD | SUB | MUL | QUOT | REM | NEG
      | LSHIFT | RSHIFT | RSHIFTL
      | ORB | XORB | ANDB | NOTB
      | CNTPOP | CNTLZ | CNTTZ
      | ROTL | ROTR
      | FDIV | FABS | FSQRT

    fun toString ADD = "add"
      | toString SUB = "sub"
      | toString MUL = "mul"
      | toString QUOT = "quot"
      | toString REM = "rem"
      | toString NEG = "neg"
      | toString LSHIFT = "lshift"
      | toString RSHIFT = "rshift"
      | toString RSHIFTL = "rshiftl"
      | toString ORB = "orb"
      | toString XORB = "xorb"
      | toString ANDB = "andb"
      | toString NOTB = "notb"
      | toString CNTPOP = "cntpop"
      | toString CNTLZ = "cntlz"
      | toString CNTTZ = "cnttz"
      | toString ROTL = "rotl"
      | toString ROTR = "rotr"
      | toString FDIV = "fdiv"
      | toString FABS = "fabs"
      | toString FSQRT = "fsqrt"

  end
