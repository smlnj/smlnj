(* arithops.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Representations of the arithmetic and comparison primitive operators.
 * These definitions are shared by both the Primop and CPS.P representations.
 *)

structure ArithOps =
  struct

  (* arithmetic operations that may overflow; for the division operators,
   * we assume that the second argument is never zero (i.e., an explicit
   * test for zero is done before the operation).
   *)
    datatype arithop
      = IADD | ISUB | IMUL | IDIV | IMOD | IQUOT | IREM | INEG

  (* arithmetic operations that do not overflow; for the division operators,
   * we assume that the second argument is never zero (i.e., an explicit
   * test for zero is done before the operation).
   *)
    datatype pureop
      = ADD | SUB | MUL | QUOT | REM | NEG
      | LSHIFT | RSHIFT | RSHIFTL
      | ORB | XORB | ANDB | NOTB
      | FDIV | FABS | FSQRT

  (* comparison operators *)
    datatype cmpop
      = GT | GTE | LT | LTE | EQL | NEQ

    fun arithopToString IADD = "iadd"
      | arithopToString ISUB = "isub"
      | arithopToString IMUL = "imul"
      | arithopToString IDIV = "idiv"
      | arithopToString IMOD = "imod"
      | arithopToString IQUOT = "iquot"
      | arithopToString IREM = "irem"
      | arithopToString INEG = "ineg"

    fun pureopToString ADD = "add"
      | pureopToString SUB = "sub"
      | pureopToString MUL = "mul"
      | pureopToString QUOT = "quot"
      | pureopToString REM = "rem"
      | pureopToString NEG = "neg"
      | pureopToString LSHIFT = "lshift"
      | pureopToString RSHIFT = "rshift"
      | pureopToString RSHIFTL = "rshiftl"
      | pureopToString ANDB = "andb"
      | pureopToString ORB = "orb"
      | pureopToString XORB = "xorb"
      | pureopToString NOTB = "notb"
      | pureopToString FDIV = "fdiv"
      | pureopToString FABS = "fabs"
      | pureopToString FSQRT = "fsqrt"

    fun cmpopToString GT = ">"
      | cmpopToString LT = "<"
      | cmpopToString GTE = ">="
      | cmpopToString LTE = "<="
      | cmpopToString EQL = "="
      | cmpopToString NEQ = "<>"

  end
