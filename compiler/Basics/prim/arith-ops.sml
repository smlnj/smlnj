(* arithops.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 * Representations of the trapping-arithmetic primitive operators.
 *)

structure ArithOps =
  struct

  (* arithmetic operations that may overflow; for the division operators,
   * we assume that the second argument is never zero (i.e., an explicit
   * test for zero is done before the operation).
   *)
    datatype t
      = IADD | ISUB | IMUL | IDIV | IMOD | IQUOT | IREM | INEG

    fun toString IADD = "iadd"
      | toString ISUB = "isub"
      | toString IMUL = "imul"
      | toString IDIV = "idiv"
      | toString IMOD = "imod"
      | toString IQUOT = "iquot"
      | toString IREM = "irem"
      | toString INEG = "ineg"

  end
