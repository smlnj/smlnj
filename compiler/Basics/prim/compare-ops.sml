(* compare-ops.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 * Representations of the numeric comparison primitive operators.
 *)

structure CompareOps =
  struct

  (* comparison operators *)
    datatype t = GT | GTE | LT | LTE | EQL | NEQ

    fun toString GT = ">"
      | toString LT = "<"
      | toString GTE = ">="
      | toString LTE = "<="
      | toString EQL = "="
      | toString NEQ = "<>"

  end
