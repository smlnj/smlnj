(* prim.sig
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

signature PRIM_OPS =
  sig

  (* numkind includes kind and number of bits *)
    datatype numkind (* = datatype NumKind.t *)
      = INT of int
      | UINT of int
      | FLOAT of int

  (* primitive operators that are expanded to lambdas during the translation
   * to PLambda.
   *)
    type inlineop = InlineOps.t

  (* arithmetic operations that may overflow; for the division operators,
   * we assume that the second argument is never zero (i.e., an explicit
   * test for zero is done before the operation).
   *)
    type arithop = ArithOps.t

  (* arithmetic operations that do not overflow; for the division operators,
   * we assume that the second argument is never zero (i.e., an explicit
   * test for zero is done before the operation).
   *)
    type pureop = PureOps.t

  (* comparison operators *)
    type cmpop = CompareOps.t

  (* Primitive operators that are common between Absyn and FLINT *)
    type commonop = CommonOps.t

  (* datatype primop:
   * Various primitive operations. Those that are designated "inline" (L:) in
   * the comments are expanded into lambda code in terms of other operators.
   * "Environmental" primops (occurring in the Inline structure) are indicated
   * by "E:" in the comment.
   *
   * See dev-notes/conversions.md for an explanation of the conversion operators.
   *)
    datatype t
      = INLINE of inlineop
      | ARITH of {oper : arithop, sz : int}
      | PURE of {oper : pureop, kind : numkind}
      | CMP of {oper: cmpop, kind: numkind}
      | PRIM of commonop

  (* return the string representation of a primitive operator *)
    val toString : t -> string

  end (* signature PRIM_OP *)
