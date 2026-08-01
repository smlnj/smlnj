(* prim.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 * Representation of primitive operations in the front-end.  These are
 * bound to visible identifiers in Semant/prim/primop-bindings.sml.
 *)

structure PrimOps : PRIM_OPS =
  struct

    datatype numkind = datatype NumKind.t

    datatype inlineop = datatype InlineOps.t

    datatype arithop = datatype ArithOps.t

    datatype pureop = datatype PureOps.t

    datatype cmpop = datatype CompareOps.t

    datatype commonop = datatype CommonOps.t

    datatype t
      = INLINE of inlineop
      | ARITH of {oper : arithop, sz : int}
      | PURE of {oper : pureop, kind : numkind}
      | CMP of {oper: cmpop, kind: numkind}
      | PRIM of commonop

    fun toString (INLINE p) = InlineOps.toString p
      | toString (ARITH{oper, sz}) =
	  concat [ArithOps.toString oper, "_", Int.toString sz]
      | toString (PURE{oper, kind}) =
	  concat [PureOps.toString oper, "_", NumKind.toString kind]
      | toString (CMP{oper, kind}) =
          CompareOps.toString oper ^ NumKind.toString kind
      | toString (PRIM p) = CommonOps.toString p

  end  (* structure PrimOp *)
