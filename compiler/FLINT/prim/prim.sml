(* prim.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 * PLambda/FLINT primitive operators
 *)

structure FPrimOps : FLINT_PRIM_OPS =
  struct

    datatype t
      = ARITH of {oper : ArithOps.t, sz : int}
      | PURE of {oper : PureOps.t, kind : NumKind.t}
      | CMP of {oper: CompareOps.t, kind: NumKind.t}
      | PRIM of CommonOps.t
      (* internal primops introduced during the FLINT phases *)
      | WCAST				(* ? *)
      | MARKEXN				(* mark an exception value with a string *)
      | MKETAG				(* make a new exception tag *)
      | WRAP				(* box a value by wrapping it *)
      | UNWRAP				(* unbox a value by unwrapping it *)

    fun toString (ARITH{oper, sz}) =
                concat [ArithOps.toString oper, "_", Int.toString sz]
      | toString (PURE{oper, kind}) =
          concat [PureOps.toString oper, "_", NumKind.toString kind]
      | toString (CMP{oper, kind}) =
          CompareOps.toString oper ^ NumKind.toString kind
      | toString (PRIM p) = CommonOps.toString p
      | toString WCAST = "wcast"
      | toString MARKEXN = "markexn"
      | toString MKETAG = "mketag"
      | toString WRAP = "wrap"
      | toString UNWRAP = "unwrap"

    val defaultIntKind = NumKind.dfltIntKind
    val defaultUIntKind = NumKind.dfltWordKind

    val IADD = ARITH{oper=ArithOps.IADD, sz=Target.defaultIntSz}
    val UADD = PURE{oper=PureOps.ADD, kind=NumKind.dfltWordKind}

    val IEQL = CMP{oper=CompareOps.EQL, kind=NumKind.dfltIntKind}

    fun mkIEQL size = CMP{oper=CompareOps.EQL, kind=NumKind.INT size}
    fun mkUIEQL size = CMP{oper=CompareOps.EQL, kind=NumKind.UINT size}

    fun impurePO (ARITH _) = true
      | impurePO (PURE _) = false
      | impurePO (CMP _) = false
      | impurePO (PRIM(CommonOps.FSGN _)) = false
      | impurePO (PRIM(CommonOps.EXTEND _)) = false
      | impurePO (PRIM(CommonOps.TRUNC _)) = false
      | impurePO (PRIM(CommonOps.COPY _)) = false
      | impurePO (PRIM CommonOps.PTREQL) = false
      | impurePO (PRIM CommonOps.PTRNEQ) = false
      | impurePO (PRIM CommonOps.POLYEQL) = false
      | impurePO (PRIM CommonOps.POLYNEQ) = false
      | impurePO (PRIM CommonOps.BOXED) = false
      | impurePO (PRIM CommonOps.UNBOXED) = false
      | impurePO (PRIM CommonOps.LENGTH) = false
      | impurePO (PRIM CommonOps.OBJLENGTH) = false
      | impurePO (PRIM CommonOps.CAST) = false
      | impurePO (PRIM CommonOps.CPTR_TO_WORD) = false
      | impurePO (PRIM CommonOps.WORD_TO_CPTR) = false
      | impurePO (PRIM(CommonOps.REAL_TO_BITS _)) = false
      | impurePO (PRIM(CommonOps.BITS_TO_REAL _)) = false
      | impurePO WCAST = false
      | impurePO WRAP = false
      | impurePO UNWRAP = false
      | impurePO _ = true

  end
