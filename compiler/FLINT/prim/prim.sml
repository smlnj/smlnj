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

  end
