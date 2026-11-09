(* prim.sig
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

signature FLINT_PRIM_OPS =
  sig

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

    val toString : t -> string

    val IADD : t        (* default integer addition *)
    val UADD : t        (* default word addition *)
    val IEQL : t        (* equality for default integers *)

    val mkIEQL : int -> t   (* make equality primop for other sizes *)
    val mkUIEQL : int -> t  (* and for unsigned (kind = UINT) *)

    (* return true if the primop is not pure (i.e., it can raise an exception,
     * update memory, allocate mutable memory, or do a continuation operation.
     *)
    val impurePO : t -> bool

  end
