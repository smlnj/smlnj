(* flint.sml
 *
 * COPYRIGHT (c) 2021 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure FLINT : FLINT =
  struct

    structure A  = Access   (* should go away soon ??? A.conrep, A.consig used *)
    structure LV = LambdaVar
    structure LT = Lty
    structure FR = FunRecMeta

  (** simple values: variables or constants. *)
    datatype value
      = VAR of LV.lvar
      | INT of int IntConst.t	(* "ty" = size = 0 for IntInf.int *)
      | WORD of int IntConst.t
      | REAL of int RealConst.t
      | STRING of string

  (** the definitions of the lambda expressions *)
    datatype lexp
      = RET of value list
      | LET of LV.lvar list * lexp * lexp

      | FIX of fundec list * lexp
      | APP of value * value list  (* 1st value (the function) is a VAR *)

      | TFN of tfundec * lexp
      | TAPP of value * LT.tyc list  (* value is a VAR *)

      | SWITCH of value * A.consig * (PLambda.con * lexp) list * lexp option
      | CON of PLambda.dataconstr * LT.tyc list * value * LV.lvar * lexp

      | RECORD of FR.rkind * value list * LV.lvar * lexp
      | SELECT of value * int * LV.lvar * lexp  (* value is a VAR *)

      | RAISE of value * LT.lty list  (* value is a VAR *)
      | HANDLE of lexp * value        (* value is a VAR *)

      | BRANCH of primop * value list * lexp * lexp (* 2 "continuations" *)
      | PRIMOP of primop * value list * LV.lvar * lexp

    withtype fundec = FR.fkind * LV.lvar * (LV.lvar * LT.lty) list * lexp
    and tfundec = FR.tfkind * LV.lvar * (LT.tvar * LT.tkind) list * lexp
    and dict = {default: LV.lvar, table: (LT.tyc list * LV.lvar) list}
    and primop = dict option * Primop.primop * LT.lty * LT.tyc list
	    (* Invariant: primop's lty is always fully closed *)

    type prog = fundec  (* was "lvar * lty * lexp" *)

  end (* structure FLINT *)
