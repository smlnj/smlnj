(* flint.sig
 *
 * COPYRIGHT (c) 2021 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature FLINT =
  sig

  (** simple values, including variables and static constants. *)
    datatype value
      = VAR of LambdaVar.lvar
      | INT of int IntConst.t	(* sz = 0 for IntInf.int *)
      | WORD of int IntConst.t
      | REAL of int RealConst.t
      | STRING of string

  (** the definitions of the lambda expressions *)
    datatype lexp
      = RET of value list
      | LET of LambdaVar.lvar list * lexp * lexp

      | FIX of fundec list * lexp
      | APP of value * value list

      | TFN of tfundec * lexp
      | TAPP of value * Lty.tyc list

      | SWITCH of value * Access.consig * (PLambda.con * lexp) list * lexp option
      | CON of PLambda.dataconstr * Lty.tyc list * value * LambdaVar.lvar * lexp

      | RECORD of FunRecMeta.rkind * value list * LambdaVar.lvar * lexp
      | SELECT of value * int * LambdaVar.lvar * lexp

      | RAISE of value * Lty.lty list
      | HANDLE of lexp * value

      | BRANCH of primop * value list * lexp * lexp
      | PRIMOP of primop * value list * LambdaVar.lvar * lexp

    withtype fundec = FunRecMeta.fkind * LambdaVar.lvar * (LambdaVar.lvar * Lty.lty) list * lexp
    and tfundec = FunRecMeta.tfkind * LambdaVar.lvar * (Lty.tvar * Lty.tkind) list * lexp
    and dict = {default: LambdaVar.lvar, table: (Lty.tyc list * LambdaVar.lvar) list}
    and primop = dict option * Primop.primop * Lty.lty * Lty.tyc list
	(* Invariant: primop's lty is always fully closed *)

    type prog = fundec  (* was "lvar * lty * lexp" *)

  end (* signature FLINT *)
