(* plambda.sig
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature PLAMBDA =
  sig

  (*
   * dataconstr records the name of the constructor, the corresponding conrep,
   * and the lambda type lty; value carrying data constructors would have
   * arrow type.
   *)
    type dataconstr = Symbol.symbol * Access.conrep * Lty.lty

  (*
   * con: used to specify all possible switching statements. Efficient switch
   * generation can be applied to DATAcon and INTcon. Otherwise, it is just a
   * shorthand for binary branch trees. In the future, we probably should make
   * it more general, including constants of any numerical types.
   *)
    datatype con
      = DATAcon of dataconstr * Lty.tyc list * LambdaVar.lvar
      | INTcon of int IntConst.t	(* sz = 0 for IntInf.int *)
      | WORDcon of int IntConst.t
      | STRINGcon of string

  (*
   * lexp: the universal typed intermediate language. TFN, TAPP is abstraction
   * and application on type constructors. Structure abstractions and functor
   * abstractions are represented as normal structure and functor definitions
   * with its component properly PACKed. FN defines normal function, FIX defines
   * a set of recursive functions, LET(v,e1,e2) is a syntactic sugar for exprs
   * of forms like APP(FN(v,_,e2), e1); the type of v will be that of e1.
   * APP is the function application. STRECD and STRSEL are structure record
   * selection, VECTOR and VCTSEL are vector record and vector selection.
   * ETAG, RAISE, and HANDLE are for exceptions.
   *)
    datatype lexp
      = VAR of LambdaVar.lvar
      | INT of int IntConst.t	(* sz = 0 for IntInf.int *)
      | WORD of int IntConst.t
      | REAL of int RealConst.t
      | STRING of string
      | PRIM of Primop.primop * Lty.lty * Lty.tyc list
      | GENOP of dict * Primop.primop * Lty.lty * Lty.tyc list

      | FN of LambdaVar.lvar * Lty.lty * lexp
      | FIX of LambdaVar.lvar list * Lty.lty list * lexp list * lexp
      | APP of lexp * lexp
      | LET of LambdaVar.lvar * lexp * lexp

      | TFN of Lty.tkind list * lexp
      | TAPP of lexp * Lty.tyc list

      | RAISE of lexp * Lty.lty
      | HANDLE of lexp * lexp
      | ETAG of lexp * Lty.lty

      | CON of dataconstr * Lty.tyc list * lexp
      | SWITCH of lexp * Access.consig * (con * lexp) list * lexp option

      | VECTOR of lexp list * Lty.tyc
      | RECORD of lexp list
      | SRECORD of lexp list
      | SELECT of int * lexp

      | WRAP of Lty.tyc * bool * lexp
      | UNWRAP of Lty.tyc * bool * lexp

    withtype dict = {default: lexp, table: (Lty.tyc list * lexp) list}

  end (* signature PLAMBDA *)
