(* plambda.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure PLambda : PLAMBDA =
  struct

  structure A  = Access
  structure LV = LambdaVar
  structure LT = Lty
  structure PO = Primop
  (* mentions Symbol *)

  (* dataconstr: record containing name of the constructor, the corresponding conrep,
   * and the lambda type lty; for value-carrying (i.e. nonconstant) data constructors
   * lty is a function/arrow type. *)
    type dataconstr = Symbol.symbol * A.conrep * LT.lty

  (* con: used to specify all possible switching statements. Efficient switch
   * generation can be applied to DATAcon and INTcon. Otherwise, it is just a
   * shorthand for binary branch trees. In the future, we probably should make
   * it more general, including constants of any numerical types.
   * -- char constants are represented as INTcon. *)
    datatype con
      = DATAcon of dataconstr * LT.tyc list * LV.lvar  (* instantiation types, decon lvar *)
      | INTcon of int IntConst.t	(* "ty" = 0 for IntInf.int *)
      | WORDcon of int IntConst.t
      | STRINGcon of string

  (* lexp: the universal typed intermediate language. TFN, TAPP are abstraction
   * and application on type constructors. Structure abstractions and functor
   * abstractions are represented as normal structure and functor definitions
   * [OBS: with its component properly PACKed]. FN defines normal function, FIX
   * defines a set of recursive functions, LET(v,e1,e2) is a syntactic sugar for
   * exprs of forms like APP(FN(v,_,e2), e1); the type of v will be that of e1.
   * APP is function application. RECORD, VECTOR, and SRECORD are record,
   * vector, and structure construction, SELECT is record, vector, and structure
   * selection. ETAG, RAISE, and HANDLE are for exceptions. *)

    datatype lexp
      = VAR of LV.lvar
      | INT of int IntConst.t	(* sz = 0 for IntInf.int, covers char type also *)
      | WORD of int IntConst.t
      | REAL of int RealConst.t
      | STRING of string
      | PRIM of PO.primop * LT.lty * LT.tyc list
      | GENOP of dict * PO.primop * LT.lty * LT.tyc list

      | FN of LV.lvar * LT.lty * lexp  (* lty is the type of the parameter lvar *)
      | FIX of LV.lvar list * LT.lty list * lexp list * lexp
      | APP of lexp * lexp
      | LET of LV.lvar * lexp * lexp

      | TFN of Lty.tkind list * lexp
      | TAPP of lexp * LT.tyc list

      | RAISE of lexp * LT.lty
      | HANDLE of lexp * lexp
      | ETAG of lexp * LT.lty

      | CON of dataconstr * LT.tyc list * lexp
      | SWITCH of lexp * A.consig * (con * lexp) list * lexp option

      | VECTOR of lexp list * LT.tyc
      | RECORD of lexp list
      | SRECORD of lexp list
      | SELECT of int * lexp

      | WRAP of LT.tyc * bool * lexp
      | UNWRAP of LT.tyc * bool * lexp

    withtype dict = {default: lexp, table: (LT.tyc list * lexp) list}

  end (* structure PLambda *)
