(* Copyright 1992 by AT&T Bell Laboratories *)

(* Utility functions to build absyn from ast *)

signature ELABUTIL =
sig

  datatype context
    = TOP    (* at top level -- not inside any module, rigid *)
    | INSTR  (* inside a rigid structure, i.e. not inside any functor body *)
    | INFCT of {flex: Stamps.stamp -> bool,  depth: DebIndex.depth}
             (* predicate recognizing flexible stamps *)
    | INSIG  (* within a signature body *)

  type compInfo = Absyn.dec CompInfo.compInfo

  val debugging : bool ref
  val for : 'a list -> ('a -> unit) -> unit
  val single : 'a -> 'a list
  val sort3 : (Symbol.symbol * 'a * 'b) list -> (Symbol.symbol * 'a * 'b) list

  val initializeMatchBind : StaticEnv.staticEnv -> unit
  val getMatchExn : unit -> Types.datacon  (* returns the Match exn datacon *)
  val getBindExn  : unit -> Types.datacon  (* returns the Bind exn datacon *)

  val EQUALsym : Symbol.symbol
  val bogusID : Symbol.symbol
  val bogusExnID : Symbol.symbol
  val anonParamName : Symbol.symbol

  val varToExp : Variable.var -> Absyn.exp
  val CONSexp : Absyn.exp
  val CONSpat : Absyn.pat -> Absyn.pat
  val FALSEexp : Absyn.exp
  val FALSEpat : Absyn.pat
  val NILexp : Absyn.exp
  val NILpat : Absyn.pat
  val TRUEexp : Absyn.exp
  val TRUEpat : Absyn.pat
  val TUPLEexp : Absyn.exp list -> Absyn.exp
  val TUPLEpat : Absyn.pat list -> Absyn.pat
  val unitExp : Absyn.exp
  val unitPat : Absyn.pat
  val bogusExp: Absyn.exp

  val bindVARp : Absyn.pat list * ErrorMsg.complainer -> StaticEnv.staticEnv

  val checkUniq : ErrorMsg.complainer * string * Symbol.symbol list -> unit
  val checkForbiddenCons : Symbol.symbol -> bool

(*
  val getCoreExn : (StaticEnv.staticEnv * string) -> Types.datacon
  val getCoreVar : (StaticEnv.staticEnv * string) -> Variable.var

  val makeAPPpat : ErrorMsg.complainer -> Absyn.pat * Absyn.pat -> Absyn.pat
*)

  val clean_pat : ErrorMsg.complainer -> Absyn.pat -> Absyn.pat
  val makeLAYEREDpat : Absyn.pat * Absyn.pat * ErrorMsg.complainer -> Absyn.pat
  val makeRECORDexp :
       (Symbol.symbol * Absyn.exp) list * ErrorMsg.complainer -> Absyn.exp
  val makeRECORDpat :
       (Symbol.symbol * Absyn.pat) list * bool * ErrorMsg.complainer
       -> Absyn.pat
  val fillPat : Absyn.pat -> Absyn.pat
  val aconvertPat : Absyn.pat -> Absyn.pat * Variable.var list * Variable.var list

  val checkBoundTyvars :
       TyvarSet.tyvarset * Types.tyvar list * ErrorMsg.complainer -> unit

  val pat_id :
       SymPath.path * StaticEnv.staticEnv * ErrorMsg.complainer * compInfo
       -> Absyn.pat option

  val sortRecord :
       (Symbol.symbol * 'a) list * ErrorMsg.complainer
       -> (Symbol.symbol * 'a) list

  val FUNdec :
       {var : Variable.var,
        clauses: {pats: Absyn.pat list,
                  resultty: Types.ty option,
                  exp: Absyn.exp} list,
        tyvars: Types.tyvar list ref,
	region: Ast.region } list
       * compInfo -> Absyn.dec (* * StaticEnv.staticEnv *)

  val labsym : Absyn.numberedLabel -> Symbol.symbol

  val hasModules : Ast.dec -> bool

end (* signature ELABUTIL *)
