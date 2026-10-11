(* reconstruct.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 * Type reconstruction for adding instrumentation.  We assume that this
 * analysis is run prior to match compilation and so it does not accept
 * the special forms added by the match compiler.
 *)

structure Reconstruct : sig

    val expType : Absyn.exp -> Types.ty

  end = struct

    structure TU = TypesUtil
    open Absyn Variable Types BasicTypes TypesUtil

    fun bug msg = ErrorMsg.impossible("Reconstruct: "^msg)

    infix -->

    fun reduceType(POLYty{tyfun=TYFUN{body,arity},...}) = headReduceType body
      | reduceType ty = headReduceType ty

  (* expType : Absyn.exp -> Types.ty
   * assertion: returned ty is a monotype (i.e. not POLYty)
   *)
    fun expType (VARexp(ref(VALvar{typ=ref ty,...}),insttvs)) = (case ty
         of POLYty{tyfun,...} => TU.applyTyfun(tyfun,map VARty insttvs)
          | _ => ty
        (* end case *))
      | expType (VARexp _) = bug "varexp"
      | expType (CONexp(DATACON{typ,...}, insttvs)) = (case typ
         of POLYty{tyfun,...} => TU.applyTyfun(tyfun,map VARty insttvs)
          | _ => typ
        (* end case *))
      | expType (NUMexp(_, {ty, ...})) = ty
      | expType (REALexp(_, {ty, ...})) = ty
      | expType (STRINGexp _) = stringTy
      | expType (CHARexp _) = charTy
      | expType (RECORDexp fields) = let
        fun extract(LABEL{name,...},exp) = (name,expType exp)
        in
          recordTy(map extract (sortFields fields))
        end
      | expType (RSELECTexp (var, index)) = bug "expType: SELECTexp"
      | expType (VSELECTexp (var, _, index)) = bug "expType: VSELECTexp"
          (* only produced in match compilation post type checking *)
      | expType (VECTORexp(nil,vty)) = CONty(vectorTycon,[vty])
      | expType (VECTORexp((a::_),vty)) = CONty(vectorTycon,[vty])
      | expType (APPexp(rator,rand)) = (case reduceType(expType rator)
         of CONty(_,[_,t]) => t
          | POLYty _ => bug "poly-rator"
          | WILDCARDty => bug "wildcard-rator"
          | UNDEFty => bug "undef-rator"
          | IBOUND _ => bug "ibound-rator"
          | VARty _ => bug "varty-rator"
          | _ => bug "rator"
        (* end case *))
      | expType (FNexp(RULE(_,e)::_, lhsTy, rhsTy)) = lhsTy --> rhsTy
      | expType (FNexp(nil, _, _)) = bug "expType(FNexp(nil,ty))"
      | expType (CASEexp(_,(RULE(_,e)::_, _, _))) = expType e
      | expType (CASEexp(_,(nil,_,_))) = bug "expType(CASEexp(_,nil))"
      | expType (HANDLEexp(e,h)) = expType e
      | expType (RAISEexp(e,t)) = t
      | expType (SWITCHexp _) = bug "expType: SWITCHexp"
      | expType (VSWITCHexp _) = bug "expType: VSWITCHexp"
      | expType (IFexp {thenCase,...}) = expType thenCase
      | expType (ANDALSOexp _) = boolTy
      | expType (ORELSEexp _) = boolTy
      | expType (WHILEexp _) = unitTy
      | expType (LETexp(_,e)) = expType e
      | expType (LETVexp(_,_,e)) = bug "expType: LETVexp"
      | expType (SEQexp[a]) = expType a
      | expType (SEQexp(_::rest)) = expType(SEQexp rest)
      | expType (SEQexp nil) = bug "expType(SEQexp nil)"
      | expType (CONSTRAINTexp(e,ty)) = expType e
      | expType (MARKexp(e,_)) = expType e

  end (* structure Reconstruct *)
