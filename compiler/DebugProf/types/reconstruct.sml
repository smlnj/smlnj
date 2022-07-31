(* reconstruct.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
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
     assertion: returned ty is a monotype (i.e. not POLYty)
   *)
    fun expType(VARexp(ref(VALvar{typ=ref ty,...}),insttvs)) =
	 (case ty
	   of POLYty{tyfun,...} => TU.applyTyfun(tyfun,map VARty insttvs)
	    | _ => ty)
      | expType(VARexp _) = bug "varexp"
      | expType(CONexp(DATACON{typ,...},insttvs)) =
	 (case typ
	   of POLYty{tyfun,...} => TU.applyTyfun(tyfun,map VARty insttvs)
	    | _ => typ)
      | expType(NUMexp(_, {ty, ...})) = ty
      | expType(REALexp(_, {ty, ...})) = ty
      | expType(STRINGexp _) = stringTy
      | expType(CHARexp _) = charTy
      | expType(RECORDexp fields) =
	  let fun extract(LABEL{name,...},exp) = (name,expType exp)
	   in recordTy(map extract (sortFields fields))
	  end
      | expType(RSELECTexp (var, index)) = bug "expType: SELECTexp"
      | expType(VSELECTexp (var, _, index)) = bug "expType: SELECTexp"
          (* only produced in match compilation post type checking *)
      | expType(VECTORexp(nil,vty)) = CONty(vectorTycon,[vty])
      | expType(VECTORexp((a::_),vty)) = CONty(vectorTycon,[vty])
      | expType(APPexp(rator,rand)) =
	 (case reduceType(expType rator)
	   of CONty(_,[_,t]) => t
	    | POLYty _ => bug "poly-rator"
	    | WILDCARDty => bug "wildcard-rator"
	    | UNDEFty => bug "undef-rator"
	    | IBOUND _ => bug "ibound-rator"
	    | VARty _ => bug "varty-rator"
	    | _ => bug "rator")
      | expType(HANDLEexp(e,h)) = expType e
      | expType(RAISEexp(e,t)) = t
      | expType(CASEexp(_,(RULE(_,e)::_, _, _))) = expType e
      | expType(CASEexp(_,(nil,_,_))) = bug "expType(CASEexp(_,nil))"
      | expType(IFexp {thenCase,...}) = expType thenCase
      | expType(ANDALSOexp _) = boolTy
      | expType(ORELSEexp _) = boolTy
      | expType(WHILEexp _) = unitTy
      | expType(FNexp(RULE(_,e)::_, lhsTy, rhsTy)) = lhsTy --> rhsTy
      | expType(FNexp(nil, _, _)) = bug "expType(FNexp(nil,ty))"
      | expType(LETexp(_,e)) = expType e
      | expType(SEQexp [a]) = expType a
      | expType(SEQexp (_::rest)) = expType(SEQexp rest)
      | expType(SEQexp nil) = bug "expType(SEQexp nil)"
      | expType(CONSTRAINTexp(e,ty)) = expType e
      | expType(MARKexp(e,_)) = expType e

  end (* structure Reconstruct *)
