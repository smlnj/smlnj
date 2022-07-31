(* mltree-check-ty.sml
 * 
 * Check that MLRISC programs have consistent types. 
 *)

functor MLTreeCheckTy
  (structure T : MLTREE
   val intTy : T.ty (* size of integer word *)) : sig
    val check : T.stm -> bool
  end = struct

   exception AmbiguousType

   exception TypeError

   fun chkEq (ty, tys) = List.all (fn SOME ty' => ty' = ty | NONE => true) tys

   fun chkTys (ty, tys) = if chkEq (ty, tys)
           then ty
           else raise TypeError

 (* check well-formedness of a list of expressions *)
   fun checkRexps (ty, es) = let
          val tys = List.map (fn e => SOME (checkRexp e) handle AmbiguousType => NONE) es
          in
             chkTys(ty, tys)
          end

   and checkRexp (T.REG(ty,_)) = ty
     (* the type of a literal expression depends on its surrounding context *)
     | checkRexp (T.LI _) = raise AmbiguousType  
     | checkRexp (T.LABEL _) = intTy
     (* the type of a literal expression depends on its surrounding context *)
     | checkRexp (T.CONST _) = raise AmbiguousType
     | checkRexp (T.LABEXP e) = checkRexp e
     | checkRexp (T.NEG(ty, e)) = checkRexps(ty, [e])
     | checkRexp (T.ADD(ty,e1,e2)) = checkRexps(ty, [e1, e2])
     | checkRexp (T.SUB(ty,e1,e2)) = checkRexps(ty, [e1, e2])
     | checkRexp (T.MULS(ty,e1,e2)) = checkRexps(ty, [e1, e2])
     | checkRexp (T.DIVS(_,ty,e1,e2)) = checkRexps(ty, [e1, e2])
     | checkRexp (T.REMS(_,ty,e1,e2)) = checkRexps(ty, [e1, e2])
     | checkRexp (T.MULU(ty,e1,e2)) = checkRexps(ty, [e1, e2])
     | checkRexp (T.DIVU(ty,e1,e2)) = checkRexps(ty, [e1, e2])
     | checkRexp (T.REMU(ty,e1,e2)) = checkRexps(ty, [e1, e2])
     | checkRexp (T.NEGT(ty,e)) = checkRexps(ty, [e])
     | checkRexp (T.ADDT(ty,e1,e2)) = checkRexps(ty, [e1, e2])
     | checkRexp (T.SUBT(ty,e1,e2)) = checkRexps(ty, [e1, e2])
     | checkRexp (T.MULT(ty,e1,e2)) = checkRexps(ty, [e1, e2])
     | checkRexp (T.DIVT(_,ty,e1,e2)) = checkRexps(ty, [e1, e2])
     | checkRexp (T.ANDB(ty,e1,e2)) = checkRexps(ty, [e1, e2])
     | checkRexp (T.ORB(ty,e1,e2)) = checkRexps(ty, [e1, e2])
     | checkRexp (T.XORB(ty,e1,e2)) = checkRexps(ty, [e1, e2])
     | checkRexp (T.EQVB(ty,e1,e2)) = checkRexps(ty, [e1, e2])
     | checkRexp (T.NOTB(ty,e)) = checkRexps(ty, [e])
     | checkRexp (T.SRA(ty,e1,e2)) = checkRexps(ty, [e1, e2])
     | checkRexp (T.SRL(ty,e1,e2)) = checkRexps(ty, [e1, e2])
     | checkRexp (T.SLL(ty,e1,e2)) = checkRexps(ty, [e1, e2])
     | checkRexp (T.SX(toTy,fromTy,e)) = (checkRexps(fromTy, [e]); toTy)
     | checkRexp (T.ZX(toTy,fromTy,e)) = (checkRexps(fromTy, [e]); toTy)
     | checkRexp (T.CVTF2I(ty,_,_,_)) = ty
     | checkRexp (T.COND(ty,cce,e1,e2)) = (checkCCexp cce; checkRexps(ty, [e1, e2]))
     | checkRexp (T.LOAD(ty,ea,_)) = (checkRexps(intTy, [ea]); ty)
     | checkRexp (T.PRED(e,_)) = checkRexp e
     | checkRexp (T.LET(_,e)) = checkRexp e
     | checkRexp (T.REXT(ty,_)) = ty
     | checkRexp (T.MARK(e,_)) = checkRexp e
     | checkRexp (T.OP(ty,_,_)) = ty
     | checkRexp (T.ARG(ty,_,_)) = ty
     | checkRexp (T.$(ty,_,_)) = ty
     | checkRexp (T.PARAM _) = intTy
     | checkRexp (T.BITSLICE(ty,_,_)) = ty
     | checkRexp (T.???) = intTy

   and checkFexps (ty, es) = let
          val tys = List.map (fn e => SOME (checkFexp e) handle AmbiguousType => NONE) es
          in
             chkTys(ty, tys)
          end

   and checkFexp (T.FREG(ty,_)) = raise AmbiguousType
     | checkFexp (T.FLOAD(ty,ea,_)) = (checkRexps(intTy, [ea]); ty)
     | checkFexp (T.FADD(ty,e1,e2)) = checkFexps(ty, [e1, e2])
     | checkFexp (T.FSUB(ty,e1,e2)) = checkFexps(ty, [e1, e2])
     | checkFexp (T.FMUL(ty,e1,e2)) = checkFexps(ty, [e1, e2])
     | checkFexp (T.FDIV(ty,e1,e2)) = checkFexps(ty, [e1, e2])
     | checkFexp (T.FABS(ty,e)) = checkFexps(ty, [e])
     | checkFexp (T.FNEG(ty,e)) = checkFexps(ty, [e])
     | checkFexp (T.FSQRT(ty,e)) = checkFexps(ty, [e])
     | checkFexp (T.FCOND(ty,ce,e1,e2)) = (checkCCexp ce; checkFexps(ty, [e1, e2]))
     | checkFexp (T.CVTI2F(ty,_,_)) = ty
     | checkFexp (T.CVTF2F(ty,_,_)) = ty
     | checkFexp (T.FCOPYSIGN(ty,e1,e2)) = checkFexps(ty, [e1, e2])
     | checkFexp (T.FPRED(e,_)) = checkFexp e
     | checkFexp (T.FEXT(ty,_)) = ty
     | checkFexp (T.FMARK(e,_)) = checkFexp e

  (* don't care about ambiguous types *)
   and checkRexpB (ty, e) = checkRexp e = ty handle AmbiguousType => true

   and checkCCexp cce = checkCCexpB cce orelse raise TypeError

   and checkCCexpB cce = (case cce
          of T.NOT cce => checkCCexpB cce
	   | ( T.AND (cce1, cce2) | T.OR (cce1, cce2) | T.XOR (cce1, cce2) | T.EQV (cce1, cce2) ) =>
	     checkCCexpB cce1 andalso checkCCexpB cce2
	   | T.CMP (ty, _, e1, e2) => ty = checkRexp e1 andalso ty = checkRexp e2
	   | T.FCMP (fty, _, e1, e2) => fty = checkFexp e1 andalso fty = checkFexp e2
	   | T.CCMARK (cce, _) => checkCCexpB cce
	   | T.CCEXT (ty, ccext) => true
          (* end case *))

    fun check stm = (case stm
	   of T.MV (ty, d, e) => checkRexpB (ty, e)
	    | T.CCMV (dst, cce) => checkCCexpB cce
	    | T.FMV (fty, dst, e) => checkFexp e = fty
	    | T.COPY _ => true
	    | T.FCOPY _ => true
	    | T.JMP (e, _) => checkRexpB (intTy, e)
	    | T.BCC (cce, _) => checkCCexpB cce
	    | T.CALL {funct, ...} => checkRexpB (intTy, funct)
	    | T.FLOW_TO (stm, _) => check stm
	    | T.RET _ => true
	    | T.IF (cce, stm1, stm2) => checkCCexpB cce andalso check stm1 andalso check stm2
	    | T.STORE (ty, e1, e2, _) => checkRexpB (intTy, e1) andalso checkRexpB(intTy, e2)
	    | T.FSTORE (fty, e1, e2, _) => checkRexpB (intTy, e1) andalso fty = checkFexp e2
	    | T.REGION (stm, _) => check stm
	    | T.SEQ stms => List.all check stms
	    | T.DEFINE _ => true
	    | T.ANNOTATION (stm, _) => check stm
	    | T.EXT _ => true
	    | T.LIVE _ => true
	    | T.KILL _ => true
	    | _ => true
           (* end case *))
	handle TypeError => false

  end (* MLTreeCheckTy *)
