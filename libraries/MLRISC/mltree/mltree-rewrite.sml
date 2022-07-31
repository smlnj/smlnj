functor MLTreeRewrite
  (structure T : MLTREE
   (* Traversal extensions *)
   val sext : T.rewriter -> T.sext -> T.sext
   val rext : T.rewriter -> T.rext -> T.rext
   val fext : T.rewriter -> T.fext -> T.fext
   val ccext : T.rewriter -> T.ccext -> T.ccext
  ) : MLTREE_REWRITE =
struct
   structure T = T

   type rewriters =
       { stm   : T.stm -> T.stm,
         rexp  : T.rexp -> T.rexp,
         fexp  : T.fexp -> T.fexp,
         ccexp : T.ccexp -> T.ccexp
       }

   fun rewrite{rexp=doRexp, fexp=doFexp, ccexp=doCCexp, stm=doStm} = 
   let fun stm s =
       let val s =
           case s of
             T.MV(ty,dst,e) => T.MV(ty,dst,rexp e)
           | T.CCMV(dst,e) => T.CCMV(dst,ccexp e)
           | T.FMV(fty,dst,e) => T.FMV(fty,dst,fexp e)
           | T.COPY _  => s
           | T.FCOPY _ => s
           | T.JMP(e,cf) => T.JMP(rexp e,cf)
           | T.BCC(cc,l) => T.BCC(ccexp cc,l)
           | T.CALL{funct,targets,defs,uses,region,pops} => 
               T.CALL{funct=rexp funct,targets=targets,
                      defs=mlriscs defs,uses=mlriscs uses,
                      region=region,pops=pops}
           | T.FLOW_TO(s,controlflow) => T.FLOW_TO(stm s,controlflow)
           | T.RET _ => s
           | T.IF(cc,yes,no) => T.IF(ccexp cc,stm yes,stm no)
           | T.STORE(ty,ea,d,r) => T.STORE(ty,rexp ea,rexp d,r)
           | T.FSTORE(fty,ea,d,r) => T.FSTORE(fty,rexp ea,fexp d,r)
           | T.REGION(s,ctrl) => T.REGION(stm s,ctrl)
           | T.SEQ s => T.SEQ(stms s)
           | T.DEFINE _ => s
           | T.ANNOTATION(s,an) => T.ANNOTATION(stm s,an)
           | T.EXT s => 
                T.EXT(sext {rexp=rexp, fexp=fexp, ccexp=ccexp, stm=stm} s)
           | T.PHI _ => s 
           | T.SOURCE => s 
           | T.SINK => s 
           | T.RTL _ => s
           | T.ASSIGN(ty,x,y) => T.ASSIGN(ty,rexp x, rexp y)
	   | T.LIVE ls => T.LIVE (mlriscs ls)
	   | T.KILL ks => T.KILL (mlriscs ks)
      in doStm stm s end
   
      and stms ss = map stm ss

      and rexp e = 
      let val e = case e of
             T.REG _ => e
           | T.LI _ => e
           | T.LABEL _ => e 
           | T.LABEXP _ => e 
           | T.CONST _ => e
           | T.NEG(ty,x)   => T.NEG(ty,rexp x)
           | T.ADD(ty,x,y) => T.ADD(ty,rexp x,rexp y)
           | T.SUB(ty,x,y) => T.SUB(ty,rexp x,rexp y)
           | T.MULS(ty,x,y) => T.MULS(ty,rexp x,rexp y)
           | T.DIVS(m,ty,x,y) => T.DIVS(m,ty,rexp x,rexp y)
           | T.REMS(m,ty,x,y) => T.REMS(m,ty,rexp x,rexp y)
           | T.MULU(ty,x,y) => T.MULU(ty,rexp x,rexp y)
           | T.DIVU(ty,x,y) => T.DIVU(ty,rexp x,rexp y)
           | T.REMU(ty,x,y) => T.REMU(ty,rexp x,rexp y)
           | T.NEGT(ty,x)   => T.NEGT(ty,rexp x)
           | T.ADDT(ty,x,y) => T.ADDT(ty,rexp x,rexp y)
           | T.SUBT(ty,x,y) => T.SUBT(ty,rexp x,rexp y)
           | T.MULT(ty,x,y) => T.MULT(ty,rexp x,rexp y)
           | T.DIVT(m,ty,x,y) => T.DIVT(m,ty,rexp x,rexp y)
           | T.ANDB(ty,x,y) => T.ANDB(ty,rexp x,rexp y)
           | T.ORB(ty,x,y) => T.ORB(ty,rexp x,rexp y)
           | T.XORB(ty,x,y) => T.XORB(ty,rexp x,rexp y)
           | T.EQVB(ty,x,y) => T.EQVB(ty,rexp x,rexp y)
           | T.NOTB(ty,x) => T.NOTB(ty,rexp x)
           | T.SRA(ty,x,y) => T.SRA(ty,rexp x,rexp y)
           | T.SRL(ty,x,y) => T.SRL(ty,rexp x,rexp y)
           | T.SLL(ty,x,y) => T.SLL(ty,rexp x,rexp y)
           | T.SX(t,t',e) => T.SX(t,t',rexp e)
           | T.ZX(t,t',e) => T.ZX(t,t',rexp e)
           | T.CVTF2I(ty,mode,fty,e) => T.CVTF2I(ty,mode,fty,fexp e)
           | T.COND(ty,cc,yes,no) => T.COND(ty,ccexp cc,rexp yes,rexp no)
           | T.LOAD(ty,ea,r) => T.LOAD(ty,rexp ea,r)
           | T.PRED(e,ctrl) => T.PRED(rexp e,ctrl)
           | T.LET(s,e) => T.LET(stm s,rexp e)
           | T.REXT(ty,e) => 
                T.REXT(ty,rext {rexp=rexp, fexp=fexp, ccexp=ccexp, stm=stm} e)
           | T.MARK(e,an) => T.MARK(rexp e,an)
           | T.$(ty,k,e) => T.$(ty,k,rexp e)
           | T.ARG _ => e
           | T.PARAM _ => e
           | T.BITSLICE(ty,sl,e) => T.BITSLICE(ty,sl,rexp e)
           | T.??? => T.???
           | T.OP(ty,oper,es) => T.OP(ty,oper,rexps es)
      in doRexp rexp e end

      and rexps es = map rexp es

      and fexp e =
      let val e = case e of
             T.FREG _ => e
           | T.FLOAD(fty,e,r) => T.FLOAD(fty,rexp e,r)
           | T.FADD(fty,x,y) => T.FADD(fty,fexp x,fexp y)
           | T.FSUB(fty,x,y) => T.FSUB(fty,fexp x,fexp y)
           | T.FMUL(fty,x,y) => T.FMUL(fty,fexp x,fexp y)
           | T.FDIV(fty,x,y) => T.FDIV(fty,fexp x,fexp y)
           | T.FABS(fty,x) => T.FABS(fty,fexp x)
           | T.FNEG(fty,x) => T.FNEG(fty,fexp x)
           | T.FSQRT(fty,x) => T.FSQRT(fty,fexp x)
           | T.FCOPYSIGN(fty,x,y) => T.FCOPYSIGN(fty,fexp x,fexp y)
           | T.FCOND(fty,c,x,y) => T.FCOND(fty,ccexp c,fexp x,fexp y)
           | T.CVTI2F(fty,ty,e) => T.CVTI2F(fty,ty,rexp e)
           | T.CVTF2F(fty,fty',e) => T.CVTF2F(fty,fty',fexp e)
           | T.FPRED(e,ctrl) => T.FPRED(fexp e,ctrl)
           | T.FEXT(fty,e) => 
                T.FEXT(fty,fext {rexp=rexp, fexp=fexp, ccexp=ccexp, stm=stm} e)
           | T.FMARK(e,an) => T.FMARK(fexp e,an)
      in doFexp fexp e end

      and fexps es = map fexp es

      and ccexp e =
      let val e = case e of
             T.CC _ => e
           | T.FCC _ => e 
           | T.TRUE => e
           | T.FALSE => e
           | T.NOT e => T.NOT(ccexp e)
           | T.AND(x,y) => T.AND(ccexp x,ccexp y)
           | T.OR(x,y) => T.OR(ccexp x,ccexp y)
           | T.XOR(x,y) => T.XOR(ccexp x,ccexp y)
           | T.EQV(x,y) => T.EQV(ccexp x,ccexp y)
           | T.CMP(ty,cond,x,y) => T.CMP(ty,cond,rexp x,rexp y)
           | T.FCMP(ty,fcond,x,y) => T.FCMP(ty,fcond,fexp x,fexp y)
           | T.CCMARK(e,an) => T.CCMARK(ccexp e,an)
           | T.CCEXT(ty,e) => 
               T.CCEXT(ty,ccext {rexp=rexp, fexp=fexp, ccexp=ccexp, stm=stm} e)
      in  doCCexp ccexp e end

      and mlriscs m = map mlrisc m

      and mlrisc m =
          let val m =
              case m of
                T.CCR e => T.CCR(ccexp e)
              | T.GPR e => T.GPR(rexp e)
              | T.FPR e => T.FPR(fexp e)
          in  m end

   in  { rexp=rexp, fexp=fexp, ccexp=ccexp, stm=stm } end
end (* MLTreeFold *)

