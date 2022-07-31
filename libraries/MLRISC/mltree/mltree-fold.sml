functor MLTreeFold
  (structure T : MLTREE
   (* Extension mechnism *)
   val sext  : 'b T.folder -> T.sext * 'b -> 'b
   val rext  : 'b T.folder -> T.ty * T.rext * 'b -> 'b
   val fext  : 'b T.folder -> T.fty * T.fext * 'b -> 'b
   val ccext : 'b T.folder -> T.ty * T.ccext * 'b -> 'b
  ) : MLTREE_FOLD =
struct
   structure T = T

   fun fold{rexp=doRexp, fexp=doFexp, ccexp=doCCexp, stm=doStm} = 
   let fun stm(s,x) =
       let val x =
           case s of
             T.MV(ty,dst,e) => rexp(e,x)
           | T.CCMV(dst,e) => ccexp(e,x)
           | T.FMV(fty,dst,e) => fexp(e,x)
           | T.COPY _  => x
           | T.FCOPY _ => x
           | T.JMP(e,cf) => rexp(e,x)
           | T.BCC(cc,l) => ccexp(cc,x)
           | T.CALL{funct,defs,uses,...} => 
               mlriscs(uses,mlriscs(defs,rexp(funct,x)))
           | T.RET _ => x
	   | T.FLOW_TO (s, _) => stm(s,x)
           | T.IF(cc,yes,no) => stm(no,stm(yes,ccexp(cc,x)))
           | T.STORE(ty,ea,d,r) => rexp(d,rexp(ea,x))
           | T.FSTORE(fty,ea,d,r) => fexp(d,rexp(ea,x))
           | T.REGION(s,ctrl) => stm(s,x)
           | T.SEQ s => stms(s,x)
           | T.DEFINE _ => x
           | T.ANNOTATION(s,an) => stm(s,x)
           | T.EXT s => 
               sext {stm=stm, rexp=rexp, fexp=fexp, ccexp=ccexp} (s,x)
           | T.PHI _ => x 
           | T.ASSIGN(_,a,b) => rexp(b,rexp(a,x))
           | T.SOURCE => x 
           | T.SINK => x 
           | T.RTL _ => x
	   | T.LIVE ls => mlriscs (ls, x)
	   | T.KILL ks => mlriscs (ks, x)
      in doStm(s,x) end
   
      and stms(ss,x) = foldr stm x ss

      and rexp(e,x) = 
      let val x = case e of
             T.REG _ => x
           | T.LI _ => x
           | T.LABEL _ => x 
           | T.LABEXP _ => x 
           | T.CONST _ => x
           | T.NEG(ty,a) => rexp(a,x)
           | T.ADD(ty,a,b) => rexp2(a,b,x)
           | T.SUB(ty,a,b) => rexp2(a,b,x)
           | T.MULS(ty,a,b) => rexp2(a,b,x)
           | T.DIVS(m,ty,a,b) => rexp2(a,b,x)
           | T.REMS(m,ty,a,b) => rexp2(a,b,x)
           | T.MULU(ty,a,b) => rexp2(a,b,x)
           | T.DIVU(ty,a,b) => rexp2(a,b,x)
           | T.REMU(ty,a,b) => rexp2(a,b,x)
           | T.NEGT(ty,a) => rexp(a,x)
           | T.ADDT(ty,a,b) => rexp2(a,b,x)
           | T.SUBT(ty,a,b) => rexp2(a,b,x)
           | T.MULT(ty,a,b) => rexp2(a,b,x)
           | T.DIVT(m,ty,a,b) => rexp2(a,b,x)
           | T.ANDB(ty,a,b) => rexp2(a,b,x)
           | T.ORB(ty,a,b) => rexp2(a,b,x)
           | T.XORB(ty,a,b) => rexp2(a,b,x)
           | T.EQVB(ty,a,b) => rexp2(a,b,x)
           | T.NOTB(ty,a) => rexp(a,x)
           | T.SRA(ty,a,b) => rexp2(a,b,x)
           | T.SRL(ty,a,b) => rexp2(a,b,x)
           | T.SLL(ty,a,b) => rexp2(a,b,x)
           | T.SX(t,t',e) => rexp(e,x)
           | T.ZX(t,t',e) => rexp(e,x)
           | T.CVTF2I(ty,mode,fty,e) => fexp(e,x)
           | T.COND(ty,cc,yes,no) => rexp(no,rexp(yes,ccexp(cc,x)))
           | T.LOAD(ty,ea,r) => rexp(ea,x)
           | T.PRED(e,ctrl) => rexp(e,x)
           | T.LET(s,e) => rexp(e,stm(s,x))
           | T.REXT(t,e) => 
                rext{stm=stm, rexp=rexp, fexp=fexp, ccexp=ccexp} (t,e,x)
           | T.MARK(e,an) => rexp(e,x)
           | T.OP(ty,oper,es) => rexps(es,x)
           | T.ARG _ => x
           | T.PARAM _ => x
           | T.BITSLICE(_,_,e) => rexp(e, x)
           | T.$(ty,k,e) => rexp(e, x)
           | T.??? => x
      in doRexp(e,x) end

      and rexp2(a,b,x) = rexp(b,rexp(a,x))

      and rexps(es,x) = foldr rexp x es

      and fexp(e,x) =
      let val x = case e of
             T.FREG _ => x
           | T.FLOAD(fty,e,r) => rexp(e,x)
           | T.FADD(fty,a,b) => fexp2(a,b,x)
           | T.FSUB(fty,a,b) => fexp2(a,b,x)
           | T.FMUL(fty,a,b) => fexp2(a,b,x)
           | T.FDIV(fty,a,b) => fexp2(a,b,x)
           | T.FABS(fty,e) => fexp(e,x)
           | T.FNEG(fty,e) => fexp(e,x)
           | T.FSQRT(fty,e) => fexp(e,x)
           | T.FCOPYSIGN(fty,a,b) => fexp2(a,b,x)
           | T.FCOND(fty,c,a,b) => fexp2(a,b,ccexp(c,x))
           | T.CVTI2F(fty,ty,e) => rexp(e,x)
           | T.CVTF2F(fty,fty',e) => fexp(e,x)
           | T.FPRED(e,ctrl) => fexp(e,x)
           | T.FEXT(t,e) => 
                fext {stm=stm, rexp=rexp, fexp=fexp, ccexp=ccexp} (t,e,x)
           | T.FMARK(e,an) => fexp(e,x)
      in doFexp(e,x) end

      and fexp2(a,b,x) = fexp(b,fexp(a,x))

      and fexps(es,x) = foldr fexp x es

      and ccexp(e,x) =
      let val x = case e of
             T.CC _ => x
           | T.FCC _ => x 
           | T.TRUE => x
           | T.FALSE => x
           | T.NOT e => ccexp(e,x)
           | T.AND(a,b) => ccexp2(a,b,x)
           | T.OR(a,b) => ccexp2(a,b,x)
           | T.XOR(a,b) => ccexp2(a,b,x)
           | T.EQV(a,b) => ccexp2(a,b,x)
           | T.CMP(ty,cond,a,b) => rexp2(a,b,x)
           | T.FCMP(ty,fcond,a,b) => fexp2(a,b,x)
           | T.CCMARK(e,an) => ccexp(e,x)
           | T.CCEXT(t,e) => 
              ccext{stm=stm, rexp=rexp, fexp=fexp, ccexp=ccexp}(t,e,x)
      in  doCCexp(e,x) end

      and ccexp2(a,b,x) = ccexp(b,ccexp(a,x))

      and mlriscs(m,x) = foldr mlrisc x m

      and mlrisc(m,x) =
          let val x =
              case m of
                T.CCR e => ccexp(e,x)
              | T.GPR e => rexp(e,x)
              | T.FPR e => fexp(e,x)
          in  x end

   in  { rexp=rexp, fexp=fexp, ccexp=ccexp, stm=stm } end
end (* MLTreeFold *)

