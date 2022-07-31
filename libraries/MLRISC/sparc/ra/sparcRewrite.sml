functor SparcRewrite(Instr:SPARCINSTR) = 
struct
   structure I = Instr
   structure C = I.C
   structure CB = CellsBasis
   structure CS = CB.CellSet

   fun error msg = MLRiscErrorMsg.error ("SparcRewrite", msg)

   fun rewriteUse(instr,rs,rt) =  let
     fun match r = CB.sameColor(r,rs) 
     fun R r = if match r then rt else r 
     fun O(i as I.REG r) = if match r then I.REG rt else i
       | O i = i
     fun EA(SOME(I.Displace{base, disp, mem})) = 
	  SOME(I.Displace{base=R base, disp=disp, mem=mem})
       | EA ea = ea
     fun sparcUse(instr) = 
       (case instr of
	   I.LOAD{l,r,i,d,mem} => I.LOAD{l=l,r=R r,i=O i,d=d,mem=mem}
	 | I.STORE{s,d,r,i,mem} => I.STORE{s=s,d=R d,r=R r,i=O i,mem=mem}
	 | I.FLOAD{l,r,i,d,mem} => I.FLOAD{l=l,r=R r,i=O i,d=d,mem=mem}
	 | I.FSTORE{s,d,r,i,mem} => I.FSTORE{s=s,d=d,r=R r,i=O i,mem=mem}
	 | I.ARITH{a,r,i,d} => I.ARITH{a=a,r=R r,i=O i,d=d}
	 | I.SHIFT{s,r,i,d} => I.SHIFT{s=s,r=R r,i=O i,d=d}
	 | I.BR{r,p,rcond,a,nop,label} =>
	      I.BR{r=R r,p=p,rcond=rcond,a=a,nop=nop,label=label}
	 | I.MOVicc{b,i,d} => I.MOVicc{b=b,i=O i,d=R d}
	 | I.MOVfcc{b,i,d} => I.MOVfcc{b=b,i=O i,d=R d}
	 | I.MOVR{rcond,r,i,d} => I.MOVR{rcond=rcond,r=R r,i=O i,d=R d}
	 | I.JMP{r,i,labs,nop} => I.JMP{r=R r,i=O i,labs=labs,nop=nop}
	 | I.JMPL{r,i,d,defs,uses,cutsTo,nop,mem} => 
	      I.JMPL{r=R r,i=O i,d=d,defs=defs,
		     uses=CS.map {from=rs,to=rt} uses,
		     cutsTo=cutsTo,nop=nop,mem=mem}
	 | I.CALL{defs,uses,label,cutsTo,nop,mem} => 
	      I.CALL{defs=defs,uses=CS.map {from=rs,to=rt} uses,
		     label=label,cutsTo=cutsTo,nop=nop,mem=mem}
	 | I.SAVE{r,i,d} => I.SAVE{r=R r,i=O i,d=d}
	 | I.RESTORE{r,i,d} => I.RESTORE{r=R r,i=O i,d=d}
	 | I.WRY{r,i} => I.WRY{r=R r,i=O i}
	 | I.Ticc{t,cc,r,i} => I.Ticc{t=t,cc=cc,r=R r,i=O i}
	 | _ => instr
     (*esac*))
   in
      case instr
       of (I.ANNOTATION{i, ...}) => rewriteUse(i, rs, rt)
	| I.LIVE{regs, spilled} => I.LIVE{regs=C.addReg(rt, C.rmvReg(rs, regs)),
					   spilled=spilled}
        | I.INSTR(i) => I.INSTR(sparcUse(i))
	| I.COPY{k as CB.GP, sz, src,dst,tmp} => 
	     I.COPY{k=k, sz=sz, src=map R src,dst=dst,tmp=EA tmp}
	| _ => error "rewriteUse"
   end

   fun rewriteDef(instr,rs,rt) = let
     fun match r = CB.sameColor(r,rs)
     fun R r = if match r then rt else r 
     fun ea(SOME(I.Direct r)) = SOME(I.Direct(R r))
       | ea x = x 
     fun sparcDef(instr) = 
       (case instr 
	of I.LOAD{l,r,i,d,mem} => I.LOAD{l=l,r=r,i=i,d=R d,mem=mem}
	 | I.ARITH{a,r,i,d} => I.ARITH{a=a,r=r,i=i,d=R d}
	 | I.SHIFT{s,r,i,d} => I.SHIFT{s=s,r=r,i=i,d=R d}
	 | I.SETHI{i,d} => I.SETHI{i=i,d=R d}
	 | I.MOVicc{b,i,d} => I.MOVicc{b=b,i=i,d=R d}
	 | I.MOVfcc{b,i,d} => I.MOVfcc{b=b,i=i,d=R d}
	 | I.MOVR{rcond,r,i,d} => I.MOVR{rcond=rcond,r=r,i=i,d=R d}
	 | I.JMPL{r,i,d,defs,uses,cutsTo,nop,mem} => 
	      I.JMPL{r=r,i=i,d=R d,defs=CS.map {from=rs,to=rt} defs,
		     uses=uses,cutsTo=cutsTo,nop=nop,mem=mem}
	 | I.CALL{defs,uses,label,cutsTo,nop,mem} => 
	      I.CALL{defs=CS.map {from=rs,to=rt} defs,
		     uses=uses,label=label,cutsTo=cutsTo,nop=nop,mem=mem}
	 | I.SAVE{r,i,d} => I.SAVE{r=r,i=i,d=R d}
	 | I.RESTORE{r,i,d} => I.RESTORE{r=r,i=i,d=R d}
	 | I.RDY{d} => I.RDY{d=R d}
	 | _ => instr
      (*esac*))
   in
	 case instr
	 of I.ANNOTATION{i, ...} => rewriteDef(i, rs, rt)
	  | I.KILL{regs, spilled} => 
	      I.KILL{regs=C.addReg(rt, C.rmvReg(rs, regs)), spilled=spilled}
	  | I.INSTR(i) => I.INSTR(sparcDef(i))
 	  | I.COPY{k as CB.GP, sz,  src,dst,tmp} => 
	     I.COPY{k=k, sz=sz, src=src, dst=map R dst,tmp=ea tmp}
	  | _ => error "rewriteDef"
   end


   fun frewriteUse(instr,rs,rt) = let
     fun match r = CB.sameColor(r,rs)
     fun R r = if match r then rt else r 
     fun sparcUse(instr) = 
      (case instr of
	   I.FPop1{a,r,d} => I.FPop1{a=a,r=R r,d=d}
	 | I.FPop2{a,r1,r2,d} => I.FPop2{a=a,r1=R r1,r2=R r2,d=d}
	 | I.FCMP{cmp,r1,r2,nop} => I.FCMP{cmp=cmp,r1=R r1,r2=R r2,nop=nop}
	 | I.FSTORE{s,r,i,d,mem} => I.FSTORE{s=s,r=r,i=i,d=R d,mem=mem}
	 | I.FMOVicc{sz,b,r,d} => I.FMOVicc{sz=sz,b=b,r=R r,d=R d}
	 | I.FMOVfcc{sz,b,r,d} => I.FMOVfcc{sz=sz,b=b,r=R r,d=R d}
	 | I.JMPL{r,i,d,defs,uses,cutsTo,nop,mem} =>
	     I.JMPL{r=r,i=i,d=d,defs=defs,
		    uses=CS.map {from=rs,to=rt} uses,
		    cutsTo=cutsTo,nop=nop,mem=mem}
	 | I.CALL{defs,uses,label,cutsTo,nop,mem} =>
	     I.CALL{defs=defs,uses=CS.map {from=rs,to=rt} uses,
		    label=label,cutsTo=cutsTo,nop=nop,mem=mem}
	 | _ => instr
     (*esac*))
   in
	case instr
	of I.ANNOTATION{i, ...} => frewriteUse(i, rs, rt)
	 | I.INSTR(i) => I.INSTR(sparcUse(i))
	 | I.LIVE{regs, spilled} => 
	     I.LIVE{regs=C.addFreg(rt, C.rmvFreg(rs, regs)), spilled=spilled}
	 | I.COPY{k as CB.FP, sz, src,dst,tmp} => 
	     I.COPY{k=k, sz=sz, src=map R src,dst=dst,tmp=tmp}
	 | _ => error "frewriteUse"

   end

   
   fun frewriteDef(instr,rs,rt) = let
     fun match r = CB.sameColor(r,rs)
     fun R r = if match r then rt else r 
     fun ea(SOME(I.FDirect r)) = SOME(I.FDirect(R r))
       | ea x = x 
     fun sparcDef(instr) = 
      (case instr of
	   I.FPop1{a,r,d} => I.FPop1{a=a,r=r,d=R d}
	 | I.FPop2{a,r1,r2,d} => I.FPop2{a=a,r1=r1,r2=r2,d=R d}
	 | I.FLOAD{l,r,i,d,mem} => I.FLOAD{l=l,r=r,i=i,d=R d,mem=mem}
	 | I.FMOVicc{sz,b,r,d} => I.FMOVicc{sz=sz,b=b,r=r,d=R d}
	 | I.FMOVfcc{sz,b,r,d} => I.FMOVfcc{sz=sz,b=b,r=r,d=R d}
	 | I.JMPL{r,i,d,defs,uses,cutsTo,nop,mem} =>
	     I.JMPL{r=r,i=i,d=d,defs=CS.map {from=rs,to=rt} defs,
		    uses=uses,cutsTo=cutsTo,nop=nop,mem=mem}
	 | I.CALL{defs,uses,label,cutsTo,nop,mem} =>
	     I.CALL{defs=CS.map {from=rs,to=rt} defs,
		    uses=uses,label=label,cutsTo=cutsTo,nop=nop,mem=mem}
	 | _ => instr
     (*esac*))
  in
	case instr
	of I.ANNOTATION{i, ...} => frewriteDef(i, rs, rt)
         | I.KILL{regs, spilled} => 
	     I.KILL{regs=C.addFreg(rt, C.rmvFreg(rs, regs)), spilled=spilled}
	 | I.INSTR(i) => I.INSTR(sparcDef(i))
	 | I.COPY{k as CB.FP, sz, src,dst,tmp} => 
	     I.COPY{k=k, sz=sz, src=src,dst=map R dst,tmp=ea tmp}
	 | _ => error "frewriteUse"

  end  
end

