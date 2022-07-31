(* alphaRewrite.sml -- rewrite an alpha instruction 
 *
 * COPYRIGHT (c) 1997 Bell Labs
 *)

functor AlphaRewrite(Instr : ALPHAINSTR) = struct
  structure I=Instr
  structure C=I.C
  structure CB=CellsBasis
  structure CS=CB.CellSet
  fun error msg = MLRiscErrorMsg.error ("AlphaRewrite", msg)

  fun rewriteUse(instr, rs, rt) = let
    fun match r = CB.sameColor(r,rs)
    fun replace r = if match r then rt else r
    fun replaceEA(SOME(I.Displace{base, disp, mem})) = 
	  SOME(I.Displace{base=replace base, disp=disp, mem=mem})
      | replaceEA ea = ea
    fun alphaUse(instr) = let
      fun isRegOp (I.REGop r) = match r
	| isRegOp _ = false
      fun rwOperand(opnd as I.REGop r) = 
	   if match r then I.REGop rt else opnd
	| rwOperand opnd = opnd
      fun load(ldClass, {ldOp, r, b, d, mem}) =
	 if match b
	 then ldClass{ldOp=ldOp, r=r, b=rt, d=d, mem=mem}
	 else instr
      fun fstore(stClass, {stOp, r, b, d, mem}) =
	 if match b then stClass{stOp=stOp, r=r, b=rt, d=d, mem=mem}
	 else instr
      fun store{stOp, r, b, d, mem} = 
	if match r then
	  if match b then
	    I.STORE{stOp=stOp, r=rt, b=rt, d=d, mem=mem}
	  else
	    I.STORE{stOp=stOp, r=rt, b=b, d=d, mem=mem}
	else if match b then
	  I.STORE{stOp=stOp, r=r, b=rt, d=d, mem=mem}
	else instr
      fun operate(opClass, {oper, ra, rb, rc}) = 
	if match ra then
	  if isRegOp rb then 
	    opClass{oper=oper, ra=rt, rb=I.REGop rt, rc=rc}
	  else opClass{oper=oper, ra=rt, rb=rb, rc=rc}
	else if isRegOp rb then
	  opClass{oper=oper, ra=ra, rb=I.REGop rt, rc=rc}
	else instr

    in
      case instr
      of I.LDA{r, b, d} => if match b then I.LDA{r=r, b=rt, d=d} else instr
       | I.LDAH{r, b, d} => if match b then I.LDAH{r=r, b=rt, d=d} else instr
       | I.LOAD arg => load(I.LOAD, arg)
       | I.FLOAD farg => load(I.FLOAD, farg)
       | I.STORE arg => store arg
       | I.FSTORE farg => fstore(I.FSTORE, farg)
       | I.JMPL({r, b, d}, labs) =>
	 if match b then I.JMPL({r=r, b=rt, d=d}, labs) else instr
       | I.JSR{r, b, d, defs, uses, cutsTo, mem} =>
	   I.JSR{r=r, b=replace b, d=d, defs=defs, 
		 uses=CS.map {from=rs,to=rt} uses, cutsTo=cutsTo, mem=mem}
       | I.BSR{r, lab, defs, uses, cutsTo, mem} =>
	   I.BSR{r=r, lab=lab, defs=defs, 
		 uses=CS.map {from=rs,to=rt} uses, cutsTo=cutsTo, mem=mem}
       | I.RET{r,b,d} => I.RET{r=r, b=replace b, d=d}
       | I.BRANCH{b=I.BR, ...} => instr
       | I.BRANCH{b, r, lab} => if match r then I.BRANCH{b=b, r=rt, lab=lab} 
				else instr
       | I.OPERATE arg => operate(I.OPERATE, arg)
       | I.OPERATEV arg => operate(I.OPERATEV, arg)
       | I.CMOVE{oper,ra,rb,rc} => 
	   I.CMOVE{oper=oper,ra=replace ra,rb=rwOperand rb,rc=replace rc}
       | I.CALL_PAL{code, def, use } => 
	   I.CALL_PAL{code=code, def=def, use=CS.map {from=rs,to=rt} use}
       | I.PSEUDOARITH{oper, ra, rb, rc, tmps} => 
	   I.PSEUDOARITH{oper=oper, ra=replace ra, rb=rwOperand rb, rc=rc,
			 tmps=tmps}
       | _ => instr
    end
  in
      case instr
       of (I.ANNOTATION{i, ...}) => rewriteUse(i, rs, rt)
	| I.LIVE{regs, spilled} => I.LIVE{regs=C.addReg(rt, C.rmvReg(rs, regs)),
					   spilled=spilled}
        | I.INSTR(i) => I.INSTR(alphaUse(i))
        | I.COPY{k as CB.GP, sz, dst, src, tmp} => 
	   I.COPY{k=k, sz=sz, dst=dst, src=map replace src, tmp=replaceEA tmp}
	| _ => error "rewriteUse"
  end


  fun frewriteUse(instr, fs, ft) = let
    fun match f = CB.sameColor(f,fs)
    fun replace f = if match f then ft else f
    fun alphaUse(instr) = let
      fun foperate(opClass, {oper, fa, fb, fc}) = 
	if match fa then 
	  opClass{oper=oper, fa=ft, fc=fc, fb=replace fb}
	else if match fb then opClass{oper=oper, fa=fa, fb=ft, fc=fc}
	else instr
    in
      case instr
      of I.FBRANCH{b, f, lab} =>
	 if match f then I.FBRANCH{b=b, f=ft, lab=lab} else instr
       | I.FSTORE{stOp, r, b, d, mem} => 
	  if match r then I.FSTORE{stOp=stOp, r=ft, b=b, d=d, mem=mem} else instr
       | I.FOPERATE arg => foperate(I.FOPERATE, arg)
       | I.FOPERATEV arg => foperate(I.FOPERATEV, arg)
       | I.FUNARY{oper,fb,fc} =>
	  if match fb then I.FUNARY{oper=oper,fb=ft,fc=fc} else instr
       | I.FCMOVE{oper,fa,fb,fc} => 
	   I.FCMOVE{oper=oper,fa=replace fa,fb=replace fb,fc=replace fc}
       | I.JSR{r, b, d, defs, uses, cutsTo, mem} => 
	   I.JSR{r=r, b=b, d=d, defs=defs, 
		 uses=CS.map {from=fs,to=ft} uses, cutsTo=cutsTo, mem=mem}
       | I.BSR{r, lab, defs, uses, cutsTo, mem} => 
	   I.BSR{r=r, lab=lab, defs=defs, 
		 uses=CS.map {from=fs,to=ft} uses, cutsTo=cutsTo, mem=mem}
       | _ => instr
    end
  in
    case instr
    of I.ANNOTATION{i, ...} => frewriteUse(i, fs, ft)
     | I.LIVE{regs, spilled} => I.LIVE{regs=C.addFreg(ft, C.rmvFreg(fs, regs)),
					spilled=spilled}
     | I.COPY{k as CB.FP, sz, dst, src, tmp} => 
	  I.COPY{k=k, sz=sz, dst=dst, tmp=tmp, src=map replace src}
     | I.INSTR(i) => I.INSTR(alphaUse(i))
     | _ => error "frewriteUse"
  end


  fun rewriteDef(instr, rs, rt) = let
    fun match r = CB.sameColor(r,rs)
    fun rewrite r = if match r then rt else r
    fun ea (SOME(I.Direct r)) = SOME(I.Direct (rewrite r))
      | ea x = x
    fun alphaDef(instr) =
     (case instr
      of I.LDA{r, b, d} => if match r then I.LDA{r=rt, b=b, d=d} else instr
       | I.LDAH{r, b, d} => if match r then I.LDAH{r=rt, b=b, d=d} else instr
       | I.LOAD{ldOp, r, b, d, mem} => 
	 if match r then I.LOAD{ldOp=ldOp, r=rt, b=b, d=d, mem=mem} else instr
       | I.JMPL({r, b, d}, labs) =>
	 if match r then I.JMPL({r=rt, b=b, d=d}, labs) else instr
       | I.JSR{r, b, d, defs, uses, cutsTo, mem} =>
	   I.JSR{r=rewrite r, b=b, d=d, defs=CS.map {from=rs,to=rt} defs, 
		 uses=uses, cutsTo=cutsTo, mem=mem}
       | I.BSR{r, lab, defs, uses, cutsTo, mem} =>
	   I.BSR{r=rewrite r, lab=lab, defs=CS.map {from=rs,to=rt} defs, 
		 uses=uses, cutsTo=cutsTo, mem=mem}
       | I.RET{r, b, d} => I.RET{r=rewrite r, b=b, d=d}
       | I.BRANCH{b=I.BR, r, lab} => 
	 if match r then I.BRANCH{b=I.BR, r=rt, lab=lab} else instr
       | I.OPERATE{oper, ra, rb, rc} => 
	 if match rc then I.OPERATE{oper=oper, ra=ra, rb=rb, rc=rt} else instr
       | I.OPERATEV{oper, ra, rb, rc} =>
	 if match rc then I.OPERATEV{oper=oper, ra=ra, rb=rb, rc=rt} else instr
       | I.CMOVE{oper,ra,rb,rc} => I.CMOVE{oper=oper,ra=ra,rb=rb,rc=rewrite rc}
       | I.CALL_PAL{code, def, use} => 
	   I.CALL_PAL{code=code, def=CS.map {from=rs,to=rt} def, use=use}
       | I.PSEUDOARITH{oper, ra, rb, rc, tmps} => 
	   I.PSEUDOARITH{oper=oper, ra=ra, rb=rb, rc=rewrite rc,
			 tmps=CS.map {from=rs,to=rt} tmps}
       | _ => instr
    (*esac*))
  in
      case instr
      of I.ANNOTATION{i, ...} => rewriteDef(i,rs,rt)
       | I.KILL{regs, spilled} => 
	   I.KILL{regs=C.addReg(rt, C.rmvReg(rs, regs)), spilled=spilled}
       | I.INSTR(i) => I.INSTR(alphaDef(i))
       | I.COPY{k as CB.GP, sz, dst, src, tmp} =>
	  I.COPY{k=k, sz=sz, dst=map rewrite dst, src=src, tmp=ea tmp}
       | _ => error "rewriteDef"
  end

  fun frewriteDef(instr, fs, ft) = let
    fun match f = CB.sameColor(f,fs)
    fun rewrite f = if match f then ft else f
    fun ea (SOME(I.FDirect f)) = SOME(I.FDirect(rewrite f))
      | ea x  = x
    fun alphaDef(instr) = 
     (case instr
      of I.FLOAD{ldOp, r, b, d, mem} => 
	  if match r then I.FLOAD{ldOp=ldOp, r=ft, b=b, d=d, mem=mem} else instr
       | I.FOPERATE{oper, fa, fb, fc} =>
	  if match fc then I.FOPERATE{oper=oper, fa=fa, fb=fb, fc=ft} else instr
       | I.FOPERATEV{oper, fa, fb, fc} =>
	  if match fc then I.FOPERATEV{oper=oper, fa=fa, fb=fb, fc=ft} else instr
       | I.FUNARY{oper,fb,fc} =>
	  if match fc then I.FUNARY{oper=oper,fb=fb,fc=ft} else instr
       | I.FCMOVE{oper,fa,fb,fc} => I.FCMOVE{oper=oper,fa=fa,fb=fb,fc=rewrite fc}
       | I.JSR{r, b, d, defs, uses, cutsTo, mem} => 
	  I.JSR{r=r, b=b, d=d, defs=CS.map {from=fs,to=ft} defs, 
		uses=uses, cutsTo=cutsTo, mem=mem}
       | I.BSR{r, lab, defs, uses, cutsTo, mem} => 
	  I.BSR{r=r, lab=lab, defs=CS.map {from=fs,to=ft} defs, 
		uses=uses, cutsTo=cutsTo, mem=mem}
       | I.PSEUDOARITH{oper, ra, rb, rc, tmps} => 
	   I.PSEUDOARITH{oper=oper, ra=ra, rb=rb, rc=rc, 
			 tmps=CS.map {from=fs,to=ft} tmps}
       | _  => instr
    (*esac*))
  in
      case instr
       of I.ANNOTATION{i,a} => frewriteDef(i, fs, ft)
        | I.KILL{regs, spilled} => 
	   I.KILL{regs=C.addFreg(ft, C.rmvFreg(fs, regs)), spilled=spilled}
        | I.COPY{k as CB.FP, sz, dst, src, tmp} =>
	   I.COPY{k=k, sz=sz, dst=map rewrite dst, src=src, tmp=ea tmp}
	| I.INSTR(i) => I.INSTR(alphaDef(i))
	| _  => error "frewriteDef"
  end
end

