functor PPCRewrite(Instr : PPCINSTR) = struct
  structure I = Instr
  structure C = I.C
  structure CB = CellsBasis
  structure CS = CB.CellSet

  fun error msg = MLRiscErrorMsg.error ("PPCRewrite", msg)
  fun ea(NONE, _, _) = NONE
    | ea(e as SOME(I.Direct r), rs, rt) =
       if CB.sameColor(r,rs) then SOME(I.Direct rt) else e 
    | ea(e as SOME(I.FDirect r), rs, rt) = 
       if CB.sameColor(r,rs) then SOME(I.FDirect rt) else e 
    | ea(e as SOME(I.Displace{base, disp, mem}), rs, rt) =
       if CB.sameColor(base,rs) then 
	   SOME(I.Displace{base=rt, disp=disp, mem=mem}) 
       else e 

  fun rewriteUse(instr, rs, rt) = let
    fun rplac r = if CB.sameColor(r,rs) then rt else r
    fun rwOperand(opnd as I.RegOp r) = 
	 if CB.sameColor(r,rs) then I.RegOp rt else opnd
      | rwOperand opnd = opnd
    fun ea(SOME(I.Displace{base, disp, mem})) = 
	SOME(I.Displace{base=rplac base, disp=disp, mem=mem}) 
      | ea x = x

    fun ppcUse(instr) = 
     (case instr
      of I.L {ld, rt, ra, d, mem} =>
	  I.L{ld=ld, rt=rt, ra=rplac ra, d=rwOperand d, mem=mem}
       | I.LF {ld, ft, ra, d, mem} =>
	  I.LF{ld=ld, ft=ft, ra=rplac ra, d=rwOperand d, mem=mem}
       | I.ST {st, rs, ra, d, mem} => 
	  I.ST{st=st, rs=rplac rs, ra=rplac ra, d=rwOperand d, mem=mem}
       | I.STF {st, fs, ra, d, mem} => 
	  I.STF{st=st, fs=fs, ra=rplac ra, d=rwOperand d, mem=mem}
       | I.UNARY {oper, rt, ra, Rc, OE} =>
	  I.UNARY{oper=oper, rt=rt, ra=rplac ra, Rc=Rc, OE=OE}
       | I.ARITH{oper, rt, ra, rb, Rc, OE} => 
	  I.ARITH{oper=oper, rt=rt, ra=rplac ra, rb=rplac rb, Rc=Rc, OE=OE}
       | I.ARITHI{oper, rt, ra, im} => 
	  I.ARITHI{oper=oper, rt=rt, ra=rplac ra, im=rwOperand im}
       | I.ROTATE {oper, ra, rs, sh, mb, me} =>
	  I.ROTATE{oper=oper, ra=ra, rs=rplac rs, sh=rplac sh, mb=mb, me=me}
       | I.ROTATEI {oper, ra, rs, sh, mb, me} =>
	  I.ROTATEI{oper=oper, ra=ra, rs=rplac rs, sh=rwOperand sh, mb=mb, me=me}
       | I.COMPARE {cmp, bf, l, ra, rb} =>
	  I.COMPARE{cmp=cmp, bf=bf, l=l, ra=rplac ra, rb=rwOperand rb}
       | I.MTSPR{rs, spr} => I.MTSPR{rs=rplac rs, spr=spr}
       | I.TW {to, ra, si} => I.TW{to=to, ra=rplac ra, si=rwOperand si}
       | I.TD {to, ra, si} => I.TD{to=to, ra=rplac ra, si=rwOperand si}
       | I.CALL {def, use, cutsTo, mem} => 
	    I.CALL{def=def, use=CS.map {from=rs,to=rt} use, 
		   cutsTo=cutsTo, mem=mem}
       | I.LWARX {rt, ra, rb} =>
            I.LWARX{rt=rt, ra=rplac ra, rb=rplac rb}
       | I.STWCX {rs, ra, rb} =>
            I.STWCX{rs=rplac rs, ra=rplac ra, rb=rplac rb}
       | _ => instr
    (*esac*))
  in
      case instr
       of (I.ANNOTATION{i, ...}) => rewriteUse(i, rs, rt)
        | I.INSTR(i) => I.INSTR(ppcUse(i))
        | I.COPY{k, sz, dst, src, tmp} =>
	  I.COPY{k=k, sz=sz, dst=dst, tmp= ea tmp,
		 src=case k of CB.GP => map rplac src | _ => src}
	| I.LIVE{regs, spilled} => 
	    I.LIVE{regs=C.addReg(rt, C.rmvReg(rs, regs)), spilled=spilled}
	| _ => error "rewriteUse"
  end


  fun rewriteDef(instr, rs, rt) = let
    fun rplac r = if CB.sameColor(r,rs) then rt else r
    fun ea (SOME(I.Direct r)) = SOME(I.Direct(rplac r))
      | ea x = x
    fun ppcDef(instr) = 
     (case instr
      of I.L {ld, rt, ra, d, mem} =>
	  I.L{ld=ld, rt=rplac rt, ra=ra, d=d, mem=mem}
       | I.UNARY {oper, rt, ra, Rc, OE} =>
	  I.UNARY{oper=oper, rt=rplac rt, ra=ra, Rc=Rc, OE=OE}
       | I.ARITH {oper, rt, ra, rb, Rc, OE} =>
	  I.ARITH{oper=oper, rt=rplac rt, ra=ra, rb=rb, Rc=Rc, OE=OE}
       | I.ARITHI {oper, rt, ra, im} =>
	  I.ARITHI {oper=oper, rt=rplac rt, ra=ra, im=im}
       | I.ROTATE {oper, ra, rs, sh, mb, me} =>
	  I.ROTATE {oper=oper, ra=rplac ra, rs=rs, sh=sh, mb=mb, me=me}
       | I.ROTATEI {oper, ra, rs, sh, mb, me} =>
	  I.ROTATEI {oper=oper, ra=rplac ra, rs=rs, sh=sh, mb=mb, me=me}
       | I.MFSPR {rt, spr} => I.MFSPR{rt=rplac rt, spr=spr}
       | I.CALL {def, use, cutsTo, mem} => 
	  I.CALL{def=CS.map {from=rs,to=rt} def, use=use, 
		 cutsTo=cutsTo, mem=mem}
       | I.LWARX {rt, ra, rb} =>
            I.LWARX{rt=rplac rt, ra=ra, rb=rb}
       | _ => instr
    (*esac*))
  in 
      case instr
       of (I.ANNOTATION{i, ...}) => rewriteDef(i, rs, rt)
        | I.INSTR(i) => I.INSTR(ppcDef(i))
	| I.KILL{regs, spilled} => 
	   I.KILL{regs=C.addReg(rt, C.rmvReg(rs, regs)), spilled=spilled}
        | I.COPY {k, sz, dst, src, tmp} =>
	  I.COPY{k=k, sz=sz, src=src, tmp=ea tmp, 
		 dst=case k of CB.GP => map rplac dst | _ => dst}
	| _ => error "rewriteDef"
  end


  fun frewriteUse(instr, fs, ft) = let
    fun rplac r = if CB.sameColor(r,fs) then ft else r
    fun ppcUse(instr) = 
     (case instr
      of I.STF {st, fs, ra, d, mem} =>
	   I.STF{st=st, fs=rplac fs, ra=ra, d=d, mem=mem}
       | I.CALL{def, use, cutsTo, mem} => 
	   I.CALL{def=def, use=CS.map {from=fs,to=ft} use, 
		  cutsTo=cutsTo, mem=mem}
       | I.FCOMPARE {cmp, bf, fa, fb} =>
	   I.FCOMPARE{cmp=cmp, bf=bf, fa=rplac fa, fb=rplac fb}
       | I.FUNARY {oper, ft, fb, Rc} =>
	   I.FUNARY{oper=oper, ft=ft, fb=rplac fb, Rc=Rc}
       | I.FARITH {oper, ft, fa, fb, Rc} =>
	   I.FARITH{oper=oper, ft=ft, fa=rplac fa, fb=rplac fb, Rc=Rc}
       | I.FARITH3 {oper, ft, fa, fb, fc, Rc} =>
	   I.FARITH3{oper=oper,ft=ft,fa=rplac fa, fb=rplac fb, fc=rplac fc,Rc=Rc}
       | _ => instr
    (*esac*))
  in
      case instr
       of (I.ANNOTATION{i, ...}) => frewriteUse(i, fs, ft)
        | I.INSTR(i) => I.INSTR(ppcUse(i))
	| I.LIVE{regs, spilled} => 
	    I.LIVE{regs=C.addFreg(ft, C.rmvFreg(fs, regs)), spilled=spilled}
        | I.COPY {k as CB.FP, sz, dst, src, tmp} =>
 	   I.COPY{k=k, sz=sz, dst=dst, src=map rplac src, tmp=tmp}

	| _ => error "frewriteUse"

  end

  fun frewriteDef(instr, fs, ft) = let
    fun rplac r = if CB.sameColor(r,fs) then ft else r
    fun rplacEA (SOME(I.FDirect f)) = SOME(I.FDirect(rplac f))
      | rplacEA ea = ea
    fun ppcDef(instr) = 
     (case instr
      of I.LF{ld, ft, ra, d, mem} =>
	  I.LF{ld=ld, ft=rplac ft, ra=ra, d=d, mem=mem}
       | I.FUNARY {oper, ft, fb, Rc} =>
	  I.FUNARY{oper=oper, ft=rplac ft, fb=fb, Rc=Rc}
       | I.FARITH{oper, ft, fa, fb, Rc} =>
	  I.FARITH{oper=oper, ft=rplac ft, fa=fa, fb=fb, Rc=Rc}
       | I.FARITH3{oper, ft, fa, fb, fc, Rc} =>
	  I.FARITH3{oper=oper, ft=rplac ft, fa=fa, fb=fb, fc=fc, Rc=Rc}
      (* CALL = BCLR {bo=ALWAYS, bf=0, bit=0, LK=true, labels=[] *)
       | I.CALL{def, use, cutsTo, mem} => 
	  I.CALL{def=CS.map {from=fs,to=ft} def, use=use, 
		 cutsTo=cutsTo, mem=mem}
       | _ => instr
    (*esac*))
  in
      case instr
       of (I.ANNOTATION{i, ...}) => frewriteDef(i, fs, ft)
        | I.INSTR(i) => I.INSTR(ppcDef(i))
        | I.KILL{regs, spilled} => 
	   I.KILL{regs=C.addFreg(ft, C.rmvFreg(fs, regs)), spilled=spilled}
        | I.COPY {k as CB.FP, sz, dst, src, tmp} =>
	  I.COPY{k=k, sz=sz, dst=map rplac dst, src=src,  tmp=rplacEA tmp}
	| _ => error "frewriteDef"
  end
end

