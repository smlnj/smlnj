(* mipsRewrite.sml -- rewrite an mips instruction 
 *
 *)

functor MIPSRewrite(Instr : MIPSINSTR) = 
struct
  structure I = Instr
  structure C = I.C

  fun error msg = MLRiscErrorMsg.error("MipsRewrite",msg)

  fun rewriteUse(instr, rs, rt) = 
  let fun match r = C.sameColor(r,rs)
      fun R r = if match r then rt else r
      fun O(i as I.Reg r) = if match r then I.Reg rt else i
        | O i = i
  in  case instr of
        I.LOAD{l,rt,b,d,mem} => I.LOAD{l=l,rt=rt,b=R b,d=O d,mem=mem}
      | I.STORE{s,rs,b,d,mem} => I.STORE{s=s,rs=R rs,b=R b,d=O d,mem=mem}
      | I.FLOAD{l,ft,b,d,mem} => I.FLOAD{l=l,ft=ft,b=R b,d=O d,mem=mem}
      | I.FSTORE{s,fs,b,d,mem} => I.FSTORE{s=s,fs=fs,b=R b,d=O d,mem=mem}
      | I.FROUND{oper, ft, fs1, rs2} => 
          I.FROUND{oper=oper, ft=ft, fs1=fs1, rs2=R rs2}
      | I.TRAP{t,rs,i} => I.TRAP{t=t,rs=R rs,i=O i}
      | I.JR{rs,labels,nop} => I.JR{rs=R rs,labels=labels,nop=nop}
      | I.JAL{lab,defs,uses,cutsTo,mem,nop} => 
          I.JAL{lab=lab,defs=defs,uses=C.CellSet.map {from=rs,to=rt} uses,
                cutsTo=cutsTo,mem=mem,nop=nop}
      | I.JALR{rt,rs,defs,uses,cutsTo,mem,nop} =>
          I.JALR{rt=rt,rs=R rs,
                 defs=defs,uses=C.CellSet.map {from=rs,to=rt} uses,
                 cutsTo=cutsTo,mem=mem,nop=nop}
      | I.BRANCH{likely,cond,rs,rt,lab,nop} =>
          I.BRANCH{likely=likely,cond=cond,rs=R rs,rt=R rt,lab=lab,nop=nop}
      | I.ARITH{oper,rt,rs,i} => I.ARITH{oper=oper,rt=rt,rs=R rs,i=O i}
      | I.UNARY{oper,rt,rs} => I.UNARY{oper=oper,rt=rt,rs=R rs}
      | I.MULTIPLY{oper,rt,rs} => I.MULTIPLY{oper=oper,rt=R rt,rs=R rs}
      | I.DIVIDE{oper,rt,rs} => I.DIVIDE{oper=oper,rt=R rt,rs=R rs}
      | I.MTLO rs => I.MTLO(R rs)
      | I.MTHI rs => I.MTHI(R rs)
      | I.CVTI2F{cvt, rs, ft} => I.CVTI2F{cvt=cvt, rs=R rs, ft=ft}
      | I.COPY{src,dst,tmp,impl} =>
          I.COPY{src=map R src,dst=dst,tmp=tmp,impl=impl}
      | I.ANNOTATION{i,a} => I.ANNOTATION{i=rewriteUse(i,rs,rt),a=a}
      | _ => instr      
  end

  fun rewriteDef(instr, rs, rt) = 
  let fun match r = C.sameColor(r,rs)
      fun R r = if match r then rt else r
  in  case instr of
        I.LUI{rt, imm} => I.LUI{rt=R rt, imm=imm}
      | I.LOAD{l,rt,b,d,mem} => I.LOAD{l=l,rt=R rt,b=b,d=d,mem=mem}
      | I.JAL{lab,defs,uses,cutsTo,mem,nop} => 
          I.JAL{lab=lab,defs=C.CellSet.map{from=rs,to=rt} defs,uses=uses,
                cutsTo=cutsTo,mem=mem,nop=nop}
      | I.JALR{rt,rs,defs,uses,cutsTo,mem,nop} =>
          I.JALR{rt=R rt,rs=rs,
                 uses=uses,defs=C.CellSet.map {from=rs,to=rt} defs,
                 cutsTo=cutsTo,mem=mem,nop=nop}
      | I.ARITH{oper,rt,rs,i} => I.ARITH{oper=oper,rt=R rt,rs=rs,i=i}
      | I.UNARY{oper,rt,rs} => I.UNARY{oper=oper,rt=R rt,rs=rs}
      | I.MFLO rt => I.MFLO(R rt)
      | I.MFHI rt => I.MFHI(R rt)
      | I.CVTF2I{cvt, rt, fs} => I.CVTF2I{cvt=cvt, rt=R rt, fs=fs}
      | I.COPY{src,dst,tmp,impl} =>
          I.COPY{src=src,dst=map R dst,tmp=tmp,impl=impl}
      | I.ANNOTATION{i,a} => I.ANNOTATION{i=rewriteUse(i,rs,rt),a=a}
      | _ => instr
  end

  fun frewriteUse(instr, fs, ft) = 
  let fun match f = C.sameColor(f,fs)
      fun R f = if match f then ft else f
  in  case instr of
        I.FSTORE{s,fs,b,d,mem} => I.FSTORE{s=s,fs=R fs,b=b,d=d,mem=mem}
      | I.JAL{lab,defs,uses,cutsTo,mem,nop} => 
          I.JAL{lab=lab,defs=defs,uses=C.CellSet.map {from=fs,to=ft} uses,
                cutsTo=cutsTo,mem=mem,nop=nop}
      | I.JALR{rt,rs,defs,uses,cutsTo,mem,nop} =>
          I.JALR{rt=rt,rs=rs,
                 defs=defs,uses=C.CellSet.map {from=fs,to=ft} uses,
                 cutsTo=cutsTo,mem=mem,nop=nop}
      | I.FARITH{oper, ft, fs1, fs2} =>
          I.FARITH{oper=oper, ft=ft, fs1=R fs1, fs2=R fs2}
      | I.FUNARY{oper, ft, fs} => I.FUNARY{oper=oper, ft=ft, fs=R fs}
      | I.FROUND{oper, ft, fs1, rs2} => 
         I.FROUND{oper=oper, ft=ft, fs1=R fs1, rs2=rs2}
      | I.CVTF2I{cvt, fs, rt} => I.CVTF2I{cvt=cvt, fs=R fs, rt=rt}
      | I.FARITH3{oper, ft, fs1, fs2, fs3} =>
          I.FARITH3{oper=oper, ft=ft, fs1=R fs1, fs2=R fs2, fs3=R fs3}
      | I.FCMP{fcond, fmt, cc, fs1, fs2} =>
          I.FCMP{fcond=fcond, fmt=fmt, cc=cc, fs1=R fs1, fs2=R fs2}
      | I.FCOPY{src,dst,tmp,impl} =>
          I.FCOPY{src=map R src,dst=dst,tmp=tmp,impl=impl}
      | I.ANNOTATION{i,a} => I.ANNOTATION{i=frewriteUse(i,fs,ft),a=a}
      | _ => instr      
  end

  fun frewriteDef(instr, fs, ft) = 
  let fun match f = C.sameColor(f,fs)
      fun R f = if match f then ft else f
  in  case instr of
        I.FLOAD{l,ft,b,d,mem} => I.FLOAD{l=l,ft=R ft,b=b,d=d,mem=mem}
      | I.JAL{lab,defs,uses,cutsTo,mem,nop} => 
          I.JAL{lab=lab,uses=uses,defs=C.CellSet.map {from=fs,to=ft} defs,
                cutsTo=cutsTo,mem=mem,nop=nop}
      | I.JALR{rt,rs,defs,uses,cutsTo,mem,nop} =>
          I.JALR{rt=rt,rs=rs,
                 uses=uses,defs=C.CellSet.map {from=fs,to=ft} defs,
                 cutsTo=cutsTo,mem=mem,nop=nop}
      | I.FARITH{oper, ft, fs1, fs2} =>
          I.FARITH{oper=oper, ft=R ft, fs1=fs1, fs2=fs2}
      | I.FUNARY{oper, ft, fs} => I.FUNARY{oper=oper, ft=R ft, fs=fs}
      | I.FROUND{oper, ft, fs1, rs2} => 
         I.FROUND{oper=oper, ft=R ft, fs1=fs1, rs2=rs2}
      | I.CVTI2F{cvt, rs, ft} => I.CVTI2F{cvt=cvt, rs=rs, ft=R ft}
      | I.FARITH3{oper, ft, fs1, fs2, fs3} =>
          I.FARITH3{oper=oper, ft=R ft, fs1=fs1, fs2=fs2, fs3=fs3}
      | I.FCOPY{src,dst,tmp,impl} =>
          I.FCOPY{src=src,dst=map R dst,tmp=tmp,impl=impl}
      | I.ANNOTATION{i,a} => I.ANNOTATION{i=frewriteDef(i,fs,ft),a=a}
      | _ => instr      
  end

end

