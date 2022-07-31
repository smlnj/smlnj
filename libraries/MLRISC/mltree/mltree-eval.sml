(* mltree-eval.sml
 *
 * COPYRIGHT (c) 2001 Lucent Technologies, Bell Laboratories.
 *
 * Utilites to evaluate and compare mltree expressions.
 *)

functor MLTreeEval
  (structure T : MLTREE  
   (* equality extensions *)
   val eqSext  : T.equality -> T.sext * T.sext -> bool
   val eqRext  : T.equality -> T.rext * T.rext -> bool
   val eqFext  : T.equality -> T.fext * T.fext -> bool
   val eqCCext : T.equality -> T.ccext * T.ccext -> bool
  ) : MLTREE_EVAL =
struct
  structure T = T
  structure I = T.I
  structure Constant = T.Constant
  structure C = CellsBasis

  val eqLabel = Label.same
  fun eqLabels([],[]) = true
    | eqLabels(a::b,c::d) = eqLabel(a,c) andalso eqLabels(b,d)
    | eqLabels _ = false
  and eqCell(C.CELL{id=x, ...},C.CELL{id=y, ...}) = x=y
  and eqCells([], []) = true
    | eqCells(x::xs,y::ys) = eqCell(x,y) andalso eqCells(xs,ys)
    | eqCells _ = false
  and eqCopy((t1,dst1,src1),(t2,dst2,src2)) =
       t1=t2 andalso eqCells(dst1,dst2) andalso eqCells(src1,src2)
  and eqCtrl(c1,c2) = eqCell(c1,c2)
  and eqCtrls(c1,c2) = eqCells(c1,c2)

  (* statements *)
  and equality() = {stm=eqStm, rexp=eqRexp, fexp=eqFexp, ccexp=eqCCexp}
  and eqStm(T.MV(a,b,c),T.MV(d,e,f)) =
          a=d andalso eqCell(b,e) andalso eqRexp(c,f)
    | eqStm(T.CCMV(a,b),T.CCMV(c,d)) = eqCell(a,c) andalso eqCCexp(b,d)
    | eqStm(T.FMV(a,b,c),T.FMV(d,e,f)) = 
          a=d andalso eqCell(b,e) andalso eqFexp(c,f)
    | eqStm(T.COPY x,T.COPY y) = eqCopy(x,y)
    | eqStm(T.FCOPY x,T.FCOPY y) = eqCopy(x,y)
    | eqStm(T.JMP(a,b),T.JMP(a',b')) = eqRexp(a,a')
    | eqStm(T.CALL{funct=a,defs=b,uses=c,...},
            T.CALL{funct=d,defs=e,uses=f,...}) =  
         eqRexp(a,d) andalso eqMlriscs(b,e) andalso eqMlriscs(c,f)
    | eqStm(T.RET _,T.RET _) = true
    | eqStm(T.STORE(a,b,c,_),T.STORE(d,e,f,_)) = 
         a=d andalso eqRexp(b,e) andalso eqRexp(c,f)
    | eqStm(T.FSTORE(a,b,c,_),T.FSTORE(d,e,f,_)) =
         a=d andalso eqRexp(b,e) andalso eqFexp(c,f)
    | eqStm(T.ANNOTATION(s1, _),s2) = eqStm(s1,s2)
    | eqStm(s1,T.ANNOTATION(s2, _)) = eqStm(s1,s2)
    | eqStm(T.PHI x,T.PHI y) = x=y
    | eqStm(T.SOURCE,T.SOURCE) = true
    | eqStm(T.SINK,T.SINK) = true
    | eqStm(T.BCC(b,c),T.BCC(b',c')) = 
        eqCCexp(b,b') andalso eqLabel(c,c')
    | eqStm(T.IF(b,c,d),T.IF(b',c',d')) = 
        eqCCexp(b,b') andalso eqStm(c,c') andalso eqStm(d,d')
    | eqStm(T.RTL{attribs=x,...},T.RTL{attribs=y,...}) = x=y
    | eqStm(T.REGION(a,b),T.REGION(a',b')) = eqCtrl(b,b') andalso eqStm(a,a')
    | eqStm(T.EXT a,T.EXT a') = eqSext (equality()) (a,a')
    | eqStm _ = false

  and eqStms([],[]) = true
    | eqStms(a::b,c::d) = eqStm(a,c) andalso eqStms(b,d)
    | eqStms _ = false

  and eqMlrisc(T.CCR a,T.CCR b) = eqCCexp(a,b)
    | eqMlrisc(T.GPR a,T.GPR b) = eqRexp(a,b)
    | eqMlrisc(T.FPR a,T.FPR b) = eqFexp(a,b)
    | eqMlrisc _ = false

  and eqMlriscs([],[]) = true
    | eqMlriscs(a::b,c::d) = eqMlrisc(a,c) andalso eqMlriscs(b,d)
    | eqMlriscs _ = false

  and eq2((a,b,c),(d,e,f)) = a=d andalso eqRexp(b,e) andalso eqRexp(c,f)
  and eq3((m,a,b,c),(n,d,e,f)) =
      m = n andalso a=d andalso eqRexp(b,e) andalso eqRexp(c,f)

  and eqRexp(T.REG(a,b),T.REG(c,d)) = a=c andalso eqCell(b,d)
    | eqRexp(T.LI a,T.LI b) = a = b
    | eqRexp(T.LABEL a,T.LABEL b) = eqLabel(a,b)
    | eqRexp(T.LABEXP a,T.LABEXP b) = eqRexp(a,b)
    | eqRexp(T.CONST a,T.CONST b) = Constant.==(a,b)
    | eqRexp(T.NEG(t,x),T.NEG(t',x')) = t = t' andalso eqRexp(x,x')
    | eqRexp(T.ADD x,T.ADD y) = eq2(x,y)
    | eqRexp(T.SUB x,T.SUB y) = eq2(x,y)
    | eqRexp(T.MULS x,T.MULS y) = eq2(x,y)
    | eqRexp(T.DIVS x,T.DIVS y) = eq3(x,y)
    | eqRexp(T.REMS x,T.REMS y) = eq3(x,y)
    | eqRexp(T.MULU x,T.MULU y) = eq2(x,y)
    | eqRexp(T.DIVU x,T.DIVU y) = eq2(x,y)
    | eqRexp(T.REMU x,T.REMU y) = eq2(x,y)
    | eqRexp(T.NEGT(t,x),T.NEGT(t',x')) = t = t' andalso eqRexp(x,x')
    | eqRexp(T.ADDT x,T.ADDT y) = eq2(x,y)
    | eqRexp(T.SUBT x,T.SUBT y) = eq2(x,y)
    | eqRexp(T.MULT x,T.MULT y) = eq2(x,y)
    | eqRexp(T.DIVT x,T.DIVT y) = eq3(x,y)
    | eqRexp(T.ANDB x,T.ANDB y) = eq2(x,y)
    | eqRexp(T.ORB x,T.ORB y) = eq2(x,y)
    | eqRexp(T.XORB x,T.XORB y) = eq2(x,y)
    | eqRexp(T.EQVB x,T.EQVB y) = eq2(x,y)
    | eqRexp(T.NOTB(a,b),T.NOTB(c,d)) = a=c andalso eqRexp(b,d)
    | eqRexp(T.SRA x,T.SRA y) = eq2(x,y)
    | eqRexp(T.SRL x,T.SRL y) = eq2(x,y)
    | eqRexp(T.SLL x,T.SLL y) = eq2(x,y)
    | eqRexp(T.COND(a,b,c,d),T.COND(e,f,g,h)) = 
         a=e andalso eqCCexp(b,f) andalso eqRexp(c,g) andalso eqRexp(d,h)
    | eqRexp(T.SX(a,b,c),T.SX(a',b',c')) = 
         a=a' andalso b=b' andalso eqRexp(c,c')
    | eqRexp(T.ZX(a,b,c),T.ZX(a',b',c')) = 
         a=a' andalso b=b' andalso eqRexp(c,c')
    | eqRexp(T.CVTF2I(a,b,c,d),T.CVTF2I(e,f,g,h)) = 
         a=e andalso b=f andalso c=g andalso eqFexp(d,h)
    | eqRexp(T.LOAD(a,b,_),T.LOAD(c,d,_)) = a=c andalso eqRexp(b,d)
    | eqRexp(T.LET(a,b),T.LET(c,d)) = eqStm(a,c) andalso eqRexp(b,d)
    | eqRexp(T.ARG x,T.ARG y) = x = y
    | eqRexp(T.PARAM x,T.PARAM y) = x = y
    | eqRexp(T.???,T.???) = true
    | eqRexp(T.$(t1,k1,e1),T.$(t2,k2,e2)) = 
        t1=t2 andalso k1=k2 andalso eqRexp(e1,e2)
    | eqRexp(T.BITSLICE(t1,s1,e1),T.BITSLICE(t2,s2,e2)) =
        t1=t2 andalso s1=s2 andalso eqRexp(e1,e2)
    | eqRexp(T.MARK(a,_),b) = eqRexp(a,b)
    | eqRexp(a,T.MARK(b,_)) = eqRexp(a,b)
    | eqRexp(T.PRED(a,b),T.PRED(a',b')) = eqCtrl(b,b') andalso eqRexp(a,a')
    | eqRexp(T.REXT(a,b),T.REXT(a',b')) =   
          a=a' andalso eqRext (equality()) (b,b') 
    | eqRexp _ = false

  and eqRexps([],[]) = true
    | eqRexps(a::b,c::d) = eqRexp(a,c) andalso eqRexps(b,d)
    | eqRexps _ = false

  and eq2'((a,b,c),(d,e,f)) = a=d andalso eqFexp(b,e) andalso eqFexp(c,f)
  and eq1'((a,b),(d,e)) = a=d andalso eqFexp(b,e) 

  and eqFexp(T.FREG(t1,x),T.FREG(t2,y)) = t1=t2 andalso eqCell(x,y)
    | eqFexp(T.FLOAD(a,b,_),T.FLOAD(c,d,_)) = a=c andalso eqRexp(b,d)
    | eqFexp(T.FADD x,T.FADD y) = eq2'(x,y) 
    | eqFexp(T.FMUL x,T.FMUL y) = eq2'(x,y)
    | eqFexp(T.FSUB x,T.FSUB y) = eq2'(x,y) 
    | eqFexp(T.FDIV x,T.FDIV y) = eq2'(x,y)
    | eqFexp(T.FCOPYSIGN x, T.FCOPYSIGN y) = eq2'(x,y)
    | eqFexp(T.FCOND(t,x,y,z), T.FCOND(t',x',y',z')) = 
        t=t' andalso eqCCexp(x,x') andalso eqFexp(y,y') andalso eqFexp(z,z')
    | eqFexp(T.FABS x,T.FABS y) = eq1'(x,y)
    | eqFexp(T.FNEG x,T.FNEG y) = eq1'(x,y)
    | eqFexp(T.FSQRT x,T.FSQRT y) = eq1'(x,y)
    | eqFexp(T.CVTI2F(a,b,c),T.CVTI2F(a',b',c')) = 
         a=a' andalso b=b' andalso eqRexp(c,c')
    | eqFexp(T.CVTF2F(a,b,c),T.CVTF2F(a',b',c')) = 
         a=a' andalso b=b' andalso eqFexp(c,c')
    | eqFexp(T.FEXT(a,f),T.FEXT(b,g)) = a=b andalso eqFext (equality()) (f,g) 
    | eqFexp(T.FMARK(a,_),b) = eqFexp(a,b)
    | eqFexp(a,T.FMARK(b,_)) = eqFexp(a,b)
    | eqFexp(T.FPRED(a,b),T.FPRED(a',b')) = eqCtrl(b,b') andalso eqFexp(a,a')
    | eqFexp _ = false

  and eqFexps([],[]) = true
    | eqFexps(a::b,c::d) = eqFexp(a,c) andalso eqFexps(b,d)
    | eqFexps _ = false

  and eqCCexp(T.CC(c1,x),T.CC(c2,y)) = c1=c2 andalso eqCell(x,y)
    | eqCCexp(T.FCC(c1,x),T.FCC(c2,y)) = c1=c2 andalso eqCell(x,y)
    | eqCCexp(T.CMP(x,a,b,c),T.CMP(y,d,e,f)) = 
        a=d andalso eqRexp(b,e) andalso eqRexp(c,f) andalso x = y
    | eqCCexp(T.FCMP(x,a,b,c),T.FCMP(y,d,e,f)) =
        a=d andalso eqFexp(b,e) andalso eqFexp(c,f) andalso x = y
    | eqCCexp(T.NOT x, T.NOT y) = eqCCexp(x,y)
    | eqCCexp(T.AND x, T.AND y) = eqCCexp2(x,y)
    | eqCCexp(T.OR x,  T.OR y) = eqCCexp2(x,y)
    | eqCCexp(T.XOR x, T.XOR y) = eqCCexp2(x,y)
    | eqCCexp(T.EQV x, T.EQV y) = eqCCexp2(x,y)
    | eqCCexp(T.CCMARK(a,_),b) = eqCCexp(a,b)
    | eqCCexp(a,T.CCMARK(b,_)) = eqCCexp(a,b)
    | eqCCexp(T.CCEXT(t,a),T.CCEXT(t',b)) = 
        t=t' andalso eqCCext (equality()) (a,b)
    | eqCCexp(T.TRUE, T.TRUE) = true
    | eqCCexp(T.FALSE, T.FALSE) = true
    | eqCCexp _ = false

  and eqCCexp2((x,y),(x',y')) = eqCCexp(x,x') andalso eqCCexp(y,y')

  and eqCCexps([],[]) = true
    | eqCCexps(a::b,c::d) = eqCCexp(a,c) andalso eqCCexps(b,d)
    | eqCCexps _ = false

  exception NonConst

  fun eval{label, const} =
  let fun drm T.DIV_TO_ZERO = I.DIV_TO_ZERO
	| drm T.DIV_TO_NEGINF = I.DIV_TO_NEGINF
      fun rexp(T.LI i) = i
	| rexp(T.CONST c) = const c
	| rexp(T.LABEL l) = IntInf.fromInt(label l)
	| rexp(T.LABEXP e) = rexp e

	| rexp(T.NEG(sz,x)) = I.NEG(sz,rexp x)
	| rexp(T.ADD(sz,x,y)) = I.ADD(sz,rexp x,rexp y)
	| rexp(T.SUB(sz,x,y)) = I.SUB(sz,rexp x,rexp y)

	| rexp(T.MULS(sz,x,y)) = I.MULS(sz,rexp x,rexp y)
	| rexp(T.DIVS(m,sz,x,y)) = I.DIVS(drm m,sz,rexp x,rexp y)
	| rexp(T.REMS(m,sz,x,y)) = I.REMS(drm m,sz,rexp x,rexp y)

	| rexp(T.MULU(sz,x,y)) = I.MULU(sz,rexp x,rexp y)
	| rexp(T.DIVU(sz,x,y)) = I.DIVU(sz,rexp x,rexp y)
	| rexp(T.REMU(sz,x,y)) = I.REMU(sz,rexp x,rexp y)

	| rexp(T.NEGT(sz,x)) = I.NEGT(sz,rexp x)
	| rexp(T.ADDT(sz,x,y)) = I.ADDT(sz,rexp x,rexp y)
	| rexp(T.SUBT(sz,x,y)) = I.SUBT(sz,rexp x,rexp y)
	| rexp(T.MULT(sz,x,y)) = I.MULT(sz,rexp x,rexp y)
	| rexp(T.DIVT(m,sz,x,y)) = I.DIVT(drm m,sz,rexp x,rexp y)

	| rexp(T.NOTB(sz,x)) = I.NOTB(sz,rexp x)
	| rexp(T.ANDB(sz,x,y)) = I.ANDB(sz,rexp x,rexp y)
	| rexp(T.ORB(sz,x,y)) = I.ORB(sz,rexp x,rexp y)
	| rexp(T.XORB(sz,x,y)) = I.XORB(sz,rexp x,rexp y)
	| rexp(T.EQVB(sz,x,y)) = I.EQVB(sz,rexp x,rexp y)
	| rexp(T.SLL(sz,x,y)) = I.SLL(sz,rexp x,rexp y)
	| rexp(T.SRL(sz,x,y)) = I.SRL(sz,rexp x,rexp y)
	| rexp(T.SRA(sz,x,y)) = I.SRA(sz,rexp x,rexp y)
	| rexp(T.BITSLICE(sz,x,y)) = I.BITSLICE(sz,x,rexp y)

	| rexp(T.COND(sz,cc,x,y)) = if ccexp cc then rexp x else rexp y
	| rexp(T.SX(a,b,x)) = I.SX(a,b,rexp x)
	| rexp(T.ZX(a,b,x)) = I.ZX(a,b,rexp x)
	| rexp(T.MARK(e,_)) = rexp e

	| rexp _ = raise NonConst
      and ccexp(T.TRUE) = true
	| ccexp(T.FALSE) = false
	| ccexp(T.CMP(sz,T.EQ,x,y)) = I.EQ(sz,rexp x,rexp y)
	| ccexp(T.CMP(sz,T.NE,x,y)) = I.NE(sz,rexp x,rexp y)
	| ccexp(T.CMP(sz,T.GT,x,y)) = I.GT(sz,rexp x,rexp y)
	| ccexp(T.CMP(sz,T.GE,x,y)) = I.GE(sz,rexp x,rexp y)
	| ccexp(T.CMP(sz,T.LT,x,y)) = I.LT(sz,rexp x,rexp y)
	| ccexp(T.CMP(sz,T.LE,x,y)) = I.LE(sz,rexp x,rexp y)
	| ccexp(T.CMP(sz,T.GTU,x,y)) = I.GTU(sz,rexp x,rexp y)
	| ccexp(T.CMP(sz,T.LTU,x,y)) = I.LTU(sz,rexp x,rexp y)
	| ccexp(T.CMP(sz,T.GEU,x,y)) = I.GEU(sz,rexp x,rexp y)
	| ccexp(T.CMP(sz,T.LEU,x,y)) = I.LEU(sz,rexp x,rexp y)
	| ccexp(T.NOT x) = not(ccexp x)
	| ccexp(T.AND(x,y)) = ccexp x andalso ccexp y
	| ccexp(T.OR(x,y)) = ccexp x orelse ccexp y
	| ccexp(T.XOR(x,y)) = ccexp x <> ccexp y
	| ccexp(T.EQV(x,y)) = ccexp x = ccexp y
	| ccexp(T.CCMARK(e,_)) = ccexp e
	| ccexp _ = raise NonConst
  in  {rexp=rexp, ccexp=ccexp} 
  end

  fun valueOf e = 
	IntInf.toInt
	   (#rexp(eval{const=fn c => IntInf.fromInt(Constant.valueOf c),
		       label=Label.addrOf}) e)
  val == = eqRexp
end
