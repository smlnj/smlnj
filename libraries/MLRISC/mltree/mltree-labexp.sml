(* labelExp.sml -- expressions involving labels
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

functor LabelExp
  (structure T : MLTREE
   (* Hashing extensions *)
   val hashSext  : T.hasher -> T.sext -> word
   val hashRext  : T.hasher -> T.rext -> word
   val hashFext  : T.hasher -> T.fext -> word
   val hashCCext : T.hasher -> T.ccext -> word
   (* Equality extensions *)
   val eqSext  : T.equality -> T.sext * T.sext -> bool
   val eqRext  : T.equality -> T.rext * T.rext -> bool
   val eqFext  : T.equality -> T.fext * T.fext -> bool
   val eqCCext : T.equality -> T.ccext * T.ccext -> bool
   (* assembly output *)
   val labelFmt : {gPrefix : string, aPrefix: string}
  ) : LABELEXP =
struct

   structure T          = T
   structure I          = T.I 
   structure Constant   = T.Constant
   structure B          = T.Basis
   structure C          = CellsBasis
   structure W          = Word

   val w = W.fromInt
   val i2s = Int.toString
   val toLower = String.map Char.toLower

   fun error msg = MLRiscErrorMsg.error("LabelExp",msg)
   fun wv(C.CELL{id, ...}) = w id
   fun wvs is = 
   let fun f([],h) = h
         | f(i::is,h) = f(is,wv i+h)
   in  f(is,0w0) end

   (*
    * Hashing
    *)
   val hashLabel = Label.hash
   fun hasher() = {stm=hashStm, rexp=hashRexp, fexp=hashFexp, ccexp=hashCCexp}
   and hashCtrl ctrl = wv ctrl
   and hashStm stm =
      case stm of  
      T.MV(t,dst,rexp) => 0w123 + w t + wv dst + hashRexp rexp
    | T.CCMV(dst,ccexp) => 0w1234 + wv dst + hashCCexp ccexp
    | T.FMV(fty,dst,fexp) => 0w12345 + w fty + wv dst + hashFexp fexp
    | T.COPY(ty,dst,src) => 0w234 + w ty + wvs dst + wvs src
    | T.FCOPY(fty,dst,src) => 0w456 + w fty + wvs dst + wvs src
    | T.JMP(ea,labels) => 0w45 + hashRexp ea
    | T.CALL{funct,targets,defs,uses,region,pops} =>
          hashRexp funct + hashMlriscs defs + hashMlriscs uses 
    | T.RET _ => 0w567
    | T.STORE(ty,ea,data,mem) => 0w888 + w ty + hashRexp ea + hashRexp data 
    | T.FSTORE(fty,ea,data,mem) => 0w7890 + w fty + hashRexp ea + hashFexp data
    | T.BCC(a,lab) => 0w233 + hashCCexp a + hashLabel lab
    | T.IF(a,b,c) => 0w233 + hashCCexp a + hashStm b + hashStm c
    | T.ANNOTATION(stm, a) => hashStm stm 
    | T.PHI{preds,block} => w block 
    | T.SOURCE => 0w123 
    | T.SINK => 0w423 
    | T.REGION(stm,ctrl) => hashStm stm + hashCtrl ctrl
    | T.RTL{hash,...} => hash
    | T.SEQ ss => hashStms(ss, 0w23)
    | T.ASSIGN(ty,lhs,rhs) => w ty + hashRexp lhs + hashRexp rhs
    | _ => error "hashStm" 

   and hashStms([],h) = h
     | hashStms(s::ss,h) = hashStms(ss,hashStm s + h)

   and hashMlrisc(T.CCR ccexp) = hashCCexp ccexp
     | hashMlrisc(T.GPR rexp) = hashRexp rexp 
     | hashMlrisc(T.FPR fexp) = hashFexp fexp

   and hashMlriscs [] = 0w123
     | hashMlriscs(m::ms) = hashMlrisc m + hashMlriscs ms

   and hash2(ty,x,y) = w ty + hashRexp x + hashRexp y

   and hashRexp rexp =  
      case rexp of
      T.REG(ty, src) => w ty + wv src
    | T.LI i => I.hash i
    | T.LABEL l => hashLabel l
    | T.LABEXP le => hashRexp rexp
    | T.CONST c => Constant.hash c
    | T.NEG(ty, x) => w ty + hashRexp x + 0w24
    | T.ADD x => hash2 x + 0w234
    | T.SUB x => hash2 x + 0w456
    | T.MULS x => hash2 x + 0w2131
    | T.DIVS x => hash2 x + 0w156
    | T.QUOTS x => hash2 x + 0w1565
    | T.REMS x => hash2 x + 0w231
    | T.MULU x => hash2 x + 0w123
    | T.DIVU x => hash2 x + 0w1234
    | T.REMU x => hash2 x + 0w211
    | T.NEGT(ty, x) => w ty + hashRexp x + 0w1224
    | T.ADDT x => hash2 x + 0w1219
    | T.SUBT x => hash2 x + 0w999
    | T.MULT x => hash2 x + 0w7887
    | T.DIVT x => hash2 x + 0w88884
    | T.QUOTT x => hash2 x + 0w8884
    | T.REMT x => hash2 x + 0w99
    | T.ANDB x => hash2 x + 0w12312
    | T.ORB x => hash2 x + 0w558
    | T.XORB x => hash2 x + 0w234
    | T.EQVB x => hash2 x + 0w734
    | T.NOTB(ty, x) => w ty + hashRexp x  
    | T.SRA x => hash2 x + 0w874 
    | T.SRL x => hash2 x + 0w223
    | T.SLL x => hash2 x + 0w499
    | T.COND(ty,e,e1,e2) => w ty + hashCCexp e + hashRexp e1 + hashRexp e2
    | T.SX(ty, ty', rexp) => 0w232 + w ty + w ty' + hashRexp rexp
    | T.ZX(ty, ty', rexp) => 0w737 + w ty + w ty' + hashRexp rexp
    | T.CVTF2I(ty, round, ty', fexp) => 
        w ty + B.hashRoundingMode round + w ty' + hashFexp fexp
    | T.LOAD(ty, ea, mem) => w ty + hashRexp ea + 0w342
    | T.LET(stm, rexp) => hashStm stm + hashRexp rexp
    | T.PRED(e, ctrl) => hashRexp e + hashCtrl ctrl
    | T.MARK(e, _) => hashRexp e
    | T.REXT(ty, rext) => w ty + hashRext (hasher()) rext
    | T.??? => 0w485
    | T.OP(ty,oper,es) => hashRexps(es, w ty + hashOper oper)
    | T.ARG _ => 0w23
    | T.$(ty, k, e) => w ty + hashRexp e
    | T.PARAM n => w n
    | T.BITSLICE(ty, sl, e) => w ty + hashRexp e

  and hashOper(T.OPER{hash, ...}) = hash

  and hashRexps([],h) = h 
    | hashRexps(e::es,h) = hashRexps(es,hashRexp e + h)

  and hash2'(ty,x,y) = w ty + hashFexp x + hashFexp y

  and hashFexp fexp =  
      case fexp of
      T.FREG(fty, src) => w fty + wv src
    | T.FLOAD(fty, ea, mem) => w fty + hashRexp ea
    | T.FADD x => hash2' x + 0w123
    | T.FMUL x => hash2' x + 0w1234
    | T.FSUB x => hash2' x + 0w12345
    | T.FDIV x => hash2' x + 0w234
    | T.FCOPYSIGN x => hash2' x + 0w883
    | T.FCOND(fty,c,x,y) => w fty + hashCCexp c + hashFexp x + hashFexp y
    | T.FABS(fty, fexp) => w fty + hashFexp fexp + 0w2345
    | T.FNEG(fty, fexp) => w fty + hashFexp fexp + 0w23456
    | T.FSQRT(fty, fexp) => w fty + hashFexp fexp + 0w345
    | T.CVTI2F(fty, ty, rexp) => w fty + w ty + hashRexp rexp
    | T.CVTF2F(fty, fty', fexp) => w fty + hashFexp fexp + w fty' 
    | T.FMARK(e, _) => hashFexp e
    | T.FPRED(e, ctrl) => hashFexp e + hashCtrl ctrl
    | T.FEXT(fty, fext) => w fty + hashFext (hasher()) fext

  and hashFexps([],h) = h
    | hashFexps(e::es,h) = hashFexps(es,hashFexp e + h)

  and hashCCexp ccexp =
      case ccexp of
      T.CC(cc, src) => B.hashCond cc + wv src
    | T.FCC(fcc, src) => B.hashFcond fcc + wv src
    | T.CMP(ty, cond, x, y) => 
        w ty + B.hashCond cond + hashRexp x + hashRexp y
    | T.FCMP(fty, fcond, x, y) => 
        w fty + B.hashFcond fcond + hashFexp x + hashFexp y
    | T.NOT x => 0w2321 + hashCCexp x 
    | T.AND(x,y) => 0w2321 + hashCCexp x + hashCCexp y
    | T.OR(x,y) => 0w8721 + hashCCexp x + hashCCexp y
    | T.XOR(x,y) => 0w6178 + hashCCexp x + hashCCexp y
    | T.EQV(x,y) => 0w178 + hashCCexp x + hashCCexp y
    | T.TRUE => 0w0
    | T.FALSE => 0w1232
    | T.CCMARK(e, _) => hashCCexp e
    | T.CCEXT(ty,ccext) => w ty + hashCCext (hasher()) ccext

  and hashCCexps([],h) = h
    | hashCCexps(e::es,h) = hashCCexps(es,hashCCexp e + h)

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

  and eqRexp(T.REG(a,b),T.REG(c,d)) = a=c andalso eqCell(b,d)
    | eqRexp(T.LI a,T.LI b) = a=b 
    | eqRexp(T.LABEL a,T.LABEL b) = eqLabel(a,b)
    | eqRexp(T.LABEXP a,T.LABEXP b) = eqRexp(a,b)
    | eqRexp(T.CONST a,T.CONST b) = Constant.==(a,b)
    | eqRexp(T.NEG(t,x),T.NEG(t',x')) = t = t' andalso eqRexp(x,x')
    | eqRexp(T.ADD x,T.ADD y) = eq2(x,y)
    | eqRexp(T.SUB x,T.SUB y) = eq2(x,y)
    | eqRexp(T.MULS x,T.MULS y) = eq2(x,y)
    | eqRexp(T.DIVS x,T.DIVS y) = eq2(x,y)
    | eqRexp(T.QUOTS x,T.QUOTS y) = eq2(x,y)
    | eqRexp(T.REMS x,T.REMS y) = eq2(x,y)
    | eqRexp(T.MULU x,T.MULU y) = eq2(x,y)
    | eqRexp(T.DIVU x,T.DIVU y) = eq2(x,y)
    | eqRexp(T.REMU x,T.REMU y) = eq2(x,y)
    | eqRexp(T.NEGT(t,x),T.NEGT(t',x')) = t = t' andalso eqRexp(x,x')
    | eqRexp(T.ADDT x,T.ADDT y) = eq2(x,y)
    | eqRexp(T.SUBT x,T.SUBT y) = eq2(x,y)
    | eqRexp(T.MULT x,T.MULT y) = eq2(x,y)
    | eqRexp(T.DIVT x,T.DIVT y) = eq2(x,y)
    | eqRexp(T.QUOTT x,T.QUOTT y) = eq2(x,y)
    | eqRexp(T.REMT x,T.REMT y) = eq2(x,y)
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
   let fun rexp(T.LI i) = i
         | rexp(T.CONST c) = const c
         | rexp(T.LABEL l) = IntInf.fromInt(label l)
         | rexp(T.LABEXP e) = rexp e

         | rexp(T.NEG(sz,x)) = I.NEG(sz,rexp x)
         | rexp(T.ADD(sz,x,y)) = I.ADD(sz,rexp x,rexp y)
         | rexp(T.SUB(sz,x,y)) = I.SUB(sz,rexp x,rexp y)

         | rexp(T.MULS(sz,x,y)) = I.MULS(sz,rexp x,rexp y)
         | rexp(T.DIVS(sz,x,y)) = I.DIVS(sz,rexp x,rexp y)
         | rexp(T.QUOTS(sz,x,y)) = I.QUOTS(sz,rexp x,rexp y)
         | rexp(T.REMS(sz,x,y)) = I.REMS(sz,rexp x,rexp y)

         | rexp(T.MULU(sz,x,y)) = I.MULU(sz,rexp x,rexp y)
         | rexp(T.DIVU(sz,x,y)) = I.DIVU(sz,rexp x,rexp y)
         | rexp(T.REMU(sz,x,y)) = I.REMU(sz,rexp x,rexp y)

         | rexp(T.NEGT(sz,x)) = I.NEGT(sz,rexp x)
         | rexp(T.ADDT(sz,x,y)) = I.ADDT(sz,rexp x,rexp y)
         | rexp(T.SUBT(sz,x,y)) = I.SUBT(sz,rexp x,rexp y)
         | rexp(T.MULT(sz,x,y)) = I.MULT(sz,rexp x,rexp y)
         | rexp(T.DIVT(sz,x,y)) = I.DIVT(sz,rexp x,rexp y)
         | rexp(T.QUOTT(sz,x,y)) = I.QUOTT(sz,rexp x,rexp y)
         | rexp(T.REMT(sz,x,y)) = I.REMT(sz,rexp x,rexp y)

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
   val hash = hashRexp
 
   val resolveConstants = MLRiscControl.getFlag "asm-resolve-constants"
   val _ = resolveConstants := true

  (* This module should be parameterised, in order to generate
   * target label expressions for assembly code purposes.
   *)
(* operator precedences:
   (Note: these differ from C's precedences)
                2 MULT, DIV, LSHIFT, RSHIFT
                1 AND, OR
                0 PLUS, MINUS
*)

  fun parens (str, prec, op_prec) = 
      if prec > op_prec then "(" ^ str ^ ")" else str

  fun prInt i = if i < 0 then "-"^Int.toString(~i) else Int.toString i
  fun prIntInf i = if IntInf.sign i < 0 then "-"^IntInf.toString(IntInf.~ i) 
                   else IntInf.toString i

  fun toString le = toStr(le, 0) 

  and toStr(T.LABEL lab, _) = Label.fmt labelFmt lab 
    | toStr(T.LABEXP le, p) = toStr(le, p)
    | toStr(T.NEG(_, T.CONST c), _) =
        if !resolveConstants then prInt(~(Constant.valueOf c))
        else "(-" ^ Constant.toString c ^ ")"
    | toStr(T.NEG(_, T.LI i), _) = prIntInf(~i)
    | toStr(T.NEG(_, lexp), prec) = parens(toStr(lexp, 3), prec, 3)
    | toStr(T.CONST c, _) = 
        if !resolveConstants then prInt(Constant.valueOf c)
        else Constant.toString c
    | toStr(T.LI i, _) = prIntInf i
    | toStr(T.MULS(_,lexp1, lexp2), prec) =
	parens(toStr(lexp1, 2) ^ "*" ^ toStr(lexp2,2), prec, 2)
    | toStr(T.DIVS(_,lexp1, lexp2), prec) =
	parens(toStr(lexp1, 2) ^ "/" ^ toStr(lexp2,2), prec, 2)
    | toStr(T.SLL(_,lexp, cnt), prec) =
	parens(toStr(lexp,2) ^ "<<" ^ toStr(cnt,2), prec, 2)
    | toStr(T.SRL(_,lexp, cnt), prec) =
	parens(toStr(lexp,2) ^ ">>" ^ toStr(cnt,2), prec, 2)
    | toStr(T.ANDB(_,lexp, mask), prec) = 
        parens(toStr(lexp,1) ^ "&" ^ toStr(mask, 1), prec, 1)
    | toStr(T.ORB(_,lexp, mask), prec) = 
        parens(toStr(lexp, 1) ^ "|" ^ toStr(mask, 1), prec, 1)
    | toStr(T.ADD(_,lexp1, lexp2), prec) = 
        parens(toStr(lexp1, 0) ^ "+" ^ toStr(lexp2, 0), prec, 0)
    | toStr(T.SUB(_,lexp1, lexp2), prec) = 
        parens(toStr(lexp1, 0) ^ "-" ^ toStr(lexp2, 0), prec, 0)
    | toStr _ = error "toStr"

end
