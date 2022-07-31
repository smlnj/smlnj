(* 
 *  Common operations on MLTREE
 *
 * -- Allen 
 *)
functor MLTreeUtils
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

   (* Pretty printing extensions *)
   val showSext  : T.printer -> T.sext -> string
   val showRext  : T.printer -> T.ty * T.rext -> string
   val showFext  : T.printer -> T.fty * T.fext -> string
   val showCCext : T.printer -> T.ty * T.ccext -> string
  ) : MLTREE_UTILS =
struct

   structure T          = T
   structure I          = T.I 
   structure Constant   = T.Constant
   structure Region     = T.Region
   structure B          = T.Basis
   structure C          = CellsBasis
   structure W          = Word
   

   val w = W.fromInt
   val i2s = Int.toString
   val toLower = String.map Char.toLower

   fun error msg = MLRiscErrorMsg.error("MLTreeUtils",msg)
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
    | T.FLOW_TO(stm, _) => hashStm stm
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

   and hashm T.DIV_TO_ZERO = 0w158
     | hashm T.DIV_TO_NEGINF = 0w159

   and hash3(m,ty,x,y) = hashm m + w ty + hashRexp x + hashRexp y

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
    | T.DIVS x => hash3 x + 0w156
    | T.REMS x => hash3 x + 0w231
    | T.MULU x => hash2 x + 0w123
    | T.DIVU x => hash2 x + 0w1234
    | T.REMU x => hash2 x + 0w211
    | T.NEGT(ty, x) => w ty + hashRexp x + 0w1224
    | T.ADDT x => hash2 x + 0w1219
    | T.SUBT x => hash2 x + 0w999
    | T.MULT x => hash2 x + 0w7887
    | T.DIVT x => hash3 x + 0w88884
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

   (*
    * Equality
    *)

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
    | eqStm(T.FLOW_TO(x,a), T.FLOW_TO(y,b)) =
         eqStm(x,y) andalso eqLabels(a,b)
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
      m=n andalso a=d andalso eqRexp(b,e) andalso eqRexp(c,f)

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

  (*
   * Pretty printing
   *)
  fun show {def,use,regionDef,regionUse} =
  let fun ty t = "."^i2s t
      fun fty 32 = ".s"
        | fty 64 = ".d"
        | fty 128 = ".q"
        | fty t   = ty t

      fun reg(t,v) = C.toString v^ty t
      fun freg(t,v) = C.toString v^fty t
      fun ccreg v = C.toString v   
      fun ctrlreg v = C.toString v

      fun srcReg(t,v) = reg(t,v)
      fun srcFreg(t,v) = freg(t,v)
      fun srcCCreg v = ccreg v
      fun srcCtrlreg v = ctrlreg v

      fun dstReg(t,v) = reg(t,v)
      fun dstFreg(t,v) = freg(t,v)
      fun dstCCreg v = ccreg v
      fun dstCtrlreg v = ctrlreg v

      fun srcParam(i) = def i handle _ => "<"^i2s i^">"
      fun dstParam(i) = use i handle _ => "<"^i2s i^">"

      fun listify f =
      let fun g(t,[]) = ""
            | g(t,[r]) = f(t,r)
            | g(t,r::rs) = f(t,r)^","^g(t,rs)
      in  g end

      fun listify' f = (String.concatWith ",") o (List.map f)

      val srcRegs = listify srcReg 
      val dstRegs = listify dstReg 
      val srcFregs = listify srcFreg 
      val dstFregs = listify dstFreg 
      val srcCCregs = listify' srcCCreg 
      val dstCCregs = listify' dstCCreg 
      val srcCtrlregs = listify' srcCtrlreg 
      val dstCtrlregs = listify' dstCtrlreg 
      fun usectrl cr  = " ["^srcCtrlreg cr^"]"
      fun usectrls [] = ""
        | usectrls cr = " ["^srcCtrlregs cr^"]"
      fun defctrl cr  = ""^dstCtrlreg cr^" <- "
      fun defctrls [] = ""
        | defctrls cr = ""^dstCtrlregs cr^" <- "

      fun copy(t,dst,src) = dstRegs(t, dst)^" := "^srcRegs(t, src)
      fun fcopy(t,dst,src) = dstFregs(t, dst)^" := "^srcFregs(t, src)

      fun shower() = {stm=stm, rexp=rexp, fexp=fexp, ccexp=ccexp, 
                      dstReg=dstReg, srcReg=srcReg}
          (* pretty print a statement *)
      and stm(T.MV(t,dst,e)) = dstReg(t,dst)^" := "^rexp e
        | stm(T.CCMV(dst,e)) = dstCCreg dst^" := "^ccexp e
        | stm(T.FMV(fty,dst,e)) = dstFreg(fty,dst)^" := "^fexp e
        | stm(T.COPY(ty,dst,src)) = copy(ty,dst,src)
        | stm(T.FCOPY(fty,dst,src)) = fcopy(fty,dst,src)
        | stm(T.JMP(ea,labels)) = "jmp "^rexp ea
        | stm(T.BCC(a,lab)) = 
             "bcc "^ccexp a^" "^Label.toString lab
        | stm(T.CALL{funct,targets,defs,uses,region,pops}) = 
              "call "^rexp funct
        | stm(T.FLOW_TO(s, targets)) =
              stm s^" ["^listify' Label.toString targets^"]"
        | stm(T.RET(flow)) = "ret"
        | stm(T.IF(a,b,T.SEQ [])) = "if "^ccexp a^" then "^stm b
        | stm(T.IF(a,b,c)) = "if "^ccexp a^" then "^stm b^" else "^stm c
        | stm(T.STORE(ty,ea,e,mem)) = store(ty,"",ea,mem,e)
        | stm(T.FSTORE(fty,ea,e,mem)) = fstore(fty,"",ea,mem,e)
        | stm(T.REGION(s,cr)) = stm s^usectrl cr
        | stm(T.SEQ []) = "skip"
        | stm(T.SEQ s) = stms(";\n",s)
	| stm(T.DEFINE lab) = Label.toString lab ^ ":"
        | stm(T.ANNOTATION(s, a)) = stm s 
        | stm(T.EXT x) = showSext (shower()) x
	| stm(T.LIVE exps) = "live: " ^ mlriscs exps
	| stm(T.KILL exps) = "kill: " ^ mlriscs exps
        | stm(T.PHI{preds, block}) = "phi["^i2s block^"]"
        | stm(T.ASSIGN(ty,lhs,T.???)) = "define "^rexp lhs
        | stm(T.ASSIGN(ty,T.???,rhs)) = "use "^rexp rhs
        | stm(T.ASSIGN(ty,x,rhs)) = lhs x^" := "^rexp rhs
        | stm(T.SOURCE) = "source"
        | stm(T.SINK) = "sink"
        | stm(T.RTL{e,...}) = stm e

      and stms(sep,[]) = ""
        | stms(sep,[s]) = stm s
        | stms(sep,s::ss) = stm s^sep^stms(sep,ss)

      and lhs(T.PARAM i) = dstParam i
        | lhs(T.$(ty,k,T.PARAM i)) = dstParam i
        | lhs(e) = rexp e

          (* pretty print an expression  *)
      and rexp(T.REG(ty, src)) = srcReg(ty,src)
        | rexp(T.LI i) = IntInf.toString i
        | rexp(T.LABEL l) = Label.toString l
        | rexp(T.CONST c) = Constant.toString c
        | rexp(T.LABEXP le) = rexp le
        | rexp(T.NEG x) = unary("~",x)
        | rexp(T.ADD x) = binary("+",x)
        | rexp(T.SUB x) = binary("-",x)
        | rexp(T.MULS x) = two("muls",x)
        | rexp(T.DIVS x) = three("divs",x)
        | rexp(T.REMS x) = three("rems",x)
        | rexp(T.MULU x) = two("mulu",x)
        | rexp(T.DIVU x) = two("divu",x)
        | rexp(T.REMU x) = two("remu",x)
        | rexp(T.NEGT x) = one("negt",x)
        | rexp(T.ADDT x) = two("addt",x)
        | rexp(T.SUBT x) = two("subt",x)
        | rexp(T.MULT x) = two("mult",x)
        | rexp(T.DIVT x) = three("divt",x)
        | rexp(T.ANDB x) = binary("&",x)
        | rexp(T.ORB x)  = binary("|",x)
        | rexp(T.XORB x) = binary("^",x)
        | rexp(T.EQVB x) = binary("eqvb",x)
        | rexp(T.NOTB x) = unary("!",x)
        | rexp(T.SRA x) = binary("~>>",x)
        | rexp(T.SRL x) = binary(">>",x)
        | rexp(T.SLL x) = binary("<<",x)
        | rexp(T.COND(t,cc,e1,e2)) = 
             "cond"^ty t^"("^ccexp cc^","^rexp e1^","^rexp e2^")"
        | rexp(T.SX(t, t', e)) = "sx"^ty t^ty t'^" "^rexp e
        | rexp(T.ZX(t, t', e)) = "zx"^ty t^ty t'^" "^rexp e
        | rexp(T.CVTF2I(t, round, t', e)) = 
             "cvtf2i"^ty t^toLower(B.roundingModeToString round)^
             fty t'^" "^fexp e
        | rexp(T.LOAD(ty, ea, mem)) = load(ty,"",ea,mem)
        | rexp(T.LET(s, e)) = stm s^";"^rexp e
        | rexp(T.PRED(e, cr)) = rexp e^usectrl cr
        | rexp(T.MARK(e, _)) = rexp e
        | rexp(T.REXT e) = showRext (shower()) e
        | rexp(T.???) = "???"
        | rexp(T.OP(t,opc,es)) = oper opc^ty t^" "^rexps es
        | rexp(T.ARG(t,ref(T.REP kind),name)) = 
             name^":"^kind^(if t = 0 then "" else ty t)
        | rexp(T.PARAM n) = srcParam n
        | rexp(T.$(ty,k,e)) =    
             "$"^C.cellkindToNickname k^"["^rexp e^"]"
        | rexp(T.BITSLICE(ty,sl,e)) = rexp e^" at "^slices sl

      and oper(T.OPER{name,...}) = name 

      and parenRexp
            (e as (T.REG _ | T.LI _ | T.$ _ | T.ARG _)) = 
              rexp e
        | parenRexp e = "("^rexp e^")"

      and slices sc = listify' (fn (from,to) => i2s from^".."^i2s to) sc

          (* pretty print a real expression  *)
      and fexp(T.FREG f) = srcFreg f
        | fexp(T.FLOAD(fty, ea, mem)) = fload(fty,"",ea,mem)
        | fexp(T.FADD x) = two'("fadd",x)
        | fexp(T.FMUL x) = two'("fmul",x)
        | fexp(T.FSUB x) = two'("fsub",x)
        | fexp(T.FDIV x) = two'("fdiv",x)
        | fexp(T.FCOPYSIGN x) = two'("fcopysign",x)
        | fexp(T.FABS x) = one'("fabs",x)
        | fexp(T.FNEG x) = one'("fneg",x)
        | fexp(T.FSQRT x) = one'("fsqrt",x)
        | fexp(T.FCOND(t,cc,e1,e2)) = 
             "fcond"^fty t^ccexp cc^"("^fexp e1^","^fexp e2^")"
        | fexp(T.CVTI2F(t, t', e)) = "cvti2f"^ty t'^" "^rexp e
        | fexp(T.CVTF2F(t, t', e)) = "cvtf2f"^fty t^fty t'^" "^fexp e
        | fexp(T.FPRED(e, cr)) = fexp e^usectrl cr
        | fexp(T.FMARK(e, _)) = fexp e
        | fexp(T.FEXT e) = showFext (shower()) e

      and ccexp(T.CC(cc,r)) = srcCCreg r^toLower(B.condToString cc)
        | ccexp(T.FCC(fcc,r)) = srcCCreg r^toLower(B.fcondToString fcc)
        | ccexp(T.CMP(t,T.SETCC,x,y)) = "setcc"^ty t^pair(x,y)
        | ccexp(T.CMP(t,cc,x,y)) = 
            "cmp"^toLower(B.condToString cc)^ty t^pair(x,y)
        | ccexp(T.FCMP(t,T.SETFCC,x,y)) = "setfcc"^ty t^pair'(x,y)
        | ccexp(T.FCMP(t,fcc,x,y)) = 
                "fcmp"^toLower(B.fcondToString fcc)^fty t^pair'(x,y)
        | ccexp(T.NOT x) = "not "^ccexp x
        | ccexp(T.AND(x,y)) = two''(" and ",x,y)
        | ccexp(T.OR(x,y)) = two''(" or ",x,y)
        | ccexp(T.XOR(x,y)) = two''(" xor ",x,y)
        | ccexp(T.EQV(x,y)) = two''(" eqv ",x,y)
        | ccexp(T.CCMARK(e, _)) = ccexp e
        | ccexp(T.TRUE) = "true"
        | ccexp(T.FALSE) = "false"
        | ccexp(T.CCEXT(e)) = showCCext (shower()) e

      and mlrisc(T.GPR e) = rexp e
        | mlrisc(T.FPR e) = fexp e
        | mlrisc(T.CCR e) = ccexp e

      and mlriscs l = listify' mlrisc l

      (* Auxiliary functions *)
      and one(opcode,(t,x)) = opcode^ty t^"("^rexp x^")"
      and two(opcode,(t,x,y)) = opcode^ty t^pair(x,y)
      and three(opcode,(m,t,x,y)) = opcode^dmr m^ty t^pair(x,y)
      and dmr T.DIV_TO_ZERO = "{0}"
	| dmr T.DIV_TO_NEGINF = "{-inf}"
      and binary(opcode,(t,x,y)) = parenRexp x^" "^opcode^ty t^" "^parenRexp y
      and unary(opcode,(t,x)) = opcode^ty t^" "^parenRexp x
      and pair(x,y) = "("^rexp x^","^rexp y^")"
      and one'(opcode,(t,x)) = opcode^fty t^"("^fexp x^")"
      and two'(opcode,(t,x,y)) = opcode^fty t^pair'(x,y)
      and two''(c,x,y) = "("^ccexp x^ c ^ ccexp y^")"
      and pair'(x,y) = "("^fexp x^","^fexp y^")"
      and rexps es = "("^foldr (fn (e,"") => rexp e
                                 | (e,x) => rexp e^","^x) "" es^")"
      and fexps es = "("^foldr (fn (e,"") => fexp e
                                 | (e,x) => fexp e^","^x) "" es^")"
      and ccexps es = "("^foldr (fn (e,"") => ccexp e
                                  | (e,x) => ccexp e^","^x) "" es^")"
      and store(t,u,ea,m,e) = memdef(t,u,ea,m)^" := "^rexp e
      and fstore(t,u,ea,m,e) = fmemdef(t,u,ea,m)^" := "^fexp e
      and ccstore(u,ea,m,e) = ccmemdef(u,ea,m)^" := "^ccexp e
      and load(t,u,ea,m) = memuse(t,u,ea,m)
      and fload(t,u,ea,m) = fmemuse(t,u,ea,m)
      and ccload(u,ea,m) = ccmemuse(u,ea,m)
      and addr(u,ea,m,show) = 
          let val r = show m handle _ => Region.toString m
              val r = if r = "" then r else ":"^r
          in  u^"["^rexp ea^r^"]" end
      and mem(t,u,ea,m,show) = "mem"^ty t^addr(u,ea,m,show)
      and fmem(t,u,ea,m,show) = "mem"^fty t^addr(u,ea,m,show)
      and ccmem(u,ea,m,show) = "mem"^addr(u,ea,m,show)
      and memdef(t,u,ea,m) = mem(t,u,ea,m,regionDef)
      and fmemdef(t,u,ea,m) = fmem(t,u,ea,m,regionDef)
      and ccmemdef(u,ea,m) = ccmem(u,ea,m,regionDef)
      and memuse(t,u,ea,m) = mem(t,u,ea,m,regionUse)
      and fmemuse(t,u,ea,m) = fmem(t,u,ea,m,regionUse)
      and ccmemuse(u,ea,m) = ccmem(u,ea,m,regionUse)
   in shower()
   end

   exception Nothing

   fun dummy _ = raise Nothing
   val dummy = {def=dummy, use=dummy, regionDef=dummy, regionUse=dummy}

   fun stmToString s   = #stm(show dummy) s
   fun rexpToString s  = #rexp(show dummy) s
   fun fexpToString s  = #fexp(show dummy) s
   fun ccexpToString s = #ccexp(show dummy) s

end 
