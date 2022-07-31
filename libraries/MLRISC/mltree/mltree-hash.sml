(* mltree-hash.sml
 *
 * COPYRIGHT (c) 2002 Bell Labs, Lucent Technologies.
 *)

functor MLTreeHash
  (structure T : MLTREE
   (* Hashing extensions *)
   val hashSext  : T.hasher -> T.sext -> word
   val hashRext  : T.hasher -> T.rext -> word
   val hashFext  : T.hasher -> T.fext -> word
   val hashCCext : T.hasher -> T.ccext -> word
  ) : MLTREE_HASH = 
struct
  structure T	     = T
  structure I	     = T.I
  structure Constant = T.Constant
  structure B        = T.Basis
  structure C        = CellsBasis
  structure W        = Word

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

  and hashm T.DIV_TO_ZERO = 0w158
    | hashm T.DIV_TO_NEGINF = 0w159

  and hash3(m,ty,x,y) = hashm m + w ty + hashRexp x + hashRexp y

  and hashRexp rexp =  
      case rexp
       of  T.REG(ty, src) => w ty + wv src
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


  val hash = hashRexp
end
