(*
 * Build MLTree-based RTLs 
 *) 

functor RTLBuild(RTL : MLTREE_RTL) : RTL_BUILD =
struct 
   structure RTL = RTL
   structure T = RTL.T
   structure I = T.I
   
   type effect  = RTL.rtl
   type exp     = T.rexp
   type ty      = T.ty
   type bool    = T.ccexp
   type region  = T.rexp
   type cond    = T.cond
   type fcond   = T.fcond
   type div_rounding_mode = T.div_rounding_mode

   fun error msg = MLRiscErrorMsg.error("RTLBuild",msg)

   val hashCounter = ref 0w23
   fun newHash() = !hashCounter before hashCounter := !hashCounter + 0w23499
   fun newOper name = {name=name, hash=newHash(), attribs=ref 0w0}

   val newOpList = ref [] : T.Basis.misc_op list ref
   fun getNewOps() = !newOpList
   fun clearNewOps() = newOpList := []

   fun newOp name = 
   let val oper = newOper name
       val _    = newOpList := oper :: !newOpList;
       val oper = T.OPER oper
   in  fn es => T.OP(32, oper, es) (* XXX *)
   end

   fun op:= ty (lhs,rhs) = T.ASSIGN(ty,lhs,rhs)

   fun $ (k,ty) e = T.$(ty,k,e)

   fun Mem (k,ty) (addr,mem) = T.$(ty,k,addr)

   fun ??? ty = T.??? 
   fun Arg (ty,kind,name) = T.ARG(ty,ref(T.REP kind),name)
   fun BitSlice ty slice e = T.BITSLICE(ty,slice,e)

   fun operand ty exp = exp
   fun immed ty exp = exp
   fun label ty exp = exp

   (* integer *)
   fun intConst ty i = T.LI(I.fromInt(ty, i))
   fun wordConst ty w = T.LI(I.fromWord32(ty, w))

   fun ternaryOp oper ty (x, y, z) = oper(x, ty, y, z)
   fun binOp oper ty (x, y) = oper(ty,x,y)
   fun unaryOp oper ty x = oper(ty,x)

   fun sx (from,to) e = T.SX(to, from, e)
   fun zx (from,to) e = T.ZX(to, from, e)

   val op~   = unaryOp T.NEG
   val op+   = binOp T.ADD
   val op-   = binOp T.SUB
   val muls  = binOp T.MULS
   val divs  = ternaryOp T.DIVS
   val rems  = ternaryOp T.REMS
   val mulu  = binOp T.MULU
   val divu  = binOp T.DIVU
   val remu  = binOp T.REMU

   val negt  = unaryOp T.NEGT
   val addt  = binOp T.ADDT
   val subt  = binOp T.SUBT
   val mult  = binOp T.MULT
   val divt  = ternaryOp T.DIVT

   val notb  = unaryOp T.NOTB
   val andb  = binOp T.ANDB
   val orb   = binOp T.ORB
   val xorb  = binOp T.XORB
   val eqvb  = binOp T.EQVB
   val ~>>   = binOp T.SRA
   val >>    = binOp T.SRL
   val <<    = binOp T.SLL

   val True  = T.TRUE
   val False = T.FALSE
   val Not   = T.NOT
   val And   = T.AND
   val Or    = T.OR
   val Xor   = T.XOR
   fun cmp cc ty (x,y) = T.CMP(ty,cc,x,y)
   fun Cond ty (cond,x,y) = T.COND(ty,cond,x,y)

   val op== = cmp T.EQ
   val op<> = cmp T.NE
   val op>= = cmp T.GE
   val op>  = cmp T.GT
   val op<= = cmp T.LE
   val op<  = cmp T.LT
   val geu  = cmp T.GEU
   val gtu  = cmp T.GTU
   val leu  = cmp T.LEU
   val ltu  = cmp T.LTU
   val setcc  = cmp T.SETCC
   fun getcc ty (e,cc) = T.CMP(ty,cc,e,T.???)
   (* floating point *)
   fun i2f(ty,x) = T.CVTI2F(ty,ty,x)
   fun f2i(ty,x) = T.CVTF2I(ty,T.TO_ZERO,ty,x)
   fun fbinOp oper ty (x,y) = f2i(ty,oper(ty,i2f(ty,x),i2f(ty,y)))
   fun funaryOp oper ty (x) = f2i(ty,oper(ty,i2f(ty,x)))
   fun fcmp fcc ty (x,y) = T.FCMP(ty,fcc,i2f(ty,x),i2f(ty, y))
   fun getfcc ty (e,cc) = T.FCMP(ty,cc,i2f(ty,e),i2f(ty,T.???))

   val fadd  = fbinOp T.FADD
   val fsub  = fbinOp T.FSUB
   val fmul  = fbinOp T.FMUL
   val fdiv  = fbinOp T.FDIV
   val fcopysign = fbinOp T.FCOPYSIGN
   val fneg  = funaryOp T.FNEG
   val fabs  = funaryOp T.FABS
   val fsqrt = funaryOp T.FSQRT

   val |?|     = fcmp T.?
   val |==|    = fcmp T.==
   val |?=|    = fcmp T.?=
   val |<|     = fcmp T.<
   val |?<|    = fcmp T.?<
   val |<=|    = fcmp T.<=
   val |?<=|   = fcmp T.?<=
   val |>|     = fcmp T.>
   val |?>|    = fcmp T.?>
   val |>=|    = fcmp T.>=
   val |?>=|   = fcmp T.?>=
   val |<>|    = fcmp T.<>
   val |<=>|   = fcmp T.<=>
   val |?<>|   = fcmp T.?<>
   val setfcc  = fcmp T.SETFCC

   (* effects *)
   val Nop = T.SEQ []
   fun Jmp ty addr = T.JMP(addr,[]) 
   fun Call ty addr = T.CALL{funct=addr, targets=[],
                             defs=[], uses=[], 
                             region=T.Region.memory,
			     pops=0}
   val Ret = T.RET([])

   fun If(T.TRUE, yes, no) = yes
     | If(T.FALSE, yes, no) = no
     | If(T.CMP(ty,cc,x,y),T.SEQ [],no) = 
         T.IF(T.CMP(ty,T.Basis.negateCond cc,x,y), no, Nop)
     | If(a,b,c) = T.IF(a,b,c)

   fun Par(T.SEQ[],y)         = y
     | Par(x,T.SEQ[])         = x
     | Par(T.SEQ xs,T.SEQ ys) = T.SEQ(xs@ys)
     | Par(T.SEQ xs,y)        = T.SEQ(xs@[y])
     | Par(x,T.SEQ ys)        = T.SEQ(x::ys)
     | Par(x,y)               = T.SEQ[x,y] 

   val map = fn _ => List.map

end
