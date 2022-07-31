(*
 * This takes a bunch of RTL and build a database that can be reused.
 *)
structure BuildRTL : BUILD_RTL =
struct
   structure RTL = MLTreeRTL
   structure T   = RTL.T

   type ty = int

   fun newOper name = ref{name=name,hash=0w0,attribs=0w0}

   fun wordConst ty w = T.LI32(w)
   fun intConst ty i = wordConst ty (Word32.fromInt i)

   fun error msg = MLRiscErrorMsg.error("BuildRTL",msg)

   fun fetch ty loc = T.REXT(ty,RTL.FETCH loc)

   val wildCard = newOper "?"

   fun op:= ty (x,y) = T.EXT(RTL.ASSIGN(x,y))
   val noregion = T.LI 0
   fun $ (k,ty) addr = RTL.CELL(k,ty,addr,noregion)
   fun $$ (k,ty) (addr,region) = RTL.CELL(k,ty,addr,region)

   fun aggb (t1,t2) cell = RTL.AGG(t2,RTL.BIG_ENDIAN,cell)
   fun aggl (t1,t2) cell = RTL.AGG(t2,RTL.LITTLE_ENDIAN,cell)
   fun idaggr t cell     = RTL.AGG(t,RTL.LITTLE_ENDIAN,cell)
   fun copy ty (dst,src) = T.COPY(ty,[],[])
   val dummyTy = 32

   fun ! (t,x,y) =  T.REXT(t,RTL.ARG(x,y))

   (* Integer operators *)
   fun unary f ty x = f(ty,x) 
   fun binary f ty (x, y) = f(ty,x,y)
   fun ternary f ty (x, y, z) = f(ty, x, y, z)

   fun operand ty opn = opn 
   fun label ty label = label
   fun immed ty imm   = imm

   datatype kind = GP | FP | CC

   fun newOp name = 
   let val oper = newOper name 
   in  fn xs => T.REXT(32,RTL.OP(oper,xs)) : RTL.exp 
   end

   val newCond = newOp

   fun sx (t1,t2) e = T.CVTI2I(t2,T.SIGN_EXTEND,t1,e)
   fun zx (t1,t2) e = T.CVTI2I(t2,T.ZERO_EXTEND,t1,e)
   fun ? ty = T.REXT(ty,RTL.OP(wildCard,[]))
   fun forall t e = T.REXT(t,RTL.FORALL e)

   fun bitslice t2 ranges e =
       let val t1 = foldr (fn ((a,b),l) => b-a+1+l) 0 ranges
           val r =  map (fn (a,b) => {from=T.LI a,to=T.LI b}) ranges
       in  T.REXT(t1,RTL.SLICE(r,t2,e)) end

   val not   = T.NOT
   val False = T.FALSE
   val True  = T.TRUE

   val op +  = binary T.ADD
   val op -  = binary T.SUB
   val muls  = binary T.MULS
   val mulu  = binary T.MULU
   val divs  = ternary T.DIVS
   val divu  = binary T.DIVU
   val rems  = ternary T.REMS
   val remu  = binary T.REMU
   fun ~ ty x = (op - ty) (intConst ty 0,x)

   val andb  = binary T.ANDB
   val orb   = binary T.ORB
   val xorb  = binary T.XORB
   val notb  = unary  T.NOTB
   val <<    = binary T.SLL
   val >>    = binary T.SRL
   val ~>>   = binary T.SRA
   fun eqvb ty (x,y) = notb ty (xorb ty (x,y))

   (* Trapping operators *)
   val addt  = binary T.ADDT
   val subt  = binary T.SUBT
   val mult  = binary T.MULT
   val divt  = binary T.DIVT
   val remt  = binary T.REMT

   fun cond ty (x,y,z) = T.COND(ty, x, y, z)

   (* Integer comparisons *)
   fun cmp cond ty (x,y) = T.CMP(ty,cond,x,y) 

   val ==    = cmp T.EQ
   val op <> = cmp T.NE
   val op >  = cmp T.GT
   val op <  = cmp T.LT
   val op <= = cmp T.LE
   val op >= = cmp T.GE
   val ltu   = cmp T.LTU
   val leu   = cmp T.LEU
   val gtu   = cmp T.GTU
   val geu   = cmp T.GEU

   (* Floating point operators *)
   fun funary f =
   let val oper = newOper f 
   in  fn ty => fn x => T.REXT(ty,RTL.OP(oper,[x])) 
   end
   fun fbinary f =
   let val oper = newOper f 
   in  fn ty => fn (x,y) => T.REXT(ty,RTL.OP(oper,[x, y])) 
   end

   val fadd  = fbinary "FADD"
   val fsub  = fbinary "FSUB"
   val fmul  = fbinary "FMUL"
   val fdiv  = fbinary "FDIV"
   val fabs  = funary  "FABS"
   val fneg  = funary  "FNEG"
   val fsqrt = funary  "FSQRT"

   (* Floating point comparisons *)
   fun fcmp fcond =
   let val name = T.Basis.fcondToString fcond
       val oper = newOper name    
   in  fn ty => fn (x,y) =>
          T.CMP(ty,T.NE,T.REXT(ty,RTL.OP(oper,[x,y])),T.LI 0) 
   end

   val |?|     = fcmp T.?
   val |!<=>|  = fcmp T.!<=>
   val |==|    = fcmp T.==
   val |?=|    = fcmp T.?=
   val |!<>|   = fcmp T.!<>
   val |!?>=|  = fcmp T.!?>=
   val |<|     = fcmp T.<
   val |?<|    = fcmp T.?<
   val |!>=|   = fcmp T.!>=
   val |!?>|   = fcmp T.!?>
   val |<=|    = fcmp T.<=
   val |?<=|   = fcmp T.?<=
   val |!>|    = fcmp T.!>
   val |!?<=|  = fcmp T.!?<=
   val |>|     = fcmp T.>
   val |?>|    = fcmp T.?>
   val |!<=|   = fcmp T.!<=
   val |!?<|   = fcmp T.!?<
   val |>=|    = fcmp T.>=
   val |?>=|   = fcmp T.?>=
   val |!<|    = fcmp T.!<
   val |!?=|   = fcmp T.!?=
   val |<>|    = fcmp T.<>
   val |!=|    = fcmp T.!=
   val |!?|    = fcmp T.!?
   val |<=>|   = fcmp T.<=>
   val |?<>|   = fcmp T.?<>

   (* Action combinators *)
   fun ||(a,b) = T.EXT(RTL.PAR(a,b))
   val Nop   = T.SEQ []
   fun Jmp  ty e = T.JMP([],e,[])
   fun Call ty e = T.CALL{funct=e,targets=[],defs=[],uses=[],
                          cdefs=[],cuses=[],region= ~1}
   val Ret   = T.RET([],[])
   fun If(x,y,z) = T.IF([],x,y,z)

   fun map ty = List.map
end
