(*
 * This signature describes the interface to a gc type system. 
 * This encapsulates everything dealing with GC safety analysis
 * into one single signature.
 *)
functor GCTypeSystem
  (structure GC    : GC_TYPE
   structure RTL   : MLTREE_RTL
   val isRecoverable : GC.gctype -> bool
  ) : GC_TYPE_SYSTEM =
struct
   structure GC    = GC
   structure RTL   = RTL
   structure T     = RTL.T

   fun error msg = MLRiscErrorMsg.error("GCTypeSystem",msg)

   fun typeOf lookup = 
   let fun T(T.REG(t,r))      = lookup r
         | T(T.LI i)          = GC.CONST i
         | T(T.CONST c)       = GC.INT
         | T(T.LABEL l)       = GC.INT
         | T(T.NEG(t,a))      = unaryArith(t,a)
         | T(T.ADD(t,a,b))    = GC.ADD(t,T a,T b)
         | T(T.SUB(t,a,b))    = GC.SUB(t,T a,T b)
         | T(T.MULS(t,a,b))   = binaryArith(t,a,b)
         | T(T.DIVS(t,a,b))   = binaryArith(t,a,b)
         | T(T.QUOTS(t,a,b))  = binaryArith(t,a,b)
         | T(T.REMS(t,a,b))   = binaryArith(t,a,b)
         | T(T.MULU(t,a,b))   = binaryArith(t,a,b)
         | T(T.DIVU(t,a,b))   = binaryArith(t,a,b)
         | T(T.REMU(t,a,b))   = binaryArith(t,a,b)
         | T(T.NEGT(t,a))     = unaryArith(t,a)
         | T(T.ADDT(t,a,b))   = binaryArith(t,a,b)
         | T(T.SUBT(t,a,b))   = binaryArith(t,a,b)
         | T(T.MULT(t,a,b))   = binaryArith(t,a,b)
         | T(T.DIVT(t,a,b))   = binaryArith(t,a,b)
         | T(T.REMT(t,a,b))   = binaryArith(t,a,b)
         | T(T.QUOTT(t,a,b))  = binaryArith(t,a,b)
         | T(T.ANDB(t,a,b))   = binaryArith(t,a,b)
         | T(T.ORB(t,a,b))    = binaryArith(t,a,b)
         | T(T.XORB(t,a,b))   = binaryArith(t,a,b)
         | T(T.SLL(t,a,b))    = binaryArith(t,a,b)
         | T(T.SRA(t,a,b))    = binaryArith(t,a,b)
         | T(T.SRL(t,a,b))    = binaryArith(t,a,b)
         | T(T.NOTB(t,a))     = unaryArith(t,a)
         | T(T.LOAD(t,ea,_))  = GC.TOP
         | T(T.COND(t,a,b,c)) = GC.TOP 
         | T(T.SX _)          = GC.TOP 
         | T(T.ZX _)          = GC.TOP 
         | T(T.PRED(e, _))    = T e
         (*| T(T.REXT(t,RTL.OP(misc_op,es))) = GC.INT
         | T(T.REXT(t,RTL.FETCH(RTL.AGG(_,_,RTL.CELL(k,ty,e,_))))) = GC.TOP*)
         | T(e) = error("typeOf: "^ RTL.expToString e)
 
       and binaryArith(t,a,b) = 
           let val ta = T(a)
               val tb = T(b)
           in  GC.join(ta,tb) end

       and unaryArith(t,a) = T(a)
   in  T 
   end

   (*
    * Compute the effect    
    *)
   fun effectOf{lookup, update} {action, dst, src, effect} =
   let fun err() = error("effectOf: "^ RTL.rtlToString action)

       (* Note the parallel copy semantics! *)
       fun copy(dst, src, e) =
       let fun upd([], [], e) = e
             | upd(d::dst,t::tys,e) = upd(dst, tys, update(d, t, e))
             | upd _ = error "copy"
       in  upd(dst, map lookup src, e) end
 
       fun E(T.COPY _,e)         = copy(dst,src,e)
         | E(T.RTL{e=s,...},e)   = E(s,e)
         | E(T.REGION(s,_),e)    = E(s,e)
         | E(T.SEQ [],e)         = e
         | E(T.JMP _, e)         = e
         | E(T.CALL _,e)         = e
         | E(T.RET _,e)          = e
         | E(T.MV(t,x,exp), e)   = update(x, typeOf lookup exp, e)
         | E(T.IF(x,y,z), e)     = e
         | E(T.STORE _, e)       = e
         (*| E(T.EXT(RTL.ASSIGN(loc,exp)),e) = 
            let val t = typeOf lookup exp
            in  case loc of
                  RTL.AGG(_,_,RTL.CELL("FP",_,T.REG(_,x),_)) => 
                      update(x, GC.REAL64, e)
                | RTL.AGG(_,_,RTL.CELL(_,_,T.REG(_,x),_)) => 
                      update(x, GC.TOP, e)
                | RTL.AGG(_,_,_) => e
            end
         | E(T.EXT(RTL.PAR(a,b)), e) = E(b,E(a,e))*)
         | E(rtl, e) = err()
   in  E(action, effect) end

   val isRecoverable = isRecoverable
end
