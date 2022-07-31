(*
 * Generate multiplication/division by a constant.
 * This module is mainly used for architectures without fast integer multiply.
 *
 * -- Allen
 *)
functor MLTreeMult
  (structure I : INSTRUCTIONS
   structure T : MLTREE

   structure CB : CELLS_BASIS = CellsBasis
   val intTy : int (* width of integer type *)

   type argi = {r:CB.cell, i:int, d:CB.cell}
   type arg  = {r1:CB.cell, r2:CB.cell, d:CB.cell} 

     (* these are always non-overflow trapping *)
   val mov   : {r:CB.cell, d:CB.cell} -> I.instruction
   val add   : arg -> I.instruction
   val slli  : argi -> I.instruction list
   val srli  : argi -> I.instruction list
   val srai  : argi -> I.instruction list
  )
  (val trapping : bool (* trap on overflow? *)
   val multCost : int ref  (* cost of multiplication *)

       (* basic ops; these have to implemented by the architecture *)

       (* if trapping = true, then the following MUST trap on overflow *)
   val addv   : arg  -> I.instruction list
   val subv   : arg  -> I.instruction list

      (* some architectures, like the PA-RISC and the Alpha,
       * have these types of special ops 
       * if trapping = true, then the following MUST also trap on overflow
       *)
   val sh1addv  : (arg -> I.instruction list) option (* a*2 + b *)
   val sh2addv  : (arg -> I.instruction list) option (* a*4 + b *)  
   val sh3addv  : (arg -> I.instruction list) option (* a*8 + b *)
  )  
  (val signed   : bool (* signed? *)
  ) : MLTREE_MULT_DIV =
struct
   structure T = T 
   structure I = I 
   structure C = I.C 
   structure W = Word
   structure A = Array

   type arg       = argi
 
   infix << >> ~>> || &&
   val itow   = W.fromInt
   val wtoi   = W.toIntX
   val op <<  = W.<<
   val op >>  = W.>>
   val op ~>> = W.~>>
   val op ||  = W.orb
   val op &&  = W.andb

   exception TooComplex

   fun error msg = MLRiscErrorMsg.error("MLTreeMult",msg)

   val zeroR   = C.zeroReg CB.GP 
   val shiftri = if signed then srai else srli

   fun isPowerOf2 w = ((w - 0w1) && w) = 0w0

   fun log2 n =  (* n must be > 0!!! *)
       let fun loop(0w1,pow) = pow 
             | loop(w,pow) = loop(w >> 0w1,pow+1)
       in loop(n,0) end

   fun zeroBits(w,lowZeroBits) = 
       if (w && 0w1) = 0w1 then (w,lowZeroBits)
       else zeroBits(w >> 0w1,lowZeroBits+0w1)

     (* Non overflow trapping version of multiply: 
      * We can use add, shadd, shift, sub to perform the multiplication
      *)
   fun multiplyNonTrap{r,i,d} =
   let fun mult(r,w,maxCost,d) = 
       if maxCost <= 0 then raise TooComplex
       else if isPowerOf2 w then slli{r=r,i=log2 w,d=d}
       else
         (case (w,sh1addv,sh2addv,sh3addv) of
               (* some base cases *)
            (0w3,SOME f,_,_) => f{r1=r,r2=r,d=d}
          | (0w5,_,SOME f,_) => f{r1=r,r2=r,d=d}
          | (0w9,_,_,SOME f) => f{r1=r,r2=r,d=d}
          | _ => (* recurse on the bit patterns of w *)
            let val tmp = C.newReg()
            in  if (w && 0w1) = 0w1 then  (* low order bit is 1 *)
                   if (w && 0w2) = 0w2 then (* second bit is 1 *)
                      mult(r,w+0w1,maxCost-1,tmp) @
                      subv{r1=tmp,r2=r,d=d}
                   else (* second bit is 0 *)
                      mult(r,w-0w1,maxCost-1,tmp) @
                      addv{r1=tmp,r2=r,d=d}
                else (* low order bit is 0 *)
                   let val (w,lowZeroBits) = zeroBits(w,0w0)
                   in  mult(r,w,maxCost-1,tmp) @
                       slli{r=tmp,i=wtoi lowZeroBits,d=d}
                   end
            end
         ) 
   in  if i <= 0 then raise TooComplex
       else if i = 1 then [mov{r=r,d=d}]
       else mult(r,itow i,!multCost,d)
   end

   (* The semantics of roundToZero{r,i,d} is:
    *   if r >= 0 then d <- r
    *   else d <- r + i
    *)
   fun roundToZero stm {ty,r,i,d} =
       let val reg = T.REG(ty,r)
       in  stm(T.MV(ty,d,
                    T.COND(ty,T.CMP(ty,T.GE,reg, T.LI 0),reg,
                              T.ADD(ty,reg,T.LI(T.I.fromInt(intTy,i))))))
       end
 

   (* 
    * Simulate rounding towards zero for signed division 
    *)  
   fun roundDiv{mode=T.TO_NEGINF,r,...} = ([],r) (* no rounding necessary *)
     | roundDiv{mode=T.TO_ZERO,stm,r,i} =
          if signed then
          let val d = C.newReg()
          in  if i = 2 then (* special case for division by 2 *)
                 let val tmpR = C.newReg()
                 in  (srli{r=r,i=intTy - 1,d=tmpR}@[add{r1=r,r2=tmpR,d=d}], d)
                 end
              else
                     (* invoke rounding callback *)
                 let val () = roundToZero stm {ty=intTy,r=r,i=i-1,d=d}
                 in ([],d) end
          end
          else ([],r) (* no rounding for unsigned division *)
     | roundDiv{mode,...} = 
          error("Integer rounding mode "^
                T.Basis.roundingModeToString mode^" is not supported")

   fun divideNonTrap{mode,stm}{r,i,d} = 
       if i > 0 andalso isPowerOf2(itow i)
       then 
       let val (code,r) = roundDiv{mode=mode,stm=stm,r=r,i=i}
       in  code@shiftri{r=r,i=log2(itow i),d=d} end (* won't overflow *)
       else raise TooComplex

     (* Overflow trapping version of multiply: 
      *   We can use only add and shadd to perform the multiplication,
      *   because of overflow trapping problem.
      *)
   fun multiplyTrap{r,i,d} =
   let fun mult(r,w,maxCost,d) =
       if maxCost <= 0 then raise TooComplex
       else 
          (case (w,sh1addv,sh2addv,sh3addv,zeroR) of
               (* some simple base cases *)
            (0w2,_,_,_,_)           => addv{r1=r,r2=r,d=d}
          | (0w3,SOME f,_,_,_)      => f{r1=r,r2=r,d=d}
          | (0w4,_,SOME f,_,SOME z) => f{r1=r,r2=z,d=d}
          | (0w5,_,SOME f,_,_)      => f{r1=r,r2=r,d=d}
          | (0w8,_,_,SOME f,SOME z) => f{r1=r,r2=z,d=d}
          | (0w9,_,_,SOME f,_)      => f{r1=r,r2=r,d=d}
          | _ => (* recurse on the bit patterns of w *)
            let val tmp = C.newReg()
            in  if (w && 0w1) = 0w1 then
                    mult(r,w - 0w1,maxCost-1,tmp) @ addv{r1=tmp,r2=r,d=d}
                else 
                   case (w && 0w7, sh3addv, zeroR) of
                     (0w0, SOME f, SOME z) => (* times 8 *)
                      mult(r,w >> 0w3,maxCost-1,tmp) @ f{r1=tmp,r2=z,d=d}
                   | _ =>
                   case (w && 0w3, sh2addv, zeroR) of
                     (0w0, SOME f, SOME z) => (* times 4 *)
                      mult(r,w >> 0w2,maxCost-1,tmp) @ f{r1=tmp,r2=z,d=d}
                   | _ =>
                      mult(r,w >> 0w1,maxCost-1,tmp) @ addv{r1=tmp,r2=tmp,d=d}
            end
         ) 
   in  if i <= 0 then raise TooComplex
       else if i = 1 then [mov{r=r,d=d}]
       else mult(r,itow i,!multCost,d) 
   end

   fun divideTrap{mode,stm}{r,i,d} =
       if i > 0 andalso isPowerOf2(itow i)
       then
       let val (code,r) = roundDiv{mode=mode,stm=stm,r=r,i=i}
       in  code@shiftri{r=r,i=log2(itow i),d=d} end (* won't overflow *)
       else raise TooComplex

   fun multiply x = if trapping then multiplyTrap x else multiplyNonTrap x
   fun divide   x = if trapping then divideTrap x else divideNonTrap x

end
