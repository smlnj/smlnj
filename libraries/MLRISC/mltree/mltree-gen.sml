(* mltree-gen.sml
 *
 * COPYRIGHT (c) 2002 Bell Labs, Lucent Technologies
 *
 * This is a generic module for transforming MLTREE expressions:
 *   (1) expressions involving non-standard type widths are promoted when
 *       necessary.
 *   (2) operators that cannot be directly handled are expanded into 
 *       more complex instruction sequences when necessary.
 * 
 * -- Allen
 *)

functor MLTreeGen (
    structure T : MLTREE
    structure Cells : CELLS
    val intTy : T.ty (* size of integer word *)

     (* This is a list of possible data widths to promote to.
      * The list must be in increasing sizes.  
      * We'll try to promote to the next largest size.
      *)
    val naturalWidths : T.ty list  

     (*
      * Are integers of widths less than the size of integer word.
      * automatically sign extended, zero extended, or neither.
      * When in doubt, choose neither since it is conservative.
      *)
    datatype rep = SE | ZE | NEITHER
    val rep : rep

  ) : MLTREEGEN = struct

   structure T = T
   structure Size = MLTreeSize(structure T = T val intTy = intTy)
   structure C  = CellsBasis

   fun error msg = MLRiscErrorMsg.error("MLTreeGen",msg)
   fun unsupported what = error ("unsupported: " ^ what)

   val zeroT = T.LI 0
   fun LI i = T.LI(T.I.fromInt(intTy, i))

   fun condOf(T.CC(cc,_)) = cc
     | condOf(T.CMP(_,cc,_,_)) = cc
     | condOf(T.CCMARK(cc,_)) = condOf cc
     | condOf _ = error "condOf"

   fun fcondOf(T.FCC(fcc,_)) = fcc
     | fcondOf(T.FCMP(_,fcc,_,_)) = fcc
     | fcondOf(T.CCMARK(cc,_)) = fcondOf cc
     | fcondOf _ = error "fcondOf"

   val W = intTy

   (* To compute f.ty(a,b) 
    *
    * let r1 <- a << (intTy - ty)
    *     r2 <- b << (intTy - ty)
    *     r3 <- f(a,b) 
    * in  r3 ~>> (intTy - ty) end
    * 
    * Lal showed me this neat trick!
    *)
   fun arith(rightShift,f,ty,a,b) = 
       let val shift = LI(W-ty)
       in  rightShift(W,f(W,T.SLL(W,a,shift),T.SLL(W,b,shift)),shift)
       end

   fun promoteTy(ty) =
   let fun loop([]) = 
           unsupported("can't promote integer width "^Int.toString ty)
         | loop(t::ts) = if t > ty then t else loop ts
   in  loop(naturalWidths) end

   fun promotable rightShift (e, f, ty, a, b) =
       case naturalWidths of 
         [] => arith(rightShift,f,ty,a,b) 
       | _  => f(promoteTy(ty), a, b)

   fun isNatural w = let
       fun loop [] = false
	 | loop (h :: t) = h = w orelse w > h andalso loop t
   in
       loop naturalWidths
   end

   (* Implement division with round-to-negative-infinity in terms
    * of division with round-to-zero.
    * The logic is as follows:
    *    - if q > 0, then we are done since any rounding was
    *      at the same time TO_ZERO and TO_NEGINF
    *      (This is the fast path that does not require calculating the remainder.)
    *    - otherwise we calculate r and see if it is zero; if so, no adjustment
    *    - finally if r and b have the same sign (i.e., r XOR b >= 0)
    *      we still don't need adjustment
    *    - otherwise adjust
    *
    * Instruction selection for machines (e.g., x86) where the hardware returns both
    * q and r anyway should implement this logic directly.
    *)
   fun divinf (xdiv, ty, aexp, bexp) = let
       val a = Cells.newReg ()
       val b = Cells.newReg ()
       val q = Cells.newReg ()
       val r = Cells.newReg ()
       val zero = T.LI 0
       val one = T.LI 1
   in
       T.LET
	(T.SEQ
         [T.MV (ty, a, aexp),
	  T.MV (ty, b, bexp),
	  T.MV (ty, q, xdiv (T.DIV_TO_ZERO, ty, T.REG (ty, a), T.REG (ty, b))),
	  T.IF (T.CMP (ty, T.Basis.GT, T.REG (ty, q), zero),
		T.SEQ [],
		T.SEQ
		 [T.MV (ty, r, T.SUB (ty, T.REG (ty, a),
				          T.MULS (ty, T.REG (ty, b),
						      T.REG (ty, q)))),
		  T.IF (T.CMP (ty, T.Basis.EQ, T.REG (ty, r), zero),
			T.SEQ [],
			T.IF (T.CMP (ty, T.Basis.GE,
				     T.XORB (ty, T.REG (ty, b), T.REG (ty, r)),
				     zero),
			      T.SEQ [],
			      T.MV (ty, q, T.SUB (ty, T.REG (ty, q),
						      one))))])],
	 T.REG (ty, q))
   end

   (* Same for rem when rounding to negative infinity.
    * Since we have to return (and therefore calculate) the remainder anyway,
    * we can skip the q > 0 test because it will be caught by the samesign(r,b)
    * test.
    *
    * The odd case is when a = MININT and b = -1 in which case the DIVS op
    * will overflow and trap on some machines.  On others the result
    * will be bogus.  Should we fix that? *)
   fun reminf (ty, aexp, bexp) = let
       val a = Cells.newReg ()
       val b = Cells.newReg ()
       val q = Cells.newReg ()
       val r = Cells.newReg ()
       val zero = T.LI 0
   in
       T.LET
	(T.SEQ
	 [T.MV (ty, a, aexp),
	  T.MV (ty, b, bexp),
	  T.MV (ty, q, T.DIVS (T.DIV_TO_ZERO, ty, T.REG (ty, a),
			                          T.REG (ty, b))),
	  T.MV (ty, r, T.SUB (ty, T.REG (ty, a),
			          T.MULS (ty, T.REG (ty, q),
					      T.REG (ty, b)))),
	  T.IF (T.CMP (ty, T.Basis.EQ, T.REG (ty, r), zero),
		T.SEQ [],
		T.IF (T.CMP (ty, T.Basis.GE,
			         T.XORB (ty, T.REG (ty, b), T.REG (ty, r)),
				 zero),
		      T.SEQ [],
		      T.MV (ty, r, T.ADD (ty, T.REG (ty, r), T.REG (ty, b)))))],
	 T.REG (ty, r))
   end

   (* Same for rem when rounding to zero. *)
   fun remzero (xdiv, xmul, ty, aexp, bexp) = let
       val a = Cells.newReg ()
       val b = Cells.newReg ()
   in
       T.LET (T.SEQ [T.MV (ty, a, aexp),
		     T.MV (ty, b, bexp)],
	      T.SUB (ty, T.REG (ty, a),
		         xmul (ty, T.REG (ty, b),
			           xdiv (T.DIV_TO_ZERO, ty, T.REG (ty, a),
					                    T.REG (ty, b)))))
   end

   (*
    * Translate integer expressions of unknown types into the appropriate
    * term.
    *)

   fun DIVREMz d (ty, a, b) = d (T.DIV_TO_ZERO, ty, a, b)

   fun compileRexp(exp) = 
       case exp of
         T.CONST c => T.LABEXP exp

         (* non overflow trapping ops *)
       | T.NEG(ty,a)    => T.SUB(ty, zeroT, a)
       | T.ADD(ty,a,b)  => promotable T.SRA (exp,T.ADD,ty,a,b)
       | T.SUB(ty,a,b)  => promotable T.SRA (exp,T.SUB,ty,a,b)
       | T.MULS(ty,a,b) => promotable T.SRA (exp,T.MULS,ty,a,b)
       | T.DIVS(T.DIV_TO_ZERO,ty,a,b) =>
	                   promotable T.SRA (exp,DIVREMz T.DIVS,ty,a,b)
       | T.DIVS(T.DIV_TO_NEGINF,ty,a,b) => divinf (T.DIVS,ty,a,b)
       | T.REMS(T.DIV_TO_ZERO,ty,a,b) =>
	 if isNatural ty then remzero (T.DIVS,T.MULS,ty,a,b)
	 else promotable T.SRA (exp,DIVREMz T.REMS,ty,a,b)
       | T.REMS(T.DIV_TO_NEGINF,ty,a,b) => reminf (ty,a,b)
       | T.MULU(ty,a,b) => promotable T.SRL (exp,T.MULU,ty,a,b)
       | T.DIVU(ty,a,b) => promotable T.SRL (exp,T.DIVU,ty,a,b)
       | T.REMU(ty,a,b) =>
	 if isNatural ty then
	     remzero (fn (_,ty,a,b) => T.DIVU (ty,a,b),T.MULU,ty,a,b)
	 else promotable T.SRL (exp,T.REMU,ty,a,b)

         (* for overflow trapping ops; we have to do the simulation *)
       | T.NEGT(ty,a)   => T.SUBT(ty,zeroT,a)
       | T.ADDT(ty,a,b) => arith (T.SRA,T.ADDT,ty,a,b)
       | T.SUBT(ty,a,b) => arith (T.SRA,T.SUBT,ty,a,b)
       | T.MULT(ty,a,b) => arith (T.SRA,T.MULT,ty,a,b)
       | T.DIVT(T.DIV_TO_ZERO,ty,a,b) => arith (T.SRA,DIVREMz T.DIVT,ty,a,b)
       | T.DIVT(T.DIV_TO_NEGINF,ty,a,b) => divinf (T.DIVT,ty,a,b)

         (* conditional evaluation rules *)
(*** XXX: Seems wrong.
       | T.COND(ty,T.CC(cond,r),x,y) =>
           T.COND(ty,T.CMP(ty,cond,T.REG(ty,r),zeroT),x,y)
***)
       | T.COND(ty,T.CCMARK(cc,a),x,y) => T.MARK(T.COND(ty,cc,x,y),a)
(*** XXX: TODO
       | T.COND(ty,T.CMP(t,cc,e1,e2),x as (T.LI 0 | T.LI32 0w0),y) => 
           T.COND(ty,T.CMP(t,T.Basis.negateCond cc,e1,e2),y,T.LI 0)
           (* we'll let others strength reduce the multiply *)
***)
       | T.COND(ty,cc as T.FCMP _, yes, no) => let
	  val tmp = Cells.newReg()
          in 
	    T.LET(
	      T.SEQ[T.MV(ty, tmp, no), T.IF(cc, T.MV(ty, tmp, yes), T.SEQ [])],
              T.REG(ty,tmp))
          end
(*** XXX: TODO
       | T.COND(ty,cc,e1,(T.LI 0 | T.LI32 0w0)) => 
           T.MULU(ty,T.COND(ty,cc,T.LI 1,T.LI 0),e1)
       | T.COND(ty,cc,T.LI m,T.LI n) =>
           T.ADD(ty,T.MULU(ty,T.COND(ty,cc,T.LI 1,T.LI 0),T.LI(m-n)),T.LI n)
***)

       | T.COND(ty,cc,e1,e2) => 
           T.ADD(ty,T.MULU(ty,T.COND(ty,cc,T.LI 1,zeroT),T.SUB(ty,e1,e2)),e2)

       (* ones-complement.
        * WARNING: we are assuming two's complement architectures here.
        * Are there any architectures in use nowadays that doesn't use 
        * two's complement for integer arithmetic?
        *)
       | T.NOTB(ty,e) => T.XORB(ty,e,T.LI ~1)

       (* 
        * Default ways of converting integers to integers
        *)
       | T.SX(ty,fromTy,e) => 
         if fromTy = ty then e
         else if rep = SE andalso fromTy < ty andalso 
              fromTy >= hd naturalWidths then e 
         else
             let val shift = T.LI(T.I.fromInt(intTy, W - fromTy))
             in  T.SRA(W,T.SLL(W,e,shift),shift) 
             end 
       | T.ZX(ty,fromTy,e) => 
         if fromTy <= ty then e else 
            (case ty of (* ty < fromTy *)
                8  => T.ANDB(ty,e,T.LI 0xff) 
              | 16 => T.ANDB(ty,e,T.LI 0xffff)
              | 32 => T.ANDB(ty,e,T.LI 0xffffffff)
              | 64 => e
              | _  => unsupported("unknown expression")
            )

       (* 
        * Converting floating point to integers.
        * The following rule handles the case when ty is not
        * one of the naturally supported widths on the machine.
        *)
       | T.CVTF2I(ty,round,fty,e) => 
         let val ty' = promoteTy(ty)
         in  T.SX(ty,ty',T.CVTF2I(ty',round,fty,e))
         end

         (* Promote to higher width and zero high bits *)
       | T.SLL(ty, data, shift) => 
         let val ty' = promoteTy(ty)
         in  T.ZX(ty, ty', T.SLL(ty', data, shift)) end

       | exp => unsupported("unknown expression")

   fun compileFexp fexp = unsupported("unknown expression")

   fun mark(s,[]) = s
     | mark(s,a::an) = mark(T.ANNOTATION(s,a),an)

   fun compileStm (T.SEQ s) = s
     | compileStm (T.IF(cond,T.JMP(T.LABEL L,_),T.SEQ [])) = 
           [T.BCC(cond,L)]
     | compileStm (T.IF(cond,yes,no)) = 
       let val L1 = Label.anon()
           val L2 = Label.anon()
       in  [T.BCC(cond,L1),
            no,
            T.JMP(T.LABEL L2,[]),
            T.DEFINE L1,
            yes,
            T.DEFINE L2
           ]
       end
     | compileStm stm = error "compileStm"

   (*
    * This function translations conditional expressions into a 
    * branch sequence.  
    * Note: we'll actually take advantage of the fact that 
    * e1 and e2 are allowed to be eagerly evaluated. 
    *)
   fun compileCond{exp=(ty,ccexp,e1,e2),rd,an} =
   let val L1 = Label.anon()
   in  [T.MV(ty,rd,e1),
        mark(T.BCC(ccexp,L1),an),
        T.MV(ty,rd,e2),
        T.DEFINE L1
       ]
   end
   fun compileFcond{exp=(fty,ccexp,e1,e2),fd,an} =
   let val L1 = Label.anon()
   in  [T.FMV(fty,fd,e1),
        mark(T.BCC(ccexp,L1),an),
        T.FMV(fty,fd,e2),
        T.DEFINE L1
       ]
   end
 
end
