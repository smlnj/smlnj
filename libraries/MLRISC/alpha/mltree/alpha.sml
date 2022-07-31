(* 
 * This is a revamping of the Alpha32 instruction selection module
 * using the new MLTREE and instruction representation.   I've dropped
 * the suffix 32 since we now support 64 bit datatypes.
 * 
 * -- Allen
 * 
 * Notes: places with optimizations are marked ***OPT**n*
 *)


functor Alpha
   (structure AlphaInstr : ALPHAINSTR 
    structure PseudoInstrs : ALPHA_PSEUDO_INSTR
    			where I = AlphaInstr
    structure ExtensionComp : MLTREE_EXTENSION_COMP
    			where I = AlphaInstr
			  and T = AlphaInstr.T

      (* Cost of multiplication in cycles *)
    val multCost : int ref

      (* Should we just use the native multiply by a constant? *)
    val useMultByConst : bool ref

      (* Should we use SUD flags for floating point and generate DEFFREG? 
       * This should be set to false for C-like clients but true for SML/NJ.
       *)
    val SMLNJfloatingPoint : bool 

      (* Should we use generate special byte/word load instructions
       * like LDBU, LDWU, STB, STW.
       *)
    val byteWordLoadStores : bool ref
   ) : MLTREECOMP =
struct

  structure I   = AlphaInstr
  structure C   = I.C
  structure T   = I.T
  structure TS  = ExtensionComp.TS
  structure R   = T.Region
  structure W32 = Word32
  structure P   = PseudoInstrs
  structure A   = MLRiscAnnotations
  structure CB  = CellsBasis
  structure CFG = ExtensionComp.CFG

 (*********************************************************

       Trap Shadows, Floating Exceptions, and Denormalized
        Numbers on the DEC Alpha

                Andrew W. Appel and Lal George
                  Nov 28, 1995

  See section 4.7.5.1 of the Alpha Architecture Reference Manual.

  The Alpha has imprecise exceptions, meaning that if a floating
  point instruction raises an IEEE exception, the exception may
  not interrupt the processor until several successive instructions have
  completed.  ML, on the other hand, may want a "precise" model
  of floating point exceptions.

  Furthermore, the Alpha hardware does not support denormalized numbers
  (for "gradual underflow").  Instead, underflow always rounds to zero.
  However, each floating operation (add, mult, etc.) has a trapping
  variant that will raise an exception (imprecisely, of course) on
  underflow; in that case, the instruction will produce a zero result
  AND an exception will occur.  In fact, there are several variants
  of each instruction; three variants of MULT are:

  MULT  s1,s2,d       truncate denormalized result to zero; no exception
  MULT/U  s1,s2,d     truncate denormalized result to zero; raise UNDERFLOW
  MULT/SU  s1,s2,d    software completion, producing denormalized result

  The hardware treats the MULT/U and MULT/SU instructions identically,
  truncating a denormalized result to zero and raising the UNDERFLOW
  exception.  But the operating system, on an UNDERFLOW exception,
  examines the faulting instruction to see if it's an /SU form, and if so,
  recalculates s1*s2, puts the right answer in d, and continues,
  all without invoking the user's signal handler.

  Because most machines compute with denormalized numbers in hardware,
  to maximize portability of SML programs, we use the MULT/SU form.
  (and ADD/SU, SUB/SU, etc.)  But to use this form successfully,
  certain rules have to be followed.  Basically, d cannot be the same
  register as s1 or s2, because the opsys needs to be able to 
  recalculate the operation using the original contents of s1 and s2,
  and the MULT/SU instruction will overwrite d even if it traps.

  More generally, we may want to have a sequence of floating-point
  instructions.  The rules for such a sequence are:

  1. The sequence should end with a TRAPB (trap barrier) instruction.
     (This could be relaxed somewhat, but certainly a TRAPB would
      be a good idea sometime before the next branch instruction or
      update of an ML reference variable, or any other ML side effect.)
  2. No instruction in the sequence should destroy any operand of itself
     or of any previous instruction in the sequence.
  3. No two instructions in the sequence should write the same destination
     register.

  We can achieve these conditions by the following trick in the
  Alpha code generator.  Each instruction in the sequence will write
  to a different temporary; this is guaranteed by the translation from
  ML-RISC.  At the beginning of the sequence, we will put a special
  pseudo-instruction (we call it DEFFREG) that "defines" the destination
  register of the arithmetic instruction.  If there are K arithmetic
  instructions in the sequence, then we'll insert K DEFFREG instructions
  all at the beginning of the sequence.
  Then, each arithop will not only "define" its destination temporary
  but will "use" it as well.  When all these instructions are fed to
  the liveness analyzer, the resulting interference graph will then
  have inteference edges satisfying conditions 2 and 3 above.

  Of course, DEFFREG doesn't actually generate any code.  In our model
  of the Alpha, every instruction generates exactly 4 bytes of code
  except the "span-dependent" ones.  Therefore, we'll specify DEFFREG
  as a span-dependent instruction whose minimum and maximum sizes are zero.

  At the moment, we do not group arithmetic operations into sequences;
  that is, each arithop will be preceded by a single DEFFREG and
  followed by a TRAPB.  To avoid the cost of all those TRAPB's, we
  should improve this when we have time.  Warning:  Don't put more 
  than 31 instructions in the sequence, because they're all required
  to write to different destination registers!  

  What about multiple traps?  For example, suppose a sequence of
  instructions produces an Overflow and  a Divide-by-Zero exception?
  ML would like to know only about the earliest trap, but the hardware
  will report BOTH traps to the operating system.  However, as long
  as the rules above are followed (and the software-completion versions
  of the arithmetic instructions are used), the operating system will
  have enough information to know which instruction produced the
  trap.  It is very probable that the operating system will report ONLY
  the earlier trap to the user process, but I'm not sure.

  For a hint about what the operating system is doing in its own
  trap-handler (with software completion), see section 6.3.2 of
  "OpenVMS Alpha Software" (Part II of the Alpha Architecture
  Manual).  This stuff should apply to Unix (OSF1) as well as VMS.







		-------------------o*o----------------------
			   LIVE/KILL instructions
				 Nov 28, 2001
				  Lal George

  The mechanism described above is now obsolete. We no longer use
  the DEFFREG instruction but the zero length LIVE instruction. 
  Therefore the code that gets generated is something like;

	f1 := f2 + f3
        trap
	LIVE f1, f2, f3

  The live ranges for f1, f2, and f3 are extended by the LIVE
  instruction, and are live simultaneously and therefore cannot
  be allocated to the same register. 

  Multiple floating point instructions should be surrounded
  by parallel copies. That is to say, if we have:

        f1 := f2 + f3
	trapb
        LIVE f1, f2, f3

	f4 := f1 * f2
	trapb
	LIVE f1, f2, f4

  Then the sequence above should be transformed to:

        [f2', f3'] := [f1, f2] ; parallel copy
        f1' := f2' + f3'
	f4' := f1' * f2'
        trapb
	LIVE f1', f2', f3', f4'
        [f4] := [f4']  ; copy assuming f4 is the only value live.

  The parallel copies are to ensure that the primed variables will 
  not spill, and there should never be more than K reigsters in the LIVE
  instruction (K is the number of registers on the machine).
  ****************************************************************)

  fun error msg = MLRiscErrorMsg.error("Alpha",msg) 

  type instrStream = (I.instruction, C.cellset, CFG.cfg) TS.stream
  type mltreeStream = (T.stm, T.mlrisc list, CFG.cfg) TS.stream

  (*
   * This module is used to simulate operations of non-standard widths.
   *)
  structure Gen = MLTreeGen(structure T = T
			    structure Cells = C
                            val intTy = 64
                            val naturalWidths = [32,64]
                            datatype rep = SE | ZE | NEITHER
                            val rep = SE
                           )

  val zeroR   = C.r31
  val zeroOpn = I.REGop zeroR
  fun LI i    = T.LI(T.I.fromInt(32, i))
  fun toInt i = T.I.toInt(32, i)
  val EQ      = op =

  (*
   * Specialize the modules for multiplication/division 
   * by constant optimizations.
   *)
  functor Multiply32 = MLTreeMult
    (structure I = I
     structure T = T
     structure CB = CellsBasis

     val intTy = 32
   
     type arg  = {r1:CB.cell,r2:CB.cell,d:CB.cell}
     type argi = {r:CB.cell,i:int,d:CB.cell}

     fun mov{r,d}    = I.COPY{k=CB.GP, sz=intTy, dst=[d],src=[r],tmp=NONE}
     fun add{r1,r2,d} = I.operate{oper=I.ADDL,ra=r1,rb=I.REGop r2,rc=d}
     (*
      * How to left shift by a constant (32bits)
      *)
     fun slli{r,i=1,d} = [I.operate{oper=I.ADDL,ra=r,rb=I.REGop r,rc=d}]
       | slli{r,i=2,d} = [I.operate{oper=I.S4ADDL,ra=r,rb=zeroOpn,rc=d}]
       | slli{r,i=3,d} = [I.operate{oper=I.S8ADDL,ra=r,rb=zeroOpn,rc=d}]
       | slli{r,i,d}   = 
          let val tmp = C.newReg()
          in  [I.operate{oper=I.SLL,ra=r,rb=I.IMMop i,rc=tmp},
               I.operate{oper=I.ADDL,ra=tmp,rb=zeroOpn,rc=d}]
          end

     (* 
      * How to right shift (unsigned) by a constant (32bits)
      *)
     fun srli{r,i,d} =
         let val tmp = C.newReg()
         in  [I.operate{oper=I.ZAP,ra=r,rb=I.IMMop 0xf0,rc=tmp},
              I.operate{oper=I.SRL,ra=tmp,rb=I.IMMop i,rc=d}]
         end

     (* 
      * How to right shift (signed) by a constant (32bits)
      *)
     fun srai{r,i,d} = 
         let val tmp = C.newReg()
         in  [I.operate{oper=I.ADDL,ra=r,rb=zeroOpn,rc=tmp},
              I.operate{oper=I.SRA,ra=tmp,rb=I.IMMop i,rc=d}]
         end 
    )

  functor Multiply64 = MLTreeMult
    (structure I = I
     structure T = T
     structure CB = CellsBasis
   
     val intTy = 64

     type arg  = {r1:CB.cell, r2:CB.cell, d:CB.cell}
     type argi = {r:CB.cell, i:int, d:CB.cell}

     fun mov{r,d}    = I.COPY{k=CB.GP, sz=intTy, dst=[d],src=[r],tmp=NONE}
     fun add{r1,r2,d}= I.operate{oper=I.ADDQ,ra=r1,rb=I.REGop r2,rc=d}
     fun slli{r,i,d} = [I.operate{oper=I.SLL,ra=r,rb=I.IMMop i,rc=d}]
     fun srli{r,i,d} = [I.operate{oper=I.SRL,ra=r,rb=I.IMMop i,rc=d}]
     fun srai{r,i,d} = [I.operate{oper=I.SRA,ra=r,rb=I.IMMop i,rc=d}]
    )

  (* signed, trapping version of multiply and divide *)
  structure Mult32 = Multiply32
    (val trapping = true
     val multCost = multCost
     fun addv{r1,r2,d} = [I.operatev{oper=I.ADDLV,ra=r1,rb=I.REGop r2,rc=d}]
     fun subv{r1,r2,d} = [I.operatev{oper=I.SUBLV,ra=r1,rb=I.REGop r2,rc=d}]
     val sh1addv = NONE
     val sh2addv = NONE
     val sh3addv = NONE
    )
    (val signed = true)

  (* non-trapping version of multiply and divide *)
  functor Mul32 = Multiply32
    (val trapping = false
     val multCost = multCost
     fun addv{r1,r2,d} = [I.operate{oper=I.ADDL,ra=r1,rb=I.REGop r2,rc=d}]
     fun subv{r1,r2,d} = [I.operate{oper=I.SUBL,ra=r1,rb=I.REGop r2,rc=d}]
     val sh1addv = NONE
     val sh2addv = SOME(fn {r1,r2,d} => 
                    [I.operate{oper=I.S4ADDL,ra=r1,rb=I.REGop r2,rc=d}])
     val sh3addv = SOME(fn {r1,r2,d} => 
                    [I.operate{oper=I.S8ADDL,ra=r1,rb=I.REGop r2,rc=d}])
    )
  structure Mulu32 = Mul32(val signed = false)
  structure Muls32 = Mul32(val signed = true)

  (* signed, trapping version of multiply and divide *)
  structure Mult64 = Multiply64
    (val trapping = true
     val multCost = multCost
     fun addv{r1,r2,d} = [I.operatev{oper=I.ADDQV,ra=r1,rb=I.REGop r2,rc=d}]
     fun subv{r1,r2,d} = [I.operatev{oper=I.SUBQV,ra=r1,rb=I.REGop r2,rc=d}]
     val sh1addv = NONE
     val sh2addv = NONE
     val sh3addv = NONE
    )
    (val signed = true)

  (* unsigned, non-trapping version of multiply and divide *)
  functor Mul64 = Multiply64
    (val trapping = false
     val multCost = multCost
     fun addv{r1,r2,d} = [I.operate{oper=I.ADDQ,ra=r1,rb=I.REGop r2,rc=d}]
     fun subv{r1,r2,d} = [I.operate{oper=I.SUBQ,ra=r1,rb=I.REGop r2,rc=d}]
     val sh1addv = NONE
     val sh2addv = SOME(fn {r1,r2,d} => 
                    [I.operate{oper=I.S4ADDQ,ra=r1,rb=I.REGop r2,rc=d}])
     val sh3addv = SOME(fn {r1,r2,d} => 
                    [I.operate{oper=I.S8ADDQ,ra=r1,rb=I.REGop r2,rc=d}])
    )
  structure Mulu64 = Mul64(val signed = false)
  structure Muls64 = Mul64(val signed = true)

  (* 
   * The main stuff
   *)

  datatype times4or8 = TIMES1 | TIMES4 | TIMES8 
  datatype zeroOne   = ZERO | ONE | OTHER
  datatype commutative = COMMUTE | NOCOMMUTE

  val zeroFR = C.f31 
  val zeroEA = I.Direct zeroR
  val zeroT  = T.LI 0
  val trapb = [I.trapb]
  val zeroImm = I.IMMop 0

  fun selectInstructions
        (instrStream as
         TS.S.STREAM{emit=emitInstruction,beginCluster,endCluster,getAnnotations,
                     defineLabel,entryLabel,pseudoOp,annotation,
                     exitBlock,comment,...}) =
  let

      infix || && << >> ~>>

      val op ||  = W32.orb
      val op &&  = W32.andb
      val op <<  = W32.<<
      val op >>  = W32.>>
      val op ~>> = W32.~>>

      val itow = Word.fromInt
      val wtoi = Word.toIntX

      val emit = emitInstruction o I.INSTR

      val newReg = C.newReg
      val newFreg = C.newFreg

      (* Choose the appropriate rounding mode to generate.
       * This stuff is used to support the alpha32x SML/NJ backend.
       *
       *
       * Floating point rounding mode.
       * When this is set to true, we use the /SU rounding mode
       * (chopped towards zero) for floating point arithmetic.
       * This flag is only used to support the old alpha32x backend.
       * 
       * Otherwise, we use /SUD.  This is the default for SML/NJ.
       *
       *)
      val (ADDTX,SUBTX,MULTX,DIVTX) =
           (I.ADDTSUD,I.SUBTSUD,I.MULTSUD,I.DIVTSUD)
      val (ADDSX,SUBSX,MULSX,DIVSX) =
            (I.ADDSSUD,I.SUBSSUD,I.MULSSUD,I.DIVSSUD)
  
      fun annotate(i, an) = List.foldl (fn (a, i) => I.ANNOTATION{i=i,a=a}) i an
      fun mark'(i, an) = emitInstruction(annotate(i,an))
      fun mark(i,an) = emitInstruction(annotate(I.INSTR i,an))

      (* Fit within 16 bits? *)
      fun literal16 n = ~32768 <= n andalso n < 32768
      fun literal16w w = 
          let val hi = W32.~>>(w,0wx16)
          in  hi = 0w0 orelse (W32.notb hi) = 0w0 end

      (* emit an LDA instruction; return the register that holds the value *)
      fun lda(base,I.IMMop 0) = base
        | lda(base,offset) = 
            let val r = newReg()
            in  emit(I.LDA{r=r, b=base, d=offset}); r end

      (* emit load immed *)
      fun loadImmed(n, base, rd, an) = let
	val n = T.I.toInt32(32, n)
      in
	if n = 0 then move(base, rd, an)
	else if ~32768 <= n andalso n < 32768 then
	  mark(I.LDA{r=rd, b=base, d=I.IMMop(Int32.toInt n)}, an)
        else loadImmed32(n, base, rd, an)
      end

      (* loadImmed32 is used to load int32 and word32 constants.
       * In either case we sign extend the 32-bit value. This is compatible 
       * with LDL which sign extends a 32-bit valued memory location.
       *)
      (* TODO: 
       *  Should handle 64 bits if immediate is not in the 32 bit range.
       *)
      and loadImmed32(n, base, rd, an) = let
	fun immed(0, high) =
	      mark(I.LDAH{r=rd, b=base, d=I.IMMop(high)}, an)
	  | immed(low, 0) = 
	      mark(I.LDA{r=rd, b=base, d=I.IMMop(low)}, an)
	  | immed(low, high) = 
	      (emit(I.LDA{r=rd, b=base, d=I.IMMop(low)});
	       mark(I.LDAH{r=rd, b=rd, d=I.IMMop(high)}, an)
	       )
	val w = Word32.fromLargeInt(Int32.toLarge n)
	val low = W32.andb(w, 0wxffff)
	val high = W32.~>>(w, 0w16)
      in
	if W32.<(low, 0wx8000) then 
	  immed(W32.toInt low, W32.toIntX high)
	else let
	    val low = W32.toIntX(W32.-(low, 0wx10000))
	    val high = W32.toIntX(W32.+(high, 0w1))
          in
	    if high <> 0x8000 then immed(low, high) 
	    else let (* transition of high from pos to neg *)
	        val tmpR1 = newReg() 
		val tmpR2 = newReg() 
		val tmpR3=newReg()
              in
		(* you just gotta do, what you gotta do! *)
		emit(I.LDA{r=tmpR3, b=base, d=I.IMMop(low)});
		emit(I.OPERATE{oper=I.ADDQ, ra=zeroR, rb=I.IMMop 1, rc=tmpR1});
		emit(I.OPERATE{oper=I.SLL, ra=tmpR1, rb=I.IMMop 31, rc=tmpR2});
		mark(I.OPERATE{oper=I.ADDQ, ra=tmpR2, rb=I.REGop tmpR3, rc=rd},an)
              end
	  end
      end


      (* emit load label expression *)
      and loadLabexp(le,d,an) = mark(I.LDA{r=d,b=zeroR,d=I.LABop le},an)

      (* emit a copy *)
      and copy(dst,src,an) = 
          mark'(I.COPY{k=CB.GP, sz=32, dst=dst,src=src,
                      tmp=case dst of
                           [_] => NONE | _ => SOME(I.Direct(newReg()))},an)

      (* emit a floating point copy *)
      and fcopy(dst,src,an) = 
          mark'(I.COPY{k=CB.FP, sz=64, dst=dst,src=src,
                      tmp=case dst of
                           [_] => NONE | _ => SOME(I.FDirect(newFreg()))},an)

      and move(s,d,an) = 
          if CB.sameCell(s,d) orelse CB.sameCell(d,zeroR) then () else 
          mark'(I.COPY{k=CB.GP, sz=32, dst=[d],src=[s],tmp=NONE},an)

      and fmove(s,d,an) = 
          if CB.sameCell(s,d) orelse CB.sameCell(d,zeroFR) then () else 
          mark'(I.COPY{k=CB.FP, sz=64, dst=[d],src=[s],tmp=NONE},an)

       (* emit an sign extension op *)
      and signExt32(r,d) =
          emit(I.OPERATE{oper=I.ADDL,ra=r,rb=zeroOpn,rc=d})

      (* emit an commutative arithmetic op *)
      and commArith(opcode,a,b,d,an) =
          case (opn a,opn b) of
            (I.REGop r,i) => mark(I.OPERATE{oper=opcode,ra=r,rb=i,rc=d},an)
          | (i,I.REGop r) => mark(I.OPERATE{oper=opcode,ra=r,rb=i,rc=d},an)
          | (r,i) => mark(I.OPERATE{oper=opcode,ra=reduceOpn r,rb=i,rc=d},an)

      (* emit an arithmetic op *)
      and arith(opcode,a,b,d,an) =
          mark(I.OPERATE{oper=opcode,ra=expr a,rb=opn b,rc=d},an)
      and arith'(opcode,a,b,d,an) =
          let val rb = opn b
              val ra = expr a
          in  mark(I.OPERATE{oper=opcode,ra=ra,rb=rb,rc=d},an) end

      (* emit a trapping commutative arithmetic op *)
      and commArithTrap(opcode,a,b,d,an) =
         (case (opn a,opn b) of
            (I.REGop r,i) => mark(I.OPERATEV{oper=opcode,ra=r,rb=i,rc=d},an)
          | (i,I.REGop r) => mark(I.OPERATEV{oper=opcode,ra=r,rb=i,rc=d},an)
          | (r,i) => mark(I.OPERATEV{oper=opcode,ra=reduceOpn r,rb=i,rc=d},an);
          emit(I.TRAPB)
         )

      (* emit a trapping arithmetic op *)
      and arithTrap(opcode,a,b,d,an) =
         (mark(I.OPERATEV{oper=opcode,ra=expr a,rb=opn b,rc=d},an);
          emit(I.TRAPB)
         )

      (* convert an operand into a register *)
      and reduceOpn(I.REGop r) = r
        | reduceOpn(I.IMMop 0) = zeroR
        | reduceOpn opn = 
           let val d = newReg()
           in  emit(I.OPERATE{oper=I.BIS,ra=zeroR,rb=opn,rc=d}); d end

      (* convert an expression into an operand *)
      and opn(T.REG(_,r)) = I.REGop r
        | opn(e as T.LI n) = 
	    if n <= 0xff andalso n >= 0 then 
	      I.IMMop(toInt(n))
            else let val tmpR = newReg()
                 in  loadImmed(n,zeroR,tmpR,[]); I.REGop tmpR end
        | opn(e as T.CONST _) = I.LABop e
        | opn(T.LABEXP x) = I.LABop x
        | opn e = I.REGop(expr e)

      (* compute base+displacement from an expression 
       *)
      and addr exp =
          let fun toLexp(I.IMMop i) = T.LI(IntInf.fromInt i)
                | toLexp(I.LABop le) = le
                | toLexp _ = error "addr.toLexp"

              fun add(t,n,I.IMMop m)  = 
                   I.IMMop(toInt(T.I.ADD(t,n,IntInf.fromInt m)))
                | add(t,n,I.LABop le) = I.LABop(T.ADD(t,T.LI n,le))
                | add(t,n,_) = error "addr.add"

              fun addLe(ty,le,I.IMMop 0) = I.LABop le
                | addLe(ty,le,disp) = I.LABop(T.ADD(ty,le,toLexp disp))

              fun sub(t,n,I.IMMop m) = 
                  I.IMMop(toInt(T.I.SUB(t,IntInf.fromInt m,n)))
                | sub(t,n,I.LABop le) = I.LABop(T.SUB(t,le,T.LI n))
                | sub(t,n,_) = error "addr.sub"

              fun subLe(ty,le,I.IMMop 0) = I.LABop le
                | subLe(ty,le,disp) = I.LABop(T.SUB(ty,le,toLexp disp))
             
              (* Should really take into account of the address width XXX *) 
              fun fold(T.ADD(t,e,T.LI n),disp) = fold(e,add(t,n,disp))
                | fold(T.ADD(t,e,x as T.CONST _),disp) = fold(e,addLe(t,x,disp))
                | fold(T.ADD(t,e,x as T.LABEL _),disp) = fold(e,addLe(t,x,disp))
                | fold(T.ADD(t,e,T.LABEXP l),disp) = fold(e,addLe(t,l,disp))
                | fold(T.ADD(t,T.LI n,e),disp) = fold(e, add(t,n,disp))
                | fold(T.ADD(t,x as T.CONST _,e),disp) = fold(e,addLe(t,x,disp))
                | fold(T.ADD(t,x as T.LABEL _,e),disp) = fold(e,addLe(t,x,disp))
                | fold(T.ADD(t,T.LABEXP l,e),disp) = fold(e,addLe(t,l,disp))
                | fold(T.SUB(t,e,T.LI n),disp) = fold(e,sub(t,n,disp))
                | fold(T.SUB(t,e,x as T.CONST _),disp) = fold(e,subLe(t,x,disp))
                | fold(T.SUB(t,e,x as T.LABEL _),disp) = fold(e,subLe(t,x,disp))
                | fold(T.SUB(t,e,T.LABEXP l),disp) = fold(e,subLe(t,l,disp))
                | fold(e,disp) = (expr e,disp)

          in  makeEA(fold(exp, zeroImm))
          end

      (* compute base+displacement+small offset *)
      and offset(base,disp as I.IMMop n,off) =
           let val n' = n+off
           in  if literal16 n' then (base,I.IMMop n')
               else 
               let val tmp = newReg()
               in  emit(I.OPERATE{oper=I.ADDQ,ra=base,rb=disp,rc=tmp});
                   (tmp,I.IMMop off)
               end
           end
        | offset(base,disp as I.LABop le,off) =
           (base, I.LABop(T.ADD(64,le,T.LI(IntInf.fromInt off))))
        | offset(base,disp,off) =
           let val tmp = newReg()
           in  emit(I.OPERATE{oper=I.ADDQ,ra=base,rb=disp,rc=tmp});
               (tmp,I.IMMop off)
           end

      (* check if base offset fits within the field *)
      and makeEA(base, off as I.IMMop offset) =
         if ~32768 <= offset andalso offset <= 32767 
         then (base, off)
         else 
         let val tmpR = newReg()
                (* unsigned low 16 bits *)
             val low = wtoi(Word.andb(itow offset, 0wxffff)) 
             val high = offset div 65536
             val (lowsgn, highsgn) =                        (* Sign-extend *)
              if low <= 32767 then (low, high) else (low -65536, high+1)
         in
             (emit(I.LDAH{r=tmpR, b=base, d=I.IMMop highsgn});
             (tmpR, I.IMMop lowsgn))
         end
       | makeEA(base, offset) = (base, offset)

      (* look for multiply by 4 and 8 of the given type *)
      and times4or8(ty,e) =
          let 
	      fun f(t,a,n) = if t = ty then 
                               if EQ(n, 4) then (TIMES4,a)
                               else if EQ(n, 8) then (TIMES8,a)
                               else (TIMES1,e)
                             else (TIMES1,e)

              fun u(t,a,n) = if t = ty then
                               if EQ(n, 2) then (TIMES4,a)
                               else if EQ(n, 3) then (TIMES8,a)
                               else (TIMES1,e)
                             else (TIMES1,e)
          in  case e of 
                T.MULU(t,a,T.LI n)   => f(t,a,n)
              | T.MULS(t,T.LI n,a)   => f(t,a,n)
              | T.SLL(t,a,T.LI n)    => u(t,a,n)
              | _                    => (TIMES1,e)
          end

      (* generate an add instruction 
       * ***OPT*** look for multiply by 4 and 8 and use the S4ADD and S8ADD
       * forms.
       *)
      and plus(ty,add,s4add,s8add,a,b,d,an) =
          (case times4or8(ty,a) of
              (TIMES4,a) => arith(s4add,a,b,d,an)
           |  (TIMES8,a) => arith(s8add,a,b,d,an)
           |  _ =>
          case times4or8(ty,b) of
              (TIMES4,b) => arith'(s4add,b,a,d,an)
           |  (TIMES8,b) => arith'(s8add,b,a,d,an)
           |  _          => commArith(add,a,b,d,an)
          )

      (* generate a subtract instruction 
       * ***OPT*** look for multiply by 4 and 8
       *)
      and minus(ty,sub,s4sub,s8sub,a,b,d,an) =
          (case times4or8(ty,a) of
              (TIMES4,a) => arith(s4sub,a,b,d,an)
           |  (TIMES8,a) => arith(s8sub,a,b,d,an)
           |  _          => 
              if ty = 64 then
              (case b of 
                 (* use LDA to handle subtraction when possible 
                  * Note: this may have sign extension problems later.
                  *)
                 T.LI i => (loadImmed(T.I.NEGT(32,i),expr a,d,an) handle _ =>
                              arith(sub,a,b,d,an))
              |  _ => arith(sub,a,b,d,an)
              ) else arith(sub,a,b,d,an)
          )

      (* look for special constants *)
      and wordOpn(T.LI n) = SOME(T.I.toWord32(32, n))
        | wordOpn e = NONE

      (* look for special byte mask constants 
       * IMPORTANT: we must ALWAYS keep the sign bit!      
       *)
      and byteMask(_,SOME 0wx00000000) = 0xff
        | byteMask(_,SOME 0wx000000ff) = 0xfe
        | byteMask(_,SOME 0wx0000ff00) = 0xfd
        | byteMask(_,SOME 0wx0000ffff) = 0xfc
        | byteMask(_,SOME 0wx00ff0000) = 0xfb
        | byteMask(_,SOME 0wx00ff00ff) = 0xfa
        | byteMask(_,SOME 0wx00ffff00) = 0xf9
        | byteMask(_,SOME 0wx00ffffff) = 0xf8
        | byteMask(ty,SOME 0wxff000000) = if ty = 64 then 0xf7 else 0x07
        | byteMask(ty,SOME 0wxff0000ff) = if ty = 64 then 0xf6 else 0x06
        | byteMask(ty,SOME 0wxff00ff00) = if ty = 64 then 0xf5 else 0x05
        | byteMask(ty,SOME 0wxff00ffff) = if ty = 64 then 0xf4 else 0x04
        | byteMask(ty,SOME 0wxffff0000) = if ty = 64 then 0xf3 else 0x03
        | byteMask(ty,SOME 0wxffff00ff) = if ty = 64 then 0xf2 else 0x02
        | byteMask(ty,SOME 0wxffffff00) = if ty = 64 then 0xf1 else 0x01
        | byteMask(ty,SOME 0wxffffffff) = if ty = 64 then 0xf0 else 0x00
        | byteMask _ = ~1

      (* generate an and instruction 
       * look for special masks.
       *)
      and andb(ty,a,b,d,an) =
          case byteMask(ty,wordOpn a) of
            ~1 => (case byteMask(ty,wordOpn b) of
                    ~1 => commArith(I.AND,a,b,d,an)
                  | mask => arith(I.ZAP,a,LI mask,d,an)
                  )
          | mask => arith(I.ZAP,b,LI mask,d,an)

      (* generate sll/sra/srl *)
      and sll32(a,b,d,an) =
          case wordOpn b of
            SOME 0w0 => doExpr(a,d,an)
          | SOME 0w1 => 
            let val r = T.REG(32,expr a) in arith(I.ADDL,r,r,d,an) end
          | SOME 0w2 => arith(I.S4ADDL,a,zeroT,d,an)
          | SOME 0w3 => arith(I.S8ADDL,a,zeroT,d,an)
          | _        => let val t = newReg()
                        in  arith(I.SLL,a,b,t,an);
                            signExt32(t,d)
                        end

      and sll64(a,b,d,an) =
          case wordOpn b of
            SOME 0w0 => doExpr(a,d,an)
          | SOME 0w1 => 
            let val r = T.REG(64,expr a) in arith(I.ADDQ,r,r,d,an) end
          | SOME 0w2 => arith(I.S4ADDQ,a,zeroT,d,an)
          | SOME 0w3 => arith(I.S8ADDQ,a,zeroT,d,an)
          | _        => arith(I.SLL,a,b,d,an)

       (* On the alpha, all 32 bit values are already sign extended. 
        * So no sign extension is necessary.  We do the same for 
        * sra32 and sra64
        *)
      and sra(a,b,d,an) = 
          mark(I.OPERATE{oper=I.SRA,ra=expr a,rb=opn b,rc=d},an)

      and srl32(a,b,d,an) = 
          let val ra = expr a 
              val rb = opn b 
              val t = newReg() 
          in  emit(I.OPERATE{oper=I.ZAP,ra=ra,rb=I.IMMop 0xf0,rc=t});
              mark(I.OPERATE{oper=I.SRL,ra=t,rb=rb,rc=d},an)
          end

      and srl64(a,b,d,an) = 
          mark(I.OPERATE{oper=I.SRL,ra=expr a,rb=opn b,rc=d},an)

      (*
       * Generic multiply.  
       * We first try to use the multiply by constant heuristic
       *)
      and multiply(ty,gen,genConst,e1,e2,rd,trapb,an) = 
          let fun nonconst(e1,e2) =
              let val instr = 
                case (opn e1,opn e2) of
                  (i,I.REGop r) => gen{ra=r,rb=i,rc=rd}
                | (I.REGop r,i) => gen{ra=r,rb=i,rc=rd}
                | (r,i)         => gen{ra=reduceOpn r,rb=i,rc=rd}
              in annotate(instr,an)::trapb end
              fun const(e,i) =
                  let val r = expr e
                  in  if !useMultByConst andalso
			 i >= 0 andalso i < 0x100 then
                          annotate(gen{ra=r,rb=I.IMMop(toInt i),rc=rd},an)::trapb
                      else    
                         (genConst{r=r,i=toInt i,d=rd}@trapb
                          handle _ => nonconst(T.REG(ty,r),T.LI i))
                  end
              val instrs =
		case (e1, e2) 
		of (_, T.LI i) => const(e1, i)
	         | (T.LI i, _) => const(e2, i)
		 | _ => nonconst(e1, e2)
          in  app emitInstruction instrs
          end

          (* Round r towards zero.
           * I generate the following sequence of code, which should get
           * mapped into conditional moves.
           *
           * d <- r + i;
           * d <- if (r > 0) then r else d
           *)
      (*
      and roundToZero{ty,r,i,d} =
          (doStmt(T.MV(ty,d,T.ADD(ty,T.REG(ty,r),T.LI i)));
           doStmt(T.MV(ty,d,T.COND(ty,T.CMP(ty,T.GE,T.REG(ty,r),T.LI 0),
                                   T.REG(ty,r),T.REG(ty,d))))
          )
       *)

      (*
       * Generic division.  
       * We first try to use the division by constant heuristic
       *)
      and divide(ty,pseudo,genDiv,e1,e2,rd,an) = 
          let fun nonconst(e1,e2) =
                  pseudo({ra=expr e1,rb=opn e2,rc=rd},reduceOpn)

              fun const(e,i) =
                  let val r = expr e
                  in  genDiv{mode=T.TO_ZERO,stm=doStmt}
                            {r=r,i=toInt i,d=rd}
                      handle _ => nonconst(T.REG(ty,r),T.LI i)
                  end
              val instrs =
                  case e2 of
                     T.LI i   => const(e1,i)
                   | _        => nonconst(e1,e2)
          in  app emitInstruction instrs
          end


      (*
      and multTrap(MULV,ADD,ADDV,e1,e2,rd,an) = (* signed multiply and trap *)
      let val ADD = fn {ra,rb,rc} => I.OPERATE{oper=ADD,ra=ra,rb=rb,rc=rc}
          val ADDV = fn {ra,rb,rc} => I.OPERATEV{oper=ADDV,ra=ra,rb=rb,rc=rc}
          val MULV = fn {ra,rb,rc} => I.OPERATEV{oper=MULV,ra=ra,rb=rb,rc=rc}
      in  multiply(MULV,ADD,ADDV,e1,e2,rd,an);
          emit(I.TRAPB)
      end 

      and mulu(MUL,ADD,e1,e2,rd,an) =  (* unsigned multiply *)
      let val ADD = fn {ra,rb,rc} => I.OPERATE{oper=ADD,ra=ra,rb=rb,rc=rc}
          val MUL = fn {ra,rb,rc} => I.OPERATE{oper=MUL,ra=ra,rb=rb,rc=rc}
      in  multiply(MUL,ADD,ADD,e1,e2,rd,an)
      end

      (* Multiplication *)
      and multiply(MULV, ADD, ADDV, e1, e2, rd, an) = 
      let val reg = expr e1
          val opn = opn e2
          fun emitMulvImmed (reg, 0, rd) =
                emit(I.LDA{r=rd, b=zeroR, d=I.IMMop 0})
            | emitMulvImmed (reg, 1, rd) =
                emit(ADD{ra=reg, rb=zeroOpn, rc=rd})
            | emitMulvImmed (reg, multiplier, rd) = 
              let fun log2 0w1 = 0 | log2 n = 1 + (log2 (Word.>> (n, 0w1)))
                  fun exp2 n = Word.<<(0w1, n)
                  fun bitIsSet (x,n) = Word.andb(x,exp2 n) <> 0w0
                  fun loop (~1) = ()
                    | loop n =
                     (if bitIsSet(itow multiplier, itow n) then
                       emit(ADDV{ra=reg,rb=I.REGop rd,rc=rd})
                      else ();
                      if n>0 then
                        emit(ADDV{ra=rd,rb=I.REGop rd,rc=rd})
                      else ();
                      loop (n-1))
              in  emit(ADDV{ra=reg, rb=I.REGop reg, rc=rd});
                  loop ((log2 (itow multiplier)) - 1)
              end
      in  case opn of 
            (I.IMMop multiplier) => emitMulvImmed (reg, multiplier, rd)
          | _ => mark(MULV{ra=reg, rb=opn, rc=rd},an)
          (*esac*)
      end
      *) 

      (* generate pseudo instruction *)
      and pseudo(instr,e1,e2,rc) =
           app emitInstruction (instr({ra=expr e1,rb=opn e2,rc=rc}, reduceOpn))

      (* generate a load *)
      and load(ldOp,ea,d,mem,an) =
          let val (base,disp) = addr ea
          in  mark(I.LOAD{ldOp=ldOp,r=d,b=base,d=disp,mem=mem},an) end

      (* generate a load with zero extension *)
      and loadZext(ea,rd,mem,EXT,an) = 
          let val (b, d) = addr ea
              val t1 = newReg()
              val _  = mark(I.LOAD{ldOp=I.LDQ_U, r=t1, b=b, d=d, mem=mem},an);
              val t2 = lda(b,d)
          in  emit(I.OPERATE{oper=EXT, ra=t1, rb=I.REGop t2, rc=rd}) end

      (* generate a load with sign extension *)
      and loadSext(ea,rd,mem,off,EXT,shift,an) = 
          let val (b, d)  = addr ea
              val (b',d') = offset(b,d,off)
              val t1      = newReg()
              val t2      = newReg()
              val t3      = newReg()
          in  mark(I.LOAD{ldOp=I.LDQ_U, r=t1, b=b, d=d, mem=mem},an);
              emit(I.LDA{r=t2, b=b', d=d'});
              emit(I.OPERATE{oper=EXT, ra=t1, rb=I.REGop t2, rc=t3});
              emit(I.OPERATE{oper=I.SRA, ra=t3, rb=I.IMMop shift, rc=rd})
          end

      (* generate a load byte with zero extension (page 4-48) *)
      and load8(ea,rd,mem,an) = 
          if !byteWordLoadStores then load(I.LDBU,ea,rd,mem,an)
          else loadZext(ea,rd,mem,I.EXTBL,an)

      (* generate a load byte with sign extension (page 4-48) *)
      and load8s(ea,rd,mem,an) = 
          if !byteWordLoadStores then load(I.LDB,ea,rd,mem,an)
          else loadSext(ea,rd,mem,1,I.EXTQH,56,an)

      (* generate a load 16 bit *)
      and load16(ea,rd,mem,an) = 
          if !byteWordLoadStores then load(I.LDWU,ea,rd,mem,an)
          else loadZext(ea,rd,mem,I.EXTWL,an)

      (* generate a load 16 bit with sign extension *)
      and load16s(ea,rd,mem,an) = 
          if !byteWordLoadStores then load(I.LDW,ea,rd,mem,an) 
          else loadSext(ea,rd,mem,2,I.EXTQH,48,an)

      (* generate a load 32 bit with sign extension *)
      and load32s(ea,rd,mem,an) = load(I.LDL,ea,rd,mem,an)

      (* generate a floating point load *)
      and fload(ldOp,ea,d,mem,an) =
          let val (base,disp) = addr ea
          in  mark(I.FLOAD{ldOp=ldOp,r=d,b=base,d=disp,mem=mem},an) end

      (* generate a store *)
      and store(stOp,ea,data,mem,an) =
          let val (base,disp) = addr ea
          in  mark(I.STORE{stOp=stOp,r=expr data,b=base,d=disp,mem=mem},an) end

      (* generate an store8 or store16 *)
      and storeUnaligned(ea,data,mem,INS,MSK,an) = 
          let val (base,disp) = addr ea
              val data = expr data
              val t1 = newReg()
              val t3 = newReg()
              val t4 = newReg()
              val t5 = newReg()
              val _ = emit(I.LOAD{ldOp=I.LDQ_U, r=t1, b=base, d=disp, mem=mem})
              val t2 = lda(base,disp)
          in  emit(I.OPERATE{oper=INS, ra=data, rb=I.REGop(t2), rc=t3});
              emit(I.OPERATE{oper=MSK, ra=t1, rb=I.REGop(t2), rc=t4});
              emit(I.OPERATE{oper=I.BIS, ra=t4, rb=I.REGop(t3), rc=t5});
              mark(I.STORE{stOp=I.STQ_U, r=t5, b=base, d=disp, mem=mem},an)
          end

      (* generate a store byte *)
      and store8(ea,data,mem,an) = 
          if !byteWordLoadStores then store(I.STB, ea, data, mem, an)
          else storeUnaligned(ea,data,mem,I.INSBL,I.MSKBL,an)  

      (* generate a store16 *)
      and store16(ea,data,mem,an) = 
          if !byteWordLoadStores then store(I.STW, ea, data, mem, an)
          else storeUnaligned(ea,data,mem,I.INSWL,I.MSKWL,an)  

      (* generate conversion from floating point to integer *)
      and cvtf2i(pseudo,rounding,e,rd,an) = 
          app emitInstruction (pseudo{mode=rounding, fs=fexpr e, rd=rd})

      (* generate an expression and return the register that holds the result *)
      and expr(e) = let
	fun comp() = let
	  val r = newReg()
        in doExpr(e, r, []); r
	end
      in
	case e
	of T.REG(_, r) => r
         | T.LI z => if z = 0 then zeroR else comp()
            (* On the alpha: all 32 bit values are already sign extended.
             * So no sign extension is necessary
             *)
         | T.SX(64, 32, e) => expr e
         | T.ZX(64, 32, e) => expr e
	 | _ => comp()
      end

      (* generate an expression that targets register d *)
      and doExpr(exp,d,an) =
          case exp of
            T.REG(_,r) => move(r,d,an)
          | T.LI n     => loadImmed(n,zeroR,d,an)
          | T.LABEL l  => loadLabexp(exp,d,an)
          | T.CONST c  => loadLabexp(exp,d,an)
          | T.LABEXP le => loadLabexp(le,d,an)

            (* special optimizations for additions and subtraction 
             * Question: using LDA for all widths is not really correct
             * since the result may not fit into the sign extension scheme.
             *)
          | T.ADD(64,e,T.LABEXP le) => mark(I.LDA{r=d,b=expr e,d=I.LABop le},an)
          | T.ADD(64,T.LABEXP le,e) => mark(I.LDA{r=d,b=expr e,d=I.LABop le},an)
          | T.ADD(64,e,x as (T.CONST _ | T.LABEL _))  => 
               mark(I.LDA{r=d,b=expr e,d=I.LABop x},an)
          | T.ADD(64,x as (T.CONST _ | T.LABEL _),e)  => 
               mark(I.LDA{r=d,b=expr e,d=I.LABop x},an)
          | T.ADD(64,e,T.LI i)     => loadImmed(i, expr e, d, an)
          | T.ADD(64,T.LI i,e)     => loadImmed(i, expr e, d, an)
	  | T.SUB(sz, a, b as T.LI z)    =>
	      if z = 0 then
		doExpr(a,d,an) 
	      else (case sz
		of 32 => minus(32,I.SUBL,I.S4SUBL,I.S8SUBL,a,b,d,an)
	         | 64 => minus(64,I.SUBQ,I.S4SUBQ,I.S8SUBQ,a,b,d,an)
		 | _ =>  doExpr(Gen.compileRexp exp,d,an)
		(*esac*))

            (* 32-bit support *)
          | T.ADD(32,a,b) => plus(32,I.ADDL,I.S4ADDL,I.S8ADDL,a,b,d,an)
          | T.SUB(32,a,b) => minus(32,I.SUBL,I.S4SUBL,I.S8SUBL,a,b,d,an)
          | T.ADDT(32,a,b) => commArithTrap(I.ADDLV,a,b,d,an)
          | T.SUBT(32,a,b) => arithTrap(I.SUBLV,a,b,d,an)
          | T.MULT(32,a,b) => 
               multiply(32,
                 fn{ra,rb,rc} => I.operatev{oper=I.MULLV,ra=ra,rb=rb,rc=rc},
                 Mult32.multiply,a,b,d,trapb,an) 
          | T.MULU(32,a,b) => 
               multiply(32,
                 fn{ra,rb,rc} => I.operate{oper=I.MULL,ra=ra,rb=rb,rc=rc},
                 Mulu32.multiply,a,b,d,[],an) 
          | T.MULS(32,a,b) => 
               multiply(32,
                 fn{ra,rb,rc} => I.operate{oper=I.MULL,ra=ra,rb=rb,rc=rc},
                 Muls32.multiply,a,b,d,[],an) 
          | T.DIVT(T.DIV_TO_ZERO,32,a,b) =>
	                      divide(32,P.divlv,Mult32.divide,a,b,d,an)
          | T.DIVU(32,a,b) => divide(32,P.divlu,Mulu32.divide,a,b,d,an)
          | T.DIVS(T.DIV_TO_ZERO,32,a,b) =>
	                      divide(32,P.divl,Muls32.divide,a,b,d,an)
(* FIXME: these two lines can go back in once the alphaMC can handle them:
          | T.REMU(32,a,b) => pseudo(P.remlu,a,b,d)
          | T.REMS(T.DIV_TO_ZERO,32,a,b) => pseudo(P.reml,a,b,d)
*)

          | T.SLL(32,a,b) => sll32(a,b,d,an)
          | T.SRA(32,a,b) => sra(a,b,d,an)
          | T.SRL(32,a,b) => srl32(a,b,d,an)

            (* 64 bit support *)
          | T.ADD(64,a,b) => plus(64,I.ADDQ,I.S4ADDQ,I.S8ADDQ,a,b,d,an)
          | T.SUB(64,a,b) => minus(64,I.SUBQ,I.S4SUBQ,I.S8SUBQ,a,b,d,an)
          | T.ADDT(64,a,b) => commArithTrap(I.ADDQV,a,b,d,an)
          | T.SUBT(64,a,b) => arithTrap(I.SUBQV,a,b,d,an)
          | T.MULT(64,a,b) =>
               multiply(64,
                 fn{ra,rb,rc} => I.operatev{oper=I.MULQV,ra=ra,rb=rb,rc=rc},
                 Mult64.multiply,a,b,d,trapb,an) 
          | T.MULU(64,a,b) => 
               multiply(64,
                 fn{ra,rb,rc} => I.operate{oper=I.MULQ,ra=ra,rb=rb,rc=rc},
                 Mulu64.multiply,a,b,d,[],an) 
          | T.MULS(64,a,b) => 
               multiply(64,
                 fn{ra,rb,rc} => I.operate{oper=I.MULQ,ra=ra,rb=rb,rc=rc},
                 Muls64.multiply,a,b,d,[],an) 
          | T.DIVT(T.DIV_TO_ZERO,64,a,b) =>
	                      divide(64,P.divqv,Mult64.divide,a,b,d,an)
          | T.DIVU(64,a,b) => divide(64,P.divqu,Mulu64.divide,a,b,d,an)
          | T.DIVS(T.DIV_TO_ZERO,64,a,b) =>
	                      divide(64,P.divq,Muls64.divide,a,b,d,an)
(* FIXME: These two lines can go back in once the alphaMC can handle them:
          | T.REMU(64,a,b) => pseudo(P.remqu,a,b,d)
          | T.REMS(T.DIV_TO_ZERO,64,a,b) => pseudo(P.remq,a,b,d)
*)

          | T.SLL(64,a,b) => sll64(a,b,d,an)
          | T.SRA(64,a,b) => sra(a,b,d,an)
          | T.SRL(64,a,b) => srl64(a,b,d,an)

            (* special bit operations with complement *)
          | T.ANDB(_,a,T.NOTB(_,b)) => arith(I.BIC,a,b,d,an)
          | T.ORB(_,a,T.NOTB(_,b))  => arith(I.ORNOT,a,b,d,an)
          | T.XORB(_,a,T.NOTB(_,b)) => commArith(I.EQV,a,b,d,an)
          | T.ANDB(_,T.NOTB(_,a),b) => arith(I.BIC,b,a,d,an)
          | T.ORB(_,T.NOTB(_,a),b)  => arith(I.ORNOT,b,a,d,an)
          | T.XORB(_,T.NOTB(_,a),b) => commArith(I.EQV,b,a,d,an)
          | T.NOTB(_,T.XORB(_,a,b)) => commArith(I.EQV,b,a,d,an)

            (* bit operations *)
          | T.ANDB(ty,a,b) => andb(ty,a,b,d,an)
          | T.XORB(_,a,b) => commArith(I.XOR,a,b,d,an)
          | T.ORB(_,a,b) => commArith(I.BIS,a,b,d,an)
          | T.NOTB(_,e) => arith(I.ORNOT,zeroT,e,d,an)

            (* loads *)
          | T.SX(_,_,T.LOAD(8,ea,mem)) => load8s(ea,d,mem,an)
          | T.SX(_,_,T.LOAD(16,ea,mem))=> load16s(ea,d,mem,an)
          | T.SX(_,_,T.LOAD(32,ea,mem))=> load32s(ea,d,mem,an)
          | T.ZX((8|16|32|64),_,T.LOAD(8,ea,mem)) => load8(ea,d,mem,an)
          | T.ZX((16|32|64),_,T.LOAD(16,ea,mem))=> load16(ea,d,mem,an)
          | T.ZX(64,_,T.LOAD(64,ea,mem)) => load(I.LDQ,ea,d,mem,an)
          | T.LOAD(8,ea,mem) => load8(ea,d,mem,an)
          | T.LOAD(16,ea,mem) => load16(ea,d,mem,an)
          | T.LOAD(32,ea,mem) => load32s(ea,d,mem,an)
          | T.LOAD(64,ea,mem) => load(I.LDQ,ea,d,mem,an) 

           (* floating -> int conversion *)
          | T.CVTF2I(ty,rounding,fty,e) =>
            (case (fty,ty) of
               (32,32) => cvtf2i(P.cvtsl,rounding,e,d,an)
             | (32,64) => cvtf2i(P.cvtsq,rounding,e,d,an)
             | (64,32) => cvtf2i(P.cvttl,rounding,e,d,an)
             | (64,64) => cvtf2i(P.cvttq,rounding,e,d,an)
             | _       => doExpr(Gen.compileRexp exp,d,an) (* other cases *)
            )

           (* conversion to boolean *)
	  | T.COND(_, T.CMP(ty,cond,e1,e2), x, y)  => 
	     (case (x, y)
	      of (T.LI n, T.LI m) =>
		if EQ(n, 1) andalso EQ(m, 0) then 
		  compare(ty,cond,e1,e2,d,an) 
		else if EQ(n, 0) andalso EQ(m, 1) then
		  compare(ty,T.Basis.negateCond cond,e1,e2,d,an)
	        else
		  cmove(ty,cond,e1,e2,x,y,d,an) 
	      | _ => cmove(ty,cond,e1,e2,x,y,d,an) 
	     (*esac*))

          | T.LET(s,e) => (doStmt s; doExpr(e, d, an))
          | T.MARK(e,A.MARKREG f) => (f d; doExpr(e,d,an))
          | T.MARK(e,a) => doExpr(e,d,a::an)
            (* On the alpha: all 32 bit values are already sign extended.
             * So no sign extension is necessary
             *)
          | T.SX(64, 32, e) => doExpr(e, d, an)
          | T.ZX(64, 32, e) => doExpr(e, d, an)

          | T.PRED(e, c) => doExpr(e, d, A.CTRLUSE c::an)
          | T.REXT e => ExtensionComp.compileRext (reducer()) {e=e, an=an, rd=d}
    
           (* Defaults *) 
          | e => doExpr(Gen.compileRexp e,d,an)

       (* Hmmm...  this is the funky thing described in the comments
        * in at the top of the file.  This should be made parametrizable
        * for other backends. 
        *)
      and farith(opcode,opcodeSMLNJ,a,b,d,an) = 
          let val fa = fexpr a
              val fb = fexpr b
          in  if SMLNJfloatingPoint then 
                   ((* emit(I.DEFFREG d); *)
                    mark(I.FOPERATEV{oper=opcodeSMLNJ,fa=fa,fb=fb,fc=d},an);
                    emit(I.TRAPB);
		    emitInstruction(I.LIVE{regs=List.foldl C.addFreg C.empty [fa,fb,d],
					   spilled=[]})
		    
                   )
              else mark(I.FOPERATE{oper=opcode,fa=fa,fb=fb,fc=d},an)
          end

      and farith'(opcode,a,b,d,an) = 
            mark(I.FOPERATE{oper=opcode,fa=fexpr a,fb=fexpr b,fc=d},an)

      and funary(opcode,e,d,an) = mark(I.FUNARY{oper=opcode,fb=fexpr e,fc=d},an)


      (* generate an floating point expression
       * return the register that holds the result 
       *)
      and fexpr(T.FREG(_,r)) = r
        | fexpr e = let val d = newFreg() in doFexpr(e,d,[]); d end

      (* generate an external floating point operation *) 
      and fcvti2f(pseudo,e,fd,an) =
          let val opnd = opn e
          in  app emitInstruction (pseudo({opnd=opnd, fd=fd}, reduceOpn))
          end

      (* generate a floating point store *)
      and fstore(stOp,ea,data,mem,an) =
          let val (base,disp) = addr ea
          in  mark(I.FSTORE{stOp=stOp,r=fexpr data,b=base,d=disp,mem=mem},an) 
          end

      (* generate a floating point expression that targets register d *)
      and doFexpr(e,d,an) =
          case e of
            T.FREG(_,f)    => fmove(f,d,an)

            (* single precision support *)
          | T.FADD(32,a,b) => farith(I.ADDS,ADDSX,a,b,d,an)
          | T.FSUB(32,a,b) => farith(I.SUBS,SUBSX,a,b,d,an)
          | T.FMUL(32,a,b) => farith(I.MULS,MULSX,a,b,d,an)
          | T.FDIV(32,a,b) => farith(I.DIVS,DIVSX,a,b,d,an)

            (* double precision support *)
          | T.FADD(64,a,b) => farith(I.ADDT,ADDTX,a,b,d,an)
          | T.FSUB(64,a,b) => farith(I.SUBT,SUBTX,a,b,d,an)
          | T.FMUL(64,a,b) => farith(I.MULT,MULTX,a,b,d,an)
          | T.FDIV(64,a,b) => farith(I.DIVT,DIVTX,a,b,d,an)

            (* copy sign (correct?) XXX *)
          | T.FCOPYSIGN(_,T.FNEG(_,a),b) => farith'(I.CPYSN,a,b,d,an)
          | T.FCOPYSIGN(_,a,T.FNEG(_,b)) => farith'(I.CPYSN,a,b,d,an)
          | T.FNEG(_,T.FCOPYSIGN(_,a,b)) => farith'(I.CPYSN,a,b,d,an)
          | T.FCOPYSIGN(_,a,b)           => farith'(I.CPYS,a,b,d,an)

            (* generic *)
          | T.FABS(_,a)   => 
               mark(I.FOPERATE{oper=I.CPYS,fa=zeroFR,fb=fexpr a,fc=d},an)
          | T.FNEG(_,a)   => 
               let val fs = fexpr a
               in mark(I.FOPERATE{oper=I.CPYSN,fa=fs,fb=fs,fc=d},an) end
          | T.FSQRT(_,a)  => error "fsqrt"

            (* loads *)
          | T.FLOAD(32,ea,mem) => fload(I.LDS,ea,d,mem,an)
          | T.FLOAD(64,ea,mem) => fload(I.LDT,ea,d,mem,an)
         
            (* floating/floating conversion 
             * Note: it is not necessary to convert single precision
             * to double on the alpha.
             *)
          | T.CVTF2F(fty,fty',e) => (* ignore rounding mode for now *)
            (case (fty,fty') of
               (64,64) => doFexpr(e,d,an) 
             | (64,32) => doFexpr(e,d,an) 
             | (32,32) => doFexpr(e,d,an) 
             | (32,64) => funary(I.CVTTS,e,d,an) (* use normal rounding *)
             | _       => error "CVTF2F"
            )

            (* integer -> floating point conversion *)
          | T.CVTI2F(fty,ty,e) => 
            let val pseudo = 
                case (ty,fty) of
                  (ty,32) => if ty <= 32 then P.cvtls else P.cvtqs
                | (ty,64) => if ty <= 32 then P.cvtlt else P.cvtqt
                | _       => error "CVTI2F"
            in  fcvti2f(pseudo,e,d,an) end

          | T.FMARK(e,A.MARKREG f) => (f d; doFexpr(e,d,an))
          | T.FMARK(e,a) => doFexpr(e,d,a::an)
          | T.FPRED(e,c) => doFexpr(e, d, A.CTRLUSE c::an)
          | T.FEXT e => ExtensionComp.compileFext (reducer()) {e=e, fd=d, an=an}
          | _ => error "doFexpr"

          (* check whether an expression is andb(e,1) *)
      and isAndb1(e as T.ANDB(_, e1, e2)) = let
	    fun isOne(n, ei) = 
	      if EQ(n, 1) then (true, ei) else (false, e)
	  in
	    case(e1, e2) 
	    of (T.LI n, _) => isOne(n, e2)
	     | (_, T.LI n) => isOne(n, e1)
	     | _ => (false, e)
	  end
	| isAndb1 e = (false, e)

      and zeroOrOne(T.LI n) =
	if n = 0 then ZERO 
	else if EQ(n, 1) then ONE
	     else OTHER
	| zeroOrOne _ = OTHER

      (* compile a branch *)
      and branch(e,lab,an) = 
          case e of
            T.CMP(ty,cc,e1 as T.LI _,e2) => 
               branchBS(ty,T.Basis.swapCond cc,e2,e1,lab,an)
          | T.CMP(ty,cc,e1,e2) => branchBS(ty,cc,e1,e2,lab,an)
            (* generate an floating point branch *)
          | T.FCMP(fty,cc,e1,e2) =>
            let val f1 = fexpr e1
                val f2 = fexpr e2
                fun bcc(cmp,br) = 
                let val tmpR = C.newFreg()
                in  (*emit(I.DEFFREG(tmpR));*)
                    emit(I.FOPERATE{oper=cmp,fa=f1,fb=f2,fc=tmpR});
                    emit(I.TRAPB);
		    emitInstruction(I.LIVE{regs=List.foldl C.addFreg C.empty [f1,f2,tmpR],
				spilled=[]});
                    mark(I.FBRANCH{b=br,f=tmpR,lab=lab},an)
                end
                fun fall(cmp1, br1, cmp2, br2) = 
                let val tmpR1 = newFreg()
                    val tmpR2 = newFreg()
                    val fallLab = Label.anon()
                in  (*emit(I.DEFFREG(tmpR1));*)
                    emit(I.FOPERATE{oper=cmp1, fa=f1, fb=f2, fc=tmpR1});
                    emit(I.TRAPB);
		    emitInstruction(I.LIVE{regs=List.foldl C.addFreg C.empty [f1,f2,tmpR1],
				spilled=[]});
                    mark(I.FBRANCH{b=br1, f=tmpR1, lab=fallLab},an);
                    (* emit(I.DEFFREG(tmpR2)); *)
                    emit(I.FOPERATE{oper=cmp2, fa=f1, fb=f2, fc=tmpR2});
                    emit(I.TRAPB);
		    emitInstruction(I.LIVE{regs=List.foldl C.addFreg C.empty [f1,f2,tmpR2],
				spilled=[]});
                    mark(I.FBRANCH{b=br2, f=tmpR2, lab=lab},an);
                    defineLabel fallLab
                end
                fun bcc2(cmp1, br1, cmp2, br2) = 
                     (bcc(cmp1, br1); bcc(cmp2, br2))
            in  case cc of 
                  T.==  => bcc(I.CMPTEQSU, I.FBNE)
                | T.?<> => bcc(I.CMPTEQSU, I.FBEQ)
                | T.?   => bcc(I.CMPTUNSU, I.FBNE)
                | T.<=> => bcc(I.CMPTUNSU, I.FBEQ)
                | T.>   => fall(I.CMPTLESU, I.FBNE, I.CMPTUNSU, I.FBEQ)
                | T.>=  => fall(I.CMPTLTSU, I.FBNE, I.CMPTUNSU, I.FBEQ)
                | T.?>  => bcc(I.CMPTLESU, I.FBEQ)
                | T.?>= => bcc(I.CMPTLTSU, I.FBEQ)
                | T.<   => bcc(I.CMPTLTSU, I.FBNE)
                | T.<=  => bcc(I.CMPTLESU, I.FBNE)
                | T.?<  => bcc2(I.CMPTLTSU, I.FBNE, I.CMPTUNSU, I.FBNE)
                | T.?<= => bcc2(I.CMPTLESU, I.FBNE, I.CMPTUNSU, I.FBNE)
                | T.<>  => fall(I.CMPTEQSU, I.FBNE, I.CMPTUNSU, I.FBEQ)
                | T.?=  => bcc2(I.CMPTEQSU, I.FBNE, I.CMPTUNSU, I.FBNE)
                | _     => error "branch"
            end
          | e => mark(I.BRANCH{b=I.BNE,r=ccExpr e,lab=lab},an)

      and br(opcode,exp,lab,an) = mark(I.BRANCH{b=opcode,r=expr exp,lab=lab},an)

            (* Use the branch on bit set/clear instruction when possible *) 
      and branchBS(ty,cc,a,b,lab,an)  =
          (case (cc,isAndb1 a,zeroOrOne b) of
             (T.EQ,(true,e),ONE)  => br(I.BLBS,e,lab,an)
           | (T.EQ,(true,e),ZERO) => br(I.BLBC,e,lab,an)
           | (T.NE,(true,e),ZERO) => br(I.BLBS,e,lab,an)
           | (T.NE,(true,e),ONE)  => br(I.BLBC,e,lab,an)
           | (cc,_,_)             => branchIt(ty,cc,a,b,lab,an)
          )
 
          (* generate a branch instruction. 
           * Check for branch on zero as a special case 
           *)

      and branchIt(ty,cc,e1,e2 as T.LI z,lab,an) = 
	   if z = 0 then branchIt0(cc,e1,lab,an)
	   else branchItOther(ty,cc,e1,e2,lab,an)
        | branchIt(ty,cc,e1,e2,lab,an) = branchItOther(ty,cc,e1,e2,lab,an)

          (* generate a branch instruction. 
           * This function optimizes the special case of comparison with zero.
           *)
      and branchIt0(T.EQ,e,lab,an) = br(I.BEQ,e,lab,an)
        | branchIt0(T.NE,e,lab,an) = br(I.BNE,e,lab,an)
        | branchIt0(T.GT,e,lab,an) = br(I.BGT,e,lab,an)
        | branchIt0(T.GE,e,lab,an) = br(I.BGE,e,lab,an)
        | branchIt0(T.LE,e,lab,an) = br(I.BLE,e,lab,an)
        | branchIt0(T.LT,e,lab,an) = br(I.BLT,e,lab,an)
        | branchIt0(T.GTU,e,lab,an) = br(I.BNE,e,lab,an)  (* always > 0! *)
        | branchIt0(T.GEU,e,lab,an) = (* always true! *) goto(lab,an)
        | branchIt0(T.LTU,e,lab,an) = (* always false! *) ()
        | branchIt0(T.LEU,e,lab,an) = br(I.BEQ,e,lab,an)  (* never < 0! *)
        | branchIt0 _               = error "brnachIt0"

        (* Generate the operands for unsigned comparisons 
         * Mask out high order bits whenever necessary.
         *)
      and unsignedCmpOpnds(ty,e1,e2) = 
          let fun zapHi(r,mask) = 
              let val d = newReg()
              in  emit(I.OPERATE{oper=I.ZAP, ra=r, rb=I.IMMop mask,rc=d}); 
                  I.REGop d
              end

              fun zap(opn as I.REGop r) =
                  (case ty of
                     8  => zapHi(r,0xfd)
                   | 16 => zapHi(r,0xfc)
                   | 32 => zapHi(r,0xf0)
                   | 64 => opn 
                   | _  => error "unsignedCmpOpnds" 
                  )
                | zap opn = opn 
              val opn1 = opn e1
              val opn2 = opn e2
          in  (zap opn1,zap opn2) end

        (* Generate a branch *)
      and branchItOther(ty,cond,e1,e2,lab,an) = 
          let val tmpR = newReg()
              fun signedCmp(cmp,br) = 
                  (emit(I.OPERATE{oper=cmp, ra=expr e1, rb=opn e2, rc=tmpR});
                   mark(I.BRANCH{b=br, r=tmpR, lab=lab},an)
                  )
              fun unsignedCmp(ty,cmp,br) = 
                  let val (x,y) = unsignedCmpOpnds(ty,e1,e2)
                  in  emit(I.OPERATE{oper=cmp,ra=reduceOpn x,rb=y,rc=tmpR});
                      mark(I.BRANCH{b=br, r=tmpR, lab=lab},an)
                  end
          in  case cond of
                T.LT  => signedCmp(I.CMPLT,I.BNE)
              | T.LE  => signedCmp(I.CMPLE,I.BNE)
              | T.GT  => signedCmp(I.CMPLE,I.BEQ)
              | T.GE  => signedCmp(I.CMPLT,I.BEQ)
              | T.EQ  => signedCmp(I.CMPEQ,I.BNE)
              | T.NE  => signedCmp(I.CMPEQ,I.BEQ)
              | T.LTU => unsignedCmp(ty,I.CMPULT,I.BNE)
              | T.LEU => unsignedCmp(ty,I.CMPULE,I.BNE)
              | T.GTU => unsignedCmp(ty,I.CMPULE,I.BEQ)
              | T.GEU => unsignedCmp(ty,I.CMPULT,I.BEQ)
              | _     => error "branchItOther"
          end

         (* This function generates a conditional move:
          *   d = if cond(a,b) then x else y
          * Apparently, only signed comparisons conditional moves
          * are supported on the alpha.
          *)
      and cmove(ty,cond,a,b,x,y,d,an) =
          let val tmp = newReg()
              val _ = doExpr(y,tmp,[]) (* evaluate false case *)

              val (cond,a,b) = 
                (* move the immed operand to b *)
                case a of
                  (T.LI _ | T.CONST _ | T.LABEL _ | T.LABEXP _) => 
                    (T.Basis.swapCond cond,b,a)
                | _ => (cond,a,b)

              fun sub(a, T.LI z) = 
		   if z = 0 then expr a else expr(T.SUB(ty,a,b))
                | sub(a,b)       = expr(T.SUB(ty,a,b))

              fun cmp(cond,e1,e2) = 
                  let val flag = newReg()
                  in  compare(ty,cond,e1,e2,flag,[]); flag end

              val (oper,ra,x,y) =
                case (cond,isAndb1 a,zeroOrOne b) of
                     (* low bit set/clear? *)
                  (T.EQ,(true,e),ONE)  => (I.CMOVLBS,expr e,x,y)
                | (T.EQ,(true,e),ZERO) => (I.CMOVLBC,expr e,x,y)
                | (T.NE,(true,e),ZERO) => (I.CMOVLBS,expr e,x,y)
                | (T.NE,(true,e),ONE)  => (I.CMOVLBC,expr e,x,y)
                     (* signed  *)
                | (T.EQ,_,_)           => (I.CMOVEQ,sub(a,b),x,y)
                | (T.NE,_,_)           => (I.CMOVNE,sub(a,b),x,y)
                | (T.GT,_,_)           => (I.CMOVGT,sub(a,b),x,y)
                | (T.GE,_,_)           => (I.CMOVGE,sub(a,b),x,y)
                | (T.LT,_,_)           => (I.CMOVLT,sub(a,b),x,y)
                | (T.LE,_,_)           => (I.CMOVLE,sub(a,b),x,y)

                   (* unsigned: do compare then use the condition code *)
                | (T.LTU,_,_)          => (I.CMOVEQ,cmp(T.GEU,a,b),x,y)
                | (T.LEU,_,_)          => (I.CMOVEQ,cmp(T.GTU,a,b),x,y)
                | (T.GTU,_,_)          => (I.CMOVEQ,cmp(T.LEU,a,b),x,y)
                | (T.GEU,_,_)          => (I.CMOVEQ,cmp(T.LTU,a,b),x,y)
                | _                    => error "cmove"
          in  mark(I.CMOVE{oper=oper,ra=ra,rb=opn x,rc=tmp},an); (* true case *)
              move(tmp, d, [])
          end


        (* This function generates a comparion between e1 and e2 and writes 
         * the result to register d.
         * It'll mask out the high order 32-bits when performing
         * unsigned 32-bit integer comparisons.
         *)
      and compare(ty,cond,e1,e2,d,an) = 
          let fun signedCmp(oper,a,b,d) = 
                  mark(I.OPERATE{oper=oper,ra=expr a,rb=opn b,rc=d},an)
              fun unsignedCmp(ty,oper,a,b,d) = 
                  let val (x,y) = unsignedCmpOpnds(ty,a,b)
                  in  mark(I.OPERATE{oper=oper,ra=reduceOpn x,rb=y,rc=d},an)
                  end
              fun eq(a,b,d) = 
                 (case (opn a,opn b) of
                    (a,I.REGop r) => 
                      mark(I.OPERATE{oper=I.CMPEQ,ra=r,rb=a,rc=d},an)
                  | (a,b) =>
                      mark(I.OPERATE{oper=I.CMPEQ,ra=reduceOpn a,rb=b,rc=d},an)
                 )
              fun neq(a,b,d) = 
                  let val tmp = newReg()  
                  in  eq(a,b,tmp);
                      emit(I.OPERATE{oper=I.CMPEQ,ra=tmp,rb=zeroOpn,rc=d})
                  end
              val (cond,e1,e2) =
		  case e1 of
                    (T.LI _ | T.CONST _ | T.LABEL _ | T.LABEXP _) => 
                       (T.Basis.swapCond cond,e2,e1)
                  | _ => (cond,e1,e2)
          in  case cond of
                T.EQ  => eq(e1,e2,d)
              | T.NE  => neq(e1,e2,d)
              | T.GT  => signedCmp(I.CMPLT,e2,e1,d)
              | T.GE  => signedCmp(I.CMPLE,e2,e1,d)
              | T.LT  => signedCmp(I.CMPLT,e1,e2,d)
              | T.LE  => signedCmp(I.CMPLE,e1,e2,d)
              | T.GTU => unsignedCmp(ty,I.CMPULT,e2,e1,d)
              | T.GEU => unsignedCmp(ty,I.CMPULE,e2,e1,d)
              | T.LTU => unsignedCmp(ty,I.CMPULT,e1,e2,d)
              | T.LEU => unsignedCmp(ty,I.CMPULE,e1,e2,d)
              | _     => error "compare"
          end

         (* generate an unconditional branch *)
      and goto(lab,an) = mark(I.BRANCH{b=I.BR,r=zeroR,lab=lab},an)

         (* generate an call instruction *)
      and call(ea,flow,defs,uses,mem,cutTo,an,0) = 
	  let val defs=cellset defs
              val uses=cellset uses
              val instr = 
		  case (ea, flow) of
                      (T.LABEL lab, [_]) => 
                      I.BSR{lab=lab,r=C.returnAddr,defs=defs,uses=uses,
                            cutsTo=cutTo,mem=mem}
		    | _ => I.JSR{r=C.returnAddr,b=expr ea,
				 d=0,defs=defs,uses=uses,cutsTo=cutTo,mem=mem}
	  in  mark(instr,an)
	  end
	| call _ = error "pops<>0 not implemented"

      and doCCexpr(T.CC(_,r),d,an) = move(r,d,an)
        | doCCexpr(T.FCC(_,r),d,an) = fmove(r,d,an)
        | doCCexpr(T.CMP(ty,cond,e1,e2),d,an)  = compare(ty,cond,e1,e2,d,an) 
        | doCCexpr(T.FCMP(fty,cond,e1,e2),d,an) = error "doCCexpr"
        | doCCexpr(T.CCMARK(e,A.MARKREG f),d,an) = (f d; doCCexpr(e,d,an))
        | doCCexpr(T.CCMARK(e,a),d,an) = doCCexpr(e,d,a::an)
        | doCCexpr(T.CCEXT e,d,an) = 
             ExtensionComp.compileCCext (reducer()) {e=e, ccd=d, an=an}
        | doCCexpr _ = error "doCCexpr"

      and ccExpr(T.CC(_,r)) = r
        | ccExpr(T.FCC(_,r)) = r
        | ccExpr e = let val d = newReg()
                     in  doCCexpr(e,d,[]); d end

      (* compile a statement *)
      and stmt(s,an) =
          case s of
            T.MV(ty,r,e) => doExpr(e,r,an)
          | T.FMV(ty,r,e) => doFexpr(e,r,an)
          | T.CCMV(r,e) => doCCexpr(e,r,an)
          | T.COPY(ty,dst,src) => copy(dst,src,an)
          | T.FCOPY(ty,dst,src) => fcopy(dst,src,an)
          | T.JMP(T.LABEL lab,_) => goto(lab,an)
          | T.JMP(e,labs) => mark(I.JMPL({r=zeroR,b=expr e,d=0},labs),an)
          | T.BCC(cc,lab) => branch(cc,lab,an)
          | T.CALL{funct,targets,defs,uses,region,pops,...} => 
              call(funct,targets,defs,uses,region,[],an,pops)
          | T.FLOW_TO(T.CALL{funct,targets,defs,uses,region,pops,...},cutTo) =>
              call(funct,targets,defs,uses,region,cutTo,an,pops)
          | T.RET _ => mark(I.RET{r=zeroR,b=C.returnAddr,d=0},an)
          | T.STORE(8,ea,data,mem) => store8(ea,data,mem,an)
          | T.STORE(16,ea,data,mem) => store16(ea,data,mem,an)
          | T.STORE(32,ea,data,mem) => store(I.STL,ea,data,mem,an)
          | T.STORE(64,ea,data,mem) => store(I.STQ,ea,data,mem,an)
          | T.FSTORE(32,ea,data,mem) => fstore(I.STS,ea,data,mem,an)
          | T.FSTORE(64,ea,data,mem) => fstore(I.STT,ea,data,mem,an)
          | T.DEFINE l => defineLabel l
          | T.ANNOTATION(s,a) => stmt(s,a::an)
          | T.EXT s => ExtensionComp.compileSext (reducer()) {stm=s,an=an}
	  | T.LIVE rs => mark'(I.LIVE{regs=cellset rs, spilled=[]}, an)
	  | T.KILL rs => mark'(I.KILL{regs=cellset rs, spilled=[]}, an)
          | s => doStmts (Gen.compileStm s)

      and reducer() =
          TS.REDUCER{reduceRexp    = expr,
                     reduceFexp    = fexpr,
                     reduceCCexp   = ccExpr,
                     reduceStm     = stmt,
                     operand       = opn,
                     reduceOperand = reduceOpn,
                     addressOf     = addr,
                     emit          = emitInstruction o annotate,
                     instrStream   = instrStream,
                     mltreeStream  = self()
                    } 

      and doStmt s = stmt(s,[])
      and doStmts ss = app doStmt ss

       (* convert mlrisc to cellset:
        * condition code registers are mapped onto general registers
        *)
      and cellset mlrisc =
          let fun g([],acc) = acc
                | g(T.GPR(T.REG(_,r))::regs,acc)  = g(regs,C.addReg(r,acc))
                | g(T.FPR(T.FREG(_,f))::regs,acc) = g(regs,C.addFreg(f,acc))
                | g(T.CCR(T.CC(_,cc))::regs,acc)  = g(regs,C.addReg(cc,acc))
                | g(T.CCR(T.FCC(_,cc))::regs,acc) = g(regs,C.addReg(cc,acc))
                | g(_::regs, acc) = g(regs, acc)
          in  g(mlrisc, C.empty) end

      and self() = 
          TS.S.STREAM
         { beginCluster   = beginCluster,
           endCluster     = endCluster,
           emit           = doStmt,
           pseudoOp       = pseudoOp,
           defineLabel    = defineLabel,
           entryLabel     = entryLabel,
           comment        = comment,
           annotation     = annotation,
           getAnnotations = getAnnotations,
           exitBlock      = fn regs => exitBlock(cellset regs)
         } 
   in  self()
   end
 
end

