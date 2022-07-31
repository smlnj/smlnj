(* hppa.sml
 * 
 * COPYRIGHT (c) 1996 AT&T Bell Laboratories.
 *
 * generates machine code from the mltree.
 *
 * This new version has been completely rewritten to take (more) advantage
 * of the new improved instruction set. 
 * 
 * Please see the README.hppa file for details.
 *
 * -- Allen
 *)

functor Hppa
  (structure HppaInstr : HPPAINSTR
   structure ExtensionComp : MLTREE_EXTENSION_COMP
      		where I = HppaInstr 
		  and T = HppaInstr.T
   structure MilliCode : HPPA_MILLICODE
      		where I = HppaInstr
   structure LabelComp : LABEL_COMP 
   		where I = HppaInstr  
		  and T = HppaInstr.T
   val costOfMultiply : int ref
   val costOfDivision : int ref
  ) : MLTREECOMP =
struct
   structure I = HppaInstr
   structure T = I.T
   structure TS = ExtensionComp.TS
   structure C = I.C
   structure CB = CellsBasis
   structure MC = MilliCode
   structure LC = LabelComp
   structure Region = I.Region
   structure A = MLRiscAnnotations
   structure CFG = ExtensionComp.CFG

   type instrStream = (I.instruction, C.cellset, CFG.cfg) TS.stream
   type mltreeStream = (T.stm, T.mlrisc list, CFG.cfg) TS.stream

   structure Gen = MLTreeGen(structure T = T
			     structure Cells = C
                             val intTy = 32
                             val naturalWidths = [32]
                             datatype rep = SE | ZE | NEITHER
                             val rep = NEITHER
                            )
   fun mkcopy{dst, src, tmp} =
       I.COPY{k=CB.GP, sz=32, dst=dst, src=src, tmp=tmp}
   fun mkfcopy{dst, src, tmp} =
       I.COPY{k=CB.FP, sz=64, dst=dst, src=src, tmp=tmp}
   structure W = Word32
   functor Multiply32 = MLTreeMult
    (structure I = I
     structure T = T
     structure CB = CB
     val intTy = 32
     type arg  = {r1:CB.cell,r2:CB.cell,d:CB.cell}
     type argi = {r:CB.cell,i:int,d:CB.cell}

     fun mov{r,d} = mkcopy{dst=[d],src=[r],tmp=NONE}
     fun add{r1,r2,d} = I.arith{a=I.ADD,r1=r1,r2=r2,t=d}
     fun slli{r,i,d} = [I.shift{s=I.ZDEP,r=r,p=31-i,len=32-i,t=d}]
     fun srli{r,i,d} = [I.shift{s=I.EXTRU,r=r,p=31-i,len=32-i,t=d}]
     fun srai{r,i,d} = [I.shift{s=I.EXTRS,r=r,p=31-i,len=32-i,t=d}]
    )

   (* signed, trapping version of multiply and divide *)
   structure Mult32 = Multiply32
    (val trapping = true
     val multCost = costOfMultiply
     val divCost  = costOfDivision
     fun addv{r1,r2,d} = [I.arith{a=I.ADDO,r1=r1,r2=r2,t=d}]
     fun subv{r1,r2,d} = [I.arith{a=I.SUBO,r1=r1,r2=r2,t=d}]
     val sh1addv = SOME(fn{r1,r2,d} => [I.arith{a=I.SH1ADDO,r1=r1,r2=r2,t=d}])
     val sh2addv = SOME(fn{r1,r2,d} => [I.arith{a=I.SH2ADDO,r1=r1,r2=r2,t=d}])
     val sh3addv = SOME(fn{r1,r2,d} => [I.arith{a=I.SH3ADDO,r1=r1,r2=r2,t=d}])
    )
    (val signed = true)

   (* unsigned, non-trapping version of multiply and divide *)
   structure Mulu32 = Multiply32
    (val trapping = false
     val multCost = costOfMultiply
     val divCost  = costOfDivision
     fun addv{r1,r2,d} = [I.arith{a=I.ADD,r1=r1,r2=r2,t=d}]
     fun subv{r1,r2,d} = [I.arith{a=I.SUB,r1=r1,r2=r2,t=d}]
     val sh1addv = SOME(fn{r1,r2,d} => [I.arith{a=I.SH1ADDL,r1=r1,r2=r2,t=d}])
     val sh2addv = SOME(fn{r1,r2,d} => [I.arith{a=I.SH2ADDL,r1=r1,r2=r2,t=d}])
     val sh3addv = SOME(fn{r1,r2,d} => [I.arith{a=I.SH3ADDL,r1=r1,r2=r2,t=d}])
    )
    (val signed   = false)

   (* signed, non-trapping version of multiply and divide *)
   structure Muls32 = Multiply32
    (val trapping = false
     val multCost = costOfMultiply
     val divCost  = costOfDivision
     fun addv{r1,r2,d} = [I.arith{a=I.ADD,r1=r1,r2=r2,t=d}]
     fun subv{r1,r2,d} = [I.arith{a=I.SUB,r1=r1,r2=r2,t=d}]
     val sh1addv = SOME(fn{r1,r2,d} => [I.arith{a=I.SH1ADDL,r1=r1,r2=r2,t=d}])
     val sh2addv = SOME(fn{r1,r2,d} => [I.arith{a=I.SH2ADDL,r1=r1,r2=r2,t=d}])
     val sh3addv = SOME(fn{r1,r2,d} => [I.arith{a=I.SH3ADDL,r1=r1,r2=r2,t=d}])
    )
    (val signed   = true)

   fun error msg = MLRiscErrorMsg.error("Hppa",msg)

   datatype ea = datatype I.addressing_mode

   datatype times248 = TIMES1 | TIMES2 | TIMES4 | TIMES8 

   datatype amode = 
       AMode of I.addressing_mode 
     | DISP of CB.cell * T.I.machine_int
     

   fun LI i        = T.LI(T.I.fromInt(32, i))
   fun toInt mi    = T.I.toInt(32, mi)
   fun toInt32 mi  = T.I.toInt32(32, mi)
   fun toWord mi   = T.I.toWord(32, mi)
   fun toWord32 mi = T.I.toWord32(32, mi)
   fun EQ(x,y)     = T.I.EQ(32, x, y)
   fun LT(x,y)     = T.I.LT(32, x, y)
   fun GE(x,y)     = T.I.GE(32, x, y)


   fun selectInstructions
        (instrStream as
         TS.S.STREAM{emit=emitInstruction, defineLabel, entryLabel, getAnnotations,
                  beginCluster, endCluster, annotation,
                  exitBlock, pseudoOp, comment, ...}) =
   let
       (* operand type and effective addresss *)
 
       val newReg  = C.newReg
       val newFreg = C.newFreg
       val CRReg   = C.Reg C.CR
       val zeroR = C.r0 
       val zeroF = C.f0
       val zeroEA = I.Direct zeroR
       val zeroT = T.REG(32,zeroR)
       val zeroImmed = I.IMMED 0
       val zeroOpn = zeroImmed

       val emit = emitInstruction o I.INSTR

       local 
	   fun f(i,[]) = i
	     | f(i, a::an) = f (I.ANNOTATION{i=i, a=a}, an)
       in
         fun mark(i, an) = emitInstruction(f(I.INSTR i, an))
	 fun mark'(i, an) = emitInstruction(f(i, an))
       end

       val ldLabelEA = LC.ldLabelEA emitInstruction
       val ldLabelOpnd = LC.ldLabelOpnd emitInstruction

       (* Check whether an expression is being multiplied by 2, 4, or 8 *)
       local
	 fun mul(mi,e, exp) = 
	   if EQ(mi, 2) then (TIMES2, e)
	   else if EQ(mi, 4) then (TIMES4, e)
		else if EQ(mi, 8) then (TIMES8, e) 
		     else (TIMES1, exp)
       in	 
	 fun times(exp) =
	  (case exp
	   of T.MULU(_, e, T.LI mi) => mul(mi, e, exp)
	    | T.MULU(_, T.LI mi, e) => mul(mi, e, exp)
	    | T.SLL(_, e, T.LI mi) => 
	       if EQ(mi, 1) then (TIMES2, e)
	       else if EQ(mi, 2) then (TIMES4, e)
		    else if EQ(mi, 3) then (TIMES8, e)
			 else (TIMES1, exp)
	    | _ => (TIMES1, exp)
          (*esac*))

	 (* trapping version of the above *)
	 fun timest(exp as T.MULT(_, e, T.LI mi)) = mul(mi, e, exp)
	   | timest(exp as T.MULT(_, T.LI mi, e)) = mul(mi, e, exp)
	   | timest e = (TIMES1, e)
       end (*local*)

       fun im5 n   = LT(n, 16) andalso GE(n, ~16)
       fun im11 n  = LT(n, 1024)  andalso GE(n, ~1024)
       fun im14 n  = LT(n, 8192)  andalso GE(n, ~8192)

       (* Split values into 11 low bits and 21 high bits *)
       fun split11w w = 
           {hi = Word32.toIntX(Word32.~>>(w,0w11)), 
            lo = Word32.toIntX(Word32.andb(w,0wx7ff))}
       fun split11 n = split11w(toWord32 n)

       (* load immediate *)
       fun loadImmed(n,t,an) =
           if im14 n 
           then mark(I.LDO{i=I.IMMED(toInt n),b=zeroR,t=t},an)
           else let val {hi,lo} = split11 n
                    val tmp = newReg()
                in  emit(I.LDIL{i=I.IMMED hi,t=tmp});
                    mark(I.LDO{i=I.IMMED lo,b=tmp,t=t},an)
                end

       (* generate code to load a immediate constant *) 
       fun immed (n: T.I.machine_int) = 
            let val t = newReg() in loadImmed(n,t,[]); t end

       (* load constant *)
       fun loadConst(c,t,an) = 
             mark(I.LDO{b=zeroR,i=I.LabExp(c,I.F),t=t},an) (* XXX *)

       (* convert an operand into a register *)
       fun reduceOpn i = 
            let val t = newReg()
            in  emit(I.LDO{i=i,b=zeroR,t=t}); t end
 
       (* emit parallel copies *)
       fun copy(dst,src,an) =
         mark'(mkcopy{dst=dst,src=src,
                    tmp=case dst of [_] => NONE | _ => SOME(I.Direct(newReg()))},an)
       fun fcopy(dst,src,an) =
         mark'(mkfcopy{dst=dst,src=src,
                     tmp=case dst of [_] => NONE | _ => SOME(I.FDirect(newFreg()))},an)
 
       (* move register s to register t *)
       fun move(s,t,an) =
           if CB.sameColor(s,t) orelse CB.registerId t = 0 then ()
           else if CB.registerId s = 0 then
                mark(I.LDO{i=zeroImmed,b=zeroR,t=t},an)
           else mark'(mkcopy{src=[s],dst=[t],tmp=NONE},an)
 
       (* move floating point register s to register t *)
       fun fmove(s,t,an) =
           if CB.sameColor(s,t) then ()
           else mark'(mkfcopy{src=[s],dst=[t],tmp=NONE},an)

       (* generate millicode function call *)
       fun milliCall(milliFn, e1, e2, rd) =
       let val rs = expr e1 
           val rt = expr e2
       in  app emitInstruction (milliFn{rs=rs,rt=rt,rd=rd}) end 

       (* emit an arithmetic op with possible immediate mode 
        * The immed operand is the first operand on the HPPA! Arrrrggggghhhh!
        *)
       and immedArith(a,ai,e1,e2,t,an) =  
          case (opn e1,expr e2) of
             (I.REG r1,r2) => mark(I.ARITH{a=a,r1=r1,r2=r2,t=t},an)
           | (i,r) => mark(I.ARITHI{ai=ai,r=r,i=i,t=t},an)

       (* emit a commutative arithmetic op with immediate mode *)
       and commImmedArith(a,ai,e1,e2,t,an) =
          case (opn e1,opn e2) of
             (I.REG r1,I.REG r2) => mark(I.ARITH{a=a,r1=r1,r2=r2,t=t},an)
          |  (I.REG r,i)  => mark(I.ARITHI{ai=ai,r=r,i=i,t=t},an)
          |  (i,I.REG r)  => mark(I.ARITHI{ai=ai,r=r,i=i,t=t},an)
          |  (i,j) => mark(I.ARITHI{ai=ai,r=reduceOpn i,i=j,t=t},an)

       (* emit an arithmetic op *)
       and arith(a,e1,e2,t,an) = 
          mark(I.ARITH{a=a,r1=expr e1,r2=expr e2,t=t},an)
 
       (* emit an unary floating point op *)
       and funary(a,e,t,an) = mark(I.FUNARY{fu=a,f=fexpr e,t=t},an)

       (* emit an conversion floating point op *)
       and fcnv(a,e,t,an) = mark(I.FCNV{fcnv=a,f=fexpr e,t=t},an)
 
       (* emit a binary floating point op *)
       and farith(a,e1,e2,t,an) = 
           mark(I.FARITH{fa=a,r1=fexpr e1,r2=fexpr e2,t=t},an)
 
       (* convert an expression into an addressing mode 
        * scale is the size of the data being addressed.
	*
	* Return the addressing mode and an infinite precision immediate
	* in the case of DISPea.
        *)
       and addr(scale,T.ADD(_,e,T.LI n))    = DISP(expr e, n)
         | addr(scale,T.ADD(_,e,c as T.CONST _)) =
              AMode(DISPea(expr e,I.LabExp(c,I.F)))
         | addr(scale,T.ADD(ty,i as T.LI _,e)) = addr(scale,T.ADD(ty,e,i))
         | addr(scale,T.ADD(_,c as T.CONST _,e)) = 
              AMode(DISPea(expr e,I.LabExp(c,I.F)))
         | addr(scale,T.ADD(_,e,T.LABEXP le)) = 
             let val rs = expr e
                 val (rt, opnd) = ldLabelEA le
             in  case (CB.registerId rt, opnd) of
                    (0, opnd) => AMode(DISPea(rs,opnd))
                 |  (_,I.IMMED 0) => AMode(INDXea(rs,rt))
                 |  (_,opnd) => 
                     let val tmp = newReg()
                     in  emit(I.ARITH{a=I.ADD,r1=rs,r2=rt,t=tmp});
                         AMode(DISPea(tmp,opnd))
                     end
             end
         | addr(scale,T.ADD(t,e1 as T.LABEXP l,e2)) = addr(scale,T.ADD(t,e2,e1))
         | addr(scale,T.ADD(_,e1,e2)) = 
           let (* check for special multiply add sequence 
                * here, e1 is is scaled 
                *)
               fun scaleIndexed(actualScale,opcode,e1,e2) = 
               if actualScale = scale then (* can we use scaled indexing mode?*)
                   let val x = expr e1
                       val b = expr e2
                   in  AMode(INDXSCALEDea(b,x))
                   end
               else  (* no, use the SHnADD operator, then *)
                   let val tmp = newReg()
                   in  emit(I.ARITH{a=opcode,r1=expr e1,r2=expr e2,t=tmp});
                       AMode(DISPea(tmp,zeroImmed))
                   end
           in  case times e1 of
                 (TIMES2,e1) => scaleIndexed(16,I.SH1ADD,e1,e2)
               | (TIMES4,e1) => scaleIndexed(32,I.SH2ADD,e1,e2)
               | (TIMES8,e1) => scaleIndexed(64,I.SH3ADD,e1,e2)
               | _ => 
		 case times e2 of
		   (TIMES2,e2) => scaleIndexed(16,I.SH1ADD,e2,e1)
		 | (TIMES4,e2) => scaleIndexed(32,I.SH2ADD,e2,e1)
		 | (TIMES8,e2) => scaleIndexed(64,I.SH3ADD,e2,e1)
		 | _ => AMode(INDXea(expr e1,expr e2))
           end
         | addr(scale,T.SUB(ty,e,T.LI n)) = addr(scale,T.ADD(ty,e,T.LI(T.I.NEGT(32,n))))
         | addr(scale,T.LABEXP lexp)     = AMode(DISPea(ldLabelEA(lexp)))
         | addr(scale,ea)                = AMode(DISPea(expr ea,zeroImmed))
 
       (* emit an integer load 
        * li - load immediate, 
        * l  - load indexed
        * ls - load indexed with scaling
        * r1 is base r2 is x
        *)
       and load(scale,li,l,ls,ea,t,mem,an) = 
           case addr(scale,ea) 
	   of DISP(r, off) =>
                 if im14 off then
                    mark(I.LOADI{li=li,r=r,i=I.IMMED(toInt off),t=t,mem=mem},an)
                 else
                    mark(I.LOAD{l=l,r1=r,r2=immed off,t=t,mem=mem},an)
            | AMode(DISPea(r,i)) => mark(I.LOADI{li=li,r=r,i=i,t=t,mem=mem},an) 
            | AMode(INDXea(r1,r2)) => mark(I.LOAD{l=l,r1=r1,r2=r2,t=t,mem=mem},an) 
            | AMode(INDXSCALEDea(b,x)) => mark(I.LOAD{l=ls,r1=b,r2=x,t=t,mem=mem},an)
 
       (* emit an integer store *)
       and store(st,ea,r,mem,an) =
           let val (b,d) =
               case addr(0,ea) 
                of DISP(b, disp) =>
                    if im14 disp then (b,I.IMMED(toInt disp) )
                    else let val {hi,lo} = split11 disp
                             val tmp1    = newReg()
                             val tmp2    = newReg()
                         in  emit(I.LDIL{i=I.IMMED hi,t=tmp1});
                             emit(I.ARITH{a=I.ADD,r1=b,r2=tmp1,t=tmp2});
                             (tmp2,I.IMMED lo)
                         end  
                | AMode(DISPea bd) => bd
                | AMode(INDXea(r1,r2)) => 
                    let val tmp = newReg()
                    in  emit(I.ARITH{a=I.ADD,r1=r1,r2=r2,t=tmp});
                        (tmp,I.IMMED 0)
                    end
                | AMode(INDXSCALEDea _) => error "store"
           in  mark(I.STORE{st=st,b=b,d=d,r=r,mem=mem},an) end

       (* emit a floating point load *)
       and fload(scale,fl,flx,flxs,ea,t,mem,an) =
           case addr(scale,ea) of
             AMode(INDXea(b,x)) => mark(I.FLOADX{flx=flx,b=b,x=x,t=t,mem=mem},an)
           | AMode(INDXSCALEDea(b,x)) => 
               mark(I.FLOADX{flx=flxs,b=b,x=x,t=t,mem=mem},an)
           | AMode(DISPea(b,d)) => 
               let val tmp = newReg()
               in  emit(I.ARITHI{ai=I.ADDI,r=b,i=d,t=tmp});
                   mark(I.FLOADX{flx=flx,b=tmp,x=zeroR,t=t,mem=mem},an)
               end
           | DISP(b, d) =>
               if im5 d then 
                  mark(I.FLOAD{fl=fl,b=b,d=toInt d,t=t,mem=mem},an)
               else
                  mark(I.FLOADX{flx=flx,b=b,x=immed d,t=t,mem=mem},an)
  
       (* emit a floating point store *)
       and fstore(scale,fst,fstx,fstxs,ea,data,mem,an) =
           let val r = fexpr data 
           in  case addr(scale,ea) of
                 DISP(b, d) => 
                   if im5 d then 
		     mark(I.FSTORE{fst=fst,b=b,d=(toInt d),r=r,mem=mem},an)
                   else mark(I.FSTOREX{fstx=fstx,b=b,x=immed d,r=r,mem=mem},an)
               | AMode(DISPea(b,d)) => 
                   let val tmp = newReg()
                   in  emit(I.ARITHI{ai=I.ADDI,r=b,i=d,t=tmp});
                       mark(I.FSTORE{fst=I.FSTDS,b=tmp,d=0,r=r,mem=mem},an)
                   end
               | AMode(INDXea(b,x)) => 
                   mark(I.FSTOREX{fstx=fstx,b=b,x=x,r=r,mem=mem},an)
               | AMode(INDXSCALEDea(b,x)) => 
                   mark(I.FSTOREX{fstx=fstxs,b=b,x=x,r=r,mem=mem},an)
           end
 
       (* emit an integer branch instruction *)
                              
         (* generate a branch *)  
       and branch(T.CMP(ty,cc,T.LI n,e),lab,an) = (* optimize cmp immed *) 
             emitBranchCmpWithImmed(ty,cc,n,e,lab,an)
         | branch(T.CMP(ty,cc,e1,e2 as T.LI _),lab,an) = (* commute *) 
             branch(T.CMP(ty,T.Basis.swapCond cc,e2,e1),lab,an)
         | branch(T.CMP(ty,cc,a,b),lab,an) = (* do the usual *)
             emitBranch(ty,cc,expr a,expr b,lab,an)
         | branch(T.FCMP(fty,cc,a,b),lab,an) =
           let val f1 = fexpr a
               val f2 = fexpr b
               val fallThrough = Label.anon()
               fun fcond T.==   = I.!=
                 | fcond T.?<>  = I.==
                 | fcond T.?    = I.<=>
                 | fcond T.<=>  = I.?
                 | fcond T.>    = I.?<=
                 | fcond T.>=   = I.?<
                 | fcond T.?>   = I.<=
                 | fcond T.?>=  = I.<
                 | fcond T.<    = I.?>=
                 | fcond T.<=   = I.?>
                 | fcond T.?<   = I.>=
                 | fcond T.?<=  = I.>
                 | fcond T.<>   = I.?=
                 | fcond T.?=   = I.<>
                 | fcond _      = error "fcond"
           in  mark(I.FBRANCH{cc=fcond cc,f1=f1,f2=f2,t=lab,f=fallThrough,
                              fmt=getFmt a,n=true,long=false},an);
               defineLabel fallThrough
           end
         | branch(e,lab,an) = error "branch: what is the semantics?" 

        (* generate a branch cmp with immed *)
       and emitBranchCmpWithImmed(ty,cc,n,e2 as T.ANDB(_,e,T.LI mask),t,an) = 
             emitBranchOnBit(ty,cc,n,e2,e,toWord32 mask,t,an)
         | emitBranchCmpWithImmed(ty,cc,n,e2 as T.ANDB(_,T.LI mask,e),t,an) = 
             emitBranchOnBit(ty,cc,n,e2,e,toWord32 mask,t,an)
         | emitBranchCmpWithImmed(ty,cc,n,e2,t,an) = 
             emitBranchI(ty,cc,n,e2,t,an)

        (* generate a branch on bit *)
       and emitBranchOnBit(ty,cc,n,e2,e,mask,t,an) =
           let fun isPowerOf2 w = W.andb(w,w-0w1) = 0w0
               fun log w = 
               let fun f(0w1,n) = n
                 | f(w,n) = f(W.>>(w,0w1),n+1)
               in  f(w,0) end
               val n' = toWord32 n
           in  if (n' = 0w0 orelse n' = mask) andalso
                  (cc = T.EQ orelse cc = T.NE) andalso
                  (mask > 0w0 andalso isPowerOf2 mask) then (* bit test! *)
                  let val bc = 
                          case (cc,n') of
                            (T.EQ,0w0) => I.BCLR (* bit is 0 *)
                          | (T.EQ,_)   => I.BSET (* bit is 1 *)
                          | (T.NE,0w0) => I.BSET (* bit is 1 *)
                          | (T.NE,_)   => I.BCLR (* bit is 0 *)
                          | _          => error "emitBranchOnBit"
                      val f = Label.anon()
                      val bit = 31 - log mask 
                  in  mark(I.BB{bc=bc,r=expr e,p=bit,t=t,f=f,
                                n=false, nop=true},an);
                      defineLabel f
                  end 
               else
                  emitBranchI(ty,cc,n,e2,t,an)
           end
                  
       (* generate a branch cmp with immediate *)
       and emitBranchI(ty,cc,n,e2,t,an) = 
           let val r2 = expr e2 
           in  if im5 n then
               let val f = Label.anon()
                   val (cmpi,bc) =
                       case cc of
                         T.LT  => (I.COMIBT, I.LT)
                       | T.LE  => (I.COMIBT, I.LE)
                       | T.GT  => (I.COMIBF, I.LE)
                       | T.GE  => (I.COMIBF, I.LT)
                       | T.EQ  => (I.COMIBT, I.EQ)
                       | T.LTU => (I.COMIBT, I.LTU)
                       | T.LEU => (I.COMIBT, I.LEU)
                       | T.GEU => (I.COMIBF, I.LTU)
                       | T.GTU => (I.COMIBF, I.LEU)
                       | T.NE  => (I.COMIBF, I.EQ)
                       | _     => error "emitBranchI"
               in  mark(I.BCONDI{cmpi=cmpi,bc=bc,i=toInt(n),r2=r2,t=t,f=f,
                                  n=false, nop=true},an);
                   defineLabel f
               end
               else emitBranch(ty,cc,immed n,r2,t,an)
           end

       (* generate a branch *)
       and emitBranch(ty,cond,r1,r2,t,an) = 
           let val f = Label.anon()
               val (cmp,bc,r1,r2) =
                    case cond of
                       T.LT  => (I.COMBT, I.LT, r1, r2)
                     | T.LE  => (I.COMBT, I.LE, r1, r2)
                     | T.GT  => (I.COMBT, I.LT, r2, r1)
                     | T.GE  => (I.COMBT, I.LE, r2, r1)
                     | T.EQ  => (I.COMBT, I.EQ, r1, r2)
                     | T.LTU => (I.COMBT, I.LTU, r1, r2)
                     | T.LEU => (I.COMBT, I.LEU, r1, r2)
                     | T.GEU => (I.COMBT, I.LEU, r2, r1)
                     | T.GTU => (I.COMBT, I.LTU, r2, r1)
                     | T.NE  => (I.COMBF, I.EQ, r1, r2)
                     | _     => error "emitBranch"
           in  mark(I.BCOND{cmp=cmp,bc=bc,r1=r1,r2=r2,t=t,f=f,
                            n=false,nop=true},an);
               defineLabel f
           end

       and getFmt e =
           case Gen.Size.fsize e of
             32  => I.SGL
           | 64  => I.DBL
           | 128 => I.QUAD
           | _   => error "getFmt"

       and goto(l,an) = mark(I.B{lab=l,n=true},an)

           (* generate code for a statement *)
       and stmt(T.MV(32,t,e),an) = doExpr(e,t,an)
         | stmt(T.FMV(64,t,e),an) = doFexpr(e,t,an)
         | stmt(T.CCMV(t,e),an) = doCCexpr(e,t,an)
         | stmt(T.COPY(32,dst,src),an) = copy(dst,src,an)
         | stmt(T.FCOPY(64,dst,src),an) = fcopy(dst,src,an)
         | stmt(T.JMP(T.LABEL l,_),an) = goto(l,an)
         | stmt(T.JMP(ea,labs),an) = jmp(ea,labs,an)
         | stmt(s as T.CALL { pops=0, ...},an) = call(s,an)
	 | stmt(T.CALL _, _) = error "pops<>0 not implemented"
         | stmt(T.RET _,an) = 
               mark(I.BV{labs=[],x=zeroR,b=C.returnPtr,n=true},an)
         | stmt(T.STORE(8,ea,t,mem),an) = store(I.STB,ea,expr t,mem,an)
         | stmt(T.STORE(16,ea,t,mem),an) = store(I.STH,ea,expr t,mem,an)
         | stmt(T.STORE(32,ea,t,mem),an) = store(I.STW,ea,expr t,mem,an)
         | stmt(T.FSTORE(32,ea,t,mem),an) = 
              fstore(32,I.FSTWS,I.FSTWX,I.FSTWX_S,ea,t,mem,an)
         | stmt(T.FSTORE(64,ea,t,mem),an) = 
              fstore(64,I.FSTDS,I.FSTDX,I.FSTDX_S,ea,t,mem,an)
         | stmt(T.BCC(cc,lab),an) = branch(cc,lab,an)
         | stmt(T.DEFINE l,_) = defineLabel l
         | stmt(T.LIVE S,an) = mark'(I.LIVE{regs=cellset S,spilled=C.empty},an)
         | stmt(T.KILL S,an) = mark'(I.KILL{regs=cellset S,spilled=C.empty},an)
         | stmt(T.ANNOTATION(i,a),an) = stmt(i,a::an)
         | stmt(T.EXT s,an) = 
              ExtensionComp.compileSext (reducer()) {stm=s, an=an}
         | stmt(s,_) = doStmts(Gen.compileStm s)

       and doStmt s = stmt(s,[])
       and doStmts ss = app doStmt ss

       and jmp(e,labs,an) = let
	     fun disp(r, i) = let
	       val b = newReg()
	     in emit(I.ARITHI{ai=I.ADDI, i=i, r=r, t=b});
	        (b, zeroR)
	     end

             val (b,x) = 
               case addr(32,e) of
		 DISP(b, i) => 
		   if i = 0 then (b, zeroR) else disp(b, I.IMMED(toInt i))
               | AMode(DISPea(r,i)) => disp(r, i) 
               | AMode(INDXea(r1,r2)) => let val b=newReg()
                                  in  emit(I.ARITH{a=I.ADD,r1=r1,r2=r2,t=b});
                                      (b,zeroR)
                                  end
               | AMode(INDXSCALEDea(b,x)) => (b,x)
           in mark(I.BV{b=b,x=x,n=true,labs=labs},an) end

       and call(s,an) = let val reduce = {stm=doStmt, rexp=expr, emit=emitInstruction}
                        in  LC.doCall(reduce,s) end

           (* Optimize addition *)
       and plus(times,sh1add,sh2add,sh3add,add,addi,a,b,t,an) =
           case times a of
              (TIMES2,a) => arith(sh1add,a,b,t,an)
           |  (TIMES4,a) => arith(sh2add,a,b,t,an)
           |  (TIMES8,a) => arith(sh3add,a,b,t,an)
           |  _ =>
           case times b of
              (TIMES2,b) => arith(sh1add,b,a,t,an)
           |  (TIMES4,b) => arith(sh2add,b,a,t,an)
           |  (TIMES8,b) => arith(sh3add,b,a,t,an)
           |  _          => commImmedArith(add,addi,a,b,t,an)

           (* Round to zero for division:
            * d <- r + i
            * d <- if r >= 0 then r else d
            *)
       and divu32 x = Mulu32.divide{mode=T.TO_ZERO,stm=doStmt} x
       and divs32 x = Muls32.divide{mode=T.TO_ZERO,stm=doStmt} x
       and divt32 x = Mult32.divide{mode=T.TO_ZERO,stm=doStmt} x
       
       and muldiv(ty,genConst,milliFn,a,b,t,commute,an) =
           let fun const(a,i) =  
               let val r = expr a 
               in  app emitInstruction (genConst{r=r,i=toInt i,d=t})
                      handle _ => milliCall(milliFn,T.REG(ty,r),T.LI i,t)
               end
           in  case (commute,a,b) of
                 (_,a,T.LI i)      => const(a,i)
               | (true,T.LI i,a)   => const(a,i)
               | (_,a,b)           => milliCall(milliFn,a,b,t)
           end 

           (* compile shift *)
       and shift(immedShift,varShift,e,T.LI n,t,an) = let
	      val n = toInt n
	   in
             if n < 0 orelse n > 31 then error "shift"
             else mark(I.SHIFT{s=immedShift,r=expr e,p=31-n,len=32-n,t=t},an)
           end
         | shift(immedShift,varShift,e1,e2,t,an) =
             let val r1 = expr e1
                 val r2 = expr e2
                 val tmp = newReg()
             in  emit(I.ARITHI{ai=I.SUBI, i=I.IMMED 31, r=r2, t=tmp});
                 emit(I.MTCTL{r=tmp, t=CRReg 11});
                 mark(I.SHIFTV{sv=varShift,r=r1,len=32, t=t},an)
             end

           (* Generate a COMCLR_LDO/COMICLR_LDO instruction sequence: 
            *  COMCLR,cond r1, r2, t1
            *  LDO i(b), t2 
            * 
            * Note: 
            *    t <- if cond(r1,r2) then i else 0 can be mapped into:
            *
            *    COMCLR,cond r1, r2, t
            *    LDO i(0), t
            *
            *    if cond(r1,r2) then t <- e can be mapped into:
            *
            *    t' <- e
            *    COMCLR,cond r1, r2, 0
            *    LDO 0(t'), t
            *   
            *    t <- if cond(r1,r2) then e1 else e2 can be mapped into:
            *
            *    t <- e2
            *    t' <- e1
            *    COMCLR,cond r1, r2, 0
            *    LDO 0(t'), t
            *)
       and comclr(cond,x,y,yes,no,t,an) = 
           let val (cond, i1, r2) = 
                   case (opn x, opn y) of
                     (x, I.REG r2) => (cond, x, r2)
                   | (I.REG r1, y) => (T.Basis.swapCond cond, y, r1)
                   | (x, y)        => (cond, x, reduceOpn y)
               val cc = case cond of
                          T.LT  => I.GE
                        | T.LE  => I.GT
                        | T.GT  => I.LE
                        | T.GE  => I.LT
                        | T.EQ  => I.NE
                        | T.LTU => I.GEU
                        | T.LEU => I.GTU
                        | T.GEU => I.LTU
                        | T.GTU => I.LEU
                        | T.NE  => I.EQ
                        | _     => error "comclr"
               val tmp = newReg()
               val (b,i) = 
                  case yes 
                  of T.LI n   => if im14 n then (zeroR, toInt(n)) else 
                                let val {hi,lo} = split11 n
                                    val b = newReg()
                                in  emit(I.LDIL{i=I.IMMED hi,t=b}); (b,lo) end
                   | e        => (expr e, 0)
		  (*esac*)

               val t1 =
                  case no 
                  of T.LI z =>			     (* false case is zero *)
		      if z = 0 then tmp else  (doExpr(no,tmp,[]); zeroR)
                   | _ => (doExpr(no,tmp,[]); zeroR) (* move false case to tmp *)
                  (*esac*)

               val instr =
                  case i1 of
                    I.REG r1 => 
                      I.COMCLR_LDO{cc=cc,r1=r1,r2=r2,b=b,i=i,t1=t1,t2=tmp}
                  | _ => I.COMICLR_LDO{cc=cc,i1=i1,r2=r2,b=b,i2=i,t1=t1,t2=tmp}
           in  mark(instr, an);
               move(tmp, t, [])
           end

           (* convert an expression into a register *) 
       and expr(exp) = let
	 fun comp() = let
	   val t = newReg()
         in doExpr(exp, t, []); t
         end
       in 
	 case exp
	 of T.REG(_, r) => r
          | T.LI z => if z = 0 then zeroR else comp()
	  | _ => comp()
       end
 
           (* compute an integer expression and put the result in register t *)
       and doExpr(e,t,an) =
           case e of
             T.REG(_,r) => move(r,t,an)
           | T.LI n     => loadImmed(n,t,an)
           | T.LABEXP le => 
                (case ldLabelOpnd{label=le,pref=SOME t} of
                   I.REG r => move(r,t,an)
                 | opnd => mark(I.LDO{i=opnd,b=zeroR,t=t},an)
                )
           | T.CONST _  => loadConst(e,t,an)
           | T.LABEL _  => loadConst(e,t,an)
           | T.ADD(_,a,b) => plus(times,
                                  I.SH1ADDL,I.SH2ADDL,I.SH3ADDL,I.ADD,I.ADDI,
                                  a,b,t,an) 
	   | T.SUB(_,a,T.LI mi) => 
	      if mi = 0 then doExpr(a,t,an)
	      else commImmedArith(I.ADD,I.ADDI,a,T.LI(T.I.NEGT(32,mi)),t,an)
           | T.SUB(_,a,b) => immedArith(I.SUB,I.SUBI,a,b,t,an)
           | T.ADDT(_,a,b) => plus(timest,
                                 I.SH1ADDO,I.SH2ADDO,I.SH3ADDO,I.ADDO,I.ADDIO,
                                 a,b,t,an) 
           | T.SUBT(_,a,T.LI n) => 
                    commImmedArith(I.ADDO,I.ADDIO,a,T.LI(T.I.NEGT(32,n)),t,an)
           | T.SUBT(_,a,b) => immedArith(I.SUBO,I.SUBIO,a,b,t,an)

           | T.ANDB(_,a,T.NOTB(_,b)) => arith(I.ANDCM,a,b,t,an)
           | T.ANDB(_,T.NOTB(_,a),b) => arith(I.ANDCM,b,a,t,an)
           | T.ANDB(_,a,b) => arith(I.AND,a,b,t,an)
           | T.ORB(_,a,b)  => arith(I.OR,a,b,t,an)
           | T.XORB(_,a,b) => arith(I.XOR,a,b,t,an)

           | T.SLL(_,a,b)  => shift(I.ZDEP,I.ZVDEP,a,b,t,an)
           | T.SRL(_,a,b)  => shift(I.EXTRU,I.VEXTRU,a,b,t,an)
           | T.SRA(_,a,b)  => shift(I.EXTRS,I.VEXTRS,a,b,t,an)
           | T.MULU(32,a,b) => muldiv(32,Mulu32.multiply,MC.mulu,a,b,t,true,an)
           | T.MULS(32,a,b) => muldiv(32,Muls32.multiply,MC.mulu,a,b,t,true,an)
           | T.MULT(32,a,b) => muldiv(32,Mult32.multiply,MC.mulo,a,b,t,true,an)
           | T.DIVU(32,a,b)  => muldiv(32,divu32,MC.divu,a,b,t,false,an)
(* FIXME: The following is a hack:  We use the trapping div in place of
 *        the non-trapping since we currently expect that the non-trapping
 *        div will only be used where there is some high-level reasoning
 *        that the trapping div would in fact not trap.  *)
           | T.DIVS(T.DIV_TO_ZERO,32,a,b) =>
	                        muldiv(32,divs32,MC.divo,a,b,t,false,an)
           | T.DIVT(T.DIV_TO_ZERO,32,a,b) =>
	                        muldiv(32,divt32,MC.divo,a,b,t,false,an)

           | T.LOAD(8,ea,mem) => load(8,I.LDB,I.LDBX,I.LDBX,ea,t,mem,an)
           | T.LOAD(16,ea,mem) => load(16,I.LDH,I.LDHX,I.LDHX_S,ea,t,mem,an)
           | T.LOAD(32,ea,mem) => load(32,I.LDW,I.LDWX,I.LDWX_S,ea,t,mem,an)

           | T.COND(_,T.CMP(_,cond,x,y),yes,no) => comclr(cond,x,y,yes,no,t,an)
           | T.LET(s,e) => (doStmt s; doExpr(e, t, an))
           | T.MARK(e,A.MARKREG f) => (f t; doExpr(e,t,an))
           | T.MARK(e,a) => doExpr(e,t,a::an)
           | T.PRED(e,c) => doExpr(e,t,A.CTRLUSE c::an)
           | T.REXT e =>
               ExtensionComp.compileRext (reducer()) {e=e, rd=t, an=an}
           | e => doExpr(Gen.compileRexp e,t,an)
 
           (* convert an expression into a floating point register *) 
       and fexpr(T.FREG(_,r))    = r
         | fexpr e               = let val t = newFreg()
                                   in  doFexpr(e,t,[]); t end

           (* compute a floating point expression and put the result in t *)
       and doFexpr(e,t,an) =
           case e of
             (* single precision *)
             T.FREG(32,r)      => fmove(r,t,an)
           | T.FLOAD(32,ea,mem) =>
                  fload(32,I.FLDWS,I.FLDWX,I.FLDWX_S,ea,t,mem,an)
           | T.FADD(32,a,b)  => farith(I.FADD_S,a,b,t,an)
           | T.FSUB(32,a,b)  => farith(I.FSUB_S,a,b,t,an)
           | T.FMUL(32,a,b)  => farith(I.FMPY_S,a,b,t,an)
           | T.FDIV(32,a,b)  => farith(I.FDIV_S,a,b,t,an)
           | T.FABS(32,a)    => funary(I.FABS_S,a,t,an)
           | T.FSQRT(32,a)   => funary(I.FSQRT_S,a,t,an)
 
             (* double precision *)
           | T.FREG(64,r)    => fmove(r,t,an)
           | T.FLOAD(64,ea,mem) => 
                  fload(64,I.FLDDS,I.FLDDX,I.FLDDX_S,ea,t,mem,an)
           | T.FADD(64,a,b)  => farith(I.FADD_D,a,b,t,an)
           | T.FSUB(64,a,b)  => farith(I.FSUB_D,a,b,t,an)
           | T.FMUL(64,a,b)  => farith(I.FMPY_D,a,b,t,an)
           | T.FDIV(64,a,b)  => farith(I.FDIV_D,a,b,t,an)
           | T.FABS(64,a)    => funary(I.FABS_D,a,t,an)
           | T.FSQRT(64,a)   => funary(I.FSQRT_D,a,t,an)

             (* conversions *)
           | T.CVTF2F(fty,fty',e) =>
               (case (fty,fty') of
                  (64,32) => fcnv(I.FCNVFF_SD,e,t,an)
                | (32,64) => fcnv(I.FCNVFF_DS,e,t,an)
                | (32,32) => doFexpr(e,t,an)
                | (64,64) => doFexpr(e,t,an)
                | _ => error "CVTF2F"
               )
           | T.CVTI2F(32,_,e) => app emitInstruction (MilliCode.cvti2s{rs=expr e,fd=t})
           | T.CVTI2F(64,_,e) => app emitInstruction (MilliCode.cvti2d{rs=expr e,fd=t})

             (* negation is implemented as subtraction *)
           | T.FNEG(ty,a)    => doFexpr(T.FSUB(ty,T.FREG(ty,zeroF),a),t,an)

           | T.FMARK(e,A.MARKREG f) => (f t; doFexpr(e,t,an))
           | T.FMARK(e,a) => doFexpr(e,t,a::an)
           | T.FPRED(e,c) => doFexpr(e,t,A.CTRLUSE c::an)
           | T.FEXT e => 
               ExtensionComp.compileFext (reducer()) {e=e, fd=t, an=an}
           | e => error "doFexpr"
 
       and doCCexpr(T.CC(_,r),t,an)  = move(r,t,an)
         | doCCexpr(T.FCC(_,r),t,an)  = move(r,t,an)
         | doCCexpr(T.CMP(ty,cond,e1,e2),t,an) = error "doCCexpr"
         | doCCexpr(T.CCMARK(e,A.MARKREG f),t,an) = (f t; doCCexpr(e,t,an))
         | doCCexpr(T.CCMARK(e,a),t,an) = doCCexpr(e,t,a::an)
         | doCCexpr(T.CCEXT e,t,an) = 
              ExtensionComp.compileCCext (reducer()) {e=e,ccd=t,an=an}
         | doCCexpr e = error "doCCexpr"
 
       and ccExpr(T.CC(_,r)) = r
         | ccExpr(T.FCC(_,r)) = r
         | ccExpr e = let val t = newReg() in doCCexpr(e,t,[]); t end

           (* convert an expression into an operand *) 
       and opn(c as T.CONST _) = I.LabExp(c,I.F)
         | opn(l as T.LABEL _) = I.LabExp(l,I.F)
         | opn(T.LABEXP le)    = ldLabelOpnd{label=le,pref=NONE}
         | opn(e as T.LI n)    = if im11 n then I.IMMED(toInt n)
                                 else I.REG(expr e)
         | opn e               = I.REG(expr e)

       and addrOf e = 
	 case addr(0, e)
	 of AMode mode => mode
          | DISP(r, mi) => DISPea(r, I.IMMED(toInt mi))

       and reducer() = 
          TS.REDUCER{reduceRexp    = expr,
		     reduceFexp    = fexpr,
		     reduceCCexp   = ccExpr,
		     reduceStm     = stmt,
		     operand       = opn,
		     reduceOperand = reduceOpn,
		     addressOf     = addrOf,
		     emit          = mark',
		     instrStream   = instrStream,
		     mltreeStream  = self()
                   }
 
       (* convert mlrisc to cellset: 
        * condition code registers are mapped onto general registers 
        *)
       and cellset mlrisc =
           let fun g([],acc) = acc
                 | g(T.GPR(T.REG(_,r))::regs,acc)  = g(regs,C.addReg(r,acc))
                 | g(T.FPR(T.FREG(_,f))::regs,acc) = g(regs,C.addFreg(f,acc))
                 | g(T.CCR(T.CC(_,cc))::regs,acc)  = g(regs,C.addReg(cc,acc))
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
