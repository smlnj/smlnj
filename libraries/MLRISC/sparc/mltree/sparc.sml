(* sparc.sml
 *
 * COPYRIGHT (c) 2002 Bell Labs, Lucent Technologies
 *
 * This is a new instruction selection module for Sparc, 
 * using the new instruction representation and the new MLTREE representation.
 * Support for V9 has been added.
 *
 * The cc bit in arithmetic op are now embedded within the arithmetic
 * opcode.  This should save some space.
 *
 * -- Allen
 *)

functor Sparc
  (structure SparcInstr : SPARCINSTR
   structure PseudoInstrs : SPARC_PSEUDO_INSTR 
   			where I = SparcInstr
   structure ExtensionComp : MLTREE_EXTENSION_COMP
   			where I = SparcInstr
			  and T = SparcInstr.T

			  
   (* 
    * The client should also specify these parameters.
    * These are the estimated cost of these instructions.
    * The code generator will use alternative sequences that are
    * cheaper when their costs are lower.
    *)
   val muluCost : int ref  (* cost of unsigned multiplication in cycles *)
   val divuCost : int ref (* cost of unsigned division in cycles *)
   val multCost : int ref (* cost of trapping/signed multiplication in cycles *)
   val divtCost : int ref (* cost of trapping/signed division in cycles *)

   (*
    * If you don't want to use register windows at all, set this to false.
    *) 
   val registerwindow : bool ref (* should we use register windows? *)

   val V9 : bool (* should we use V9 instruction set? *)
   val useBR : bool ref
        (* should we use the BR instruction (when in V9)?
         * I think it is a good idea to use it.
         *)
  ) : MLTREECOMP = 
struct
  structure I  = SparcInstr
  structure T  = I.T
  structure TS = ExtensionComp.TS
  structure R  = T.Region
  structure C  = I.C
  structure CB = CellsBasis
  structure W  = Word32
  structure P  = PseudoInstrs
  structure A  = MLRiscAnnotations
  structure CFG = ExtensionComp.CFG

  type instrStream = (I.instruction, C.cellset, CFG.cfg) TS.stream
  type mltreeStream = (T.stm, T.mlrisc list, CFG.cfg) TS.stream

  fun toInt n = T.I.toInt(32, n)
  fun LI i = T.LI(T.I.fromInt(32, i))
  fun LT (n,m) = T.I.LT(32, n, m)
  fun LE (n,m) = T.I.LE(32, n, m)
  fun COPY{dst, src, tmp} = 
      I.COPY{k=CB.GP, sz=32, dst=dst, src=src, tmp=tmp}
  fun FCOPY{dst, src, tmp} = 
      I.COPY{k=CB.FP, sz=64, dst=dst, src=src, tmp=tmp}

  val intTy = if V9 then 64 else 32
  structure Gen = MLTreeGen(structure T = T
			    structure Cells = C
                            val intTy = intTy
                            val naturalWidths = if V9 then [32,64] else [32]
                            datatype rep = SE | ZE | NEITHER
                            val rep = NEITHER 
                           )

  functor Multiply32 = MLTreeMult
    (structure I = I
     structure T = T
     structure CB = CellsBasis
     type arg  = {r1:CB.cell,r2:CB.cell,d:CB.cell}
     type argi = {r:CB.cell,i:int,d:CB.cell}
  
     val intTy = 32    
     fun mov{r,d} = COPY{dst=[d],src=[r],tmp=NONE}
     fun add{r1,r2,d} = I.arith{a=I.ADD,r=r1,i=I.REG r2,d=d}
     fun slli{r,i,d} = [I.shift{s=I.SLL,r=r,i=I.IMMED i,d=d}]
     fun srli{r,i,d} = [I.shift{s=I.SRL,r=r,i=I.IMMED i,d=d}]
     fun srai{r,i,d} = [I.shift{s=I.SRA,r=r,i=I.IMMED i,d=d}]
    )

  functor Multiply64 = MLTreeMult
    (structure I = I
     structure T = T
     structure CB = CellsBasis
     type arg  = {r1:CB.cell,r2:CB.cell,d:CB.cell}
     type argi = {r:CB.cell,i:int,d:CB.cell}
      
     val intTy = 64    
     fun mov{r,d} = COPY{dst=[d],src=[r],tmp=NONE}
     fun add{r1,r2,d} = I.arith{a=I.ADD,r=r1,i=I.REG r2,d=d}
     fun slli{r,i,d} = [I.shift{s=I.SLLX,r=r,i=I.IMMED i,d=d}]
     fun srli{r,i,d} = [I.shift{s=I.SRLX,r=r,i=I.IMMED i,d=d}]
     fun srai{r,i,d} = [I.shift{s=I.SRAX,r=r,i=I.IMMED i,d=d}]
    )

  (* signed, trapping version of multiply and divide *)
  structure Mult32 = Multiply32
    (val trapping = true
     val multCost = multCost 
     fun addv{r1,r2,d} = 
         I.arith{a=I.ADDCC,r=r1,i=I.REG r2,d=d}::PseudoInstrs.overflowtrap32 
     fun subv{r1,r2,d} = 
         I.arith{a=I.SUBCC,r=r1,i=I.REG r2,d=d}::PseudoInstrs.overflowtrap32 
     val sh1addv = NONE 
     val sh2addv = NONE 
     val sh3addv = NONE 
    )
    (val signed = true)

  (* unsigned, non-trapping version of multiply and divide *)
  functor Mul32 = Multiply32
    (val trapping = false
     val multCost = muluCost
     fun addv{r1,r2,d} = [I.arith{a=I.ADD,r=r1,i=I.REG r2,d=d}]
     fun subv{r1,r2,d} = [I.arith{a=I.SUB,r=r1,i=I.REG r2,d=d}]
     val sh1addv = NONE 
     val sh2addv = NONE 
     val sh3addv = NONE 
    )
  structure Mulu32 = Mul32(val signed = false)

  structure Muls32 = Mul32(val signed = true)

  (* signed, trapping version of multiply and divide *)
  structure Mult64 = Multiply64
    (val trapping = true
     val multCost = multCost 
     fun addv{r1,r2,d} = 
         I.arith{a=I.ADDCC,r=r1,i=I.REG r2,d=d}::PseudoInstrs.overflowtrap64 
     fun subv{r1,r2,d} = 
         I.arith{a=I.SUBCC,r=r1,i=I.REG r2,d=d}::PseudoInstrs.overflowtrap64 
     val sh1addv = NONE 
     val sh2addv = NONE 
     val sh3addv = NONE 
    )
    (val signed = true)

  (* unsigned, non-trapping version of multiply and divide *)
  functor Mul64 = Multiply64
    (val trapping = false
     val multCost = muluCost
     fun addv{r1,r2,d} = [I.arith{a=I.ADD,r=r1,i=I.REG r2,d=d}]
     fun subv{r1,r2,d} = [I.arith{a=I.SUB,r=r1,i=I.REG r2,d=d}]
     val sh1addv = NONE 
     val sh2addv = NONE 
     val sh3addv = NONE 
    )
  structure Mulu64 = Mul64(val signed = false)

  structure Muls64 = Mul64(val signed = true)

  datatype commutative = COMMUTE | NOCOMMUTE
  datatype cc = REG    (* write to register *)
              | CC     (* set condition code *)
              | CC_REG (* do both *)

  fun error msg = MLRiscErrorMsg.error("Sparc",msg)



  fun selectInstructions
       (instrStream as
        TS.S.STREAM{emit=emitInstruction,defineLabel,entryLabel,pseudoOp,annotation,getAnnotations,
                 beginCluster,endCluster,exitBlock,comment,...}) =
  let
      val emit		 = emitInstruction o I.INSTR
      (* Flags *)
      val useBR          = !useBR
      val registerwindow = !registerwindow

      val trap32  = PseudoInstrs.overflowtrap32 
      val trap64  = PseudoInstrs.overflowtrap64 
      val zeroR   = C.r0
      val newReg  = C.newReg
      val newFreg = C.newFreg
      fun immed13 n = LE(~4096, n) andalso LT(n, 4096)
      fun immed13w w = let val x = W.~>>(w,0w12)
                       in  x = 0w0 orelse (W.notb x) = 0w0 end
      fun splitw w = {hi=W.toInt(W.>>(w,0w10)),lo=W.toInt(W.andb(w,0wx3ff))}
      fun split n  = splitw(T.I.toWord32(32, n))

     
      val zeroOpn = I.REG zeroR (* zero value operand *)

      fun cond T.LT  = I.BL
        | cond T.LTU = I.BCS
        | cond T.LE  = I.BLE
        | cond T.LEU = I.BLEU
        | cond T.EQ  = I.BE
        | cond T.NE  = I.BNE
        | cond T.GE  = I.BGE
        | cond T.GEU = I.BCC
        | cond T.GT  = I.BG
        | cond T.GTU = I.BGU
        | cond _     = error "cond"

      fun rcond T.LT  = I.RLZ
        | rcond T.LE  = I.RLEZ
        | rcond T.EQ  = I.RZ
        | rcond T.NE  = I.RNZ
        | rcond T.GE  = I.RGEZ
        | rcond T.GT  = I.RGZ
        | rcond _ = error "rcond"

      fun signedCmp(T.LT | T.LE | T.EQ | T.NE | T.GE | T.GT) = true
        | signedCmp _ = false

      fun fcond T.==  = I.FBE
        | fcond T.?<> = I.FBNE
        | fcond T.?   = I.FBU
        | fcond T.<=> = I.FBO
        | fcond T.>   = I.FBG
        | fcond T.>=  = I.FBGE
        | fcond T.?>  = I.FBUG
        | fcond T.?>= = I.FBUGE
        | fcond T.<   = I.FBL
        | fcond T.<=  = I.FBLE
        | fcond T.?<  = I.FBUL
        | fcond T.?<= = I.FBULE
        | fcond T.<>  = I.FBLG
        | fcond T.?=  = I.FBUE
        | fcond fc = error("fcond "^T.Basis.fcondToString fc)

      fun annotate(i,[]) = i
        | annotate(i,a::an) = annotate(I.ANNOTATION{i=i,a=a},an)
      fun mark'(i,an) = emitInstruction(annotate(i,an)) 
      fun mark(i,an) = emitInstruction(annotate(I.INSTR i,an)) 

      (* convert an operand into a register *)
      fun reduceOpn(I.REG r) = r
        | reduceOpn(I.IMMED 0) = zeroR
        | reduceOpn i = 
          let val d = newReg() 
          in  emit(I.ARITH{a=I.OR,r=zeroR,i=i,d=d}); d end

      (* emit parallel copies *)
      fun copy(dst,src,an) =
         mark'(COPY{dst=dst,src=src,
                    tmp=case dst of [_] => NONE
                               | _ => SOME(I.Direct(newReg()))},an)
      fun fcopy(dst,src,an) =
         mark'(FCOPY{dst=dst,src=src,
                     tmp=case dst of [_] => NONE
                                 | _ => SOME(I.FDirect(newFreg()))},an)

      (* move register s to register d *)
      fun move(s,d,an) =
          if CB.sameColor(s,d) orelse CB.registerId d = 0 then ()
          else mark'(COPY{dst=[d],src=[s],tmp=NONE},an)

      (* move floating point register s to register d *)
      fun fmoved(s,d,an) =
          if CB.sameColor(s,d) then ()
          else mark'(FCOPY{dst=[d],src=[s],tmp=NONE},an)
      fun fmoves(s,d,an) = fmoved(s,d,an) (* error "fmoves" for now!!! XXX *)
      fun fmoveq(s,d,an) = error "fmoveq"

      (* load immediate *)
      and loadImmed(n,d,cc,an) =
      let val or = if cc <> REG then I.ORCC else I.OR
      in  if immed13 n then mark(I.ARITH{a=or,r=zeroR,i=I.IMMED(toInt n),d=d},an)
          else let val {hi,lo} = split n
               in  if lo = 0 then 
                      (mark(I.SETHI{i=hi,d=d},an); genCmp0(cc,d))
                   else let val t = newReg()
                        in  emit(I.SETHI{i=hi,d=t});
                            mark(I.ARITH{a=or,r=t,i=I.IMMED lo,d=d},an)
                        end
               end
      end

      (* load label expression *)
      and loadLabel(lab,d,cc,an) = 
      let val or = if cc <> REG then I.ORCC else I.OR 
      in  mark(I.ARITH{a=or,r=zeroR,i=I.LAB lab,d=d},an) end

      (* emit an arithmetic op *)
      and arith(a,acc,e1,e2,d,cc,comm,trap,an) = 
      let val (a,d) = case cc of
                         REG    => (a,d)
                      |  CC     => (acc,zeroR)
                      |  CC_REG => (acc,d)
      in  case (opn e1,opn e2,comm) of
            (i,I.REG r,COMMUTE)=> mark(I.ARITH{a=a,r=r,i=i,d=d},an)
          | (I.REG r,i,_)      => mark(I.ARITH{a=a,r=r,i=i,d=d},an)
          | (r,i,_)            => mark(I.ARITH{a=a,r=reduceOpn r,i=i,d=d},an)
          ;
          case trap of [] => () | _ => app emitInstruction trap 
      end   

      (* emit a shift op *)
      and shift(s,e1,e2,d,cc,an) = 
         (mark(I.SHIFT{s=s,r=expr e1,i=opn e2,d=d},an);
          genCmp0(cc,d)
         )

      (* emit externally defined multiply or division operation (V8) *)
      and extarith(gen,genConst,e1,e2,d,cc,comm) =
          let fun nonconst(e1,e2) = 
                  case (opn e1,opn e2,comm) of
                    (i,I.REG r,COMMUTE) => gen({r=r,i=i,d=d},reduceOpn)
                  | (I.REG r,i,_) => gen({r=r,i=i,d=d},reduceOpn)
                  | (r,i,_) => gen({r=reduceOpn r,i=i,d=d},reduceOpn)
              fun const(e,i) = 
                  let val r = expr e
                  in  genConst{r=r,i=toInt i,d=d}
                      handle _ => gen({r=r,i=opn(T.LI i),d=d},reduceOpn)
                 end
              val instrs =
                 case (comm,e1,e2) of
                   (_,e1,T.LI i) => const(e1,i)
                 | (COMMUTE,T.LI i,e2) => const(e2,i)
                 |  _ => nonconst(e1,e2)
          in  app emitInstruction instrs; 
              genCmp0(cc,d)
          end

      (* emit 64-bit multiply or division operation (V9) *)
      and muldiv64(a,genConst,e1,e2,d,cc,comm,an) =
          let fun nonconst(e1,e2) = 
                 [annotate( 
                  case (opn e1,opn e2,comm) of
                    (i,I.REG r,COMMUTE) => I.arith{a=a,r=r,i=i,d=d}
                  | (I.REG r,i,_) => I.arith{a=a,r=r,i=i,d=d}
                  | (r,i,_) => I.arith{a=a,r=reduceOpn r,i=i,d=d},an)
                 ]
              fun const(e,i) = 
                  let val r = expr e
                  in  genConst{r=r,i=toInt i,d=d}
                      handle _ => [annotate(I.arith{a=a,r=r,i=opn(T.LI i),d=d},an)]
                  end
              val instrs =
                 case (comm,e1,e2) of
                   (_,e1,T.LI i) => const(e1,i)
                 | (COMMUTE,T.LI i,e2) => const(e2,i)
                 |  _ => nonconst(e1,e2)
          in  app emitInstruction instrs; 
              genCmp0(cc,d)
          end

          (* divisions *)
      and divu32 x = Mulu32.divide{mode=T.TO_ZERO,stm=doStmt} x
      and divs32 x = Muls32.divide{mode=T.TO_ZERO,stm=doStmt} x
      and divt32 x = Mult32.divide{mode=T.TO_ZERO,stm=doStmt} x
      and divu64 x = Mulu64.divide{mode=T.TO_ZERO,stm=doStmt} x
      and divs64 x = Muls64.divide{mode=T.TO_ZERO,stm=doStmt} x
      and divt64 x = Mult64.divide{mode=T.TO_ZERO,stm=doStmt} x

      (* emit an unary floating point op *)
      and funary(a,e,d,an) = mark(I.FPop1{a=a,r=fexpr e,d=d},an)

      (* emit a binary floating point op *)
      and farith(a,e1,e2,d,an) = 
          mark(I.FPop2{a=a,r1=fexpr e1,r2=fexpr e2,d=d},an)

      (* convert an expression into an addressing mode *)
      and addr(T.ADD(ty, (T.ADD (_, e, T.LI n)|
			  T.ADD (_, T.LI n, e)), T.LI n')) =
	  addr(T.ADD (ty, e, T.LI (T.I.ADD (ty, n, n'))))
	| addr(T.ADD(ty, T.SUB (_, e, T.LI n), T.LI n')) =
	  addr(T.ADD (ty, e, T.LI (T.I.SUB (ty, n', n))))
	| addr(T.ADD(_,e,T.LI n)) = 
          if immed13 n then (expr e,I.IMMED(toInt n))
          else let val d = newReg()
               in  loadImmed(n,d,REG,[]); (d,opn e) end
        | addr(T.ADD(_,e,x as T.CONST c)) = (expr e,I.LAB x)
        | addr(T.ADD(_,e,x as T.LABEL l)) = (expr e,I.LAB x)
        | addr(T.ADD(_,e,T.LABEXP x)) = (expr e,I.LAB x)
        | addr(T.ADD(ty,i as T.LI _,e)) = addr(T.ADD(ty,e,i))
        | addr(T.ADD(_,x as T.CONST c,e)) = (expr e,I.LAB x)
        | addr(T.ADD(_,x as T.LABEL l,e)) = (expr e,I.LAB x)
        | addr(T.ADD(_,T.LABEXP x,e)) = (expr e,I.LAB x)
        | addr(T.ADD(_,e1,e2))       = (expr e1,I.REG(expr e2))
        | addr(T.SUB(ty,e,T.LI n))   = addr(T.ADD(ty,e,T.LI(T.I.NEG(32,n))))
        | addr(x as T.LABEL l)       = (zeroR,I.LAB x)
        | addr(T.LABEXP x)           = (zeroR,I.LAB x)
        | addr a                     = (expr a,zeroOpn)

      (* emit an integer load *)
      and load(l,a,d,mem,cc,an) = 
          let val (r,i) = addr a
          in  mark(I.LOAD{l=l,r=r,i=i,d=d,mem=mem},an);
              genCmp0(cc,d)
          end

      (* emit an integer store *)
      and store(s,a,d,mem,an) =
          let val (r,i) = addr a
          in  mark(I.STORE{s=s,r=r,i=i,d=expr d,mem=mem},an) end

      (* emit a floating point load *)
      and fload(l,a,d,mem,an) =
          let val (r,i) = addr a
          in  mark(I.FLOAD{l=l,r=r,i=i,d=d,mem=mem},an) end
 
      (* emit a floating point store *)
      and fstore(s,a,d,mem,an) =
          let val (r,i) = addr a
          in  mark(I.FSTORE{s=s,r=r,i=i,d=fexpr d,mem=mem},an) end

      (* emit a jump *)
      and jmp(a,labs,an) =
          let val (r,i) = addr a
          in  mark(I.JMP{r=r,i=i,labs=labs,nop=true},an) end

      (* convert mlrisc to cellset *)
      and cellset mlrisc =
      let fun g([],set) = set
            | g(T.GPR(T.REG(_,r))::regs,set) = g(regs,CB.CellSet.add(r,set))
            | g(T.FPR(T.FREG(_,f))::regs,set) = g(regs,CB.CellSet.add(f,set))
            | g(T.CCR(T.CC(_,cc))::regs,set) = g(regs,CB.CellSet.add(cc,set))
            | g(_::regs, set) = g(regs,set)
      in  g(mlrisc, C.empty) end
 
      (* emit a function call *)
      and call(a,flow,defs,uses,mem,cutsTo,an,0) =
	  let val (r,i) = addr a
              val defs=cellset(defs)
              val uses=cellset(uses)
	  in  case (CB.registerId r,i) of
		  (0,I.LAB(T.LABEL l)) =>
		  mark(I.CALL{label=l,defs=C.addReg(C.linkReg,defs),uses=uses,
                              cutsTo=cutsTo,mem=mem,nop=true},an)
		| _ => mark(I.JMPL{r=r,i=i,d=C.linkReg,defs=defs,uses=uses,
				   cutsTo=cutsTo,mem=mem,nop=true},an)
	  end
	| call _ = error "pops<>0 not implemented"

      (* emit an integer branch instruction *)
      and branch(T.CMP(ty,cond,a,b),lab,an) =
          let val (cond,a,b) =
                  case a of
                    (T.LI _ | T.CONST _ | T.LABEL _) => 
                      (T.Basis.swapCond cond,b,a)
                  | _ => (cond,a,b)
          in  if V9 then
                 branchV9(cond,a,b,lab,an)
              else 
                 (doExpr(T.SUB(ty,a,b),newReg(),CC,[]); br(cond,lab,an)) 
          end
        | branch(T.CC(cond,r),lab,an) = 
              if CB.sameColor(r, C.psr) then br(cond,lab,an)
              else (genCmp0(CC,r); br(cond,lab,an))
        | branch(T.FCMP(fty,cond,a,b),lab,an) =
          let val cmp = case fty of
                          32 => I.FCMPs
                        | 64 => I.FCMPd
                        | _  => error "fbranch"
          in  emit(I.FCMP{cmp=cmp,r1=fexpr a,r2=fexpr b,nop=true});
              mark(I.FBfcc{b=fcond cond,a=false,label=lab,nop=true},an)
          end
        | branch _ = error "branch"

      and branchV9(cond,a,b,lab,an) =
          let val size = Gen.Size.size a
          in  if useBR andalso signedCmp cond then 
                 let val r = newReg()
                 in  doExpr(T.SUB(size,a,b),r,REG,[]); 
                     brcond(cond,r,lab,an)
                 end
              else
                 let val cc = case size of 32 => I.ICC 
                                         | 64 => I.XCC
                                         | _ => error "branchV9"
                 in  doExpr(T.SUB(size,a,b),newReg(),CC,[]); 
                     bp(cond,cc,lab,an)
                 end
         end

      and br(c,lab,an) = mark(I.Bicc{b=cond c,a=true,label=lab,nop=true},an)

      and brcond(c,r,lab,an) = 
           mark(I.BR{rcond=rcond c,r=r,p=I.PT,a=true,label=lab,nop=true},an)

      and bp(c,cc,lab,an) = 
           mark(I.BP{b=cond c,cc=cc,p=I.PT,a=true,label=lab,nop=true},an)

          (* generate code for a statement *)
      and stmt(T.MV(_,d,e),an) = doExpr(e,d,REG,an)
        | stmt(T.FMV(_,d,e),an) = doFexpr(e,d,an)
        | stmt(T.CCMV(d,e),an) = doCCexpr(e,d,an)
        | stmt(T.COPY(_,dst,src),an) = copy(dst,src,an)
        | stmt(T.FCOPY(_,dst,src),an) = fcopy(dst,src,an)
        | stmt(T.JMP(T.LABEL l,_),an) =
            mark(I.Bicc{b=I.BA,a=true,label=l,nop=false},an)
        | stmt(T.JMP(e,labs),an) = jmp(e,labs,an)
        | stmt(T.CALL{funct,targets,defs,uses,region,pops,...},an) = 
            call(funct,targets,defs,uses,region,[],an,pops)
        | stmt(T.FLOW_TO
                 (T.CALL{funct,targets,defs,uses,region,pops,...},cutsTo),an) =
            call(funct,targets,defs,uses,region,cutsTo,an,pops)
        | stmt(T.RET _,an) = mark(I.RET{leaf=not registerwindow,nop=true},an)
        | stmt(T.STORE(8,a,d,mem),an)   = store(I.STB,a,d,mem,an)
        | stmt(T.STORE(16,a,d,mem),an)  = store(I.STH,a,d,mem,an)
        | stmt(T.STORE(32,a,d,mem),an)  = store(I.ST,a,d,mem,an)
        | stmt(T.STORE(64,a,d,mem),an)  = 
             store(if V9 then I.STX else I.STD,a,d,mem,an)
        | stmt(T.FSTORE(32,a,d,mem),an) = fstore(I.STF,a,d,mem,an)
        | stmt(T.FSTORE(64,a,d,mem),an) = fstore(I.STDF,a,d,mem,an)
        | stmt(T.BCC(cc,lab),an) = branch(cc,lab,an)
        | stmt(T.DEFINE l,_) = defineLabel l
        | stmt(T.ANNOTATION(s,a),an) = stmt(s,a::an)
        | stmt(T.EXT s,an) = ExtensionComp.compileSext(reducer()) {stm=s, an=an}
        | stmt(s,an) = doStmts(Gen.compileStm s)

      and doStmt s = stmt(s,[])

      and doStmts ss = app doStmt ss

          (* convert an expression into a register *) 
      and expr e = let
	fun comp() = let
	  val d = newReg()
        in doExpr(e, d, REG, []); d 
        end
      in case e
	 of T.REG(_,r) => r
          | T.LI z => if z = 0 then zeroR else comp()
	  | _ => comp()
      end

          (* compute an integer expression and put the result in register d 
           * If cc is set then set the condition code with the result.
           *)
      and doExpr(e,d,cc,an) =
          case e of
            T.REG(_,r) => (move(r,d,an); genCmp0(cc,r))
          | T.LI n     => loadImmed(n,d,cc,an)
          | T.LABEL l  => loadLabel(e,d,cc,an)
          | T.CONST c  => loadLabel(e,d,cc,an)
          | T.LABEXP x  => loadLabel(x,d,cc,an)

                (* generic 32/64 bit support *)
          | T.ADD(_,a,b) => arith(I.ADD,I.ADDCC,a,b,d,cc,COMMUTE,[],an)
	  | T.SUB(_,a,b) => let
	      fun default() = arith(I.SUB,I.SUBCC,a,b,d,cc,NOCOMMUTE,[],an)
            in
	      case b 
              of T.LI z => 
		  if z = 0 then doExpr(a,d,cc,an) else default()
	       | _ => default()
              (*esac*)
	    end

          | T.ANDB(_,a,T.NOTB(_,b)) => 
               arith(I.ANDN,I.ANDNCC,a,b,d,cc,NOCOMMUTE,[],an)
          | T.ORB(_,a,T.NOTB(_,b)) => 
               arith(I.ORN,I.ORNCC,a,b,d,cc,NOCOMMUTE,[],an)
          | T.XORB(_,a,T.NOTB(_,b)) =>
               arith(I.XNOR,I.XNORCC,a,b,d,cc,COMMUTE,[],an)
          | T.ANDB(_,T.NOTB(_,a),b) => 
               arith(I.ANDN,I.ANDNCC,b,a,d,cc,NOCOMMUTE,[],an)
          | T.ORB(_,T.NOTB(_,a),b) => 
               arith(I.ORN,I.ORNCC,b,a,d,cc,NOCOMMUTE,[],an)
          | T.XORB(_,T.NOTB(_,a),b) =>
               arith(I.XNOR,I.XNORCC,b,a,d,cc,COMMUTE,[],an)
          | T.NOTB(_,T.XORB(_,a,b)) =>
               arith(I.XNOR,I.XNORCC,a,b,d,cc,COMMUTE,[],an)

          | T.ANDB(_,a,b) => arith(I.AND,I.ANDCC,a,b,d,cc,COMMUTE,[],an)
          | T.ORB(_,a,b) => arith(I.OR,I.ORCC,a,b,d,cc,COMMUTE,[],an)
          | T.XORB(_,a,b) => arith(I.XOR,I.XORCC,a,b,d,cc,COMMUTE,[],an)
          | T.NOTB(_,a) => arith(I.XNOR,I.XNORCC,a,LI 0,d,cc,COMMUTE,[],an)

               (* 32 bit support *)
          | T.SRA(32,a,b) => shift(I.SRA,a,b,d,cc,an)
          | T.SRL(32,a,b) => shift(I.SRL,a,b,d,cc,an)
          | T.SLL(32,a,b) => shift(I.SLL,a,b,d,cc,an)
          | T.ADDT(32,a,b)=>
               arith(I.ADDCC,I.ADDCC,a,b,d,CC_REG,COMMUTE,trap32,an)
          | T.SUBT(32,a,b)=> 
               arith(I.SUBCC,I.SUBCC,a,b,d,CC_REG,NOCOMMUTE,trap32,an)
          | T.MULU(32,a,b) => extarith(P.umul32,
                                       Mulu32.multiply,a,b,d,cc,COMMUTE)
          | T.MULS(32,a,b) => extarith(P.smul32,
                                       Muls32.multiply,a,b,d,cc,COMMUTE)
          | T.MULT(32,a,b) => extarith(P.smul32trap,
                                       Mult32.multiply,a,b,d,cc,COMMUTE)
          | T.DIVU(32,a,b) => extarith(P.udiv32,divu32,a,b,d,cc,NOCOMMUTE)
          | T.DIVS(T.DIV_TO_ZERO,32,a,b) =>
	                      extarith(P.sdiv32,divs32,a,b,d,cc,NOCOMMUTE)
          | T.DIVT(T.DIV_TO_ZERO,32,a,b) =>
	                      extarith(P.sdiv32trap,divt32,a,b,d,cc,NOCOMMUTE)

               (* 64 bit support *)
          | T.SRA(64,a,b) => shift(I.SRAX,a,b,d,cc,an)
          | T.SRL(64,a,b) => shift(I.SRLX,a,b,d,cc,an)
          | T.SLL(64,a,b) => shift(I.SLLX,a,b,d,cc,an)
          | T.ADDT(64,a,b)=>
               arith(I.ADDCC,I.ADDCC,a,b,d,CC_REG,COMMUTE,trap64,an)
          | T.SUBT(64,a,b)=>
               arith(I.SUBCC,I.SUBCC,a,b,d,CC_REG,NOCOMMUTE,trap64,an)
          | T.MULU(64,a,b) => 
              muldiv64(I.MULX,Mulu64.multiply,a,b,d,cc,COMMUTE,an)
          | T.MULS(64,a,b) => 
              muldiv64(I.MULX,Muls64.multiply,a,b,d,cc,COMMUTE,an)
          | T.MULT(64,a,b) => 
              (muldiv64(I.MULX,Mult64.multiply,a,b,d,CC_REG,COMMUTE,an);
               app emitInstruction trap64)
          | T.DIVU(64,a,b) => muldiv64(I.UDIVX,divu64,a,b,d,cc,NOCOMMUTE,an)
          | T.DIVS(T.DIV_TO_ZERO,64,a,b) =>
	                      muldiv64(I.SDIVX,divs64,a,b,d,cc,NOCOMMUTE,an)
          | T.DIVT(T.DIV_TO_ZERO,64,a,b) =>
	                      muldiv64(I.SDIVX,divt64,a,b,d,cc,NOCOMMUTE,an)

              (* loads *) 
          | T.LOAD(8,a,mem) => load(I.LDUB,a,d,mem,cc,an)
          | T.SX(_,_,T.LOAD(8,a,mem)) => load(I.LDSB,a,d,mem,cc,an)
          | T.LOAD(16,a,mem) => load(I.LDUH,a,d,mem,cc,an)
          | T.SX(_,_,T.LOAD(16,a,mem)) => load(I.LDSH,a,d,mem,cc,an)
          | T.LOAD(32,a,mem) => load(I.LD,a,d,mem,cc,an)
          | T.LOAD(64,a,mem) => 
               load(if V9 then I.LDX else I.LDD,a,d,mem,cc,an)

             (* conditional expression *)
          | T.COND exp => doStmts (Gen.compileCond{exp=exp,rd=d,an=an})

             (* misc *)
          | T.LET(s,e) => (doStmt s; doExpr(e, d, cc, an))
          | T.MARK(e,A.MARKREG f) => (f d; doExpr(e,d,cc,an))
          | T.MARK(e,a) => doExpr(e,d,cc,a::an)
          | T.PRED(e,c) => doExpr(e,d,cc,A.CTRLUSE c::an)
          | T.REXT e => ExtensionComp.compileRext (reducer()) {e=e, rd=d, an=an}
          | e => doExpr(Gen.compileRexp e,d,cc,an)

         (* generate a comparison with zero *)
      and genCmp0(REG,_) = ()
        | genCmp0(_,d) = emit(I.ARITH{a=I.SUBCC,r=d,i=zeroOpn,d=zeroR})

          (* convert an expression into a floating point register *) 
      and fexpr(T.FREG(_,r)) = r
        | fexpr e            = let val d = newFreg() in doFexpr(e,d,[]); d end

          (* compute a floating point expression and put the result in d *)
      and doFexpr(e,d,an) =
          case e of
            (* single precision *)
            T.FREG(32,r)    => fmoves(r,d,an)
          | T.FLOAD(32,ea,mem)  => fload(I.LDF,ea,d,mem,an)
          | T.FADD(32,a,b)  => farith(I.FADDs,a,b,d,an)
          | T.FSUB(32,a,b)  => farith(I.FSUBs,a,b,d,an)
          | T.FMUL(32,a,b)  => farith(I.FMULs,a,b,d,an)
          | T.FDIV(32,a,b)  => farith(I.FDIVs,a,b,d,an)
          | T.FABS(32,a)    => funary(I.FABSs,a,d,an)
          | T.FNEG(32,a)    => funary(I.FNEGs,a,d,an)
          | T.FSQRT(32,a)   => funary(I.FSQRTs,a,d,an)

            (* double precision *)
          | T.FREG(64,r)    => fmoved(r,d,an)
          | T.FLOAD(64,ea,mem)  => fload(I.LDDF,ea,d,mem,an)
          | T.FADD(64,a,b)  => farith(I.FADDd,a,b,d,an)
          | T.FSUB(64,a,b)  => farith(I.FSUBd,a,b,d,an)
          | T.FMUL(64,a,b)  => farith(I.FMULd,a,b,d,an)
          | T.FDIV(64,a,b)  => farith(I.FDIVd,a,b,d,an)
          | T.FABS(64,a)    => funary(I.FABSd,a,d,an)
          | T.FNEG(64,a)    => funary(I.FNEGd,a,d,an)
          | T.FSQRT(64,a)   => funary(I.FSQRTd,a,d,an)

            (* quad precision *)
          | T.FREG(128,r)   => fmoveq(r,d,an)
          | T.FADD(128,a,b) => farith(I.FADDq,a,b,d,an)
          | T.FSUB(128,a,b) => farith(I.FSUBq,a,b,d,an)
          | T.FMUL(128,a,b) => farith(I.FMULq,a,b,d,an)
          | T.FDIV(128,a,b) => farith(I.FDIVq,a,b,d,an)
          | T.FABS(128,a)   => funary(I.FABSq,a,d,an)
          | T.FNEG(128,a)   => funary(I.FNEGq,a,d,an)
          | T.FSQRT(128,a)  => funary(I.FSQRTq,a,d,an)

            (* floating point to floating point *)
          | T.CVTF2F(ty,ty',e) =>
              (case (ty,ty') of
                 (32,32)  => doFexpr(e,d,an)
               | (64,32)  => funary(I.FsTOd,e,d,an)
               | (128,32) => funary(I.FsTOq,e,d,an)
               | (32,64)  => funary(I.FdTOs,e,d,an)
               | (64,64)  => doFexpr(e,d,an)
               | (128,64) => funary(I.FdTOq,e,d,an)
               | (32,128) => funary(I.FqTOs,e,d,an)
               | (64,128) => funary(I.FqTOd,e,d,an)
               | (128,128) => doFexpr(e,d,an)
               | _ => error "CVTF2F"
              )

            (* integer to floating point *)
          | T.CVTI2F(32,32,e) => app emitInstruction (P.cvti2s({i=opn e,d=d},reduceOpn))
          | T.CVTI2F(64,32,e) => app emitInstruction (P.cvti2d({i=opn e,d=d},reduceOpn))
          | T.CVTI2F(128,32,e) => app emitInstruction (P.cvti2q({i=opn e,d=d},reduceOpn))

          | T.FMARK(e,A.MARKREG f) => (f d; doFexpr(e,d,an))
          | T.FMARK(e,a) => doFexpr(e,d,a::an)
          | T.FPRED(e,c) => doFexpr(e,d,A.CTRLUSE c::an)
          | T.FEXT e => ExtensionComp.compileFext (reducer()) {e=e, fd=d, an=an}
          | e => doFexpr(Gen.compileFexp e,d,an)

      and doCCexpr(T.CMP(ty,cond,e1,e2),cc,an) =
             if CB.sameColor(cc,C.psr) then
                  doExpr(T.SUB(ty,e1,e2),newReg(),CC,an)
             else error "doCCexpr"
        | doCCexpr(T.CC(_,r),d,an) = 
             if CB.sameColor(r,C.psr) then error "doCCexpr"
             else move(r,d,an)
        | doCCexpr(T.CCMARK(e,A.MARKREG f),d,an) = (f d; doCCexpr(e,d,an))
        | doCCexpr(T.CCMARK(e,a),d,an) = doCCexpr(e,d,a::an)
        | doCCexpr(T.CCEXT e,d,an) =
             ExtensionComp.compileCCext (reducer()) {e=e, ccd=d, an=an}
        | doCCexpr e = error "doCCexpr"

      and ccExpr e = let val d = newReg() in doCCexpr(e,d,[]); d end

          (* convert an expression into an operand *) 
      and opn(x as T.CONST c) = I.LAB x
        | opn(x as T.LABEL l) = I.LAB x
        | opn(T.LABEXP x)     = I.LAB x
        | opn(e as T.LI n)   = 
	    if n = 0 then zeroOpn
	    else if immed13 n then I.IMMED(toInt n)
		 else I.REG(expr e)
        | opn e              = I.REG(expr e)

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

(*
 * Machine code generator for SPARC.
 *
 * The SPARC architecture has 32 general purpose registers (%g0 is always 0)
 * and 32 single precision floating point registers.  
 *
 * Some Ugliness: double precision floating point registers are 
 * register pairs.  There are no double precision moves, negation and absolute
 * values.  These require two single precision operations.  I've created
 * composite instructions FMOVd, FNEGd and FABSd to stand for these. 
 *
 * All integer arithmetic instructions can optionally set the condition 
 * code register.  We use this to simplify certain comparisons with zero.
 *
 * Integer multiplication, division and conversion from integer to floating
 * go thru the pseudo instruction interface, since older sparcs do not
 * implement these instructions in hardware.
 *
 * In addition, the trap instruction for detecting overflow is a parameter.
 * This allows different trap vectors to be used.
 *
 * -- Allen
 *) 
