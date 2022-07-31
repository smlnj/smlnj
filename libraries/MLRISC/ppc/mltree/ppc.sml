(* ppc.sml
 *
 * COPYRIGHT (c) 2002 Bell Labs, Lucent Technologies
 *
 * I've substantially modified this code generator to support the new MLTREE.
 * Please see the file README.hppa for the ugly details.
 *
 * -- Allen
 *)

functor PPC
  (structure PPCInstr : PPCINSTR
   structure PseudoInstrs : PPC_PSEUDO_INSTR 
   			where I = PPCInstr
   structure ExtensionComp : MLTREE_EXTENSION_COMP 
   			where I = PPCInstr and T = PPCInstr.T

   (* 
    * Support 64 bit mode? 
    * This should be set to false for SML/NJ
    *)
   val bit64mode : bool 

   (*
    * Cost of multiplication in cycles
    *)
   val multCost : int ref
  ) = 
struct
  structure I   = PPCInstr
  structure T   = I.T
  structure TS  = ExtensionComp.TS
  structure C   = PPCInstr.C
  structure CB  = CellsBasis
  structure W32 = Word32
  structure A   = MLRiscAnnotations
  structure CFG = ExtensionComp.CFG

  fun error msg = MLRiscErrorMsg.error("PPC",msg)

  type instrStream = (I.instruction, CB.CellSet.cellset, CFG.cfg) TS.stream
  type mltreeStream = (T.stm, T.mlrisc list, CFG.cfg) TS.stream


  val (intTy,naturalWidths) = if bit64mode then (64,[32,64]) else (32,[32])
  structure Gen = MLTreeGen
    (structure T = T
     structure Cells = C
     val intTy = intTy
     val naturalWidths = naturalWidths
     datatype rep = SE | ZE | NEITHER
     val rep = NEITHER
    )

  (* 
   * Special instructions
   *)
  fun MTLR r = I.MTSPR{rs=r, spr=C.lr}
  fun MFLR r = I.MFSPR{rt=r, spr=C.lr}
  val CR0 = C.Reg CB.CC 0
  val RET = I.BCLR{bo=I.ALWAYS, bf=CR0, bit=I.LT, LK=false, labels=[]}
  fun SLLI32{r,i,d} = 
      I.ROTATEI{oper=I.RLWINM,ra=d,rs=r,sh=I.ImmedOp i,mb=0,me=SOME(31-i)}
  fun SRLI32{r,i,d} = 			
      I.ROTATEI{oper=I.RLWINM,ra=d,rs=r,sh=I.ImmedOp(Int.mod(32-i,32)),mb=i,me=SOME(31)}
  fun COPY{dst, src, tmp} = 
      I.COPY{k=CB.GP, sz=32, dst=dst, src=src, tmp=tmp}
  fun FCOPY{dst, src, tmp} = 
      I.COPY{k=CB.FP, sz=64, dst=dst, src=src, tmp=tmp}

  (*  
   * Integer multiplication 
   *)
  functor Multiply32 = MLTreeMult
    (structure I = I
     structure T = T
     structure CB = CellsBasis
     val intTy = 32
     type arg  = {r1:CB.cell,r2:CB.cell,d:CB.cell}
     type argi = {r:CB.cell,i:int,d:CB.cell}

     fun mov{r,d} = COPY{dst=[d],src=[r],tmp=NONE}
     fun add{r1,r2,d}= I.arith{oper=I.ADD,ra=r1,rb=r2,rt=d,Rc=false,OE=false}
     fun slli{r,i,d} = [I.INSTR(SLLI32{r=r,i=i,d=d})]
     fun srli{r,i,d} = [I.INSTR(SRLI32{r=r,i=i,d=d})]
     fun srai{r,i,d} = [I.arithi{oper=I.SRAWI,rt=d,ra=r,im=I.ImmedOp i}]
    )

  structure Mulu32 = Multiply32
    (val trapping = false
     val multCost = multCost
     fun addv{r1,r2,d}=[I.arith{oper=I.ADD,ra=r1,rb=r2,rt=d,Rc=false,OE=false}]
     fun subv{r1,r2,d}=[I.arith{oper=I.SUBF,ra=r2,rb=r1,rt=d,Rc=false,OE=false}]
     val sh1addv = NONE
     val sh2addv = NONE
     val sh3addv = NONE
    )
    (val signed = false)

  structure Muls32 = Multiply32
    (val trapping = false
     val multCost = multCost
     fun addv{r1,r2,d}=[I.arith{oper=I.ADD,ra=r1,rb=r2,rt=d,Rc=false,OE=false}]
     fun subv{r1,r2,d}=[I.arith{oper=I.SUBF,ra=r2,rb=r1,rt=d,Rc=false,OE=false}]
     val sh1addv = NONE
     val sh2addv = NONE
     val sh3addv = NONE
    )
    (val signed = true)

  structure Mult32 = Multiply32
    (val trapping = true
     val multCost = multCost
     fun addv{r1,r2,d} = error "Mult32.addv"
     fun subv{r1,r2,d} = error "Mult32.subv"
     val sh1addv = NONE
     val sh2addv = NONE
     val sh3addv = NONE
    )
    (val signed = true)

  fun selectInstructions
      (instrStream as 
       TS.S.STREAM{emit=emitInstruction,comment,getAnnotations,
                defineLabel,entryLabel,pseudoOp,annotation,
                beginCluster,endCluster,exitBlock,...}) = 
  let 
      val emit = emitInstruction o I.INSTR

      (* mark an instruction with annotations *)
      fun annotate(instr,[]) = instr
        | annotate(instr,a::an) = annotate(I.ANNOTATION{i=instr,a=a},an)
      fun mark'(instr, an) = emitInstruction(annotate(instr, an))
      fun mark(instr,an) = emitInstruction(annotate(I.INSTR instr,an))

      (* Label where trap is generated.   
       * For overflow trapping instructions, we generate a branch 
       * to this label.
       *)
      val trapLabel : Label.label option ref = ref NONE 
      val zeroR = C.r0 

      val newReg = C.newReg
      val newFreg = C.newFreg
      val newCCreg = C.newCell CB.CC

      
      fun LT (x,y)    = T.I.LT(32, x, y)
      fun LE (x,y)    = T.I.LE(32, x, y)
      fun toInt mi = T.I.toInt(32, mi)
      fun LI i = T.I.fromInt(32, i)

      fun signed16 mi   = LE(~0x8000, mi) andalso LT(mi, 0x8000)
      fun signed12 mi   = LE(~0x800, mi) andalso LT(mi, 0x800)
      fun unsigned16 mi = LE(0, mi) andalso LT(mi, 0x10000)
      fun unsigned5 mi  = LE(0, mi) andalso LT(mi, 32)
      fun unsigned6 mi  = LE(0, mi) andalso LT(mi, 64)

      fun move(rs,rd,an) =
        if CB.sameColor(rs,rd) then () 
        else mark'(COPY{dst=[rd],src=[rs],tmp=NONE},an)

      fun fmove(fs,fd,an) =
        if CB.sameColor(fs,fd) then () 
        else mark'(FCOPY{dst=[fd],src=[fs],tmp=NONE},an)

      fun ccmove(ccs,ccd,an) =
        if CB.sameColor(ccd,ccs) then () else mark(I.MCRF{bf=ccd, bfa=ccs},an)

      fun copy(dst, src, an) =
          mark'(COPY{dst=dst, src=src,
                     tmp=case dst of [_] => NONE 
                                    | _ => SOME(I.Direct(newReg()))},an)
      fun fcopy(dst, src, an) =
          mark'(FCOPY{dst=dst, src=src, 
                      tmp=case dst of [_] => NONE 
                                    | _ => SOME(I.FDirect(newFreg()))},an)

      fun emitBranch{bo, bf, bit, addr, LK} = 
      let val fallThrLab = Label.anon()
          val fallThrOpnd = I.LabelOp(T.LABEL fallThrLab)
      in
          emit(I.BC{bo=bo, bf=bf, bit=bit, addr=addr, LK=LK, fall=fallThrOpnd});
          defineLabel fallThrLab
      end

      fun split n = let 
        val wtoi = Word32.toIntX
	val w = T.I.toWord32(32, n)
	val hi = W32.~>>(w, 0w16)
	val lo = W32.andb(w, 0w65535)
	val (high, low) = 
	  if W32.<(lo,0w32768) then (hi, lo) else (hi+0w1, lo-0w65536)
      in 
	(wtoi high, wtoi low) 
      end

      fun loadImmedHiLo(0, lo, rt, an) =
            mark(I.ARITHI{oper=I.ADDI, rt=rt, ra=zeroR, im=I.ImmedOp lo}, an)
        | loadImmedHiLo(hi, lo, rt, an) = 
           (mark(I.ARITHI{oper=I.ADDIS, rt=rt, ra=zeroR, im=I.ImmedOp hi}, an);
            if lo = 0 then () 
               else emit(I.ARITHI{oper=I.ADDI, rt=rt, ra=rt, im=I.ImmedOp lo}))

      fun loadImmed(n, rt, an) = 
        if signed16 n then
           mark(I.ARITHI{oper=I.ADDI, rt=rt, ra=zeroR, im=I.ImmedOp(toInt(n))}, an)
        else let val (hi, lo) = split n
           in loadImmedHiLo(hi, lo, rt, an) 
	   end

      fun loadLabexp(lexp, rt, an) = 
          mark(I.ARITHI{oper=I.ADDI, rt=rt, ra=zeroR, im=I.LabelOp lexp}, an)

      fun immedOpnd range (e1, e2 as T.LI i) =
           (expr e1, if range i then I.ImmedOp(toInt i) else I.RegOp(expr e2))
        | immedOpnd _ (e1, x as T.CONST _) = (expr e1, I.LabelOp x)
        | immedOpnd _ (e1, x as T.LABEL _) = (expr e1, I.LabelOp x)
        | immedOpnd _ (e1, T.LABEXP lexp) = (expr e1, I.LabelOp lexp)
        | immedOpnd _ (e1, e2) = (expr e1, I.RegOp(expr e2))

      and commImmedOpnd range (e1 as T.LI _, e2) = 
           immedOpnd range (e2, e1)
        | commImmedOpnd range (e1 as T.CONST _, e2) = 
           immedOpnd range (e2, e1)
        | commImmedOpnd range (e1 as T.LABEL _, e2) =
           immedOpnd range (e2, e1)
        | commImmedOpnd range (e1 as T.LABEXP _, e2) =
           immedOpnd range (e2, e1)
        | commImmedOpnd range arg = immedOpnd range arg

      and eCommImm range (oper, operi, e1, e2, rt, an) = 
       (case commImmedOpnd range (e1, e2)
        of (ra, I.RegOp rb) =>
            mark(I.ARITH{oper=oper, ra=ra, rb=rb, rt=rt, Rc=false, OE=false},an)
         | (ra, opnd) => 
            mark(I.ARITHI{oper=operi, ra=ra, im=opnd, rt=rt},an)
        (*esac*))

      (*
       * Compute a base/displacement effective address
       *)
      and addr(size,T.ADD(_, e, T.LI i)) =
          let val ra = expr e
          in  if size i then (ra, I.ImmedOp(toInt i)) else 
              let val (hi, lo) = split i 
                  val tmpR = newReg()
              in  emit(I.ARITHI{oper=I.ADDIS, rt=tmpR, ra=ra, im=I.ImmedOp hi});
                  (tmpR, I.ImmedOp lo)
               end
          end
        | addr(size,T.ADD(ty, T.LI i, e)) = addr(size,T.ADD(ty, e, T.LI i))
        | addr(size,exp as T.SUB(ty, e, T.LI i)) = 
            (addr(size,T.ADD(ty, e, T.LI (T.I.NEGT(32, i)))) 
               handle Overflow => (expr exp, I.ImmedOp 0))
        | addr(size,T.ADD(_, e1, e2)) = (expr e1, I.RegOp (expr e2))
        | addr(size,e) = (expr e, I.ImmedOp 0)

       (* convert mlrisc to cellset: *)
       and cellset mlrisc =
           let val addCCReg = CB.CellSet.add 
               fun g([],acc) = acc
                 | g(T.GPR(T.REG(_,r))::regs,acc)  = g(regs,C.addReg(r,acc))
                 | g(T.FPR(T.FREG(_,f))::regs,acc) = g(regs,C.addFreg(f,acc))
                 | g(T.CCR(T.CC(_,cc))::regs,acc)  = g(regs,addCCReg(cc,acc))
                 | g(T.CCR(T.FCC(_,cc))::regs,acc) = g(regs,addCCReg(cc,acc))
                 | g(_::regs, acc) = g(regs, acc)
           in  g(mlrisc, C.empty) end

      (*
       * Translate a statement, and annotate it   
       *)
      and stmt(T.MV(_, rd, e),an) = doExpr(e, rd, an)
        | stmt(T.FMV(_, fd, e),an) = doFexpr(e, fd, an)
        | stmt(T.CCMV(ccd, ccexp), an) = doCCexpr(ccexp, ccd, an)
        | stmt(T.COPY(_, dst, src), an) = copy(dst, src, an)
        | stmt(T.FCOPY(_, dst, src), an) = fcopy(dst, src, an)
        | stmt(T.JMP(T.LABEXP lexp, labs),an) =
             mark(I.B{addr=I.LabelOp lexp, LK=false},an)
        | stmt(T.JMP(x as (T.LABEL _ | T.CONST _), labs),an) =
             mark(I.B{addr=I.LabelOp x, LK=false},an)
        | stmt(T.JMP(rexp, labs),an) =
          let val rs = expr(rexp)
          in  emit(MTLR(rs));
              mark(I.BCLR{bo=I.ALWAYS,bf=CR0,bit=I.LT,LK=false,labels=labs},an)
          end
        | stmt(T.CALL{funct, targets, defs, uses, region, pops, ...}, an) = 
            call(funct, targets, defs, uses, region, [], an, pops) 
        | stmt(T.FLOW_TO(T.CALL{funct, targets, defs, uses, region, pops,...}, 
                         cutTo), an) = 
            call(funct, targets, defs, uses, region, cutTo, an, pops) 
        | stmt(T.RET flow,an) = mark(RET,an)
        | stmt(T.STORE(ty,ea,data,mem),an) = store(ty,ea,data,mem,an)
        | stmt(T.FSTORE(ty,ea,data,mem),an) = fstore(ty,ea,data,mem,an)
        | stmt(T.BCC(cc, lab),an) = branch(cc,lab,an)
        | stmt(T.DEFINE l, _) = defineLabel l
        | stmt(T.LIVE S, an) = mark'(I.LIVE{regs=cellset S,spilled=C.empty},an)
        | stmt(T.KILL S, an) = mark'(I.KILL{regs=cellset S,spilled=C.empty},an)
        | stmt(T.ANNOTATION(s,a),an) = stmt(s,a::an)
        | stmt(T.EXT s,an) = ExtensionComp.compileSext(reducer()) {stm=s, an=an}
        | stmt(s, _) = doStmts(Gen.compileStm s)

      and call(funct, targets, defs, uses, region, cutsTo, an, 0) = 
          let val defs=cellset(defs)
              val uses=cellset(uses)
          in  emit(MTLR(expr funct));
              mark(I.CALL{def=defs, use=uses, cutsTo=cutsTo, mem=region}, an)
          end
	| call _ = error "pops<>0 not implemented"

      and branch(T.CMP(_, _, T.LI _, T.LI _), _, _) = error "branch(LI,LI)"
        | branch(T.CMP(ty, cc, e1 as T.LI _, e2), lab, an) = 
          let val cc' = T.Basis.swapCond cc
          in  branch(T.CMP(ty, cc', e2, e1), lab, an)
          end
        | branch(cmp as T.CMP(ty, cond, e1, e2), lab, an) = let
	    val (bo, cf) = 
	      (case cond  
	       of T.LT  => (I.TRUE,  I.LT)
		| T.LE  => (I.FALSE, I.GT)
		| T.EQ  => (I.TRUE,  I.EQ)
		| T.NE  => (I.FALSE, I.EQ)
		| T.GT  => (I.TRUE,  I.GT)
		| T.GE  => (I.FALSE, I.LT)
		| T.LTU => (I.TRUE,  I.LT)
		| T.LEU => (I.FALSE, I.GT)
		| T.GTU => (I.TRUE,  I.GT)
		| T.GEU => (I.FALSE, I.LT)
		| (T.SETCC | T.MISC_COND _) => error "branch(CMP)"
	     (*esac*))
            val ccreg = if true then CR0 else newCCreg() (* XXX *)
	    val addr = I.LabelOp(T.LABEL lab)
	    fun default() = 
	      (doCCexpr(cmp, ccreg, []);
	       emitBranch{bo=bo, bf=ccreg, bit=cf, addr=addr, LK=false})
          in
	    case (e1, e2)
	    of (T.ANDB(_, a1, a2), T.LI z) =>
	        if z = 0 then 
		  (case commImmedOpnd unsigned16 (a1, a2)
		   of (ra, I.RegOp rb) =>
			emit(I.ARITH{oper=I.AND, ra=ra, rb=rb, rt=newReg(), Rc=true, OE=false})
		    | (ra, opnd) =>
			emit(I.ARITHI{oper=I.ANDI_Rc, ra=ra, im=opnd, rt=newReg()})
		   (*esac*);
		   branch(T.CC(cond, CR0), lab, an))
	        else 
		  default()
             | _ => 
		  default()
          end
        | branch(T.CC(cc, cr), lab, an) = 
          let val addr=I.LabelOp(T.LABEL lab)
              fun branch(bo, bit) = 
                 emitBranch{bo=bo, bf=cr, bit=bit, addr=addr, LK=false}
          in  case cc of 
                T.EQ => branch(I.TRUE, I.EQ)
              | T.NE => branch(I.FALSE, I.EQ)
              | (T.LT | T.LTU) => branch(I.TRUE, I.LT)
              | (T.LE | T.LEU) => branch(I.FALSE, I.GT)
              | (T.GE | T.GEU) => branch(I.FALSE, I.LT)
              | (T.GT | T.GTU) => branch(I.TRUE, I.GT)
	      | (T.SETCC | T.MISC_COND _) => error "branch(CC)"
          end  
        | branch(cmp as T.FCMP(fty, cond, _, _), lab, an) = 
          let val ccreg = if true then CR0 else newCCreg() (* XXX *)
              val labOp = I.LabelOp(T.LABEL lab)
              fun branch(bo, bf, bit) = 
                  emitBranch{bo=bo, bf=bf, bit=bit, addr=labOp, LK=false}
              fun test2bits(bit1, bit2) = 
              let val ba=(ccreg, bit1)
                  val bb=(ccreg, bit2)
                  val bt=(ccreg, I.FL)
              in  emit(I.CCARITH{oper=I.CROR, bt=bt, ba=ba, bb=bb});
                  branch(I.TRUE, ccreg, I.FL)
              end
          in  doCCexpr(cmp, ccreg, []);
              case cond of 
                T.==  => branch(I.TRUE,  ccreg, I.FE)
              | T.?<> => branch(I.FALSE,  ccreg, I.FE)
              | T.?   => branch(I.TRUE,  ccreg, I.FU)
              | T.<=> => branch(I.FALSE,  ccreg, I.FU)
              | T.>   => branch(I.TRUE,  ccreg, I.FG)
              | T.>=  => test2bits(I.FG, I.FE)
              | T.?>  => test2bits(I.FU, I.FG)
              | T.?>= => branch(I.FALSE,  ccreg, I.FL)
              | T.<   => branch(I.TRUE,  ccreg, I.FL)
              | T.<=  => test2bits(I.FL, I.FE)
              | T.?<  => test2bits(I.FU, I.FL)
              | T.?<= => branch(I.FALSE,  ccreg, I.FG)
              | T.<>  => test2bits(I.FL, I.FG)
              | T.?=  => test2bits(I.FU, I.FE)
	      | (T.SETFCC | T.MISC_FCOND _) => error "branch(FCMP)"
             (*esac*)
          end
        | branch _ = error "branch"
  
      and doStmt s = stmt(s,[]) 

      and doStmts ss = app doStmt ss
   
        (* Emit an integer store *) 
      and store(ty, ea, data, mem, an) = 
          let val (st,size) = case (ty,Gen.Size.size ea) of
                         (8,32)  => (I.STB,signed16)
                       | (8,64)  => (I.STBE,signed12)
                       | (16,32) => (I.STH,signed16)
                       | (16,64) => (I.STHE,signed12)
                       | (32,32) => (I.STW,signed16)
                       | (32,64) => (I.STWE,signed12)
                       | (64,64) => (I.STDE,signed12)
                       | _  => error "store"
              val (r, disp) = addr(size,ea)
          in  mark(I.ST{st=st, rs=expr data, ra=r, d=disp, mem=mem}, an) end

        (* Emit a floating point store *) 
      and fstore(ty, ea, data, mem, an) =
          let val (st,size) = case (ty,Gen.Size.size ea) of
                         (32,32) => (I.STFS,signed16)
                       | (32,64) => (I.STFSE,signed12)
                       | (64,32) => (I.STFD,signed16)
                       | (64,64) => (I.STFDE,signed12)
                       | _  => error "fstore"
              val (r, disp) = addr(size,ea)
          in  mark(I.STF{st=st,fs=fexpr data, ra=r, d=disp, mem=mem},an) end
    
      and subfImmed(i, ra, rt, an) = 
          if signed16 i then
             mark(I.ARITHI{oper=I.SUBFIC, rt=rt, ra=ra, im=I.ImmedOp(toInt i)}, an)
          else
             mark(I.ARITH{oper=I.SUBF, rt=rt, ra=ra, rb=expr(T.LI i), 
                          Rc=false, OE=false}, an)
    
      (*  Generate an arithmetic instruction *)
      and arith(oper, e1, e2, rt, an) = 
          mark(I.ARITH{oper=oper,ra=expr e1,rb=expr e2,rt=rt,OE=false,Rc=false},
               an)
   
      (*  Generate a trapping instruction *)
      and arithTrapping(oper, e1, e2, rt, an) = 
          let val ra = expr e1 val rb = expr e2
          in  mark(I.ARITH{oper=oper,ra=ra,rb=rb,rt=rt,OE=true,Rc=true},an);
              overflowTrap()
          end

      (*  Generate an overflow trap *)
      and overflowTrap() =
          let val label = case !trapLabel of
                            NONE => let val l = Label.anon()
                                    in  trapLabel := SOME l; l end
                          | SOME l => l
          in  emitBranch{bo=I.TRUE, bf=CR0, bit=I.SO, LK=false,
                         addr=I.LabelOp(T.LABEL label)}
          end

      (* Generate a load and annotate the instruction *)
      and load(ld32, ld64, ea, mem, rt, an) = 
          let val (ld,size) = 
              if bit64mode andalso Gen.Size.size ea = 64 
              then (ld64,signed12) 
              else (ld32,signed16)
              val (r, disp) = addr(size,ea)
          in  mark(I.L{ld=ld, rt=rt, ra=r, d=disp, mem=mem},an)
          end

      (* Generate a SRA shift operation and annotate the instruction *) 
      and sra(oper, operi, e1, e2, rt, an) = 
          case immedOpnd unsigned5 (e1, e2) of 
            (ra, I.RegOp rb) => 
              mark(I.ARITH{oper=oper,rt=rt,ra=ra,rb=rb,Rc=false,OE=false},an)
          | (ra, rb) => 
              mark(I.ARITHI{oper=operi, rt=rt, ra=ra, im=rb},an)

      (* Generate a SRL shift operation and annotate the instruction *) 
      and srl32(e1, e2, rt, an) = 
          case immedOpnd unsigned5 (e1, e2) of
            (ra, I.ImmedOp n) =>
              mark(SRLI32{r=ra,i=n,d=rt},an)
          | (ra, rb) =>
              mark(I.ARITH{oper=I.SRW,rt=rt,ra=ra,rb=reduceOpn rb,
                           Rc=false,OE=false},an)
    
      and sll32(e1, e2, rt, an) = 
          case immedOpnd unsigned5 (e1, e2) of
            (ra, rb as I.ImmedOp n) =>
              mark(SLLI32{r=ra,i=n,d=rt},an)
          | (ra, rb) =>
              mark(I.ARITH{oper=I.SLW,rt=rt,ra=ra,rb=reduceOpn rb,
                           Rc=false,OE=false},an)

      (* Generate a subtract operation *)
      and subtract(ty, e1, e2 as T.LI i, rt, an) =
            (doExpr(T.ADD(ty, e1, T.LI (T.I.NEGT(32, i))), rt, an)
              handle Overflow => 
              mark(I.ARITH{oper=I.SUBF, rt=rt, ra=expr e2, 
                           rb=expr e1, OE=false, Rc=false}, an)
            )
        | subtract(ty, T.LI i, e2, rt, an) = subfImmed(i, expr e2, rt, an)
        | subtract(ty, x as (T.CONST _ | T.LABEL _), e2, rt, an) =
             mark(I.ARITHI{oper=I.SUBFIC,rt=rt,ra=expr e2,
                           im=I.LabelOp x},an)
        | subtract(ty, e1, e2, rt, an) =
          let val rb = expr e1 val ra = expr e2
          in  mark(I.ARITH{oper=I.SUBF,rt=rt,ra=ra,rb=rb,Rc=false,OE=false},an)
          end

          (* Generate optimized multiplication code *)
      and multiply(ty,oper,operi,genMult,e1,e2,rt,an) =
          let fun nonconst(e1,e2) = 
                  [annotate( 
                     case commImmedOpnd signed16 (e1,e2) of
                       (ra,I.RegOp rb) => 
                         I.arith{oper=oper,ra=ra,rb=rb,rt=rt,OE=false,Rc=false}
                     | (ra,im) => I.arithi{oper=operi,ra=ra,im=im,rt=rt},
                     an)]
              fun const(e,i) =
                  let val r = expr e
                  in  genMult{r=r,i=toInt(i),d=rt}
                      handle _ => nonconst(T.REG(ty,r),T.LI i)
                  end
              val instrs =
                 case (e1,e2) of
                   (_,T.LI i)   => const(e1,i)
                 | (T.LI i,_)   => const(e2,i)
                 | _            => nonconst(e1,e2)
          in  app emitInstruction instrs end

      and divu32 x = Mulu32.divide{mode=T.TO_ZERO,stm=doStmt} x 

      and divs32 x = Muls32.divide{mode=T.TO_ZERO,stm=doStmt} x

      and divt32 x = Mult32.divide{mode=T.TO_ZERO,stm=doStmt} x 

          (* Generate optimized division code *)
      and divide(ty,oper,genDiv,e1,e2,rt,overflow,an) =
          let fun nonconst(e1,e2) = 
                 (mark(I.ARITH{oper=oper,ra=expr e1,rb=expr e2,rt=rt,
                               OE=overflow,Rc=overflow},an);
                  if overflow then overflowTrap() else ()
                  )
              fun const(e,i) =
                  let val r = expr e
                  in  app emitInstruction (genDiv{r=r,i=toInt(i),d=rt})
                      handle _ => nonconst(T.REG(ty,r),T.LI i)
                  end
          in  case (e1,e2) of
                (_,T.LI i)   => const(e1,i)
              | _            => nonconst(e1,e2)
          end

      (* Reduce an operand into a register *)
      and reduceOpn(I.RegOp r) = r
        | reduceOpn opn =
          let val rt = newReg()
          in  emit(I.ARITHI{oper=I.ADDI, rt=rt, ra=zeroR, im=opn});
              rt
          end

      (* Reduce an expression, and returns the register that holds
       * the value.
       *)
      and expr(rexp as T.REG(_,r)) =     
          if CB.sameColor(C.lr, r) then
          let val rt = newReg()
          in  doExpr(rexp, rt, []); rt end 
          else r
        | expr(rexp) = 
          let val rt = newReg()
          in  doExpr(rexp, rt, []); rt end 
    
      (* doExpr(e, rt, an) -- 
       *    reduce the expression e, assigns it to rd,
       *    and annotate the expression with an
       *)
      and doExpr(e, rt, an) =
           if CB.sameColor(rt,C.lr) then 
           let val rt = newReg() in doExpr(e,rt,[]); mark(MTLR rt,an) end
           else
           case e of
             T.REG(_,rs)  => if CB.sameColor(rs,C.lr) then mark(MFLR rt,an)
                             else move(rs,rt,an)
           | T.LI i        => loadImmed(i, rt, an)
           | T.LABEXP lexp => loadLabexp(lexp, rt, an)
           | T.CONST _     => loadLabexp(e, rt, an)
           | T.LABEL _     => loadLabexp(e, rt, an)

             (* All data widths *)
           | T.ADD(_, e1, e2) => eCommImm signed16 (I.ADD,I.ADDI,e1,e2,rt,an)
           | T.SUB(ty, e1, e2) => subtract(ty, e1, e2, rt, an)

             (* Special PPC bit operations *)
           | T.ANDB(_,e1,T.NOTB(_,e2)) => arith(I.ANDC,e1,e2,rt,an)
           | T.ORB(_,e1,T.NOTB(_,e2))  => arith(I.ORC,e1,e2,rt,an)
           | T.XORB(_,e1,T.NOTB(_,e2)) => arith(I.EQV,e1,e2,rt,an)
           | T.EQVB(_,e1,e2)           => arith(I.EQV,e1,e2,rt,an)
           | T.ANDB(_,T.NOTB(_,e1),e2) => arith(I.ANDC,e2,e1,rt,an)
           | T.ORB(_,T.NOTB(_,e1),e2)  => arith(I.ORC,e2,e1,rt,an)
           | T.XORB(_,T.NOTB(_,e1),e2) => arith(I.EQV,e2,e1,rt,an)
           | T.NOTB(_,T.ANDB(_,e1,e2)) => arith(I.NAND,e1,e2,rt,an)
           | T.NOTB(_,T.ORB(_,e1,e2))  => arith(I.NOR,e1,e2,rt,an)
           | T.NOTB(_,T.XORB(_,e1,e2)) => arith(I.EQV,e1,e2,rt,an)

           | T.ANDB(_, e1, e2) => 
               eCommImm unsigned16(I.AND,I.ANDI_Rc,e1,e2,rt,an)
           | T.ORB(_, e1, e2) => eCommImm unsigned16(I.OR,I.ORI,e1,e2,rt,an)
           | T.XORB(_, e1, e2) => eCommImm unsigned16(I.XOR,I.XORI,e1,e2,rt,an)

             (* 32 bit support *)
           | T.MULU(32, e1, e2) => multiply(32,I.MULLW,I.MULLI,
                                            Mulu32.multiply,e1,e2,rt,an)
           | T.DIVU(32, e1, e2) => divide(32,I.DIVWU,divu32,e1,e2,rt,false,an)

	   | T.MULS(32, e1, e2) => multiply(32,I.MULLW,I.MULLI,
					    Muls32.multiply,e1,e2,rt,an)
	   | T.DIVS(T.DIV_TO_ZERO, 32, e1, e2) =>
	       (* On the PPC we turn overflow checking on despite this
		* being DIVS.  That's because divide-by-zero is also
		* indicated through "overflow" instead of causing a trap. *)
	                           divide(32,I.DIVW,divs32,e1,e2,rt,
					  true (* !! *),
					  an)

           | T.ADDT(32, e1, e2) => arithTrapping(I.ADD, e1, e2, rt, an)
           | T.SUBT(32, e1, e2) => arithTrapping(I.SUBF, e2, e1, rt, an)
           | T.MULT(32, e1, e2) => arithTrapping(I.MULLW, e1, e2, rt, an)
           | T.DIVT(T.DIV_TO_ZERO, 32, e1, e2) =>
	                           divide(32,I.DIVW,divt32,e1,e2,rt,true,an)
    
           | T.SRA(32, e1, e2)  => sra(I.SRAW, I.SRAWI, e1, e2, rt, an)
           | T.SRL(32, e1, e2)  => srl32(e1, e2, rt, an)
           | T.SLL(32, e1, e2)  => sll32(e1, e2, rt, an)
 
              (* 64 bit support *) 
           | T.SRA(64, e1, e2) => sra(I.SRAD, I.SRADI, e1, e2, rt, an)
           (*| T.SRL(64, e1, e2) => srl(32, I.SRD, I.RLDINM, e1, e2, rt, an)
           | T.SLL(64, e1, e2) => sll(32, I.SLD, I.RLDINM, e1, e2, rt, an)*)

              (* loads *) 
           | T.LOAD(8,ea,mem)   => load(I.LBZ,I.LBZE,ea,mem,rt,an)
           | T.LOAD(16,ea, mem) => load(I.LHZ,I.LHZE,ea,mem,rt,an)
           | T.LOAD(32,ea, mem) => load(I.LWZ,I.LWZE,ea,mem,rt,an)
           | T.LOAD(64,ea, mem) => load(I.LDE,I.LDE,ea,mem,rt,an)
            
              (* Conditional expression *)
           | T.COND exp =>
              doStmts(Gen.compileCond{exp=exp,an=an,rd=rt})

              (* Misc *)
           | T.LET(s,e) => (doStmt s; doExpr(e, rt, an))
           | T.MARK(e, A.MARKREG f) => (f rt; doExpr(e,rt,an))
           | T.MARK(e, a) => doExpr(e,rt,a::an)
           | T.REXT e => ExtensionComp.compileRext (reducer()) {e=e,rd=rt,an=an}
           | e => doExpr(Gen.compileRexp e,rt,an)
  
      (* Generate a floating point load *) 
      and fload(ld32, ld64, ea, mem, ft, an) =
          let val (ld,size) = 
               if bit64mode andalso Gen.Size.size ea = 64 then (ld64,signed12) 
               else (ld32,signed16)
              val (r, disp) = addr(size,ea)
          in  mark(I.LF{ld=ld, ft=ft, ra=r, d=disp, mem=mem}, an) end
    
      (* Generate a floating-point binary operation *)
      and fbinary(oper, e1, e2, ft, an) = 
          mark(I.FARITH{oper=oper,fa=fexpr e1,fb=fexpr e2,ft=ft,Rc=false}, an)

      (* Generate a floating-point 3-operand operation
       * These are of the form
       *     +/- e1 * e3 +/- e2
       *)
      and f3(oper, e1, e2, e3, ft, an) =
          mark(I.FARITH3{oper=oper,fa=fexpr e1,fb=fexpr e2,fc=fexpr e3,
                         ft=ft,Rc=false}, an)

      (* Generate a floating-point unary operation *)
      and funary(oper, e, ft, an) = 
          mark(I.FUNARY{oper=oper, ft=ft, fb=fexpr e, Rc=false}, an)

      (* Reduce the expression fexp, return the register that holds
       * the value. 
       *)
      and fexpr(T.FREG(_,f)) = f
        | fexpr(e) = 
          let val ft = newFreg()
          in  doFexpr(e, ft, []); ft end
    
      (* doExpr(fexp, ft, an) -- 
       *   reduce the expression fexp, and assigns
       *   it to ft. Also annotate fexp. 
       *)
      and doFexpr(e, ft, an) =
          case e of
            T.FREG(_,fs) => fmove(fs,ft,an)

            (* Single precision support *)
          | T.FLOAD(32, ea, mem) => fload(I.LFS,I.LFSE,ea,mem,ft,an)

            (* special 3 operand floating point arithmetic *)
          | T.FADD(32,T.FMUL(32,a,c),b) => f3(I.FMADDS,a,b,c,ft,an)
          | T.FADD(32,b,T.FMUL(32,a,c)) => f3(I.FMADDS,a,b,c,ft,an)
          | T.FSUB(32,T.FMUL(32,a,c),b) => f3(I.FMSUBS,a,b,c,ft,an)
          | T.FSUB(32,b,T.FMUL(32,a,c)) => f3(I.FNMSUBS,a,b,c,ft,an)
          | T.FNEG(32,T.FADD(32,T.FMUL(32,a,c),b)) => f3(I.FNMADDS,a,b,c,ft,an)
          | T.FNEG(32,T.FADD(32,b,T.FMUL(32,a,c))) => f3(I.FNMADDS,a,b,c,ft,an)
          | T.FSUB(32,T.FNEG(32,T.FMUL(32,a,c)),b) => f3(I.FNMADDS,a,b,c,ft,an)

          | T.FADD(32, e1, e2) => fbinary(I.FADDS, e1, e2, ft, an)
          | T.FSUB(32, e1, e2) => fbinary(I.FSUBS, e1, e2, ft, an)
          | T.FMUL(32, e1, e2) => fbinary(I.FMULS, e1, e2, ft, an)
          | T.FDIV(32, e1, e2) => fbinary(I.FDIVS, e1, e2, ft, an)

            (* Double precision support *)
          | T.FLOAD(64, ea, mem) => fload(I.LFD,I.LFDE,ea,mem,ft,an)

            (* special 3 operand floating point arithmetic *)
          | T.FADD(64,T.FMUL(64,a,c),b) => f3(I.FMADD,a,b,c,ft,an)
          | T.FADD(64,b,T.FMUL(64,a,c)) => f3(I.FMADD,a,b,c,ft,an)
          | T.FSUB(64,T.FMUL(64,a,c),b) => f3(I.FMSUB,a,b,c,ft,an)
          | T.FSUB(64,b,T.FMUL(64,a,c)) => f3(I.FNMSUB,a,b,c,ft,an)
          | T.FNEG(64,T.FADD(64,T.FMUL(64,a,c),b)) => f3(I.FNMADD,a,b,c,ft,an)
          | T.FNEG(64,T.FADD(64,b,T.FMUL(64,a,c))) => f3(I.FNMADD,a,b,c,ft,an)
          | T.FSUB(64,T.FNEG(64,T.FMUL(64,a,c)),b) => f3(I.FNMADD,a,b,c,ft,an)

          | T.FADD(64, e1, e2) => fbinary(I.FADD, e1, e2, ft, an)
          | T.FSUB(64, e1, e2) => fbinary(I.FSUB, e1, e2, ft, an)
          | T.FMUL(64, e1, e2) => fbinary(I.FMUL, e1, e2, ft, an)
          | T.FDIV(64, e1, e2) => fbinary(I.FDIV, e1, e2, ft, an)
          | T.CVTI2F(64,_,e) => 
               app emitInstruction (PseudoInstrs.cvti2d{reg=expr e,fd=ft})

            (* Single/double precision support *)
          | T.FABS((32|64), e) => funary(I.FABS, e, ft, an)
          | T.FNEG((32|64), e) => funary(I.FNEG, e, ft, an)
	  | T.FSQRT(32, e)     => funary(I.FSQRTS, e, ft, an)
	  | T.FSQRT(64, e)     => funary(I.FSQRT, e, ft, an)

	  | T.CVTF2F(64,32,e)  => doFexpr(e,ft,an) (* 32->64 is a no-op *)
	  | T.CVTF2F(32,32,e)  => doFexpr(e,ft,an)
	  | T.CVTF2F(64,64,e)  => doFexpr(e,ft,an)
	  | T.CVTF2F(32,64,e)  => funary(I.FRSP,e,ft,an)

            (* Misc *)
          | T.FMARK(e, A.MARKREG f) => (f ft; doFexpr(e,ft,an))
          | T.FMARK(e, a) => doFexpr(e,ft,a::an)
          | T.FEXT e => ExtensionComp.compileFext (reducer()) {e=e,fd=ft,an=an}
          | _ => error "doFexpr"

       and ccExpr(T.CC(_,cc)) = cc
         | ccExpr(T.FCC(_,cc)) = cc
         | ccExpr(ccexp) =
           let val cc = newCCreg()
           in  doCCexpr(ccexp,cc,[]); cc end

       (* Reduce an condition expression, and assigns the result to ccd *)
       and doCCexpr(ccexp, ccd, an) = 
           case ccexp of 
              T.CMP(ty, cc, e1, e2) => 
              let val (opnds, cmp) =
                   case cc of 
                     (T.LT | T.LE | T.EQ | T.NE | T.GT | T.GE) =>
                       (immedOpnd signed16, I.CMP)
                   | _ => (immedOpnd unsigned16, I.CMPL)
                  val (opndA, opndB) = opnds(e1, e2)
                  val l  = case ty of
                             32 => false 
                           | 64 => true 
                           | _  => error "doCCexpr" 
              in mark(I.COMPARE{cmp=cmp, l=l, bf=ccd, ra=opndA, rb=opndB},an) 
              end
          | T.FCMP(fty, fcc, e1, e2) => 
             mark(I.FCOMPARE{cmp=I.FCMPU, bf=ccd, fa=fexpr e1, fb=fexpr e2},an) 
          | T.CC(_,cc) => ccmove(cc,ccd,an)
          | T.CCMARK(cc,A.MARKREG f) => (f ccd; doCCexpr(cc,ccd,an))
          | T.CCMARK(cc,a) => doCCexpr(cc,ccd,a::an)
          | T.CCEXT e =>
	    ExtensionComp.compileCCext (reducer()) {e=e, ccd=ccd, an=an}
          | _ => error "doCCexpr: Not implemented"
   
      and emitTrap() = emit(I.TW{to=31,ra=zeroR,si=I.ImmedOp 0}) 

      and beginCluster' _ =
	   (trapLabel := NONE; beginCluster 0)

      and endCluster' a =
           (case !trapLabel of 
              SOME label => 
              (defineLabel label; emitTrap(); trapLabel := NONE) 
            | NONE => ();
           endCluster a)

      and reducer() =
          TS.REDUCER{reduceRexp    = expr,
	             reduceFexp    = fexpr,
		     reduceCCexp   = ccExpr,
		     reduceStm     = stmt,
		     operand       = (fn _ => error "operand"),
		     reduceOperand = reduceOpn,
		     addressOf     = (fn _ => error "addressOf"),
		     emit          = emitInstruction o annotate,
		     instrStream   = instrStream,
		     mltreeStream  = self()
	  }
       and self() = 
       TS.S.STREAM
       { beginCluster  = beginCluster',
         endCluster    = endCluster',
         emit          = doStmt,
         pseudoOp      = pseudoOp,
         defineLabel   = defineLabel,
         entryLabel    = entryLabel,
         comment       = comment,
         annotation    = annotation,
         getAnnotations=getAnnotations,
         exitBlock     = fn mlrisc => exitBlock(cellset mlrisc)
       }
   in  self()
   end
    
end

