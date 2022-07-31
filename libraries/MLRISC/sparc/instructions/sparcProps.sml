(* sparcProps.sml
 *
 * COPYRIGHT (c) 2002 Bell Labs, Lucent Technologies
 *)

functor SparcProps
  (structure SparcInstr : SPARCINSTR
   structure MLTreeEval : MLTREE_EVAL where T = SparcInstr.T
   structure MLTreeHash : MLTREE_HASH where T = SparcInstr.T
   ) : INSN_PROPERTIES =
struct
  structure I = SparcInstr
  structure C = I.C
  structure T = I.T 
  structure CB = CellsBasis

  exception NegateConditional

  fun error msg = MLRiscErrorMsg.error("SparcProps",msg)

  datatype kind = IK_JUMP | IK_NOP | IK_INSTR | IK_COPY | IK_CALL 
                | IK_CALL_WITH_CUTS | IK_PHI | IK_SOURCE | IK_SINK
  datatype target = LABELLED of Label.label | FALLTHROUGH | ESCAPES

  val zeroR = Option.valOf(C.zeroReg CB.GP)
  val r15   = C.Reg CB.GP 15
  val r31   = C.Reg CB.GP 31

  (*========================================================================
   *  Instruction Kinds
   *========================================================================*)
  fun instrKind(I.ANNOTATION{i, ...}) = instrKind i
    | instrKind(I.COPY _)  = IK_COPY
    | instrKind(I.INSTR instr) = 
      (case instr
       of (I.Bicc _)  => IK_JUMP
	| (I.FBfcc _) => IK_JUMP
	| (I.JMP _)   => IK_JUMP
	| (I.RET _)   => IK_JUMP
	| (I.BR _)    => IK_JUMP
	| (I.BP _)    => IK_JUMP
	| (I.Ticc {t=I.BA, ...}) => IK_JUMP (* trap always *)
	| (I.CALL{cutsTo=_::_,...})  => IK_CALL_WITH_CUTS
	| (I.CALL _)  => IK_CALL
	| (I.JMPL{cutsTo=_::_,...})  => IK_CALL_WITH_CUTS
	| (I.JMPL _)  => IK_CALL
	| (I.PHI _)    => IK_PHI
	| (I.SOURCE _) => IK_SOURCE
	| (I.SINK _)   => IK_SINK
	|  _          => IK_INSTR
      (*esac*))
    | instrKind _ = error "instrKind"

  fun branchTargets(I.ANNOTATION{i,...}) = branchTargets i
    | branchTargets(I.INSTR(instr)) = 
      (case instr 
	of (I.Bicc{b=I.BA,label,...}) => [LABELLED label]
	 | (I.Bicc{label,...}) => [LABELLED label, FALLTHROUGH] 
	 | (I.FBfcc{b=I.FBA,label,...}) => [LABELLED label]
	 | (I.FBfcc{label,...}) => [LABELLED label, FALLTHROUGH]
	 | (I.BR{label,...}) => [LABELLED label, FALLTHROUGH]
	 | (I.BP{label,...}) => [LABELLED label, FALLTHROUGH]
	 | (I.JMP{labs=[],...}) => [ESCAPES] 
	 | (I.RET _)   => [ESCAPES]
	 | (I.JMP{labs,...})    => map LABELLED labs
	 | (I.CALL{cutsTo,...}) => FALLTHROUGH::map LABELLED cutsTo
	 | (I.JMPL{cutsTo,...}) => FALLTHROUGH::map LABELLED cutsTo
	 | (I.Ticc{t=I.BA, ...}) => [ESCAPES]
	 |  _ => error "branchTargets"
      (*esac*))
    | branchTargets _  = error "branchTargets"


  fun setJumpTarget(I.ANNOTATION{a,i}, l) = I.ANNOTATION{a=a, i=setJumpTarget(i,l)}
    | setJumpTarget(I.INSTR(I.Bicc{b=I.BA,a,nop,...}), L) = 
          I.bicc{b=I.BA,a=a,label=L,nop=nop}
    | setJumpTarget _ = error "setJumpTarget"


  fun setBranchTargets{i=I.ANNOTATION{a,i}, t, f} = 
          I.ANNOTATION{a=a, i=setBranchTargets{i=i, t=t, f=f}}
    | setBranchTargets{i=I.INSTR(I.Bicc{b=I.BA,a,nop,...}), ...} =  
          error "setBranchTargets: Bicc"
    | setBranchTargets{i=I.INSTR(I.Bicc{b,a,nop,...}), t, f}  =
          I.bicc{b=b,a=a,label=t,nop=nop}
    | setBranchTargets{i=I.INSTR(I.FBfcc{b,a,nop,...}), t=T, ...}  = 
          I.fbfcc{b=b, a=a, label=T, nop=nop}
    | setBranchTargets{i=I.INSTR(I.BR{rcond,p,r,a,nop,...}), t=T, ...}   = 
          I.br{rcond=rcond, p=p, r=r, a=a, label=T, nop=nop}
    | setBranchTargets{i=I.INSTR(I.BP{b,cc,p,a,nop,...}), t=T, ...}   = 
          I.bp{b=b, cc=cc, p=p, a=a, label=T, nop=nop}
    | setBranchTargets _ = error "setBranchTargets"

   fun revCond I.BA = I.BN
     | revCond I.BN = I.BA
     | revCond I.BNE = I.BE
     | revCond I.BE  = I.BNE
     | revCond I.BG  = I.BLE
     | revCond I.BLE = I.BG
     | revCond I.BGE = I.BL
     | revCond I.BL  = I.BGE
     | revCond I.BGU = I.BLEU
     | revCond I.BLEU = I.BGU
     | revCond I.BCC  = I.BCS
     | revCond I.BCS  = I.BCC
     | revCond I.BPOS = I.BNEG
     | revCond I.BNEG = I.BPOS
     | revCond I.BVC  = I.BVS
     | revCond I.BVS  = I.BVC

   fun revFcond I.FBA   = I.FBN
     | revFcond I.FBN   = I.FBA
     | revFcond I.FBU   = I.FBO
     | revFcond I.FBG   = I.FBULE
     | revFcond I.FBUG  = I.FBLE
     | revFcond I.FBL   = I.FBUGE
     | revFcond I.FBUL  = I.FBGE
     | revFcond I.FBLG  = I.FBUE
     | revFcond I.FBNE  = I.FBE
     | revFcond I.FBE   = I.FBNE
     | revFcond I.FBUE  = I.FBLG
     | revFcond I.FBGE  = I.FBUL
     | revFcond I.FBUGE = I.FBL
     | revFcond I.FBLE  = I.FBUG
     | revFcond I.FBULE = I.FBG
     | revFcond I.FBO   = I.FBU

  fun revRcond I.RZ   = I.RNZ
    | revRcond I.RLEZ = I.RGZ
    | revRcond I.RLZ  = I.RGEZ
    | revRcond I.RNZ  = I.RZ
    | revRcond I.RGZ  = I.RLEZ
    | revRcond I.RGEZ = I.RLZ

  fun revP I.PT = I.PN
    | revP I.PN = I.PT

  fun negateConditional (I.INSTR(I.Bicc{b,a,nop,...}), lab) =
         I.bicc{b=revCond b,a=a,label=lab,nop=nop}
    | negateConditional (I.INSTR(I.FBfcc{b,a,nop,...}), lab) =
         I.fbfcc{b=revFcond b,a=a,label=lab,nop=nop} 
    | negateConditional (I.INSTR(I.BR{p,r,rcond,a,nop,...}), lab) =
         I.br{p=revP p,a=a,r=r,rcond=revRcond rcond,label=lab,nop=nop} 
    | negateConditional (I.INSTR(I.BP{b,cc,p,a,nop,...}), lab) =
         I.bp{p=revP p,a=a,b=revCond b,cc=cc,label=lab,nop=nop} 
    | negateConditional (I.ANNOTATION{i,a}, lab) = 
         I.ANNOTATION{i=negateConditional(i, lab), a=a}
    | negateConditional _ = raise NegateConditional

  fun jump label = I.bicc{b=I.BA,a=true,label=label,nop=true}

  val immedRange = {lo= ~4096, hi = 4095}

  fun loadImmed{immed,t} = 
      I.arith{a=I.OR,r=zeroR,i=
              if #lo immedRange <= immed andalso immed <= #hi immedRange 
              then I.IMMED immed else I.LAB(T.LI(IntInf.fromInt immed)),d=t}
  fun loadOperand{opn, t} = I.arith{a=I.OR,r=zeroR,i=opn, d=t}

  fun moveInstr(I.ANNOTATION{i,...}) = moveInstr i
    | moveInstr(I.COPY _)	    = true
    | moveInstr(I.LIVE _)           = false
    | moveInstr(I.KILL _)           = false
    | moveInstr _          = false

  fun nop() = I.sethi{d=zeroR, i=0}

  (*========================================================================
   *  Parallel Move
   *========================================================================*)
  fun moveTmpR(I.COPY{tmp, ...}) = 
      (case tmp 
	of SOME(I.Direct r) => SOME r
	 | SOME(I.FDirect f) => SOME f
	 | _ => NONE
      (*esac*))
    | moveTmpR(I.ANNOTATION{i,...}) = moveTmpR i
    | moveTmpR _ = NONE


  fun moveDstSrc(I.COPY{dst,src,...}) = (dst,src)
    | moveDstSrc(I.ANNOTATION{i,...}) = moveDstSrc i
    | moveDstSrc _ = error "moveDstSrc"

  (*========================================================================
   *  Equality and hashing
   *========================================================================*)
   fun hashOpn(I.REG r) = CB.hashCell r
     | hashOpn(I.IMMED i) = Word.fromInt i
     | hashOpn(I.LAB l) = MLTreeHash.hash l
     | hashOpn(I.LO l) = MLTreeHash.hash l
     | hashOpn(I.HI l) = MLTreeHash.hash l
   fun eqOpn(I.REG a,I.REG b) = CB.sameColor(a,b)
     | eqOpn(I.IMMED a,I.IMMED b) = a = b
     | eqOpn(I.LAB a,I.LAB b) = MLTreeEval.==(a,b)
     | eqOpn(I.LO a,I.LO b) = MLTreeEval.==(a,b)
     | eqOpn(I.HI a,I.HI b) = MLTreeEval.==(a,b)
     | eqOpn _ = false

  fun defUseR instr = let
    fun oper (I.REG r,def,use) = (def,r::use)
      | oper (_,def,use)       = (def,use)
    fun sparcDU instr =
      (case instr 
       of  I.LOAD {r,d,i,...} => oper(i,[d],[r])
	| I.STORE {r,d,i,...} => oper(i,[],[r,d])
	| I.FLOAD {r,d,i,...} => oper(i,[],[r])
	| I.FSTORE {r,d,i,...} => oper(i,[],[r])
	| I.SETHI {d,...} => ([d],[])
	| I.ARITH {r,i,d,...} => oper(i,[d],[r])
	| I.SHIFT {r,i,d,...} => oper(i,[d],[r])
	| I.JMPL{defs,uses,d,r,i,...} => 
	     oper(i,d:: C.getReg defs,r:: C.getReg uses)
	| I.BR{r,...} => ([],[r])
	| I.MOVicc{i,d,...} => oper(i,[d],[d])
	| I.MOVfcc{i,d,...} => oper(i,[d],[d])
	| I.MOVR{r,i,d,...} => oper(i,[d],[r,d])
	| I.CALL{defs,uses,...} => (r15 :: C.getReg defs, C.getReg uses)
	| I.JMP{r,i,...} => oper(i,[],[r])
	| I.RET{leaf=false,...} => ([],[r31])
	| I.RET{leaf=true,...} => ([],[r15])
	| I.SAVE{r,i,d} => oper(i,[d],[r])
	| I.RESTORE{r,i,d} => oper(i,[d],[r])
	| I.Ticc{r,i,...} => oper(i,[],[r]) 
	| I.RDY{d,...} => ([d],[]) 
	| I.WRY{r,i,...} => oper(i,[],[r]) 
	| _ => ([],[])  
     (*esac*))
  in 
      case instr
       of I.ANNOTATION{i, ...} => defUseR i
	| I.LIVE{regs, ...} => ([], C.getReg regs)
	| I.KILL{regs, ...} => (C.getReg regs, [])
	| I.INSTR(i) => sparcDU(i)
	| I.COPY{k, dst, src, tmp, ...} => let
	    val (d,u) = case k of CB.GP => (dst, src) | _ => ([], [])
          in
	      case tmp 
	      of SOME(I.Direct r) => (r::d, u)
	       | SOME(I.Displace{base, ...}) => (d, base::u)
	       | _ => (d,u)
          end
  end

  (* Use of FP registers *)
  fun defUseF instr = let
    fun sparcDU instr =
     (case instr of
        I.FLOAD{r,d,i,...} => ([d],[])
      | I.FSTORE{r,d,i,...} => ([],[d])
      | I.FPop1{r,d,...} => ([d],[r])
      | I.FPop2{r1,r2,d,...} => ([d],[r1,r2])
      | I.FCMP{r1,r2,...} => ([],[r1,r2])
      | I.JMPL{defs,uses,...} => (C.getFreg defs,C.getFreg uses)
      | I.CALL{defs,uses,...} => (C.getFreg defs,C.getFreg uses)
      | I.FMOVicc{r,d,...} => ([d],[r,d])
      | I.FMOVfcc{r,d,...} => ([d],[r,d])
      | _ => ([],[])
     (*esac*))
  in
     case instr
      of I.ANNOTATION{i, ...} => defUseF i
       | I.LIVE{regs, ...} => ([], C.getFreg regs)
       | I.KILL{regs, ...} => (C.getFreg regs, [])
       | I.COPY{k, dst, src, tmp, ...} => let
	   val (d, u) = case k of CB.FP => (dst, src) | _ => ([],[])
         in
	     case tmp
	      of SOME(I.FDirect f) => (f::d, u)
	       | _ => (d, u)
         end
       | I.INSTR(i) => sparcDU(i)
  end

  fun defUse CB.GP = defUseR
    | defUse CB.FP = defUseF
    | defUse _    = error "defUse"

  (*========================================================================
   *  Annotations 
   *========================================================================*)
  fun getAnnotations(I.ANNOTATION{i,a}) = 
       let val (i,an) = getAnnotations i in (i,a::an) end
    | getAnnotations i = (i,[])
  fun annotate(i,a) = I.ANNOTATION{i=i,a=a}

  (*========================================================================
   *  Replicate an instruction
   *========================================================================*)
  fun replicate(I.ANNOTATION{i,a}) = I.ANNOTATION{i=replicate i,a=a}
    | replicate(I.COPY{k, sz, tmp=SOME _, dst, src}) =  
        I.COPY{k=k, sz=sz, tmp=SOME(I.Direct(C.newReg())), dst=dst, src=src}
    | replicate i = i
end
