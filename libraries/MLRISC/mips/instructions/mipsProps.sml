(* mipsProps.sml
 *
 * Modified from AlphaProps
 * -- Allen
 *)

functor MIPSProps(MIPSInstr:MIPSINSTR) : INSN_PROPERTIES =
struct
    structure I = MIPSInstr
    structure C = I.C
    structure LE = I.LabelExp

    exception NegateConditional

    fun error msg = MLRiscErrorMsg.error ("mipsProps.",msg)

    val zeroR = C.r0 

    datatype kind = IK_JUMP | IK_NOP | IK_INSTR | IK_COPY | IK_CALL 
                  | IK_CALL_WITH_CUTS | IK_PHI | IK_SOURCE | IK_SINK
    datatype target = LABELLED of Label.label | FALLTHROUGH | ESCAPES

   (*========================================================================
    *  Instruction Kinds
    *========================================================================*)
    fun instrKind(I.BRANCH _)  = IK_JUMP
      | instrKind(I.FBRANCH _) = IK_JUMP
      | instrKind(I.JR _)      = IK_JUMP
      | instrKind(I.J _)       = IK_JUMP
      | instrKind(I.COPY _)    = IK_COPY
      | instrKind(I.FCOPY _)   = IK_COPY
      | instrKind(I.JAL{cutsTo=[],...}) = IK_CALL
      | instrKind(I.JAL _) = IK_CALL_WITH_CUTS
      | instrKind(I.JALR{cutsTo=[],...}) = IK_CALL
      | instrKind(I.JALR _) = IK_CALL_WITH_CUTS
      | instrKind(I.RET _)     = IK_JUMP
      | instrKind(I.PHI _)     = IK_PHI
      | instrKind(I.SOURCE _)  = IK_SOURCE
      | instrKind(I.SINK _)    = IK_SINK
      | instrKind(I.ANNOTATION{i,...}) = instrKind i
      | instrKind _            = IK_INSTR

    fun moveInstr(I.COPY _)  = true
      | moveInstr(I.FCOPY _) = true
      | moveInstr(I.ANNOTATION{i,...}) = moveInstr i
      | moveInstr _	     = false

    val nop = fn () => I.ARITH{oper=I.ADD, rs=zeroR, i=I.Reg zeroR, rt=zeroR}

   (*========================================================================
    *  Parallel Move
    *========================================================================*)
    fun moveTmpR(I.COPY{tmp=SOME(I.Direct r), ...}) = SOME r
      | moveTmpR(I.FCOPY{tmp=SOME(I.FDirect f), ...}) = SOME f
      | moveTmpR(I.ANNOTATION{i,...}) = moveTmpR i
      | moveTmpR _ = NONE

    fun moveDstSrc(I.COPY{dst, src, ...}) = (dst, src)
      | moveDstSrc(I.FCOPY{dst, src, ...}) = (dst, src)
      | moveDstSrc(I.ANNOTATION{i,...}) = moveDstSrc i
      | moveDstSrc _ = error "moveDstSrc"

   (*========================================================================
    *  Branches and Calls/Returns
    *========================================================================*)
    fun branchTargets(I.J{lab, ...}) = [LABELLED lab]
      | branchTargets(I.BRANCH{lab, ...})  = [LABELLED lab, FALLTHROUGH] 
      | branchTargets(I.FBRANCH{lab, ...}) = [LABELLED lab, FALLTHROUGH] 
      | branchTargets(I.JR{labels=[],...}) = [ESCAPES]
      | branchTargets(I.JR{labels,...})    = map LABELLED labels
      | branchTargets(I.JAL{cutsTo, ...})  = FALLTHROUGH::map LABELLED cutsTo
      | branchTargets(I.JALR{cutsTo, ...}) = FALLTHROUGH::map LABELLED cutsTo
      | branchTargets(I.RET _)             = [ESCAPES]
      | branchTargets(I.ANNOTATION{i,...}) = branchTargets i
      | branchTargets _ = error "branchTargets"

    fun jump label = I.J{lab=label, nop=true}

    val immedRange = {lo= ~32768, hi = 32768}
    fun loadImmed{immed,t} = error "loadImmed"
    fun loadOperand{opn,t} = error "loadOperand"

    fun setTargets(I.ANNOTATION{i,a},labs) = 
            I.ANNOTATION{i=setTargets(i,labs),a=a}
      | setTargets(i,_) = i

    fun negateConditional br = let
    in
       raise NegateConditional
    end

   (*========================================================================
    *  Equality and hashing for operands
    *========================================================================*)
   fun hashOpn(I.Reg r) = C.hashCell r
     | hashOpn(I.Imm i) = Word.fromInt i
     | hashOpn(I.HiLab l) = I.LabelExp.hash l
     | hashOpn(I.LoLab l) = I.LabelExp.hash l
     | hashOpn(I.Lab l) = I.LabelExp.hash l

   fun eqOpn(I.Reg a,I.Reg b) = C.sameColor(a,b)
     | eqOpn(I.Imm a,I.Imm b) = a = b
     | eqOpn(I.HiLab a,I.HiLab b) = I.LabelExp.==(a,b)
     | eqOpn(I.LoLab a,I.LoLab b) = I.LabelExp.==(a,b)
     | eqOpn(I.Lab a,I.Lab b) = I.LabelExp.==(a,b)
     | eqOpn _ = false

   (*========================================================================
    *  Definition and use (for register allocation mainly)
    *========================================================================*)
    fun defUseR instr =
      let
        fun opnd2(rs1, I.Reg rs2) = [rs1,rs2]
          | opnd2(rs1, _) = [rs1]
      in
	case instr of
           I.LUI{rt, ...} => ([rt], [])

	  (* load/store instructions *)
	 | I.LOAD{rt, b, ...} => ([rt], [b])
         | I.STORE{rs, b, ...} => ([], [rs, b])
	 | I.FLOAD{b, d, ...} => ([], opnd2(b, d))
	 | I.FSTORE{b, d, ...} => ([], opnd2(b, d))

	 (* branch+call instructions *)
         | I.JR{rs, ...} => ([], [rs])
	 | I.JAL{defs, uses, ...} => (C.linkR::C.getReg defs, C.getReg uses)
	 | I.JALR{rt, rs, defs, uses, ...} => 
		(rt::C.getReg defs,rs::C.getReg uses)
         | I.RET _ => ([], [C.linkR])

	 (* arithmetic *)
         | I.TRAP{rs, i, ...} => ([], opnd2(rs, i))
	 | I.ARITH{rt, rs, i, ...} => ([rt], opnd2(rs, i)) 
         | I.UNARY{rt, rs, ...} => ([rt], [rs])
         | I.MULTIPLY{rs, rt, ...} => ([], [rs, rt])
         | I.DIVIDE{rs, rt, ...} => ([], [rs, rt])
         | I.MTLO rs => ([], [rs])
         | I.MTHI rs => ([], [rs])
         | I.MFLO rt => ([rt], [])
         | I.MFHI rt => ([rt], [])

         | I.FROUND{rs2, ...} => ([], [rs2]) 
         | I.CVTI2F{rs, ...} => ([], [rs])
         | I.CVTF2I{rt, ...} => ([rt], [])

	 (* copy *)
	 | I.COPY{dst, src, tmp=SOME(I.Direct r), ...} => (r::dst, src)
	 | I.COPY{dst, src, ...} => (dst, src)

         | I.ANNOTATION{a=C.DEF_USE{cellkind=C.GP,defs,uses}, i, ...} => 
           let val (d,u) = defUseR i in (defs@d, u@uses) end
         | I.ANNOTATION{a, i, ...} => defUseR i
	 | _  		=> ([],[])
      end

    (* Use of FP registers *)
    fun defUseF instr =
      case instr of
        I.FLOAD{ft, ...}			=> ([ft], [])
      | I.FSTORE{fs, ...}			=> ([], [fs])
      | I.FARITH{fs1, fs2, ft, ...}		=> ([ft], [fs1, fs2])
      | I.FARITH3{fs1, fs2, fs3, ft, ...}	=> ([ft], [fs1, fs2, fs3])
      | I.FUNARY{fs, ft, ...}		        => ([ft], [fs])
      | I.FROUND{ft, fs1, ...} => ([ft], [fs1]) 
      | I.CVTF2I{fs, ...} => ([], [fs])
      | I.CVTI2F{ft, ...} => ([ft], [])
      | I.FCMP{fs1, fs2, ...} => ([], [fs1, fs2])
      | I.FCOPY{dst, src, tmp=SOME(I.FDirect f), ...} => (f::dst, src)
      | I.FCOPY{dst, src, ...}			=> (dst, src) 
      | I.JALR{defs,uses, ...}	     => (C.getFreg defs,C.getFreg uses)
      | I.JAL{defs,uses, ...}	     => (C.getFreg defs,C.getFreg uses)
      | I.ANNOTATION{a=C.DEF_USE{cellkind=C.FP,defs,uses}, i, ...} => 
        let val (d,u) = defUseF i in (defs@d, u@uses) end
      | I.ANNOTATION{a, i, ...} => defUseF i
      | _ => ([],[])

    fun defUse C.GP = defUseR
      | defUse C.FP = defUseF
      | defUse _ = error "defUse"

    (*=======================================================================
     *  Annotations 
     *=======================================================================*)
    fun getAnnotations(I.ANNOTATION{i,a}) = 
         let val (i,an) = getAnnotations i in (i,a::an) end
      | getAnnotations i = (i,[])
    fun annotate(i,a) = I.ANNOTATION{i=i,a=a}

  (*========================================================================
   *  Replicate an instruction
   *========================================================================*)
  fun replicate(I.ANNOTATION{i,a}) = I.ANNOTATION{i=replicate i,a=a}
    | replicate(I.COPY{tmp=SOME _, dst, src, impl}) =  
        I.COPY{tmp=SOME(I.Direct(C.newReg())), dst=dst, src=src, impl=ref NONE}
    | replicate(I.FCOPY{tmp=SOME _, dst, src, impl}) = 
        I.FCOPY{tmp=SOME(I.FDirect(C.newFreg())), 
                dst=dst, src=src, impl=ref NONE}
    | replicate i = i
end
