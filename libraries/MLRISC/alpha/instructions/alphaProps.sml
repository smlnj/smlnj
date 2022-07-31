(* alphaProps.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

functor AlphaProps
   (structure Instr : ALPHAINSTR
    structure MLTreeHash :  MLTREE_HASH where T = Instr.T
    structure MLTreeEval : MLTREE_EVAL where T = Instr.T
    ):INSN_PROPERTIES =
struct
    structure I = Instr
    structure C = I.C
    structure CB = CellsBasis

    exception NegateConditional

    fun error msg = MLRiscErrorMsg.impossible ("alphaProps."^msg)

    val zeroR = Option.valOf(C.zeroReg CB.GP)

    datatype kind = IK_JUMP | IK_NOP | IK_INSTR | IK_COPY | IK_CALL 
                  | IK_CALL_WITH_CUTS | IK_PHI | IK_SOURCE | IK_SINK

    datatype target = LABELLED of Label.label | FALLTHROUGH | ESCAPES

   (*========================================================================
    *  Instruction Kinds
    *========================================================================*)
    fun instrKind(I.ANNOTATION{i, ...}) = instrKind i
      | instrKind(I.COPY _)	 = IK_COPY
      | instrKind(I.INSTR instr) = 
	(case instr
          of (I.BRANCH _)	=> IK_JUMP
	   | (I.FBRANCH _)	=> IK_JUMP
	   | (I.JMPL _)		=> IK_JUMP
	   | (I.JSR{cutsTo=[],...}) => IK_CALL
	   | (I.JSR _)		=> IK_CALL_WITH_CUTS
	   | (I.BSR{cutsTo=[],...}) => IK_CALL
	   | (I.BSR _)		=> IK_CALL_WITH_CUTS
	   | (I.RET _)		=> IK_JUMP
	   | (I.PHI _)		=> IK_PHI
	   | (I.SOURCE _)	=> IK_SOURCE
	   | (I.SINK _)		=> IK_SINK
	   |  _			=> IK_INSTR
        (*esac*))
      | instrKind _ = IK_INSTR

    fun moveInstr(I.ANNOTATION{i, ...}) = moveInstr i
      | moveInstr(I.COPY _) = true
      | moveInstr _ = false

    val nop = 
      fn () => I.operate{oper=I.BIS, ra=zeroR, rb=I.REGop zeroR, rc=zeroR}

   (*========================================================================
    *  Parallel Move
    *========================================================================*)
    fun moveTmpR(I.COPY{tmp=SOME t, ...}) = 
	(case t of I.Direct r => SOME r | I.FDirect f => SOME f | _ => NONE)
      | moveTmpR(I.ANNOTATION{i,...}) = moveTmpR i
      | moveTmpR _ = NONE

    fun moveDstSrc(I.COPY{dst, src, ...}) = (dst, src)
      | moveDstSrc(I.ANNOTATION{i,...}) = moveDstSrc i
      | moveDstSrc _ = error "moveDstSrc"

   (*========================================================================
    *  Branches and Calls/Returns
    *========================================================================*)
    fun branchTargets(I.ANNOTATION{i,...}) = branchTargets i
      | branchTargets(I.INSTR(instr)) = 
	(case instr 
	 of (I.BRANCH{b=I.BR, lab, ...}) => [LABELLED lab]
	  | (I.BRANCH{lab, ...})  => [LABELLED lab, FALLTHROUGH] 
	  | (I.FBRANCH{lab, ...}) => [LABELLED lab, FALLTHROUGH] 
	  | (I.JMPL(_,[]))       => [ESCAPES]
	  | (I.JMPL(_,labs))     => map LABELLED labs
	  | (I.RET _)            => [ESCAPES]
	  | (I.JSR{cutsTo, ...}) => FALLTHROUGH::map LABELLED cutsTo
	  | (I.BSR{cutsTo, ...}) => FALLTHROUGH::map LABELLED cutsTo
	  |  _ => error "branchTargets"
        (*esac*))
      | branchTargets _ = error "branchTargets"

    fun jump label = I.branch{b=I.BR,r=zeroR,lab=label}

    val immedRange = {lo= ~32768, hi = 32768}
    fun loadImmed{immed,t} = 
        I.lda{r=t,b=zeroR,
              d=if #lo immedRange <= immed andalso immed <= #hi immedRange
              then I.IMMop immed else I.LABop(I.T.LI(I.T.I.fromInt(64,immed)))}
    fun loadOperand{opn,t} = I.lda{r=t,b=zeroR,d=opn}

    fun setJumpTarget(I.ANNOTATION{i,a},lab) = 
	  I.ANNOTATION{i=setJumpTarget(i,lab),a=a}
      | setJumpTarget(I.INSTR(I.BRANCH{b=I.BR,r as CB.CELL{id=31,...}, ...}), lab) = 
	  I.branch{b=I.BR,r=r,lab=lab}
      | setJumpTarget _ = error "setJumpTarget"

   fun setBranchTargets{i=I.ANNOTATION{a,i}, t, f} = 
	 I.ANNOTATION{a=a, i=setBranchTargets{i=i, t=t, f=f}}
     | setBranchTargets{i=I.INSTR(I.BRANCH{b=I.BR,r as CB.CELL{id=31,...}, ...}), ...} = 
         error "setBranchTargets"
     | setBranchTargets{i=I.INSTR(I.BRANCH{b,r,...}), t, f} =  I.branch{b=b,r=r,lab=t}
     | setBranchTargets{i=I.INSTR(I.FBRANCH{b,f,...}), t, ...} = I.fbranch{b=b,f=f,lab=t}
     | setBranchTargets _ = error "setBranchTargets"


    fun negateConditional (br, lab) = let
      fun revBranch I.BEQ  = I.BNE 
	| revBranch I.BGE  = I.BLT 
	| revBranch I.BGT  = I.BLE 
	| revBranch I.BLE  = I.BGT 
	| revBranch I.BLT  = I.BGE 
	| revBranch I.BLBC = I.BLBS 
	| revBranch I.BLBS = I.BLBC 
	| revBranch _ = raise NegateConditional
      fun revFBranch I.FBEQ  = I.FBNE 
        | revFBranch I.FBNE  = I.FBEQ 
	| revFBranch I.FBGE  = I.FBLT 
	| revFBranch I.FBGT  = I.FBLE 
	| revFBranch I.FBLE  = I.FBGT 
	| revFBranch I.FBLT  = I.FBGE 
    in
      case br
      of I.INSTR(I.BRANCH{b,r,...}) => I.branch{b=revBranch b,r=r,lab=lab}
       | I.INSTR(I.FBRANCH{b,f,...}) => I.fbranch{b=revFBranch b,f=f,lab=lab}
       | I.ANNOTATION{i,a} => I.ANNOTATION{i=negateConditional(i, lab),a=a}
       | _ => raise NegateConditional
    end

   (*========================================================================
    *  Equality and hashing for operands
    *========================================================================*)
   fun hashOpn(I.REGop r) = CB.hashCell r
     | hashOpn(I.IMMop i) = Word.fromInt i
     | hashOpn(I.HILABop l) = MLTreeHash.hash l
     | hashOpn(I.LOLABop l) = MLTreeHash.hash l
     | hashOpn(I.LABop l) = MLTreeHash.hash l

   fun eqOpn(I.REGop a,I.REGop b) = CB.sameColor(a,b)
     | eqOpn(I.IMMop a,I.IMMop b) = a = b
     | eqOpn(I.HILABop a,I.HILABop b) = MLTreeEval.==(a,b)
     | eqOpn(I.LOLABop a,I.LOLABop b) = MLTreeEval.==(a,b)
     | eqOpn(I.LABop a,I.LABop b) = MLTreeEval.==(a,b)
     | eqOpn _ = false

   (*========================================================================
    *  Definition and use (for register allocation mainly)
    *========================================================================*)
    fun defUseR instr = let
        fun alphaDU(instr) = let
	  fun Oper {oper, ra, rb=I.REGop rb, rc} = ([rc], [ra, rb])
	    | Oper {oper, ra, rb, rc} = ([rc], [ra])
	  fun Opn(I.REGop rb,rs) = rb::rs
	    | Opn(_,rs) = rs
	  fun FMem (freg, (rd, _)) = ([], [rd])
	  fun trap (def,use) =(def, use)

	in
	  case instr of
	    (* load/store instructions *)
	     I.LDA{r, b, ...} => ([r], [b])
	   | I.LDAH{r, b, ...} => ([r], [b])
	   | I.LOAD{r, b, ...} => ([r], [b])
	   | I.STORE{r, b, ...} => ([], [r,b])
	   | I.FLOAD{b, ...} => ([], [b])
	   | I.FSTORE{b, ...} => ([], [b])
	   (* branch instructions *)
	   | I.JMPL ({r, b, ...},_) => ([r], [b])
	   | I.JSR{r, b, defs, uses, ...} => (r::C.getReg defs, b::C.getReg uses)
	   | I.BSR{r, defs, uses, ...} => (r::C.getReg defs,C.getReg uses)
	   | I.RET{r, b, ...} => ([r],[b])
	   | I.BRANCH{b=I.BR, r, ...} => ([r], [])
	   | I.BRANCH{r, ...} => ([], [r])
	   (* operate *)
	   | I.OPERATE arg => Oper arg
	   | I.PSEUDOARITH {oper, ra, rb=I.REGop rb, rc, tmps} => 
	       (rc:: C.getReg tmps, [ra, rb])
	   | I.PSEUDOARITH {oper, ra, rb, rc, tmps} => (rc:: C.getReg tmps, [ra])
	   | I.OPERATEV arg => trap(Oper arg)
	   | I.CMOVE{ra,rb,rc,...} => ([rc],Opn(rb,[ra,rc]))
	   (* floating operate *)
	   | I.FOPERATEV _ => trap([], [])
	   | I.TRAPB 	=> trap([],[])
	   (* macro *)
	   | I.CALL_PAL{def,use, ...} => (C.getReg def, C.getReg use)
	   | _  		=> ([],[])
	end
    in 
      case instr
       of I.ANNOTATION{a, i, ...} => defUseR i
	| I.LIVE{regs, ...} => ([], C.getReg regs)
	| I.KILL{regs, ...} => (C.getReg regs, [])
	| I.COPY{k=CB.GP, dst, src, tmp, ...} =>
	  (case tmp
	   of SOME(I.Direct r) => (r::dst, src)
	    | SOME(I.Displace{base, ...}) => (dst, base::src)
	    | _ => (dst, src)
          (*esac*))
        | I.COPY _ => ([], [])
	| I.INSTR(i) => alphaDU(i)

    end

    (* Use of FP registers *)
    fun defUseF instr = let
      fun alphaDU instr =
	case instr of
	  I.FBRANCH{f, ...}			=>  ([],[f])
	| I.FLOAD{r, ...}			=> ([r], [])
	| I.FSTORE{r, ...}			=> ([], [r])
	| I.FOPERATE{fa, fb, fc, ...}		=> ([fc], [fa, fb])
	| I.FUNARY{fb, fc, ...}		        => ([fc], [fb])
	| I.PSEUDOARITH{tmps, ...}		=> (C.getFreg tmps, [])
	| I.FOPERATEV{fa, fb, fc, ...}		=> ([fc], [fa, fb])
	| I.FCMOVE{fa,fb,fc,...}                => ([fc], [fa, fb])
	| I.JSR{defs,uses, ...}			=> (C.getFreg defs,C.getFreg uses)
	| I.BSR{defs,uses, ...}			=> (C.getFreg defs,C.getFreg uses)
	| _ => ([],[])
    in
	case instr
	of I.ANNOTATION{a, i, ...} => defUseF i
	 | I.INSTR(i) => alphaDU(i)
	 | I.LIVE{regs, ...} => ([], C.getFreg regs)
         | I.COPY{k=CB.FP, dst, src, tmp, ...} =>
           (case tmp
             of SOME(I.FDirect f) => (f::dst, src)
	      | _ => (dst, src)
	   (*esac*))
 	 | I.COPY _ => ([], [])
	 | I.KILL{regs, ...} => (C.getFreg regs, [])
    end

    fun defUse CB.GP = defUseR
      | defUse CB.FP = defUseF
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
    | replicate(I.COPY{k, sz, tmp=SOME _, dst, src}) =  
        I.COPY{tmp=SOME(I.Direct(C.newReg())), dst=dst, src=src, k=k, sz=sz}
    | replicate i = i
end
