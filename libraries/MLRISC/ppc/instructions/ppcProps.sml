(* ppcProps.sml
 *
 * COPYRIGHT (c) 2002 Bell Labs, Lucent Technologies
 *)

functor PPCProps
   ( structure PPCInstr : PPCINSTR
     structure MLTreeEval : MLTREE_EVAL where T = PPCInstr.T
     structure MLTreeHash : MLTREE_HASH where T = PPCInstr.T
    ) : INSN_PROPERTIES = 
struct
  structure I = PPCInstr
  structure C = I.C
  structure T = I.T 
  structure CB = CellsBasis

  exception NegateConditional

  fun error msg = MLRiscErrorMsg.error("PPCProps",msg)

  datatype kind = IK_JUMP | IK_NOP | IK_INSTR | IK_COPY | IK_CALL 
                | IK_CALL_WITH_CUTS | IK_PHI | IK_SOURCE | IK_SINK
  datatype target = LABELLED of Label.label | FALLTHROUGH | ESCAPES

  (* This stupid architecture doesn't really have a dedicated zero register *)
  fun zeroR() = C.Reg CB.GP 0

  fun instrKind(I.ANNOTATION{i, ...}) = instrKind i
    | instrKind(I.COPY _) = IK_COPY
    | instrKind(I.INSTR instr) = let
	fun eqTest to = Word.andb(Word.fromInt to, 0w4) <> 0w0
	fun trapAlways{to, ra, si} = 
	  (case si
	    of I.RegOp rb => 
	       if CellsBasis.sameColor(ra,rb) andalso eqTest(to) then IK_JUMP
	       else IK_INSTR
             | I.ImmedOp 0 =>
	       if CellsBasis.registerId ra = 0 andalso eqTest(to) then IK_JUMP
	       else IK_INSTR
	     | _ => error "trapAlways: neither RegOp nor ImmedOp(0)"
          (*esac*))
      in
	case instr
	 of (I.BC _) => IK_JUMP
	  | (I.BCLR _) => IK_JUMP
	  | (I.B _) => IK_JUMP
	  | (I.TW t) => trapAlways(t)
	  | (I.TD t) => trapAlways(t)
	  | (I.ARITHI{oper=I.ORI, rt, ra, im=I.ImmedOp 0}) => 
	       if CB.registerId rt = 0 andalso CB.registerId ra = 0 then IK_NOP
	       else IK_INSTR
	  | (I.CALL{cutsTo=_::_,...}) => IK_CALL_WITH_CUTS
	  | (I.CALL _) => IK_CALL
	  | (I.PHI _)    => IK_PHI
	  | (I.SOURCE _) => IK_SOURCE
	  | (I.SINK _)   => IK_SINK
	  |  _ => IK_INSTR
        (*esac*)
      end
    | instrKind _ = error "instrKind"

  fun moveInstr(I.COPY _) = true
    | moveInstr(I.ANNOTATION{i,...}) = moveInstr i
    | moveInstr  _ = false

  fun nop () = I.arithi{oper=I.ORI, rt=zeroR(), ra=zeroR(), im=I.ImmedOp 0}

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


  fun branchTargets(I.INSTR(I.BC{bo=I.ALWAYS, addr,  ...})) = 
      (case addr
        of I.LabelOp(T.LABEL lab) => [LABELLED lab]
         | _ => error "branchTargets:BC:ALWAYS"
      (*esac*))
    | branchTargets(I.INSTR(I.BC{addr, ...})) = 
      (case addr
        of I.LabelOp(T.LABEL lab) => [LABELLED lab, FALLTHROUGH]
         | _ => error "branchTargets:BC"
      (*esac*))
    | branchTargets(I.INSTR(I.BCLR{labels, bo=I.ALWAYS, ...})) = 
      (case labels of [] => [ESCAPES] | _ => map LABELLED labels)
    | branchTargets(I.INSTR(I.BCLR{labels,  ...})) = 
      (case labels of [] => [ESCAPES, FALLTHROUGH] | _ => map LABELLED labels)
    | branchTargets(I.INSTR(I.B{addr=I.LabelOp(T.LABEL lab), LK})) = [LABELLED lab]
    | branchTargets(I.INSTR(I.CALL{cutsTo, ...})) = FALLTHROUGH::map LABELLED cutsTo
    | branchTargets(I.INSTR(I.TD _)) = [ESCAPES]
    | branchTargets(I.INSTR(I.TW _)) = [ESCAPES]
    | branchTargets(I.ANNOTATION{i,...}) = branchTargets i
    | branchTargets _ = error "branchTargets"

  fun labelOp l = I.LabelOp(T.LABEL l)

  fun setJumpTarget(I.ANNOTATION{a,i}, l) = I.ANNOTATION{a=a, i=setJumpTarget(i,l)}
    | setJumpTarget(I.INSTR(I.BC{bo as I.ALWAYS, bf, bit, addr, fall, LK}), lab) = 
        I.bc{bo=bo, bf=bf, bit=bit, fall=fall, LK=LK, addr=labelOp lab}
    | setJumpTarget(I.INSTR(I.B{addr, LK}), lab) = I.b{addr=labelOp(lab), LK=LK}
    | setJumpTarget _ = error "setJumpTarget"

  fun setBranchTargets{i=I.ANNOTATION{a,i}, t, f} = 
        I.ANNOTATION{a=a, i=setBranchTargets{i=i, t=t, f=f}}
    | setBranchTargets{i=I.INSTR(I.BC{bo=I.ALWAYS, bf, bit, addr, fall, LK}), ...} = 
        error "setBranchTargets"
    | setBranchTargets{i=I.INSTR(I.BC{bo, bf, bit, addr, fall, LK}), t, f} = 
        I.bc{bo=bo, bf=bf, bit=bit, LK=LK, addr=labelOp t, fall=labelOp f}
    | setBranchTargets _ = error "setBranchTargets"

  fun jump lab = I.b{addr=I.LabelOp(T.LABEL lab), LK=false}

  fun negateConditional(I.ANNOTATION{a,i}, l) = 
        I.ANNOTATION{a=a, i=negateConditional(i, l)}
    | negateConditional(I.INSTR(I.BC{bo, bf, bit, addr, fall, LK}), lab) = let
       val bo' = (case bo 
	 of I.TRUE => I.FALSE
	  | I.FALSE => I.TRUE
	  | I.ALWAYS => error "negateCondtional: ALWAYS"
	  | I.COUNTER{eqZero, cond=NONE} => I.COUNTER{eqZero=not eqZero, cond=NONE}
	  | I.COUNTER{eqZero, cond=SOME b} => error "negateConditional: COUNTER"
        (*esac*))
      in 
	  I.bc{bo=bo', bf=bf, bit=bit, addr=labelOp lab, fall=fall, LK=LK}
      end
    | negateConditional _ = error "negateConditional"

  val immedRange = {lo= ~32768, hi=32767}

  fun loadImmed{immed,t} = 
       I.arithi
         {oper=I.ADDI, rt=t, ra=zeroR(), 
          im=if #lo immedRange <= immed andalso immed <= #hi immedRange
             then I.ImmedOp immed else I.LabelOp(I.T.LI(IntInf.fromInt immed))}
  fun loadOperand{opn,t} = 
       I.arithi{oper=I.ADDI, rt=t, ra=zeroR(), im=opn}


  fun hashOpn(I.RegOp r) = CB.hashCell r
    | hashOpn(I.ImmedOp i) = Word.fromInt i
    | hashOpn(I.LabelOp l) = MLTreeHash.hash l
  fun eqOpn(I.RegOp a,I.RegOp b) = CB.sameColor(a,b)
    | eqOpn(I.ImmedOp a,I.ImmedOp b) = a = b
    | eqOpn(I.LabelOp a,I.LabelOp b) = MLTreeEval.==(a,b)
    | eqOpn _ = false

  fun defUseR instr = let
    fun ppcDU instr = let
      fun operand(I.RegOp r,use) = r::use
	| operand(_,use) = use
    in
      case instr
      of I.L{rt, ra, d, ...} => ([rt], operand(d,[ra]))
       | I.LF{ra, d, ...} => ([], operand(d,[ra]))
       | I.ST{rs, ra, d, ...} => ([], operand(d,[rs,ra]))
       | I.STF{ra, d, ...} => ([], operand(d,[ra]))
       | I.UNARY{rt, ra, ...} => ([rt], [ra])
       | I.ARITH{rt, ra, rb, ...} => ([rt], [ra,rb])
       | I.ARITHI{rt, ra, im, ...} => ([rt], operand(im,[ra]))
       | I.ROTATE{ra, rs, sh, ...} => ([ra], [rs,sh])
       | I.ROTATEI{ra, rs, sh, ...} => ([ra], operand(sh,[rs]))
       | I.COMPARE{ra, rb, ...} => ([], operand(rb,[ra]))
       | I.MTSPR{rs, ...} => ([], [rs])
       | I.MFSPR{rt, ...} => ([rt], [])
       | I.TW{to, ra, si} => ([], operand(si,[ra]))
       | I.TD{to, ra, si} => ([], operand(si,[ra]))
       | I.CALL{def, use, ...} => (C.getReg def, C.getReg use)
       | I.LWARX{rt, ra, rb, ...} => ([rt], [ra, rb])
       | I.STWCX{rs, ra, rb, ...} => ([], [rs, ra, rb]) 
       | _ => ([], [])
    end
  in
    case instr
     of I.ANNOTATION{i, ...} => defUseR i
      | I.LIVE{regs, ...} => ([], C.getReg regs)
      | I.KILL{regs, ...} => (C.getReg regs, [])
      | I.INSTR(i) => ppcDU(i)
      | I.COPY{k, dst, src, tmp, ...} => let
	   val (d,u) = case k of CB.GP => (dst, src) | _ => ([], [])
	 in
	     case tmp 
	     of SOME(I.Direct r) => (r::d, u)
	      | SOME(I.Displace{base, ...}) => (d, base::u)
	      | _ => (d,u)
	 end
  end

  fun defUseF instr = let
    fun ppcDU instr = 
     (case instr
      of I.LF{ft, ...} => ([ft],[])
       | I.STF{fs, ...} => ([], [fs])
       | I.FCOMPARE{fa, fb, ...}  => ([], [fa, fb])
       | I.FUNARY{ft, fb, ...}  => ([ft], [fb])
       | I.FARITH{ft, fa, fb, ...}  => ([ft], [fa, fb])
       | I.FARITH3{ft, fa, fb, fc, ...}  => ([ft], [fa, fb, fc])
       | I.CALL{def, use, ...} => (C.getFreg def,C.getFreg use)
       | _ => ([], [])
      (*esac*))
  in
    case instr
     of I.ANNOTATION{i, ...} => defUseF i
      | I.LIVE{regs, ...} => ([], C.getFreg regs)
      | I.KILL{regs, ...} => (C.getFreg regs, [])
      | I.INSTR(i) => ppcDU(i)
      | I.COPY{k, dst, src, tmp, ...} => let
	   val (d, u) = case k of CB.FP => (dst, src) | _ => ([],[])
         in
	     case tmp
	      of SOME(I.FDirect f) => (f::d, u)
	       | _ => (d, u)
         end
  end
  fun defUseCC instr = error "defUseCC: not implemented"

  fun defUse CB.GP = defUseR
    | defUse CB.FP = defUseF
    | defUse CB.CC = defUseCC
    | defUse _ = error "defUse"

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


