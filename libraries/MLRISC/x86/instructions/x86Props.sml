(* x86Props.sml -- 32bit, x86 instruction semantic properties
 *
 * COPYRIGHT (c) 1997 Bell Laboratories.
 *)

functor X86Props
  (structure Instr : X86INSTR
   structure MLTreeHash : MLTREE_HASH where T = Instr.T
   structure MLTreeEval : MLTREE_EVAL where T = Instr.T
  ) : INSN_PROPERTIES =
struct
  structure I = Instr
  structure C = I.C
  structure T = I.T 
  structure CB = CellsBasis

  exception NegateConditional

  fun error msg = MLRiscErrorMsg.error("X86Props",msg)

  datatype kind = IK_JUMP | IK_NOP | IK_INSTR | IK_COPY | IK_CALL 
                | IK_CALL_WITH_CUTS | IK_PHI | IK_SOURCE | IK_SINK
  datatype target = LABELLED of Label.label | FALLTHROUGH | ESCAPES
 (*========================================================================
  *  Instruction Kinds
  *========================================================================*)
  fun instrKind (I.ANNOTATION{i, ...}) = instrKind i
    | instrKind (I.COPY _) = IK_COPY
    | instrKind (I.INSTR i)  = 
       (case i 
	 of I.JMP _ => IK_JUMP
	  | I.JCC _ => IK_JUMP
	  | I.CALL{cutsTo=_::_,...} => IK_CALL_WITH_CUTS
	  | I.CALL _ => IK_CALL
	  | I.PHI _    => IK_PHI
	  | I.SOURCE _ => IK_SOURCE
	  | I.SINK _   => IK_SINK
	  | I.RET _ => IK_JUMP
	  | I.INTO => IK_JUMP
	  | _ => IK_INSTR)
    | instrKind _ = IK_INSTR

  fun moveInstr(I.ANNOTATION{i, ...}) = moveInstr i
    | moveInstr(I.LIVE _) = false
    | moveInstr(I.KILL _) = false
    | moveInstr(I.COPY _) = true
    | moveInstr(I.INSTR i)  = 
       (case i
         of I.MOVE{mvOp=I.MOVL, src=I.Direct _, dst=I.MemReg _, ...} => true
	  | I.MOVE{mvOp=I.MOVL, src=I.MemReg _, dst=I.Direct _, ...} => true
	  | I.FMOVE{fsize=I.FP64,src=I.FPR _,dst=I.FPR _, ...} => true
	  | I.FMOVE{fsize=I.FP64,src=I.FPR _,dst=I.FDirect _, ...} => true
	  | I.FMOVE{fsize=I.FP64,src=I.FDirect _,dst=I.FPR _, ...} => true
	  | I.FMOVE{fsize=I.FP64,src=I.FDirect _,dst=I.FDirect _, ...} => true
	  | _ => false )


  fun isMemMove(I.INSTR(i)) = 
      (case i
	of I.MOVE{mvOp=I.MOVL, src=I.Direct _, dst=I.MemReg _, ...} => true
	 | I.MOVE{mvOp=I.MOVL, src=I.MemReg _, dst=I.Direct _, ...} => true
	 | I.FMOVE{fsize=I.FP64,src=I.FPR _,dst=I.FPR _, ...} => true
	 | I.FMOVE{fsize=I.FP64,src=I.FPR _,dst=I.FDirect _, ...} => true
	 | I.FMOVE{fsize=I.FP64,src=I.FDirect _,dst=I.FPR _, ...} => true
	 | I.FMOVE{fsize=I.FP64,src=I.FDirect _,dst=I.FDirect _, ...} => true
	 | _ => false 
      (*esac*))
    | isMemMove _ = false


  fun memMove(I.INSTR(i)) = 
      (case i
        of I.MOVE{src=I.Direct rs, dst=I.MemReg rd, ...} => ([rd], [rs])
	 | I.MOVE{src=I.MemReg rs, dst=I.Direct rd, ...} => ([rd], [rs])
	 | I.FMOVE{src=I.FPR rs, dst=I.FPR rd, ...} => ([rd], [rs])
	 | I.FMOVE{src=I.FDirect rs, dst=I.FPR rd, ...} => ([rd], [rs])
	 | I.FMOVE{src=I.FPR rs, dst=I.FDirect rd, ...} => ([rd], [rs])
	 | I.FMOVE{src=I.FDirect rs, dst=I.FDirect rd, ...} => ([rd], [rs])
	 |  _ => error "memMove: INSTR"
      (*esac*))
    | memMove _ = error "memMove"
	
    val nop = fn () => I.nop


 (*========================================================================
  *  Parallel Move
  *========================================================================*)
  fun moveTmpR(I.ANNOTATION{i,...}) = moveTmpR i
    | moveTmpR(I.COPY{k=CB.GP, tmp=SOME(I.Direct r), ...}) = SOME r
    | moveTmpR(I.COPY{k=CB.FP, tmp=SOME(I.FDirect f), ...}) = SOME f
    | moveTmpR(I.COPY{k=CB.FP, tmp=SOME(I.FPR f), ...}) = SOME f 
    | moveTmpR _ = NONE

  fun moveDstSrc(I.ANNOTATION{i,...}) = moveDstSrc i
    | moveDstSrc(I.COPY{src, dst, ...}) = (dst, src)
    | moveDstSrc(I.INSTR i) = 
      (case i
        of I.MOVE{src=I.Direct rs, dst=I.MemReg rd, ...} => ([rd], [rs])
	 | I.MOVE{src=I.MemReg rs, dst=I.Direct rd, ...} => ([rd], [rs])
	 | I.FMOVE{src=I.FPR rs, dst=I.FPR rd, ...} => ([rd], [rs])
	 | I.FMOVE{src=I.FDirect rs, dst=I.FPR rd, ...} => ([rd], [rs])
	 | I.FMOVE{src=I.FPR rs, dst=I.FDirect rd, ...} => ([rd], [rs])
	 | I.FMOVE{src=I.FDirect rs, dst=I.FDirect rd, ...} => ([rd], [rs])
	 |  _ => error "moveDstSrc")
    | moveDstSrc _ = error "moveDstSrc2"
 (*=====================================================================
  *  Branches and Calls/Returns
  *=====================================================================*)
  fun branchTargets(I.ANNOTATION{i,...}) = branchTargets i
    | branchTargets(I.INSTR i) = 
      (case i
        of I.JMP(_, []) => [ESCAPES]
	 | I.JMP(_, labs) => map LABELLED labs
	 | I.RET _ => [ESCAPES]
	 | I.JCC{opnd=I.ImmedLabel(T.LABEL(lab)), ...} => 
	     [FALLTHROUGH, LABELLED lab]
	 | I.CALL{cutsTo, ...} => FALLTHROUGH :: map LABELLED cutsTo
	 | I.INTO => [ESCAPES]
	 |  _ => error "branchTargets")
    | branchTargets _ = error "branchTargets"

  fun jump label = I.jmp (I.ImmedLabel(T.LABEL label), [label])

  exception NotImplemented

  fun setJumpTarget(I.ANNOTATION{a,i}, l) = I.ANNOTATION{a=a, i=setJumpTarget(i,l)}
    | setJumpTarget(I.INSTR(I.JMP(I.ImmedLabel _, _)), lab) = jump lab
    | setJumpTarget _ = error "setJumpTarget"

  fun setBranchTargets{i=I.ANNOTATION{a,i}, t, f} = 
        I.ANNOTATION{a=a, i=setBranchTargets{i=i, t=t, f=f}}
    | setBranchTargets{i=I.INSTR(I.JCC{cond,opnd=I.ImmedLabel _}), t, ...} = 
        I.jcc{cond=cond,opnd=I.ImmedLabel(T.LABEL t)}
    | setBranchTargets _ = error "setBranchTargets"

  fun negateConditional (I.ANNOTATION{i,a}, lab) =
	I.ANNOTATION{i=negateConditional(i,lab), a=a}
    | negateConditional (I.INSTR(I.JCC{cond,opnd=I.ImmedLabel(T.LABEL _)}), lab) =
	let
	val cond' = (case cond
	       of I.EQ => I.NE
		| I.NE => I.EQ
		| I.LT => I.GE
		| I.LE => I.GT
		| I.GT => I.LE
		| I.GE => I.LT
		| I.B => I.AE
		| I.BE => I.A
		| I.A => I.BE
		| I.AE => I.B
		| I.C => I.NC
		| I.NC => I.C
		| I.P => I.NP
		| I.NP => I.P
		| I.O => I.NO
		| I.NO => I.O
	      (* end case *))
	in
	  I.INSTR(I.JCC{cond=cond', opnd=I.ImmedLabel(T.LABEL lab)})
	end
    | negateConditional _ = error "negateConditional"

  val immedRange={lo= ~1073741824, hi=1073741823}
  val toInt32 = Int32.fromLarge o Int.toLarge
  fun loadImmed{immed,t} =
      I.move{mvOp=I.MOVL,src=I.Immed(toInt32 immed),dst=I.Direct t}
  fun loadOperand{opn,t} = I.move{mvOp=I.MOVL,src=opn,dst=I.Direct t}

 (*=====================================================================
  *  Hashing and Equality on operands
  *=====================================================================*)
   fun hashOpn(I.Immed i) = Word.fromInt(Int32.toInt i)
     | hashOpn(I.ImmedLabel le) = MLTreeHash.hash le + 0w123
     | hashOpn(I.Relative i) = Word.fromInt i + 0w1232
     | hashOpn(I.LabelEA le) = MLTreeHash.hash le + 0w44444
     | hashOpn(I.Direct r)  = CB.hashCell r
     | hashOpn(I.MemReg r)  = CB.hashCell r + 0w2123
     | hashOpn(I.ST f) = CB.hashCell f + 0w88
     | hashOpn(I.FPR f) = CB.hashCell f + 0w881
     | hashOpn(I.FDirect f) = CB.hashCell f + 0w31245
     | hashOpn(I.Displace {base, disp, ...}) = 
         hashOpn disp + CB.hashCell base
     | hashOpn(I.Indexed {base, index, scale, disp, ...}) =
         CB.hashCell index + Word.fromInt scale + hashOpn disp
   fun eqOpn(I.Immed a,I.Immed b) = a = b
     | eqOpn(I.ImmedLabel a,I.ImmedLabel b) = MLTreeEval.==(a,b)
     | eqOpn(I.Relative a,I.Relative b) = a = b
     | eqOpn(I.LabelEA a,I.LabelEA b) = MLTreeEval.==(a,b)
     | eqOpn(I.Direct a,I.Direct b) = CB.sameColor(a,b)
     | eqOpn(I.MemReg a,I.MemReg b) = CB.sameColor(a,b)
     | eqOpn(I.FDirect a,I.FDirect b) = CB.sameColor(a,b)
     | eqOpn(I.ST a,I.ST b) = CB.sameColor(a,b)
     | eqOpn(I.FPR a,I.FPR b) = CB.sameColor(a,b)
     | eqOpn(I.Displace{base=a,disp=b,...},I.Displace{base=c,disp=d,...}) =
          CB.sameColor(a,c) andalso eqOpn(b,d)
     | eqOpn(I.Indexed{base=a,index=b,scale=c,disp=d,...},
             I.Indexed{base=e,index=f,scale=g,disp=h,...}) =
          CB.sameColor(b,f) andalso c = g
          andalso sameCellOption(a,e) andalso eqOpn(d,h)
     | eqOpn _ = false
   and sameCellOption(NONE, NONE) = true
     | sameCellOption(SOME x, SOME y) = CB.sameColor(x,y)
     | sameCellOption _ = false

 (*========================================================================
  *  Definition and use (for register allocation mainly)
  *========================================================================*)
  val eaxPair = [C.edx, C.eax]

  fun defUseR instr = let
    fun operandAcc(I.Direct r, acc) = r::acc
      | operandAcc(I.MemReg r, acc) = r::acc
      | operandAcc(I.Displace{base, ...}, acc) = base::acc
      | operandAcc(I.Indexed{base=SOME b, index, ...}, acc) = b::index::acc
      | operandAcc(I.Indexed{base=NONE, index, ...}, acc) = index::acc
      | operandAcc(_, acc) = acc

    fun x86DefUseR instr = let
      fun operandUse opnd = operandAcc(opnd, [])

      fun operandUse2(src1, src2) = ([], operandAcc(src1, operandUse src2))
      fun operandUse3(x, y, z) = ([], operandAcc(x, operandAcc(y, operandUse y)))

      fun operandDef(I.Direct r) = [r]
	| operandDef(I.MemReg r) = [r]
	| operandDef _ = []

      fun multdiv{src, multDivOp} = let
	val uses = operandUse src
      in
	case multDivOp
	 of (I.IDIVL1 | I.DIVL1) => (eaxPair, C.edx::C.eax::uses)
	  | (I.IMULL1 | I.MULL1) => (eaxPair, C.eax::uses)
      end

      fun unary opnd = (operandDef opnd, operandUse opnd)
      fun cmptest{lsrc, rsrc} = ([], operandAcc(lsrc, operandUse rsrc))
      fun espOnly()  = let val sp = [C.stackptrR] in (sp, sp) end
      fun push arg = ([C.stackptrR], operandAcc(arg, [C.stackptrR]))
      fun float opnd = ([], operandUse opnd)
    in
      case instr
       of I.JMP(opnd, _)        => ([], operandUse opnd)
	| I.JCC{opnd, ...}      => ([], operandUse opnd)
	| I.CALL{opnd,defs,uses,...} => 
	     (C.getReg defs, operandAcc(opnd, C.getReg uses))
	| I.MOVE{src, dst=I.Direct r, ...} => ([r], operandUse src)
	| I.MOVE{src, dst=I.MemReg r, ...} => ([r], operandUse src)
	| I.MOVE{src, dst, ...} => ([], operandAcc(dst, operandUse src))
	| I.LEA{r32, addr}      => ([r32], operandUse addr)
	| ( I.CMPL arg | I.CMPW arg | I.CMPB arg
	  | I.TESTL arg | I.TESTW arg | I.TESTB arg ) => cmptest arg 
	| I.BITOP{lsrc, rsrc, ...} => cmptest{lsrc=lsrc,rsrc=rsrc}
	| I.BINARY{binOp=I.XORL,src=I.Direct rs,dst=I.Direct rd,...} =>   
	     if CB.sameColor(rs,rd) then ([rd],[]) else ([rd],[rs,rd])
	| I.BINARY{src,dst,...} =>   
	     (operandDef dst, operandAcc(src, operandUse dst))
	| I.SHIFT{src,dst,count,...} =>   
	     (operandDef dst, 
              operandAcc(count, operandAcc(src, operandUse dst)))
	| I.CMPXCHG{src, dst, ...} =>
	     (C.eax::operandDef dst, C.eax::operandAcc(src, operandUse dst))
	| I.ENTER _             => ([C.esp, C.ebp], [C.esp, C.ebp])
	| I.LEAVE               => ([C.esp, C.ebp], [C.esp, C.ebp])
	| I.MULTDIV arg	      => multdiv arg
	| I.MUL3{src1, dst, ...}=> ([dst], operandUse src1)

	| I.UNARY{opnd, ...}    => unary opnd
	| I.SET{opnd, ...}      => unary opnd
	| ( I.PUSHL arg | I.PUSHW arg | I.PUSHB arg ) => push arg
	| I.POP arg	      => (C.stackptrR::operandDef arg, [C.stackptrR])
	| I.PUSHFD	      => espOnly()
	| I.POPFD		      => espOnly()
	| I.CDQ		      => ([C.edx], [C.eax])
	| I.FSTPT opnd	      => float opnd
	| I.FSTPL opnd	      => float opnd
	| I.FSTPS opnd	      => float opnd 
	| I.FSTL opnd	      => float opnd
	| I.FSTS opnd	      => float opnd 
	| I.FLDL opnd	      => float opnd
	| I.FLDS opnd	      => float opnd
	| I.FILD opnd           => float opnd
	| I.FILDL opnd          => float opnd
	| I.FILDLL opnd         => float opnd
	| I.FBINARY{src, ...}   => ([], operandUse src)
	| I.FIBINARY{src, ...}  => ([], operandUse src)
	| I.FENV{opnd, ...}     => ([], operandUse opnd)
	| I.FNSTSW	      => ([C.eax], [])
	| I.FUCOM opnd          => float opnd
	| I.FUCOMP opnd         => float opnd
	| I.FCOMI opnd          => float opnd
	| I.FCOMIP opnd         => float opnd
	| I.FUCOMI opnd         => float opnd
	| I.FUCOMIP opnd        => float opnd

	| I.FMOVE{src, dst, ...} => operandUse2(src, dst) 
	| I.FILOAD{ea, dst, ...} => operandUse2(ea, dst) 
	| I.FCMP{lsrc, rsrc, ...} => operandUse2(lsrc, rsrc)
	| I.FBINOP{lsrc, rsrc, dst, ...} => operandUse3(lsrc, rsrc, dst)
	| I.FIBINOP{lsrc, rsrc, dst, ...} => operandUse3(lsrc, rsrc, dst)
	| I.FUNOP{src, dst, ...} => operandUse2(src, dst)

	| I.SAHF		      => ([], [C.eax])
	| I.LAHF		      => ([C.eax], [])
	  (* This sets the low order byte, 
	   * do potentially it may define *and* use 
	   *)
	| I.CMOV{src,dst,...} => ([dst], operandAcc(src, [dst]))
	| _		      => ([], [])
    end 
  in
      case instr
       of I.ANNOTATION{i, ...} => defUseR i
	| I.LIVE{regs, ...} => ([], C.getReg regs)
	| I.KILL{regs, ...} => (C.getReg regs, [])
	| I.COPY{k=CB.GP, dst, src, tmp, ...} => 
	  (case tmp
	    of NONE => (dst, src)
             | SOME(I.Direct r) => (r::dst, src)
	     | SOME(I.MemReg r) => (r::dst, src)
	     | SOME(ea) => (dst, operandAcc(ea, src))
          (*esac*))
	| I.COPY _ => ([], [])
	| I.INSTR i  => x86DefUseR(i)
  end

  fun defUseF instr = let

    fun x86DefUseF instr = let
      fun operand(I.FDirect f) = [f]
	| operand(I.FPR f) = [f]
	| operand _ = []

      fun operandAcc(I.FDirect f, acc) = f::acc
	| operandAcc(I.FPR f, acc) = f::acc
	| operandAcc(_ , acc) = acc

      fun fbinop(lsrc, rsrc, dst) = 
      let val def = operand dst
	  val use = operandAcc(lsrc, operand rsrc)
      in  (def, use) 
      end

      val fcmpTmp = [C.ST 0]

    in
      case instr
       of I.FSTPT opnd          => (operand opnd, [])  
	| I.FSTPL opnd		=> (operand opnd, [])
	| I.FSTPS opnd		=> (operand opnd, [])
	| I.FSTL opnd		=> (operand opnd, [])
	| I.FSTS opnd		=> (operand opnd, [])
	| I.FLDT opnd		=> ([], operand opnd)
	| I.FLDL opnd		=> ([], operand opnd)
	| I.FLDS opnd		=> ([], operand opnd)
	| I.FUCOM opnd          => ([], operand opnd)
	| I.FUCOMP opnd         => ([], operand opnd)
	| I.FCOMI opnd          => ([], operand opnd)
	| I.FCOMIP opnd         => ([], operand opnd)
	| I.FUCOMI opnd         => ([], operand opnd)
	| I.FUCOMIP opnd        => ([], operand opnd)
	| I.CALL{defs, uses, ...}	=> (C.getFreg defs, C.getFreg uses)
	| I.FBINARY{dst, src, ...}=> (operand dst, operand dst @ operand src)

	| I.FMOVE{src, dst, ...} => (operand dst, operand src) 
	| I.FILOAD{ea, dst, ...} => (operand dst, []) 
	| I.FCMP{lsrc, rsrc, ...} => (fcmpTmp, operandAcc(lsrc, operand rsrc))
	| I.FBINOP{lsrc, rsrc, dst, ...} => fbinop(lsrc, rsrc, dst)
	| I.FIBINOP{lsrc, rsrc, dst, ...} => fbinop(lsrc, rsrc, dst)
	| I.FUNOP{src, dst, ...} => (operand dst, operand src)
	| _  => ([], [])
    end
  in 
     case instr
     of (I.ANNOTATION{i, ...}) => defUseF(i)
      | I.LIVE{regs, ...} => ([], C.getFreg regs)
      | I.KILL{regs, ...} => (C.getFreg regs, [])
      | I.COPY{k=CB.FP, dst, src, tmp, ...} => 
	(case tmp
	  of NONE => (dst, src)
	   | SOME(I.FDirect f) => (f::dst, src)
	   | SOME(I.FPR f) => (f::dst, src)
	   | _ => (dst, src)
        (*esac*))
      | I.COPY _  => ([], [])
      | (I.INSTR i) => x86DefUseF(i)
  end

  fun defUse CB.GP = defUseR
    | defUse CB.FP = defUseF
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
(*
    | replicate(I.COPY{tmp=SOME _, dst, src}) =  
        I.COPY{tmp=SOME(I.Direct(C.newReg())), dst=dst, src=src}
    | replicate(I.FCOPY{tmp=SOME _, dst, src}) = 
        I.FCOPY{tmp=SOME(I.FDirect(C.newFreg())), dst=dst, src=src}
*)
    | replicate i = i
end

