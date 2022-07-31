(* x86omit-frameptr.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * replaces uses and definitions of a virtual frame pointer (vfp) with the appropriate
 * operation on the stack pointer.
 *
 * Invariant: fp = sp + delta &&  stack grows from high to low 	&& fp >= sp
 *
 * Assumptions: At the entry node fp = sp + idelta
 *
 * The tricky business is to recognize that things that look like register may
 * really be memory registers.
 *)

functor X86OmitFramePointer (
    structure I : X86INSTR
    structure CFG : CONTROL_FLOW_GRAPH where I = I
    structure MemRegs : MEMORY_REGISTERS where I=I
    val memRegBase : CellsBasis.cell option): OMIT_FRAME_POINTER =
struct
  structure CFG = CFG
  structure I = I
  structure C = I.C
  structure CB = CellsBasis
  structure HT = IntHashTable
  val sp = C.esp

  val dumpCfg = MLRiscControl.mkFlag ("dump-cfg-after-omit-frame-pointer",
				      "whether CFG is shown after omit-framepointer phase")

  fun error msg = MLRiscErrorMsg.error("X86OmitFramePointer", msg)

  fun omitframeptr{vfp:CB.cell, idelta:Int32.int option, cfg as Graph.GRAPH graph} = let

    (* rewrite a list of instructions where the gap between fp and sp is delta *)
    fun rewrite(instrs, idelta) = let

      (* What kind of register? *)
      datatype which = SP | FP | OTHER
      fun isSp cell = CB.sameColor(cell, sp)
      fun isVfp cell = CB.sameColor(cell, vfp)
      fun which(cell) = if isSp(cell) then SP else if isVfp(cell) then FP else OTHER
      fun either(cell) = isSp(cell) orelse isVfp(cell)


      (* Has the instruction been rewritten? *)
      val changedFlag = ref false


      (*
       * rewrite a single instruction assuming gap (fp=sp+delta)
       * returns NONE is instruction is deleted and SOME(instruction) otherwise.
       *)

      fun doInstr(instr, delta:Int32.int option) = let
	(* if a delta exists then add to it,
	 * otherwise maintain that there is no delta
	 *)
	fun addToDelta i =
	  (case delta
	   of SOME d => SOME(i+d)
	    | NONE => NONE
	  (*esac*))

	fun incOffset(i) =
	  (case delta
	   of NONE => error "incOffset"
	    | SOME k => k+i
	  (*esac*))

	fun incDisp(I.Immed i) = I.Immed(incOffset(i))
	  | incDisp _ = error "incDisp"	(* CONSTANTS? *)

	fun operand(opnd as I.Displace{base, disp, mem}) =
	    if isVfp base then
	      (changedFlag := true;
	       I.Displace{base=sp, mem=mem, disp=incDisp(disp)})
	    else opnd
	  | operand(opnd as I.Indexed{base, index, scale, disp, mem}) =
	    if isVfp index then
	      error "operand: frame pointer used in index"
	    else (case base
	      of NONE => opnd
	       | SOME b =>
		 if isVfp b then
		    (changedFlag := true;
		     I.Indexed{base=SOME(sp), index=index, scale=scale, mem=mem,
			       disp=incDisp(disp)})
		 else opnd
	     (*esac*))
	  | operand(opnd as I.MemReg _) =
	      operand(MemRegs.memReg{reg=opnd, base=Option.valOf memRegBase})
	  | operand(opnd as I.FDirect _) =
	      operand(MemRegs.memReg{reg=opnd, base=Option.valOf memRegBase})
	  | operand(opnd) = opnd


	fun annotate(i, k:Int32.int option) = let
	  val instr =
	    if !changedFlag then
	       (changedFlag := false;
		case k
		of NONE => i
		 | SOME d =>
		   if d <> 0 then let
		      val cmt = "offset adjusted to " ^ Int32.toString d
		      val ann = #create MLRiscAnnotations.COMMENT cmt
		    in I.ANNOTATION{i=i, a=ann}
		    end
		  else i
		(*esac*))
	    else i
	in (SOME(instr),k)
	end

	fun unchanged(i:I.instr) = annotate(I.INSTR i, delta)
	fun changedto(i, k) = annotate(I.INSTR i, k)

	fun compare(test, lsrc, rsrc) = unchanged(test{lsrc=operand(lsrc), rsrc=operand(rsrc)})
	fun float(oper, opnd) = unchanged(oper(operand(opnd)))

	fun doX86Instr (instr: I.instr) =
	 (case instr
	  of I.JMP(opnd,labs) => unchanged(I.JMP(operand opnd, labs))
	   | I.JCC{cond:I.cond, opnd:I.operand} =>
	      unchanged(I.JCC{cond=cond, opnd=operand(opnd)})
	   | I.CALL{opnd, defs, uses, cutsTo, mem, return, pops=0} =>
	      unchanged(I.CALL{opnd=operand(opnd), defs=defs, uses=uses,
			       cutsTo=cutsTo, mem=mem, pops=0,
			       return=return})
	   | I.CALL{opnd, defs, uses, cutsTo, mem, return, pops} =>
	     changedto(I.CALL{opnd=operand(opnd), defs=defs, uses=uses,
			      cutsTo=cutsTo, mem=mem, pops=pops,
			      return=return},
		       addToDelta(~pops))
	   | I.ENTER{src1=I.Immed i1, src2=I.Immed i2} => changedto(instr,  addToDelta(i1 + i2*4))
	   | I.LEAVE => (SOME(I.INSTR instr), NONE)
	   | I.RET opnd => (SOME(I.INSTR instr), NONE)
	   | I.MOVE{mvOp:I.move, src=I.Direct s, dst=I.Direct d} =>
	     (case (which d, which s)
	      of (FP, SP) => (NONE, SOME 0)
	       | (SP, FP) => (case delta
		   of NONE => error "MOVE: (SP, FP)"
		    | SOME 0 => (NONE, SOME 0)
		    | SOME n => let
			 val addr = I.Displace{base=sp, disp=I.Immed(n), mem=I.Region.stack}
		       in
			 (SOME(I.lea{r32=sp, addr=addr}), SOME 0)
		       end
		   (*esac*))
	       | (OTHER, OTHER) => unchanged(instr)
	       | (FP, FP) => (NONE, delta)
	       | (SP, SP) => (NONE, delta)
	       | (FP, _) => error "MOVE: to FP"
	       | (SP, _) => error "MOVE: to SP"
	       | (OTHER, SP) => unchanged(instr)
	       | (OTHER, FP) => error "MOVE: FP to OTHER"	(* d:=sp+delta; lazy!*)
	     (*esac*))
	   | I.MOVE{mvOp, src, dst as I.Direct d} =>
	       if either(d) then error "MOVE: assignment to FP/SP"
	       else unchanged(I.MOVE{mvOp=mvOp, src=operand(src), dst=dst})
	   | I.MOVE{mvOp, src, dst} =>
	       unchanged(I.MOVE{mvOp=mvOp, src=operand(src), dst=operand(dst)})
	   | I.LEA{r32:CB.cell, addr as I.Displace{base, disp=I.Immed d, mem}} =>
	     (case (which r32, which base)
	      of (SP, SP) =>
		   (* assumes stack grows from high to low.
		    * if sp is incremented by a positive delta, then the gap is
		    * reduced by delta-d;
		    * if sp is decremented, the the gap is increased and d is negative.
		    *)
		   changedto(instr, addToDelta(~d))
	       | (SP, FP) =>
		   (*    sp = fp + d
		    * or sp = sp + delta + d
		    *)
		   changedto(I.LEA{r32=r32, addr=operand(addr)}, SOME(incOffset(d)))
	       | (FP, FP) =>
		   (* fp = fp + d
		    * if d is positive, then the gap is increased to delta+d,
		    * if d is negative, then the gap is reduced.
		    *)
		   (NONE, SOME(incOffset(d)))
	       | (FP, SP) => (NONE, addToDelta(d))
	       | (SP, OTHER) => error "LEA: sp changed by non-immed"
	       | (FP, OTHER) => error "LEA: fp changed by non-immed"
	       | _ => unchanged(instr)
	     (*esac*))
	   | I.LEA{r32, addr} =>
	     if either(r32) then error "LEA: SP/FP changed by non-immed"
	     else unchanged(I.LEA{r32=r32, addr=operand(addr)})
	   | I.CMPL{lsrc: I.operand, rsrc: I.operand} => compare(I.CMPL, lsrc, rsrc)
	   | I.CMPW{lsrc: I.operand, rsrc: I.operand} => compare(I.CMPW, lsrc, rsrc)
	   | I.CMPB{lsrc: I.operand, rsrc: I.operand} => compare(I.CMPB, lsrc, rsrc)
	   | I.TESTL{lsrc: I.operand, rsrc: I.operand} => compare(I.TESTL, lsrc, rsrc)
	   | I.TESTW{lsrc: I.operand, rsrc: I.operand} => compare(I.TESTW, lsrc, rsrc)
	   | I.TESTB{lsrc: I.operand, rsrc: I.operand} => compare(I.TESTB, lsrc, rsrc)
	   | I.BITOP{bitOp:I.bitOp, lsrc: I.operand, rsrc: I.operand} =>
	      unchanged(I.BITOP{bitOp=bitOp, lsrc=operand(lsrc), rsrc=operand(rsrc)})
	   | I.BINARY{binOp=I.ADDL, src=I.Immed(k), dst=I.Direct(d)} =>
	     (case which d
	      of SP => changedto(instr, addToDelta(~k))
	       | FP => (NONE, SOME(incOffset(k)))
	       | OTHER => unchanged(instr)
	     (*esac*))
	   | I.BINARY{binOp=I.SUBL, src=I.Immed(k), dst=I.Direct(d)} =>
	     (case which d
	      of SP => changedto(instr, addToDelta(k))
	       | FP => (NONE, SOME(incOffset(~k)))
	       | OTHER => unchanged(instr)
	     (*esac*))
	   | I.BINARY{binOp, dst as I.Direct(d), src} =>
	     if either(d) then error "binary: assignment to SP | FP"
	     else unchanged(I.BINARY{binOp=binOp, src=operand(src), dst=dst})
	   | I.BINARY{binOp, src, dst} =>
	      unchanged(I.BINARY{binOp=binOp, src=operand(src), dst=operand(dst)})
	   | I.CMPXCHG{lock:bool, sz:I.isize, src:I.operand, dst:I.operand} =>
	      unchanged(I.CMPXCHG{lock=lock, sz=sz, src=operand(src), dst=operand(dst)})
	   | I.MULTDIV{multDivOp:I.multDivOp, src:I.operand} =>
	      unchanged(I.MULTDIV{multDivOp=multDivOp, src=operand(src)})
	   | I.MUL3{dst:CB.cell, src2:Int32.int, src1:I.operand} =>
	     if either(dst) then error "MUL3: assignment to FP/SP"
	     else unchanged(I.MUL3{dst=dst, src2=src2, src1=operand(src1)})
	   | I.UNARY{unOp=I.INCL, opnd as I.Direct(r)} =>
	     (case (which r)
	      of SP => changedto(instr, addToDelta(~1))
	       | FP => (NONE, SOME(incOffset(1)))
	       | OTHER => unchanged(I.UNARY{unOp=I.INCL, opnd=opnd})
	     (*esac*))
	   | I.UNARY{unOp=I.DECL, opnd as I.Direct(r)} =>
	     (case (which r)
	      of SP => changedto(instr, addToDelta(1))
	       | FP => (NONE, SOME(incOffset(~1)))
	       | OTHER => unchanged(I.UNARY{unOp=I.DECL, opnd=opnd})
	     (*esac*))
	   | I.UNARY{unOp, opnd} => unchanged(I.UNARY{unOp=unOp, opnd=operand(opnd)})
	   | I.SET{cond:I.cond, opnd:I.operand} =>
	       unchanged(I.SET{cond=cond, opnd=operand(opnd)})
	   | I.CMOV{cond:I.cond, src as I.Direct(s), dst:CB.cell} =>
	       if either(s) orelse either(dst) then
		 error "CMOV: FP/SP in conditional move"
	       else unchanged(I.CMOV{cond=cond, src=operand(src), dst=dst})
	   | I.PUSHL opnd => changedto(I.PUSHL(operand(opnd)), addToDelta(4))
	   | I.PUSHW opnd => changedto(I.PUSHW(operand(opnd)), addToDelta(2))
	   | I.PUSHB opnd => changedto(I.PUSHB(operand(opnd)), addToDelta(1))
	   | I.POP opnd => changedto(I.POP(operand(opnd)), addToDelta(~4))
	   | I.FBINARY{binOp:I.fbinOp, src:I.operand, dst:I.operand} =>
	      unchanged(I.FBINARY{binOp=binOp, src=operand(src), dst=operand(dst)})
	   | I.FIBINARY{binOp:I.fibinOp, src:I.operand} =>
	      unchanged(I.FIBINARY{binOp=binOp, src=operand(src)})
	   | I.FUCOM opnd => unchanged(I.FUCOM(operand opnd))
	   | I.FUCOMP opnd => unchanged(I.FUCOMP(operand (opnd)))
	   | I.FCOMI opnd => unchanged(I.FCOMI(operand opnd))
	   | I.FCOMIP opnd => unchanged(I.FCOMIP(operand (opnd)))
	   | I.FUCOMI opnd => unchanged(I.FUCOMI(operand opnd))
	   | I.FUCOMIP opnd => unchanged(I.FUCOMIP(operand (opnd)))
	   | I.FSTPL opnd => float(I.FSTPL, opnd)
	   | I.FSTPS opnd => float(I.FSTPS, opnd)
	   | I.FSTPT opnd  => float(I.FSTPT, opnd)
	   | I.FSTL opnd => float(I.FSTL, opnd)
	   | I.FSTS opnd => float(I.FSTS, opnd)
	   | I.FLDL opnd => float(I.FLDL, opnd)
	   | I.FLDS opnd => float(I.FLDS, opnd)
	   | I.FLDT opnd => float(I.FLDT, opnd)
	   | I.FILD opnd => float(I.FILD, opnd)
	   | I.FILDL opnd => float(I.FILDLL, opnd)
	   | I.FILDLL opnd => float(I.FILDLL, opnd)
	   | I.FENV{fenvOp:I.fenvOp, opnd:I.operand} =>
	       unchanged(I.FENV{fenvOp=fenvOp, opnd=operand(opnd)})
	   | I.FMOVE{fsize:I.fsize, src:I.operand, dst:I.operand} =>
	       unchanged(I.FMOVE{fsize=fsize, src=operand(src), dst=operand(dst)})
	   | I.FILOAD{isize:I.isize, ea:I.operand, dst:I.operand} =>
	       unchanged(I.FILOAD{isize=isize, ea=operand(ea), dst=operand(dst)})
	   | I.FBINOP{fsize, binOp, lsrc, rsrc, dst} =>
	       unchanged(I.FBINOP{fsize=fsize, binOp=binOp, lsrc=operand(lsrc),
				  rsrc=operand(rsrc), dst=operand(dst)})
	   | I.FIBINOP{isize, binOp, lsrc, rsrc, dst} =>
	       unchanged(I.FIBINOP{isize=isize, binOp=binOp, lsrc=operand(lsrc),
				  rsrc=operand(rsrc), dst=operand(dst)})
	   | I.FUNOP{fsize:I.fsize, unOp:I.funOp, src:I.operand, dst:I.operand} =>
	       unchanged(I.FUNOP{fsize=fsize, unOp=unOp, src=operand(src),
				 dst=operand(dst)})
	   | I.FCMP{i,fsize:I.fsize, lsrc:I.operand, rsrc:I.operand} =>
	       unchanged(I.FCMP{i=i,fsize=fsize, lsrc=operand(lsrc), rsrc=operand(rsrc)})
	   | _ => unchanged(instr)
         (*esac*))
      in
	  case instr
	  of I.ANNOTATION{i,a} => let
	       val (instr, delta) = doInstr(i, delta)
	     in
		case instr
		of NONE => (NONE, delta)
		 | SOME(i) => annotate(I.ANNOTATION{i=i, a=a}, delta)
	     end
	   | I.COPY{k=CB.GP, dst, src, ...} => let
	      (* the situation where SP <- FP is somewhat complicated.
	       * The copy must be extracted, and a lea generated.
	       * Should it be before or after the parallel copy? Depends on if SP is used.
	       * However, will such a thing ever exist in a parallel copy!?
	       *)
	      fun okay(s, d, acc) =
		(case (which s, which d)
		 of (FP, SP) => true
		  | (SP, FP) => error "COPY:SP<-FP; lazy!"
		  | (SP, OTHER) => error "COPY:SP<-OTHER"
		  | (FP, OTHER) => error "COPY:FP<-OTHER"
		  | (OTHER, SP) => error "COPY:OTHER<-SP"
		  | (OTHER, FP)  => error "COPY:OTHER<-FP"
		  | _ => acc
		(*esac*))
	     in annotate(instr, if ListPair.foldl okay false (dst, src) then SOME 0 else delta)
	     end
	   | I.INSTR instr => doX86Instr instr
	   | _ => annotate(instr, delta)			(* unchanged *)
      end (*doInstr*)

      (* rewrite instructions *)
      fun doInstrs([], instrs, delta) = (instrs, delta)
        | doInstrs(instr::rest, acc, delta) = let
            val (instr, delta2) = doInstr(instr, delta)
	  in
	    case instr
	    of NONE => doInstrs(rest, acc, delta2)
	     | SOME(i) => doInstrs(rest, i::acc, delta2)
	  end


    in doInstrs(instrs, [], idelta)
    end (* rewrite *)




    (* rewrite blocks using a depth first traversal of the blocks *)
    val info : {visited:bool, delta: Int32.int option} HT.hash_table =
      HT.mkTable(32, General.Fail "X86OmitFramePtr: Not Found")
    val noInfo = {visited=false, delta=NONE}

    fun dfs (nid, delta) = let
      fun doSucc(delta) =
	app (fn snid => dfs(snid, delta)) (#succ graph nid)
      val CFG.BLOCK{insns, kind, ...} = #node_info graph nid
    in
      case kind
      of CFG.STOP => ()
       | CFG.START => doSucc(delta)
       | CFG.NORMAL => let
	   val {visited, delta=d} = Option.getOpt(HT.find info nid, noInfo)
	   fun sameDelta(NONE, NONE) = true
	     | sameDelta(SOME i1: Int32.int option, SOME i2) = i1 = i2
	     | sameDelta _ = false
	 in
	   if visited then (if sameDelta(d, delta) then () else error "dfs")
	   else let
	       val (instrs, delta2) = rewrite(rev(!insns), delta)
	     in
	       insns := instrs;
	       HT.insert info (nid, {visited=true, delta=delta});
	       doSucc(delta2)
	     end
	 end
      (*esac*)
    end

    val CB.CELL{col, ...} = vfp
  in
    (*
     * check that virtual frame pointer is a pseudo register or
     * aliased to the stack pointer.
     *)
    case !col
     of CB.PSEUDO => app (fn nid => dfs(nid, idelta)) (#entries graph ())
      | _ => error "virtual frame pointer not a pseudo register"
    (*esac*)

    (* output cluster  *)
(*    if !dumpCfg then
      PC.printCluster TextIO.stdOut "after omit frame pointer"  cl
      else () *)
  end
end

