(* Stolen from John Reppy's Moby compiler:
 *
 * x86-leaf-opt.sml
 *
 * COPYRIGHT (c) 2001 Bell Labs, Lucent Technologies
 *
 * Optimization of leaf procedures for the IA32.  We define a leaf procedure
 * to be one that does not make calls and does not allocate any extra stack
 * space (other than the usual linkage).  We optimize by removing the saved
 * frame-pointer and rewriting instructions that use the frame-pointer to
 * ones that use the stack pointer.
 *
 * Eventually, we may support tail calls from leaf procedures.
 *
 *)

functor X86LeafOpt
   (structure X86Instr : X86INSTR
    structure FlowGraph : FLOWGRAPH where I = X86Instr
    val isLeaf : FlowGraph.cluster -> bool
   ) : CLUSTER_OPTIMIZATION = 
struct

    structure F = FlowGraph
    structure I = X86Instr
    structure C = I.C

    type flowgraph = F.cluster

    val name = "X86LeafOpt"

  (* is a register the frame pointer? *)
    fun isFP reg = C.sameColor(reg, C.ebp)
  (* is a register the stack pointer? *)
    fun isSP reg = C.sameColor(reg, C.esp)

    fun error msg = MLRiscErrorMsg.error("X86LeafOpt",msg)

    fun err (blknum, msg) = error(concat[
	    "BLOCK ", Int.toString blknum, ": ", msg
	  ])

    fun optimize (F.CLUSTER cluster) = let
	  fun rewriteOpnd (opnd as I.Displace{base, disp, mem}) =
		if (isFP base)
		  then (case disp
		     of I.Immed n =>
			  I.Displace{base = C.esp, disp = I.Immed(n-4), mem = mem}
		      | _ => error "unable to rewrite displacement operand"
		    (* end case *))
		  else opnd
	    | rewriteOpnd (opnd as I.Indexed{base=SOME r, index, scale, disp, mem}) =
		if (isFP r)
		  then (case disp
		     of I.Immed n => I.Indexed{
			    base = SOME C.esp, index = index, scale = scale,
			    disp = I.Immed(n-4), mem = mem
			  }
		      | _ => error "unable to rewrite indexed operand"
		    (* end case *))
		  else opnd
	    | rewriteOpnd opnd = opnd
	  fun rewriteInsn insn = (case insn
		 of I.JMP(opnd, labs) => I.JMP(rewriteOpnd opnd, labs)
		  | I.JCC{cond, opnd} => I.JCC{cond = cond, opnd = rewriteOpnd opnd}
		  | I.CALL _ => error "unexpected call"
		  | I.MOVE{mvOp, src, dst} => I.MOVE{
			mvOp = mvOp,
			src = rewriteOpnd src,
			dst = rewriteOpnd dst
		      }
		  | I.LEA{r32, addr} => I.LEA{r32 = r32, addr = rewriteOpnd addr}
		  | I.CMPL{lsrc, rsrc} =>
		      I.CMPL{lsrc = rewriteOpnd lsrc, rsrc = rewriteOpnd rsrc}
		  | I.CMPW{lsrc, rsrc} =>
		      I.CMPW{lsrc = rewriteOpnd lsrc, rsrc = rewriteOpnd rsrc}
		  | I.CMPB{lsrc, rsrc} =>
		      I.CMPB{lsrc = rewriteOpnd lsrc, rsrc = rewriteOpnd rsrc}
		  | I.TESTL{lsrc, rsrc} =>
		      I.TESTL{lsrc = rewriteOpnd lsrc, rsrc = rewriteOpnd rsrc}
		  | I.TESTW{lsrc, rsrc} =>
		      I.TESTW{lsrc = rewriteOpnd lsrc, rsrc = rewriteOpnd rsrc}
		  | I.TESTB{lsrc, rsrc} =>
		      I.TESTB{lsrc = rewriteOpnd lsrc, rsrc = rewriteOpnd rsrc}
		  | I.BITOP{bitOp, lsrc, rsrc} => I.BITOP{
			bitOp = bitOp,
			lsrc = rewriteOpnd lsrc,
			rsrc = rewriteOpnd rsrc
		      }
		  | I.BINARY{binOp, src, dst} => I.BINARY{
			binOp = binOp,
			src = rewriteOpnd src,
			dst = rewriteOpnd dst
		      }
		  | I.MULTDIV{multDivOp, src} => I.MULTDIV{
			multDivOp = multDivOp, src = rewriteOpnd src
		      }
		  | I.MUL3{dst, src2, src1} => I.MUL3{
			dst = dst, src2 = src2, src1 = rewriteOpnd src1
		      }
		  | I.UNARY{unOp, opnd} =>
		      I.UNARY{unOp = unOp, opnd = rewriteOpnd opnd}
		  | I.SET{cond, opnd} => I.SET{cond = cond, opnd = rewriteOpnd opnd}
		  | I.CMOV{cond, src, dst} => I.CMOV{
			cond = cond, src = rewriteOpnd src, dst = dst
		      }
		  | I.PUSHL _ => error "unexpected pushl"
		  | I.PUSHW _ => error "unexpected pushw"
		  | I.PUSHB _ => error "unexpected pushb"
		  | I.POP _ => error "unexpected popl"
		  | I.COPY _ => error "unexpected copy"
		  | I.FCOPY _ => error "unexpected fcopy"
		  | I.FBINARY{binOp, src, dst} => I.FBINARY{
			binOp = binOp, src = rewriteOpnd src, dst = rewriteOpnd dst
		      }
		  | I.FIBINARY{binOp, src} => I.FIBINARY{
			binOp = binOp, src = rewriteOpnd src
		      }
		  | I.FUCOM opnd => I.FUCOM(rewriteOpnd opnd)
		  | I.FUCOMP opnd => I.FUCOMP(rewriteOpnd opnd)
		  | I.FSTPL opnd => I.FSTPL(rewriteOpnd opnd)
		  | I.FSTPS opnd => I.FSTPS(rewriteOpnd opnd)
		  | I.FSTPT opnd => I.FSTPT(rewriteOpnd opnd)
		  | I.FSTL opnd => I.FSTL(rewriteOpnd opnd)
		  | I.FSTS opnd => I.FSTS(rewriteOpnd opnd)
		  | I.FLDL opnd => I.FLDL(rewriteOpnd opnd)
		  | I.FLDS opnd => I.FLDS(rewriteOpnd opnd)
		  | I.FLDT opnd => I.FLDT(rewriteOpnd opnd)
		  | I.FILD opnd => I.FILD(rewriteOpnd opnd)
		  | I.FILDL opnd => I.FILDL(rewriteOpnd opnd)
		  | I.FILDLL opnd => I.FILDLL(rewriteOpnd opnd)
		  | I.FENV{fenvOp, opnd} =>
		      I.FENV{fenvOp = fenvOp, opnd = rewriteOpnd opnd}
		  | I.ANNOTATION{i, a} => I.ANNOTATION{i = rewriteInsn i, a = a}
		  | _ => insn
		(* end case *))
	(* rewrite the instructions of a block *)
	  fun rewriteBlock (F.BBLOCK{insns, ...}) =
		insns := List.map rewriteInsn (!insns)
	    | rewriteBlock _ = ()
	(* rewrite the exit protocol of an exit block *)
	  fun rewriteExit (F.BBLOCK{blknum, insns, ...}, _) = (
		case !insns
		 of (ret as I.RET _)::I.LEAVE::rest =>
		      insns := ret :: rest
		  | (I.JMP _ :: _) => ()  (* non-local control flow *)
		  | _ => err(blknum,"unable to rewrite exit protocol")
		(* end case *))
	(* rewrite the entry protocol of an entry block *)
	  fun rewriteEntry (F.BBLOCK{blknum, insns, ...}, _) = let
		fun rewrite [
			I.BINARY{binOp=I.SUBL, src=I.ImmedLabel _, dst=I.Direct a},
			I.MOVE{mvOp=I.MOVL, src=I.Direct b, dst=I.Direct c},
			I.PUSHL(I.Direct d)
		      ] = if ((isSP a) andalso (isSP b)
			andalso (isFP c) andalso (isFP d))
			  then []
			  else err(blknum, "unable to rewrite entry protocol")
		  | rewrite (insn::rest) = insn :: rewrite rest
		  | rewrite [] = err(blknum, "unable to rewrite entry protocol")
		in
		  insns := rewrite(!insns)
		end
	  in
	  (* first, we rewrite the exit and entry blocks *)
	    case #exit cluster
	     of F.EXIT{pred, ...} => List.app rewriteExit (!pred)
	    (* end case *);
	    case #entry cluster
	     of F.ENTRY{succ, ...} => List.app rewriteEntry (!succ)
	    (* end case *);
	  (* then rewrite the instructions to use the %esp instead of %ebp *)
	    List.app rewriteBlock (#blocks cluster)
	  end

    fun run cluster = 
        (if isLeaf cluster then optimize cluster else (); cluster)

 end
