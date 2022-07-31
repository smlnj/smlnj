(* amd64Rewrite.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

functor AMD64Rewrite (Instr : AMD64INSTR) : sig

    structure I  : AMD64INSTR
    structure CB : CELLS_BASIS = CellsBasis

    val rewriteUse : I.instruction * CB.cell * CB.cell -> I.instruction
    val rewriteDef : I.instruction * CB.cell * CB.cell -> I.instruction

    val frewriteUse : I.instruction * CB.cell * CB.cell -> I.instruction
    val frewriteDef : I.instruction * CB.cell * CB.cell -> I.instruction

  end = struct

    structure I = Instr
    structure C=I.C
    structure CB : CELLS_BASIS = CellsBasis

    fun replace (rs, rd) r = if CB.sameColor(rs, r) then rd else r

  (* rewrite an operand *)
    fun operand (rs, rd) opnd = (case opnd
	   of I.Direct(n, r) => if CB.sameColor(rs, r) then I.Direct(n, rd) else opnd
	    | I.FDirect r => if CB.sameColor(rs, r) then I.FDirect rd else opnd
	    | I.Displace{base, disp, mem} => if CB.sameColor(base,rs)
		then I.Displace{base=rd, disp=disp, mem=mem}
		else opnd
	    | I.Indexed{base=NONE, index, scale, disp, mem} => if CB.sameColor(rs, index)
		then I.Indexed{base=NONE, index=rd, scale=scale, disp=disp, mem=mem}
		else opnd
	    | I.Indexed{base as SOME rb, index, scale, disp, mem} => let
		val base' = if CB.sameColor(rs, rb) then SOME rd else base
		val index' = if CB.sameColor(rs, index) then rd else index
		in
		  I.Indexed{base=base', index=index', scale=scale, disp=disp, mem=mem}
		end
	    | _ => opnd
	  (* end case *))

    fun rewriteUse' (instr, rs, rd) = let
	  val operand = operand (rs, rd)
	  in
	    case instr
	     of I.JMP(opnd, labs) => I.JMP(operand opnd, labs)
	      | I.JCC{cond, opnd} => I.JCC{cond=cond, opnd = operand opnd}
	      | I.CALL{opnd, defs, uses, return, cutsTo, mem, pops} => I.CALL{
		    opnd=operand opnd, defs=defs, return=return,
		    uses=CB.CellSet.map {from=rs,to=rd} uses, cutsTo=cutsTo,
		    mem=mem, pops=pops
		  }
	      | I.MOVE{mvOp, src, dst as I.Direct _} => I.MOVE{mvOp=mvOp, src=operand src, dst=dst}
	      | I.MOVE{mvOp, src, dst} => I.MOVE{mvOp=mvOp, src=operand src, dst=operand dst}
	      | I.LEAL{r32, addr} => I.LEAL{r32=r32, addr=operand addr}
	      | I.LEAQ{r64, addr} => I.LEAQ{r64=r64, addr=operand addr}
	      | I.CMPQ{lsrc, rsrc} => I.CMPQ{lsrc=operand lsrc, rsrc=operand rsrc}
	      | I.CMPL{lsrc, rsrc} => I.CMPL{lsrc=operand lsrc, rsrc=operand rsrc}
	      | I.CMPW{lsrc, rsrc} => I.CMPW{lsrc=operand lsrc, rsrc=operand rsrc}
	      | I.CMPB{lsrc, rsrc} => I.CMPB{lsrc=operand lsrc, rsrc=operand rsrc}
	      | I.TESTQ{lsrc, rsrc} => I.TESTQ{lsrc=operand lsrc, rsrc=operand rsrc}
	      | I.TESTL{lsrc, rsrc} => I.TESTL{lsrc=operand lsrc, rsrc=operand rsrc}
	      | I.TESTW{lsrc, rsrc} => I.TESTW{lsrc=operand lsrc, rsrc=operand rsrc}
	      | I.TESTB{lsrc, rsrc} => I.TESTB{lsrc=operand lsrc, rsrc=operand rsrc}
	      | I.BITOP{bitOp, lsrc, rsrc} =>
		  I.BITOP{bitOp=bitOp, lsrc=operand lsrc, rsrc=operand rsrc}
	      | I.BINARY{binOp, src, dst} =>
		  I.BINARY{binOp=binOp, src=operand src, dst=operand dst}
	      | I.SHIFT{shiftOp, src, dst, count} =>
		  I.SHIFT{shiftOp=shiftOp, src=operand src, dst=operand dst, count=operand src}
	      | I.MULTDIV{multDivOp, src} =>
		  I.MULTDIV{multDivOp=multDivOp, src=operand src}
	      | I.MUL3{dst, src1, src2} => I.MUL3{dst=dst, src1=operand src1, src2=src2}
	      | I.MULQ3{dst, src1, src2} => I.MULQ3{dst=dst, src1=operand src1, src2=src2}
	      | I.UNARY{unOp, opnd} => I.UNARY{unOp=unOp, opnd=operand opnd}
	      | I.SET{cond, opnd} => I.SET{cond=cond, opnd=operand opnd}
(* Q: what if dst is not I.Direct? *)
	      | I.CMOV{cond, src, dst} => I.CMOV{cond=cond, src=operand src, dst=dst}
	      | I.PUSH opnd => I.PUSH(operand opnd)
	      | I.POP opnd => I.POP(operand opnd)
	      | I.FMOVE{fmvOp, src, dst} => I.FMOVE{fmvOp=fmvOp, src=operand src, dst=operand dst}
	      | I.FBINOP{binOp, dst, src} => I.FBINOP{binOp=binOp, src=operand src, dst=dst}
	      | I.FCOM{comOp, src, dst} => I.FCOM{comOp=comOp, src=operand src, dst=dst}
	      | I.FSQRTS{src, dst} => I.FSQRTS{src=operand src, dst=operand dst}
	      | I.FSQRTD{src, dst} => I.FSQRTD{src=operand src, dst=operand dst}
	      | I.XCHG{lock, sz, src, dst} =>
		  I.XCHG{lock=lock, sz=sz, src=operand src, dst=operand dst}
	      | I.CMPXCHG{lock, sz, src, dst} =>
		  I.CMPXCHG{lock=lock, sz=sz, src=operand src, dst=operand dst}
	      | I.XADD{lock, sz, src, dst} =>
		  I.XADD{lock=lock, sz=sz, src=operand src, dst=operand dst}
	      | _ => instr
	    (* end case *)
	  end

    fun rewriteDef' (instr, rs, rd)= let
	  val operand = operand (rs, rd)
	  in
	    case instr
	     of I.CALL{opnd, defs, uses, return, cutsTo, mem, pops} => I.CALL{
		    opnd=opnd, cutsTo=cutsTo, pops=pops,
		    return=CB.CellSet.map {from=rs,to=rd} return,
		    defs=CB.CellSet.map {from=rs,to=rd} defs,
		    uses=uses, mem=mem
		  }
	      | I.MOVE{mvOp, src, dst} => I.MOVE{mvOp=mvOp, src=src, dst=operand dst}
	      | I.LEAL{r32, addr} =>
		  if CB.sameColor(rs, r32) then I.LEAL{r32=rd, addr=addr} else instr
	      | I.LEAQ{r64, addr} =>
		  if CB.sameColor(rs, r64) then I.LEAQ{r64=rd, addr=addr} else instr
	      | I.BINARY{binOp, src, dst} => I.BINARY{binOp=binOp, src=src, dst=operand dst}
	      | I.SHIFT{shiftOp, src, dst, count} =>
		  I.SHIFT{shiftOp=shiftOp, src=src, count=count, dst=operand dst}
	      | I.MUL3{dst, src1, src2} =>
		  if CB.sameColor(rs, dst) then I.MUL3{dst=rd, src1=src1, src2=src2} else instr
	      | I.MULQ3{dst, src1, src2} =>
		  if CB.sameColor(rs, dst) then I.MULQ3{dst=rd, src1=src1, src2=src2} else instr
	      | I.UNARY{unOp, opnd} => I.UNARY{unOp=unOp, opnd=operand opnd}
	      | I.SET{cond, opnd} => I.SET{cond=cond, opnd=operand opnd}
	      | I.CMOV{cond, src, dst} =>
		  if CB.sameColor(rs, dst) then I.CMOV{cond=cond, src=src, dst=rd} else instr
	      | I.POP opnd => I.POP(operand opnd)
	      | _ => instr
	    (* end case *)
	  end

    fun frewriteUse' (instr, rs, rd) = let
	  val operand = operand (rs, rd)
	  val replace = replace (rs, rd)
	  in
	    case instr
	     of I.FMOVE{fmvOp, dst, src} => I.FMOVE{fmvOp=fmvOp, dst=dst, src=operand src}
	      | I.FBINOP{binOp, dst, src} =>
		  I.FBINOP{binOp=binOp, dst=replace dst, src=operand src}
	      | I.FCOM{comOp, dst, src} =>
		  I.FCOM{comOp=comOp, dst=replace dst, src=operand src}
	      | I.FSQRTS{dst, src} => I.FSQRTS{dst=dst, src=operand src}
	      | I.FSQRTD{dst, src} => I.FSQRTD{dst=dst, src=operand src}
	      | _ => instr
	    (* end case *)
	  end

    fun frewriteDef' (instr, rs, rd) = let
	  val operand = operand (rs, rd)
	  in
	    case instr
	     of I.CALL{opnd, defs, uses, return, cutsTo, mem, pops} => I.CALL{
		    opnd=opnd, cutsTo=cutsTo, pops=pops,
		    return=CB.CellSet.map {from=rs,to=rd} return,
		    defs=CB.CellSet.map {from=rs,to=rd} defs,
		    uses=uses, mem=mem
		  }
	      | I.FMOVE{fmvOp, dst, src} => I.FMOVE{fmvOp=fmvOp, dst=operand dst, src=src}
	      | I.FBINOP{binOp, dst, src} =>
		  if CB.sameColor(rs, dst) then I.FBINOP{binOp=binOp, dst=rd, src=src} else instr
	      | I.FSQRTS{dst, src} => I.FSQRTS{dst=operand dst, src=src}
	      | I.FSQRTD{dst, src} => I.FSQRTD{dst=operand dst, src=src}
	      | _ => instr
	    (* end case *)
	  end

  (* lift rewrting of the `instr` type to work on the `instruction` type *)
    fun rewrite (rwInstr, isRWUse) = let
	  fun rewrite' (instr, rs, rd) = let
		val replace = replace (rs, rd)
		in
		  case instr
		   of I.ANNOTATION{a, i} => I.ANNOTATION{
			  i = rewrite' (i, rs, rd),
			  a = case a
			       of CB.DEF_USE{cellkind=CB.FP, defs, uses} => let
				    val (defs', uses') = if isRWUse
					  then (defs, List.map replace uses)
					  else (List.map replace defs, uses)
				    in
				      CB.DEF_USE{cellkind = CB.FP, defs = defs', uses = uses'}
				    end
				| _ => a
			      (* end case *)
			}
		    | I.INSTR i => I.INSTR(rwInstr(i, rs, rd))
		    | I.COPY{k, sz, dst, src, tmp} => let
			val (dst', src') = if isRWUse
			      then (dst, List.map replace src)
			      else (List.map replace dst, src)
			in
			  I.COPY{k=k, sz=sz, dst=dst', src=src', tmp=tmp}
			end
		  (* end case *)
		end
	  in
	    rewrite'
	  end

    val rewriteUse = rewrite (rewriteUse', true)
    val rewriteDef = rewrite (rewriteDef', false)

    val frewriteUse = rewrite (frewriteUse', true)
    val frewriteDef = rewrite (frewriteDef', false)

  end
