(* X86Spill.sml
 *
 * X86 spilling is complicated business.
 * Allen: and it just got more complicated; now we have to recognize the regmap.
 * I've also improved the spilling code so that more instructions are
 * recognized.  Addressing modes are now folded into the existing instruction
 * whenever possible.  This eliminates some redundant temporaries which were
 * introduced before.
 *)
functor X86SpillInstr(structure Instr: X86INSTR
                 structure Props: INSN_PROPERTIES where I = Instr
		) : ARCH_SPILL_INSTR = struct

  structure I  = Instr
  structure C  = I.C
  structure CB = CellsBasis

  fun error msg = MLRiscErrorMsg.impossible("X86Spill: "^ msg)

  fun immed(I.Immed _) = true
    | immed(I.ImmedLabel _) = true
    | immed _ = false

  fun immedOrReg(I.Direct r) = true
    | immedOrReg(I.Immed _) = true
    | immedOrReg(I.ImmedLabel _) = true
    | immedOrReg _ = false

  fun isMemory(I.MemReg _) = true
    | isMemory(I.Displace _) = true
    | isMemory(I.Indexed _) = true
    | isMemory(I.LabelEA _) = true
    | isMemory _ = false

  (* Annotate instruction *)
  fun annotate(instr,[]) = instr
    | annotate(instr,a::an) = annotate(I.ANNOTATION{i=instr,a=a},an)

  fun mark(instr, an) = annotate(I.INSTR instr, an)

  fun liveKill(add, rmv) ({regs, spilled}, reg) =
      {regs=rmv(reg, regs), spilled=add(reg, spilled)}

  val fLiveKill = liveKill (C.addFreg, C.rmvFreg)
  val rLiveKill = liveKill (C.addReg, C.rmvReg)

  val newReg = C.newReg


  fun spillR(instr, reg, spillLoc) = let
    fun x86Spill(instr, an) = let
      fun done(instr, an) = {code=[mark(instr, an)], proh=[], newReg=NONE}
    in
      case instr of
	I.CALL{opnd=addr, defs, uses, return, cutsTo, mem, pops} =>
	  done(I.CALL{opnd=addr, defs=C.rmvReg(reg,defs),
				 return=return, uses=uses,
		      cutsTo=cutsTo, mem=mem, pops=pops}, an)
      | I.MOVE{mvOp as (I.MOVZBL|I.MOVSBL|I.MOVZWL|I.MOVSWL), src, dst} =>
	  let val tmpR = newReg() val tmp = I.Direct tmpR
	  in  {proh=[tmpR], newReg=SOME tmpR,
	       code=[mark(I.MOVE{mvOp=mvOp, src=src, dst=tmp}, an),
		     I.move{mvOp=I.MOVL, src=tmp, dst=spillLoc}]
	      }
	  end
      | I.MOVE{mvOp, src as I.Direct rs, dst} =>
	  if CB.sameColor(rs,reg) then {code=[], proh=[], newReg=NONE}
	  else done(I.MOVE{mvOp=mvOp, src=src, dst=spillLoc}, an)
      | I.MOVE{mvOp, src, dst=I.Direct _} =>
	  if Props.eqOpn(src, spillLoc) then {code=[], proh=[], newReg=NONE}
	  else if immed src then
	     done(I.MOVE{mvOp=mvOp, src=src, dst=spillLoc}, an)
	  else
	  let val tmpR = newReg()
	      val tmp  = I.Direct tmpR
	  in  {proh=[tmpR],
	       newReg=SOME tmpR,
	       code=[mark(I.MOVE{mvOp=mvOp, src=src, dst=tmp}, an),
		     I.move{mvOp=mvOp, src=tmp, dst=spillLoc}]
	      }
	  end
      | I.LEA{addr, r32} =>
	  let val tmpR = newReg()
	  in  {proh=[tmpR],
	       newReg=SOME tmpR,
	       code=[mark(I.LEA{addr=addr, r32=tmpR}, an),
		     I.move{mvOp=I.MOVL, src=I.Direct tmpR, dst=spillLoc}]
	      }
	  end
      | I.BINARY{binOp=I.XORL, src as I.Direct rs, dst=I.Direct rd} =>
	  if CB.sameColor(rs,rd) then
	     {proh=[],
	      code=[mark(I.MOVE{mvOp=I.MOVL, src=I.Immed 0, dst=spillLoc}, an)],
	      newReg=NONE
	     }
	  else
	     {proh=[],
	      code=[mark(I.BINARY{binOp=I.XORL, src=src, dst=spillLoc}, an)],
	      newReg=NONE
	     }
      | I.BINARY{binOp, src, dst} => let (* note: dst = reg *)
	 fun multBinOp(I.IMULL|I.IMULW|I.IMULB) = true
	   | multBinOp _ = false
	in
	  if multBinOp binOp then let
	     (* destination must remain a register *)
	      val tmpR = newReg()
	      val tmp = I.Direct tmpR
	    in
	      {proh=[tmpR],
	       code=  [I.move{mvOp=I.MOVL, src=spillLoc, dst=tmp},
		       I.binary{binOp=binOp, src=src, dst=tmp},
		       I.move{mvOp=I.MOVL, src=tmp, dst=spillLoc}],
	       newReg=SOME tmpR
	      }
	    end
	  else if immedOrReg src then
	     (* can replace the destination directly *)
	     done(I.BINARY{binOp=binOp, src=src, dst=spillLoc}, an)
	  else let
	     (* a memory src and non multBinOp
	      * --- cannot have two memory operands
	      *)
	      val tmpR = newReg()
	      val tmp = I.Direct tmpR
	    in
	      { proh=[tmpR],
		code=[I.move{mvOp=I.MOVL, src=src, dst=tmp},
		      I.binary{binOp=binOp, src=tmp, dst=spillLoc}],
		newReg=NONE
	       }
	    end
	end
      | I.SHIFT{shiftOp, count, src, dst} => error "go and implement SHIFT"
      | I.CMOV{cond, src, dst} =>
           (* note: dst must be a register *)
         (case spillLoc of
           I.Direct r =>
	      {proh=[],
	       newReg=NONE,
	       code=[mark(I.CMOV{cond=cond,src=src,dst=r},an)]
	      }
         | _ =>
	  let val tmpR = newReg()
	      val tmp  = I.Direct tmpR
	  in  {proh=[tmpR],
	       newReg=SOME tmpR,
	       code=[I.move{mvOp=I.MOVL, src=spillLoc, dst=tmp},
                     mark(I.CMOV{cond=cond,src=src,dst=tmpR},an),
		     I.move{mvOp=I.MOVL, src=tmp, dst=spillLoc}]
	      }
	  end
         )

      | I.CMPXCHG{lock,sz,src,dst} =>
	   if immedOrReg src then
	       {proh=[],
		code=[mark(I.CMPXCHG{lock=lock,sz=sz,src=src,dst=spillLoc},an)],
		newReg=NONE
	       }
	   else
	   let val tmpR = newReg()
	       val tmp  = I.Direct tmpR
	   in {proh=[],
	       code=[I.move{mvOp=I.MOVL, src=src, dst=tmp},
		     mark(I.CMPXCHG{lock=lock,sz=sz,src=tmp,dst=spillLoc},an)],
	       newReg=NONE
	      }
	   end
      | I.MULTDIV _ => error "spill: MULTDIV"
      | I.MUL3{src1, src2, dst} =>
	  let val tmpR = newReg()
	  in  {proh=[tmpR], newReg=SOME tmpR,
	       code=[mark(I.MUL3{src1=src1, src2=src2, dst=tmpR}, an),
		     I.move{mvOp=I.MOVL, src=I.Direct tmpR, dst=spillLoc}]
	      }
	  end
      | I.UNARY{unOp, opnd} => done(I.UNARY{unOp=unOp, opnd=spillLoc}, an)
      | I.SET{cond, opnd} => done(I.SET{cond=cond, opnd=spillLoc}, an)
      | I.POP _ => done(I.POP spillLoc, an)
      | I.FNSTSW  => error "spill: FNSTSW"
      | _ => error "spill"
    end (* x86Spill *)

    fun f(I.INSTR instr, an) = x86Spill(instr, an)
      | f(I.ANNOTATION{a, i}, an) = f(i, a::an)
      | f(I.KILL lk, an) =
	   {code=[annotate(I.KILL(rLiveKill (lk, reg)), an)],
	    proh=[],
	    newReg=NONE}
      | f _ = error "spill:f"
  in f(instr, [])
  end

  fun reloadR(instr, reg, spillLoc) = let
    fun x86Reload(instr, reg, spillLoc, an) = let
        fun operand(rt, opnd) =
	(case opnd
	 of I.Direct r => if CB.sameColor(r,reg) then I.Direct rt else opnd
	  | I.Displace{base, disp, mem} =>
	     if CB.sameColor(base,reg)
	     then I.Displace{base=rt, disp=disp, mem=mem}
	     else opnd
	  | I.Indexed{base=NONE, index, scale, disp, mem=mem} =>
	     if CB.sameColor(index,reg) then
	       I.Indexed{base=NONE, index=rt, scale=scale, disp=disp, mem=mem}
	     else opnd
	  | I.Indexed{base as SOME b, index, scale, disp, mem=mem} =>
	     if CB.sameColor(b,reg) then
	       operand(rt, I.Indexed{base=SOME rt, index=index,
				     scale=scale, disp=disp, mem=mem})
	     else if CB.sameColor(index,reg) then
	       I.Indexed{base=base, index=rt, scale=scale, disp=disp, mem=mem}
		  else opnd
	  | opnd => opnd
	(*esac*))

      fun done(instr, an) = {code=[mark(instr, an)], proh=[], newReg=NONE}

      fun isReloading (I.Direct r) = CB.sameColor(r,reg)
        | isReloading _ = false

      (* This version assumes that the value of tmpR is killed *)
      fun withTmp(f, an) =
	  case spillLoc of
	    I.Direct tmpR =>
		{newReg=NONE,
		 proh=[],
		 code=[mark(f tmpR, an)]
		}
	  |  _ =>
	    let val tmpR = newReg()
	    in  {newReg=NONE,
		 proh=[tmpR],
		 code=[I.move{mvOp=I.MOVL, src=spillLoc, dst=I.Direct tmpR},
		       mark(f tmpR, an)
		      ]
		}
	    end

      (* This version assumes that the value of tmpR is available afterwards *)
      fun withTmpAvail(f, an) =
	  case spillLoc of
	     I.Direct tmpR =>
	      {newReg=SOME tmpR,
	       proh=[tmpR],
	       code=[mark(f tmpR, an)]
	      }
	  |  _ =>
	      let val tmpR = newReg()
		  val tmp  = I.Direct tmpR
	      in  {newReg=SOME tmpR,
		   proh=[tmpR],
		   code=[I.move{mvOp=I.MOVL, src=spillLoc, dst=I.Direct tmpR},
			 mark(f tmpR, an)
			]
		  }
	      end

      fun replace(opn as I.Direct r) =
	    if CB.sameColor(r,reg) then spillLoc else opn
	| replace opn         = opn

      (* Fold in a memory operand if possible.  Makes sure that both operands
       * are not in memory.  lsrc cannot be immediate.
       *)
      fun reloadCmp(cmp, lsrc, rsrc, an) =
	  let fun reloadIt() =
		withTmp(fn tmpR =>
		  cmp{lsrc=operand(tmpR, lsrc), rsrc=operand(tmpR, rsrc)}, an)
	  in  if immedOrReg lsrc andalso immedOrReg rsrc then
	      let val lsrc' = replace lsrc
		  val rsrc' = replace rsrc
	      in  if isMemory lsrc' andalso isMemory rsrc' then
		     reloadIt()
		  else
		     done(cmp{lsrc=lsrc', rsrc=rsrc'}, an)
	      end
	      else reloadIt()
	  end

      fun reloadBT(bitOp, lsrc, rsrc, an) =
	     reloadCmp(fn {lsrc,rsrc} => I.BITOP{bitOp=bitOp,lsrc=lsrc,rsrc=rsrc},
		       lsrc, rsrc, an)

      (* Fold in a memory operand if possible.  Makes sure that the right
       * operand is not in memory and left operand is not an immediate.
       *  lsrc   rsrc
       *   AL,   imm8  opc1 A8
       *  EAX,   imm32 opc1 A9
       *  r/m8,  imm8  opc2 F6/0 ib
       *  r/m32, imm32 opc2 F7/0 id
       *  r/m32, r32   opc3 85/r
       *)
      fun reloadTest(test, lsrc, rsrc, an) =
	  let fun reloadIt() =
		 withTmp(fn tmpR =>
		   test{lsrc=operand(tmpR, lsrc), rsrc=operand(tmpR, rsrc)}, an)
	  in  if immedOrReg lsrc andalso immedOrReg rsrc then
	      let val lsrc = replace lsrc
		  val rsrc = replace rsrc
	      in  if isMemory rsrc then
		     if isMemory lsrc then reloadIt()
		     else (* it is commutative! *)
			done(test{lsrc=rsrc, rsrc=lsrc}, an)
		  else
		     done(test{lsrc=lsrc, rsrc=rsrc}, an)
	      end
	      else reloadIt()
	  end

      fun reloadPush(push, arg as I.Direct _, an) =
	    done(push(replace arg), an)
	| reloadPush(push, arg, an) =
	    withTmpAvail(fn tmpR => push(operand(tmpR, arg)), an)

      fun reloadReal(realOp, opnd, an) =
	    withTmpAvail(fn tmpR => realOp(operand(tmpR, opnd)), an)
   in
      case instr
      of I.JMP(I.Direct _, labs) => done(I.JMP(spillLoc, labs), an)
       | I.JMP(opnd, labs) => withTmp(fn t => I.JMP(operand(t, opnd), labs), an)
       | I.JCC{opnd=I.Direct _, cond} => done(I.JCC{opnd=spillLoc, cond=cond}, an)
       | I.JCC{opnd, cond} =>
	    withTmp(fn t => I.JCC{opnd=operand(t,opnd), cond=cond}, an)
       | I.CALL{opnd, defs, uses, return, cutsTo, mem, pops} =>
	    withTmp(fn t =>
		I.CALL{opnd=operand(t, opnd), defs=defs, return=return,pops=pops,
		       uses=C.rmvReg(reg, uses), cutsTo=cutsTo, mem=mem}, an)
       | I.MOVE{mvOp, src as I.Direct _, dst as I.Direct _} =>
	  done(I.MOVE{mvOp=mvOp, src=replace src, dst=dst},an)
       | I.MOVE{mvOp, src, dst as I.Direct _} =>
	  withTmpAvail(fn t =>I.MOVE{mvOp=mvOp, src=operand(t, src), dst=dst},an)
       | I.MOVE{mvOp, src as I.Direct _, dst} =>
	  if Props.eqOpn(dst, spillLoc) then {code=[], proh=[], newReg=NONE}
	  else withTmpAvail (* dst is not the spill reg *)
	    (fn t =>
	       I.MOVE{mvOp=mvOp, src=operand(t, src), dst=operand(t, dst)}, an)
       | I.MOVE{mvOp, src, dst} =>
	  withTmpAvail (* dst is not the spill reg *)
	   (fn t =>
	      I.MOVE{mvOp=mvOp, src=operand(t, src), dst=operand(t, dst)}, an)
       | I.LEA{r32, addr} =>
	  withTmpAvail(fn tmpR => I.LEA{r32=r32, addr=operand(tmpR, addr)}, an)
       | I.CMPL{lsrc, rsrc} => reloadCmp(I.CMPL, lsrc, rsrc, an)
       | I.CMPW{lsrc, rsrc} => reloadCmp(I.CMPW, lsrc, rsrc, an)
       | I.CMPB{lsrc, rsrc} => reloadCmp(I.CMPB, lsrc, rsrc, an)
       | I.TESTL{lsrc, rsrc} => reloadTest(I.TESTL, lsrc, rsrc, an)
       | I.TESTW{lsrc, rsrc} => reloadTest(I.TESTW, lsrc, rsrc, an)
       | I.TESTB{lsrc, rsrc} => reloadTest(I.TESTB, lsrc, rsrc, an)
       | I.BITOP{bitOp,lsrc, rsrc} => reloadBT(bitOp, lsrc, rsrc, an)
       | I.BINARY{binOp, src, dst as I.Direct _} =>
	    (case src of
	      I.Direct _ =>
		done(I.BINARY{binOp=binOp, src=replace src, dst=dst},an)
	    | _ => withTmp(fn tmpR =>
		I.BINARY{binOp=binOp, src=operand(tmpR, src), dst=dst}, an)
	    )
       | I.BINARY{binOp, src, dst} =>
	  withTmp(fn tmpR => I.BINARY{binOp=binOp, src=operand(tmpR, src),
						   dst=operand(tmpR, dst)}, an)
       | I.CMOV{cond, src, dst} =>
         if CB.sameColor(dst,reg) then
            error "CMOV"
         else
            done(I.CMOV{cond=cond, src=spillLoc, dst=dst}, an)
       | I.SHIFT{shiftOp, count, src, dst} => error "go and implement SHIFT"
       | I.CMPXCHG{lock,sz,src,dst} =>
	  withTmp(fn tmpR => I.CMPXCHG{lock=lock, sz=sz,
				       src=operand(tmpR, src),
				       dst=operand(tmpR, dst)},an)
       | I.MULTDIV{multDivOp, src as I.Direct _} =>
	  done(I.MULTDIV{multDivOp=multDivOp, src=replace src}, an)
       | I.MULTDIV{multDivOp, src} =>
	  withTmp(fn tmpR =>
	      I.MULTDIV{multDivOp=multDivOp, src=operand(tmpR, src)}, an)
       | I.MUL3{src1, src2, dst} =>
	  withTmp(fn tmpR =>
	    I.MUL3{src1=operand(tmpR, src1), src2=src2,
		   dst=if CB.sameColor(dst,reg)
		       then error "reload:MUL3" else dst}, an)
       | I.UNARY{unOp, opnd} =>
	  withTmpAvail
	     (fn tmpR => I.UNARY{unOp=unOp, opnd=operand(tmpR, opnd)}, an)
       | I.SET{cond, opnd} =>
	  withTmpAvail(fn tmpR => I.SET{cond=cond, opnd=operand(tmpR, opnd)}, an)
       | I.PUSHL arg => reloadPush(I.PUSHL, arg, an)
       | I.PUSHW arg => reloadPush(I.PUSHW, arg, an)
       | I.PUSHB arg => reloadPush(I.PUSHB, arg, an)
       | I.FILD opnd => reloadReal(I.FILD, opnd, an)
       | I.FILDL opnd => reloadReal(I.FILDL, opnd, an)
       | I.FILDLL opnd => reloadReal(I.FILDLL, opnd, an)
       | I.FLDT opnd => reloadReal(I.FLDT, opnd, an)
       | I.FLDL opnd => reloadReal(I.FLDL, opnd, an)
       | I.FLDS opnd => reloadReal(I.FLDS, opnd, an)
       | I.FSTPT opnd => reloadReal(I.FSTPT, opnd, an)
       | I.FSTPL opnd => reloadReal(I.FSTPL, opnd, an)
       | I.FSTPS opnd => reloadReal(I.FSTPS, opnd, an)
       | I.FSTL opnd => reloadReal(I.FSTL, opnd, an)
       | I.FSTS opnd => reloadReal(I.FSTS, opnd, an)
       | I.FUCOM opnd => reloadReal(I.FUCOM, opnd, an)
       | I.FUCOMP opnd => reloadReal(I.FUCOMP, opnd, an)
       | I.FCOMI opnd => reloadReal(I.FCOMI, opnd, an)
       | I.FCOMIP opnd => reloadReal(I.FCOMIP, opnd, an)
       | I.FUCOMI opnd => reloadReal(I.FUCOMI, opnd, an)
       | I.FUCOMIP opnd => reloadReal(I.FUCOMIP, opnd, an)
       | I.FENV{fenvOp, opnd} => reloadReal(fn opnd =>
				   I.FENV{fenvOp=fenvOp,opnd=opnd}, opnd, an)
       | I.FBINARY{binOp, src, dst} =>
	  withTmpAvail(fn tmpR =>
		   I.FBINARY{binOp=binOp, src=operand(tmpR, src), dst=dst}, an)
       | I.FIBINARY{binOp, src} =>
	  withTmpAvail
	    (fn tmpR => I.FIBINARY{binOp=binOp, src=operand(tmpR, src)}, an)

	 (* Pseudo fp instrctions *)
       | I.FMOVE{fsize,src,dst} =>
	  withTmpAvail
	    (fn tmpR => I.FMOVE{fsize=fsize, src=operand(tmpR, src),
				dst=operand(tmpR, dst)}, an)
       | I.FILOAD{isize,ea,dst} =>
	  withTmpAvail
	    (fn tmpR => I.FILOAD{isize=isize, ea=operand(tmpR, ea),
				 dst=operand(tmpR, dst)}, an)
       | I.FBINOP{fsize,binOp,lsrc,rsrc,dst} =>
	  withTmpAvail(fn tmpR =>
	     I.FBINOP{fsize=fsize, binOp=binOp, lsrc=operand(tmpR, lsrc),
		      rsrc=operand(tmpR, rsrc), dst=operand(tmpR, dst)}, an)
       | I.FIBINOP{isize,binOp,lsrc,rsrc,dst} =>
	  withTmpAvail(fn tmpR =>
	     I.FIBINOP{isize=isize, binOp=binOp, lsrc=operand(tmpR, lsrc),
		       rsrc=operand(tmpR, rsrc), dst=operand(tmpR, dst)}, an)
       | I.FUNOP{fsize,unOp,src,dst} =>
	  withTmpAvail(fn tmpR =>
	     I.FUNOP{fsize=fsize, unOp=unOp, src=operand(tmpR, src),
		     dst=operand(tmpR, dst)}, an)
       | I.FCMP{i,fsize,lsrc,rsrc} =>
	  withTmpAvail(fn tmpR =>
	     I.FCMP{i=i,fsize=fsize,
		    lsrc=operand(tmpR, lsrc), rsrc=operand(tmpR, rsrc)
		   }, an)

       | _ => error "reload"
    end (*x86Reload*)

    fun f(I.ANNOTATION{a, i}, an) = f(i, a::an)
      | f(I.INSTR i, an) = x86Reload(i, reg, spillLoc, an)
      | f(I.LIVE lk, an) =
	   {code=[annotate(I.LIVE(rLiveKill (lk, reg)), an)],
	    proh=[],
	    newReg=NONE}
      | f _ = error "reload: f"
  in f(instr, [])
  end (* reload *)




  fun spillF(instr, reg, spillLoc) = let
    fun x86Fspill(instr, reg, spillLoc, an) = let
      fun withTmp(f, fsize, an) = let
	val tmpR = C.newFreg()
	val tmp  = I.FPR tmpR
      in
	 { proh=[tmpR],
	   code=[mark(f tmp, an),
		 I.fmove{fsize=fsize, src=tmp, dst=spillLoc}],
	   newReg=SOME tmpR (* XXX Should we propagate the definition? *)
	  }
      end
    in
      case instr
      of I.FSTPL _ => {proh=[], code=[mark(I.FSTPL spillLoc, an)], newReg=NONE}
       | I.FSTPS _ => {proh=[], code=[mark(I.FSTPS spillLoc, an)], newReg=NONE}
       | I.FSTPT _ => {proh=[], code=[mark(I.FSTPT spillLoc, an)], newReg=NONE}
       | I.FSTL _ => {proh=[], code=[mark(I.FSTL spillLoc, an)], newReg=NONE}
       | I.FSTS _ => {proh=[], code=[mark(I.FSTS spillLoc, an)], newReg=NONE}
       | I.CALL{opnd, defs, uses, return, cutsTo, mem, pops} =>
	 {proh=[],
	  code=[mark(I.CALL{opnd=opnd, defs=C.rmvFreg(reg,defs),
			    return=return, uses=uses,
			    cutsTo=cutsTo, mem=mem, pops=pops}, an)],
	  newReg=NONE}

       (* Pseudo fp instrctions *)
       | I.FMOVE{fsize,src,dst} =>
	  if Props.eqOpn(src,spillLoc) then
	    {proh=[], code=[], newReg=NONE}
	  else
	    {proh=[],code=[mark(I.FMOVE{fsize=fsize,src=src,dst=spillLoc},an)],
	     newReg=NONE}
       | I.FILOAD{isize,ea,dst} =>
	    {proh=[],code=[mark(I.FILOAD{isize=isize,ea=ea,dst=spillLoc},an)],
	     newReg=NONE} (* XXX bad for single precision *)
       | I.FBINOP{fsize as I.FP64,binOp,lsrc,rsrc,dst} =>
	    {proh=[],code=[mark(I.FBINOP{fsize=fsize,binOp=binOp,
					 lsrc=lsrc, rsrc=rsrc,
					 dst=spillLoc},an)],
	     newReg=NONE}
       | I.FBINOP{fsize,binOp,lsrc,rsrc,dst} =>
	  withTmp(fn tmpR =>
		  I.FBINOP{fsize=fsize, binOp=binOp,
			   lsrc=lsrc, rsrc=rsrc, dst=tmpR},
		  fsize, an)
       | I.FIBINOP{isize,binOp,lsrc,rsrc,dst} =>
	  withTmp(fn tmpR =>
		  I.FIBINOP{isize=isize, binOp=binOp,
			    lsrc=lsrc, rsrc=rsrc, dst=tmpR},
		  I.FP64, an) (* XXX *)
       | I.FUNOP{fsize,unOp,src,dst} =>
	  {proh=[],code=[mark(I.FUNOP{fsize=fsize,unOp=unOp,
				      src=src,dst=spillLoc},an)],
			newReg=NONE}
       | _ => error "fspill"
      (*esac*)
    end (* x86Fspill *)
    fun f(I.ANNOTATION{a,i}, an) = f(i, a::an)
      | f(I.INSTR(i), an) = x86Fspill(i, reg, spillLoc, an)
      | f(I.KILL lk, an) =
	   {code=[annotate(I.KILL(fLiveKill (lk, reg)), an)],
	    proh=[],
	    newReg=NONE}
      | f _ = error "fspill:f"
  in f(instr, [])
  end


  fun reloadF(instr, reg, spillLoc) = let
    fun x86Freload(instr, reg, spillLoc, an) = let
	fun rename(src as I.FDirect f) =
	    if CB.sameColor(f,reg) then spillLoc else src
	  | rename(src as I.FPR f) =
	    if CB.sameColor(f,reg) then spillLoc else src
	  | rename src = src

	fun withTmp(fsize, f, an) =
	    case spillLoc of
	      I.FDirect _ => {newReg=NONE, proh=[], code=[mark(f spillLoc, an)]}
	    | I.FPR _ => {newReg=NONE, proh=[], code=[mark(f spillLoc, an)]}
	    |  _ =>
	      let val ftmpR = C.newFreg()
		  val ftmp  = I.FPR(ftmpR)
	      in  {newReg=NONE,
		   proh=[ftmpR],
		   code=[I.fmove{fsize=fsize, src=spillLoc, dst=ftmp},
			 mark(f ftmp, an)
			]
		  }
	      end
     in
	(case instr of
	   I.FLDT opnd => {code=[mark(I.FLDT spillLoc, an)], proh=[], newReg=NONE}
	 | I.FLDL opnd => {code=[mark(I.FLDL spillLoc, an)], proh=[], newReg=NONE}
	 | I.FLDS opnd => {code=[mark(I.FLDS spillLoc, an)], proh=[], newReg=NONE}
	 | I.FUCOM opnd => {code=[mark(I.FUCOM spillLoc, an)],proh=[],newReg=NONE}
	 | I.FUCOMP opnd => {code=[mark(I.FUCOMP spillLoc, an)],proh=[],newReg=NONE}
	 | I.FCOMI opnd => {code=[mark(I.FCOMI spillLoc, an)],proh=[],newReg=NONE}
	 | I.FCOMIP opnd => {code=[mark(I.FCOMIP spillLoc, an)],proh=[],newReg=NONE}
	 | I.FUCOMI opnd => {code=[mark(I.FUCOMI spillLoc, an)],proh=[],newReg=NONE}
	 | I.FUCOMIP opnd => {code=[mark(I.FUCOMIP spillLoc, an)],proh=[],newReg=NONE}
	 | I.FBINARY{binOp, src=I.FDirect f, dst} =>
	     if CB.sameColor(f,reg) then
	       {code=[mark(I.FBINARY{binOp=binOp, src=spillLoc, dst=dst}, an)],
		proh=[],
		newReg=NONE}
	     else error "reloadF:FBINARY"

	 (* Pseudo fp instructions.
	  *)
	 | I.FMOVE{fsize,src,dst} =>
	    if Props.eqOpn(dst,spillLoc) then
	      {code=[], proh=[], newReg=NONE}
	    else
	      {code=[mark(I.FMOVE{fsize=fsize,src=spillLoc,dst=dst},an)],
		     proh=[], newReg=NONE}
	 | I.FBINOP{fsize,binOp,lsrc,rsrc,dst} =>
	    {code=[mark(I.FBINOP{fsize=fsize,binOp=binOp,
				 lsrc=rename lsrc, rsrc=rename rsrc,dst=dst},an)],
		   proh=[], newReg=NONE}
	 | I.FIBINOP{isize,binOp,lsrc,rsrc,dst} =>
	    {code=[mark(I.FIBINOP{isize=isize,binOp=binOp,
				  lsrc=rename lsrc,rsrc=rename rsrc,dst=dst},an)],
		   proh=[], newReg=NONE}
	 | I.FUNOP{fsize,unOp,src,dst} =>
	    {code=[mark(I.FUNOP{fsize=fsize,unOp=unOp,
				src=rename src, dst=dst},an)],
		   proh=[], newReg=NONE}
	 | I.FCMP{i,fsize,lsrc,rsrc} =>
	    (* Make sure that both the lsrc and rsrc cannot be in memory *)
	    (case (lsrc, rsrc) of
	      (I.FPR fs1, I.FPR fs2) =>
		(case (CB.sameColor(fs1,reg), CB.sameColor(fs2,reg)) of
		   (true, true) =>
		   withTmp(fsize,
		      fn tmp => I.FCMP{i=i,fsize=fsize,lsrc=tmp, rsrc=tmp}, an)
		 | (true, false) =>
		   {code=[mark(I.FCMP{i=i,fsize=fsize,lsrc=spillLoc,rsrc=rsrc},an)],
		    proh=[], newReg=NONE}
		 | (false, true) =>
		   {code=[mark(I.FCMP{i=i,fsize=fsize,lsrc=lsrc,rsrc=spillLoc},an)],
		    proh=[], newReg=NONE}
		 | _ => error "fcmp.1"
		)
	     | (I.FPR _, _) =>
		withTmp(fsize,
		   fn tmp => I.FCMP{i=i,fsize=fsize,lsrc=tmp, rsrc=rsrc}, an)
	     | (_, I.FPR _) =>
		withTmp(fsize,
		   fn tmp => I.FCMP{i=i,fsize=fsize,lsrc=lsrc, rsrc=tmp}, an)
	     | _ => error "fcmp.2"
	    )
	 | I.CALL{opnd, defs, uses, return, cutsTo, mem, pops} =>
	   {proh=[],
	    code=[mark(I.CALL{opnd=opnd, defs=C.rmvFreg(reg,defs),
			      return=return, pops=pops,
			      uses=uses, cutsTo=cutsTo, mem=mem}, an)],
	    newReg=NONE}
	 | _  => error "reloadF"
	(*esac*))
    end (* x86Freload *)

    fun f(I.ANNOTATION{a, i}, an) = f(i, a::an)
      | f(I.INSTR i, an) = x86Freload(i, reg, spillLoc, an)
      | f(I.LIVE lk, an) =
	   {code=[annotate(I.LIVE(fLiveKill (lk, reg)), an)],
	    proh=[],
	    newReg=NONE}
      | f _ = error "freload.f"

  in f(instr, [])
  end

  fun spillToEA CB.GP (reg, ea) = let
	fun returnMove() =
	  {code=[I.move{mvOp=I.MOVL, src=I.Direct reg, dst=ea}],
	   proh=[], newReg=NONE}
      in
	 case ea
	  of I.MemReg _  => returnMove()
	   | I.Displace _ => returnMove()
	   | I.Indexed _ => returnMove()
	   | _ => error "spillToEA: GP"
      end
    | spillToEA CB.FP (freg, ea) = error "spillToEA: FP"
    | spillToEA _ _ = error "spillToEA"

  fun reloadFromEA CB.GP (reg, ea) = let
        fun returnMove() =
	  {code=[I.move{mvOp=I.MOVL, dst=I.Direct reg, src=ea}],
	   proh=[],
	   newReg=NONE}
      in
	 case ea
	  of I.MemReg _  => returnMove()
	   | I.Displace _ => returnMove()
	   | I.Indexed _ => returnMove()
	   | _ => error "reloadFromEA: GP"
      end
    | reloadFromEA CB.FP (freg, ea) = error "spillToEA: FP"
    | reloadFromEA _ _ = error "spillToEA"


  fun reload CB.GP = reloadR
    | reload CB.FP = reloadF
    | reload _ = error "reload"

  fun spill CB.GP = spillR
    | spill CB.FP = spillF
    | spill _ = error "spill"
end
