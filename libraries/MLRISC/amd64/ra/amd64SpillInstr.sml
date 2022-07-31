(* amd64SpillInstr.sml
 *
 * This functor contains callback functions for spilling and reloading
 * instructions.
 *)

functor AMD64SpillInstr (
      structure I : AMD64INSTR
      structure Props : AMD64INSN_PROPERTIES
          where I = I

   (* guaranteeing that floats are stored at 16-byte aligned addresses reduces the number of instructions *)
    val floats16ByteAligned : bool
    ) : ARCH_SPILL_INSTR =
  struct

    structure CB = CellsBasis
    structure I = I
    structure C = I.C

    fun error msg = MLRiscErrorMsg.impossible("AMD64Spill: "^ msg)

    fun liveKill(add, rmv) ({regs, spilled}, reg) =
        {regs=rmv(reg, regs), spilled=add(reg, spilled)}
    val fLiveKill = liveKill (C.addFreg, C.rmvFreg)
    val rLiveKill = liveKill (C.addReg, C.rmvReg)

    val newReg = C.newReg
    val newFreg = C.newFreg

    fun annotate (instr,[]) = instr
      | annotate (instr,a::an) = annotate(I.ANNOTATION{i=instr,a=a},an)

    fun mark (instr, an) = annotate(I.INSTR instr, an)

    fun immed(I.Immed _) = true
      | immed(I.Immed64 _) = true
      | immed(I.ImmedLabel _) = true
      | immed _ = false

    fun immedOrReg(I.Direct r) = true
      | immedOrReg(I.Immed _) = true
      | immedOrReg(I.Immed64 _) = true
      | immedOrReg(I.ImmedLabel _) = true
      | immedOrReg _ = false

    fun isMemory(I.Displace _) = true
      | isMemory(I.Indexed _) = true
      | isMemory(I.LabelEA _) = true
      | isMemory _ = false

    fun mvInstr instr = let
        fun mvOp 8 = I.MOVB
          | mvOp 16 = I.MOVW
          | mvOp 32 = I.MOVL
          | mvOp 64 = I.MOVQ
          | mvOp _ = error "mvInstr"
        val sz = Props.szOfInstr instr
        in
          (sz, mvOp sz)
        end (* mvInstr *)

    fun fmvInstr instr = let
        fun mvOp 32 = I.MOVSS
          | mvOp 64 = I.MOVSD
          | mvOp _ = error "fmvInstr"
        val sz = Props.szOfFinstr instr
        in
          (sz, mvOp sz)
        end (* fmvInstr *)

    fun spillToEA CB.GP (r, ea) = let
        fun move () = {code=[I.move {mvOp=I.MOVQ, dst=ea, src=I.Direct (64, r)}],
                       proh=[], newReg=NONE}
        in
          (case ea
            of ( I.Displace _ | I.Indexed _ ) => move ()
             | _ => error "spillToEA"
          (* end case *))
        end
      | spillToEA CB.FP _ = error "spillToEA: FP"
      | spillToEA _ _ = error "spillToEA"

    fun reloadFromEA CB.GP (r, ea) = let
        fun move () = {code=[I.move {mvOp=I.MOVQ, dst=I.Direct (64, r), src=ea}],
                       proh=[], newReg=NONE}
        in
          (case ea
            of ( I.Displace _ | I.Indexed _ ) => move ()
             | _ => error "reloadFromEA"
          (* end case *))
        end
      | reloadFromEA CB.FP _ = error "reloadFromEA: FP"
      | reloadFromEA _ _ = error "reloadFromEA"

    (* spill a general purpose register r at instruction i to spillLoc *)
    fun spillR (i, r, spillLoc) = let
        fun spill (instr, an) = let
            fun done (instr, an) = {code=[mark (instr, an)], proh=[], newReg=NONE}
            val (defaultSz, defaultMov) = (64, I.MOVQ)
	    (* freshTmp generates a fresh temporary register, an operand with the
	     * instruction's operand size, and an operand with a 64-bit operand size. *)
	    fun freshTmp () = let
		val tmpR = newReg ()
		val sz = Props.szOfInstr instr
		val tmpOpnd = I.Direct (sz, tmpR)
		val tmpOpnd64 = I.Direct (64, tmpR)
	        in
		   (tmpR, tmpOpnd, tmpOpnd64)
		end
            in
              (case instr
                of I.CALL {opnd=addr, defs, uses, return, cutsTo, mem, pops} =>
		   done (I.CALL {opnd=addr, defs=C.rmvReg (r, defs),
		    	        return=return, uses=uses,
			        cutsTo=cutsTo, mem=mem, pops=pops}, an)
		 (* With sign or zero extended spills we use different operand sizes
		  * for copying to the tmp operand and for copying from the tmp operand. *)
		 | I.MOVE {mvOp as (I.MOVZBQ|I.MOVSBQ|I.MOVZWQ|I.MOVSWQ|
		                    I.MOVSLQ|I.MOVZBL|I.MOVSBL|I.MOVZWL|
		                    I.MOVSWL), src, dst} => let
		   val (tmpR, tmpOpnd, tmpOpnd64) = freshTmp ()
		   in
		     {proh=[tmpR], newReg=SOME tmpR,
		      code=[mark (I.MOVE {mvOp=mvOp, src=src, dst=tmpOpnd}, an),
		            I.move {mvOp=defaultMov, src=tmpOpnd64, dst=spillLoc}]}
		   end
		 (* spill is unnecessary *)
		 | I.MOVE {mvOp, src as I.Direct (_, rs), dst} =>
		   if CB.sameColor (rs, r) then {code=[], proh=[], newReg=NONE}
		   else done (I.MOVE {mvOp=mvOp, src=src, dst=spillLoc}, an)
		 | I.MOVE {mvOp, src, dst=I.Direct _} =>
		   if Props.eqOpn(src, spillLoc)
		     then {code=[], proh=[], newReg=NONE}
		     else if immed src then
			  done(I.MOVE{mvOp=mvOp, src=src, dst=spillLoc}, an)
		      else let
		        val (tmpR, tmpOpnd, tmpOpnd64) = freshTmp ()
			in
			  {proh=[tmpR],
			   newReg=SOME tmpR,
			   code=[mark(I.MOVE {mvOp=mvOp, src=src, dst=tmpOpnd}, an),
				 I.move {mvOp=defaultMov, src=tmpOpnd64, dst=spillLoc}]}
			end
	         | I.LEAL {addr, r32} => let
	           val tmpR = newReg()
		   in
		     {proh=[tmpR], newReg=SOME tmpR,
		      code=[mark(I.LEAL {addr=addr, r32=tmpR}, an),
				 I.move{mvOp=I.MOVL, src=I.Direct (32, tmpR),
				        dst=spillLoc}]}
		   end
		 | I.LEAQ {addr, r64} => let
                   val tmpR = newReg()
		   in
		     {proh=[tmpR], newReg=SOME tmpR,
		      code=[mark(I.LEAQ{addr=addr, r64=tmpR}, an),
				 I.move{mvOp=I.MOVQ, src=I.Direct (64,tmpR),
				        dst=spillLoc}]}
		      end
		 (* handle xorl and xorq with the special case when both operands are the
		  * same register *)
		 | I.BINARY {binOp=I.XORL, src as I.Direct (_,rs),
		             dst=I.Direct (_,rd)} =>
		   if CB.sameColor (rs,rd)
		     then {proh=[], newReg=NONE,
		           code=[mark(I.MOVE{mvOp=I.MOVL, src=I.Immed 0,
		                 dst=spillLoc}, an)]}
		      else
			  {proh=[], newReg=NONE,
			   code=[mark(I.BINARY{binOp=I.XORL, src=src,
			         dst=spillLoc}, an)]}
		 | I.BINARY{binOp=I.XORQ, src as I.Direct (_,rs),
		            dst=I.Direct (_,rd)} =>
		   if CB.sameColor(rs,rd)
		     then {proh=[], newReg=NONE,
			   code=[mark(I.MOVE{mvOp=I.MOVQ, src=I.Immed 0,
			         dst=spillLoc}, an)]}
		     else {proh=[], newReg=NONE,
			   code=[mark(I.BINARY{binOp=I.XORQ, src=src,
			         dst=spillLoc}, an)]}

		 | I.BINARY {binOp, src, dst} => let
		   (* note: dst = r *)
		   fun multBinOp (I.IMULQ|I.IMULL|I.IMULW|I.IMULB) = true
		     | multBinOp _ = false
		   in
		     if multBinOp binOp
		       then let (* destination must remain a register *)
			 val (tmpR, tmpOpnd, tmpOpnd64) = freshTmp ()
			 in
		           {proh=[tmpR], newReg=SOME tmpR,
		            code=[I.move{mvOp=defaultMov, src=spillLoc, dst=tmpOpnd64},
			          I.binary{binOp=binOp, src=src, dst=tmpOpnd},
			          I.move{mvOp=defaultMov, src=tmpOpnd64, dst=spillLoc}]}
			 end
			else if immedOrReg src
			      then (* can replace the destination directly *)
			        done(I.BINARY{binOp=binOp, src=src, dst=spillLoc},
			             an)
			  else let (* a memory src and non multBinOp
				   * --- cannot have two memory operands
				   *)
				val (tmpR, tmpOpnd, tmpOpnd64) = freshTmp ()
			        in
				  { proh=[tmpR], newReg=NONE,
				    code=[I.move{mvOp=defaultMov, src=src, dst=tmpOpnd64},
					  I.binary{binOp=binOp, src=tmpOpnd,
					           dst=spillLoc}]}
			        end
		        end
		 | I.SHIFT {shiftOp, count=I.Direct (_, ecx), src, dst} =>
		   error "implement shift"
		 | I.SHIFT {shiftOp, count, src, dst} =>
		   if immedOrReg src
		     then done (I.SHIFT {shiftOp=shiftOp, src=src, dst=spillLoc,
		             count=count}, an)
		     else let
		       val (tmpR, tmpOpnd, tmpOpnd64) = freshTmp ()
		       in
		         {proh=[tmpR], newReg=SOME tmpR,
		          code=[I.move {mvOp=defaultMov, src=src, dst=tmpOpnd64},
		                I.shift {shiftOp=shiftOp, src=tmpOpnd, dst=spillLoc,
		                   count=count}]}
		       end
		 | I.CMOV {cond, src, dst} =>
		   (* note: dst must be a register *)
		   (case spillLoc
		     of I.Direct (_,r) =>
			   {proh=[], newReg=NONE,
			    code=[mark(I.CMOV{cond=cond,src=src,dst=r},an)]
			   }
		       | _ => let
			 val (tmpR, _, tmpOpnd64) = freshTmp ()
			 in
			   {proh=[tmpR], newReg=SOME tmpR,
		            code=[I.move{mvOp=I.MOVQ, src=spillLoc, dst=tmpOpnd64},
				  mark(I.CMOV{cond=cond,src=src,dst=tmpR},an),
				  I.move{mvOp=I.MOVQ, src=tmpOpnd64, dst=spillLoc}]}
			   end
		    (* end case *))
		 | I.XADD{lock,sz=isz,src as I.Direct (_, srcR),dst} =>
		   if CB.sameColor (srcR, r)
		      then let val (tmpR, tmpOpnd, tmpOpnd64) = freshTmp()
		       in {proh=[tmpR],
			   code=[I.move{mvOp=I.MOVQ, src=src, dst=tmpOpnd64},
				 mark(I.XADD{lock=lock,sz=isz,src=tmpOpnd,dst=dst},an),
				 I.move{mvOp=I.MOVQ, src=tmpOpnd64, dst=spillLoc}
				],
			   newReg=SOME tmpR
			  }
		       end
		     else let val (tmpR, tmpOpnd, tmpOpnd64) = freshTmp()
		       in {proh=[tmpR],
			   code=[I.move{mvOp=I.MOVQ, src=dst, dst=tmpOpnd64},
				 mark(I.XADD{lock=lock,sz=isz,src=src,dst=tmpOpnd},an),
				 I.move{mvOp=I.MOVQ, src=tmpOpnd64, dst=spillLoc}
				],
			   newReg=SOME tmpR
			  }
		       end
		 | I.CMPXCHG{lock,sz=isz,src,dst} =>
		   if immedOrReg src then
		       {proh=[],
			code=[mark(I.CMPXCHG{lock=lock,sz=isz,src=src,dst=spillLoc},an)],
			newReg=NONE
		       }
		   else
		       let val (tmpR, tmpOpnd, tmpOpnd64) = freshTmp()
		       in {proh=[tmpR],
			   code=[I.move{mvOp=I.MOVQ, src=src, dst=tmpOpnd64},
				 mark(I.CMPXCHG{lock=lock,sz=isz,src=tmpOpnd,dst=spillLoc},an)],
			   newReg=SOME tmpR
			  }
		       end
		 | I.XCHG{lock,sz=isz,src,dst} =>
		   if immedOrReg src then
		       {proh=[],
			code=[mark(I.XCHG{lock=lock,sz=isz,src=src,dst=spillLoc},an)],
			newReg=NONE
		       }
		   else
		       let val (tmpR, tmpOpnd, tmpOpnd64) = freshTmp()
		       in {proh=[tmpR],
			   code=[I.move{mvOp=I.MOVQ, src=src, dst=tmpOpnd64},
				 mark(I.XCHG{lock=lock,sz=isz,src=tmpOpnd,dst=spillLoc},an)],
			   newReg=SOME tmpR
			  }
		       end
		 | I.RDTSC => error "spill: RDTSC"
		 | I.RDTSCP => error "spill: RDTSCP"
		 | I.MULTDIV _ => error "spill: MULTDIV"
		 | I.MUL3 {src1, src2, dst} => let
		   val tmpR = newReg()
		   in
		     {proh=[tmpR], newReg=SOME tmpR,
		      code=[mark(I.MUL3{src1=src1, src2=src2, dst=tmpR}, an),
		            I.move{mvOp=I.MOVL, src=I.Direct (32,tmpR),
                            dst=spillLoc}]
			  }
		   end
		 | I.MULQ3 {src1, src2, dst} => let
		   val tmpR = newReg()
		   in
		     {proh=[tmpR], newReg=SOME tmpR,
		      code=[mark(I.MULQ3{src1=src1, src2=src2, dst=tmpR}, an),
				 I.move{mvOp=I.MOVQ, src=I.Direct (64,tmpR),
				        dst=spillLoc}]}
		      end
		 | I.UNARY{unOp, opnd} =>
		   done(I.UNARY{unOp=unOp, opnd=spillLoc}, an)
		 | I.SET{cond, opnd} =>
		   done(I.SET{cond=cond, opnd=spillLoc}, an)
		 | I.POP _ => done (I.POP spillLoc, an)
		 | _ => error "AMD64SpillInstr.spillR"
	       (* end case *))
            end (* spill *)
        fun f (i, ans) = (case i
            of I.INSTR instr => spill (instr, ans)
             | I.ANNOTATION {i, a} => f (i, a :: ans)
             | I.KILL lk => {code=[annotate (I.KILL (rLiveKill (lk, r)), ans)],
                             proh=[], newReg=NONE}
             | _ => error "AMD64SpillInstr.spillR"
            (* end case *))
        in
          f (i, [])
        end (* spillR *)

    fun spillF (i, r, spillLoc) = let
    	fun spill (instr, an) = let
    	    val (sz, fmvOp) = fmvInstr instr
    	    fun withTmp f = let
    	        val tmpR = newFreg ()
    	        val tmp = I.FDirect tmpR
    	        in
    	          {code=[mark (f tmpR, an),
    	                 I.fmove {fmvOp=fmvOp, src=tmp, dst=spillLoc}],
    	           proh=[tmpR], newReg=SOME tmpR}
    	        end (* withTmp *)
	    fun binOpWithTmp f = let
                   val tmpR = newFreg ()
		   val tmp = I.FDirect tmpR
		   in
		       {code=[I.fmove {fmvOp=fmvOp, src=spillLoc, dst=tmp},
			      mark (f tmpR, an),
			      I.fmove {fmvOp=fmvOp, src=tmp, dst=spillLoc}],
			proh=[tmpR], newReg=SOME tmpR}
		   end (* binOpWithTmp *)
    	    in
    	      (case instr
    	        of I.FMOVE {fmvOp, src=src as I.FDirect r', dst} =>
    	           if CB.sameColor (r, r')
    	             then {code=[], proh=[], newReg=NONE}
    	             else {code=[mark (I.FMOVE {fmvOp=fmvOp, src=src,
    	                                  dst=spillLoc}, an)],
    	                   proh=[], newReg=NONE}
    	         | I.FMOVE {fmvOp, src, dst as I.FDirect _} => withTmp (fn tmpR =>
    	           I.FMOVE {fmvOp=fmvOp, src=src, dst=I.FDirect tmpR})
    	         | I.FBINOP {binOp, src, dst} => binOpWithTmp (fn tmpR =>
		   I.FBINOP {binOp=binOp, src=src, dst=tmpR})
     	         | I.FSQRTS {dst, src} => withTmp (fn tmpR =>
     	           I.FSQRTS {src=src, dst=I.FDirect tmpR})
     	         | I.FSQRTD {dst, src} => withTmp (fn tmpR =>
     	           I.FSQRTD {src=src, dst=I.FDirect tmpR})
     	         | I.CALL {opnd, defs, uses, return, cutsTo, mem, pops} =>
		   {code=[mark (I.CALL {opnd=opnd, defs=C.rmvFreg (r, defs),
			    return=return, uses=uses,
			    cutsTo=cutsTo, mem=mem, pops=pops}, an)],
	            proh=[], newReg=NONE}
     	         | _ => error "spillF"
    	      (* end case *))
    	    end (* spill *)
    	fun f (i, ans) = (case i
    	    of I.INSTR instr => spill (instr, ans)
    	     | I.ANNOTATION {i, a} => f (i, a :: ans)
    	     | I.KILL lk => {code=[annotate (I.KILL (fLiveKill (lk, r)), ans)],
		             proh=[], newReg=NONE}
             | _ => error "spillF"
            (* end case *))
        in
          f (i, [])
        end (* spillF *)

    fun spill CB.GP = spillR
      | spill CB.FP = spillF
      | spill _ = error "spill"

    val comment : string -> Annotations.annotation = (#create MLRiscAnnotations.COMMENT)

    (* reload a general purpose register r at instruction i from spillLoc *)
    fun reloadR (i, r, spillLoc) = let
        fun reload (instr, an) = let
	    fun done (instr, an) = {code=[mark (instr, an)], proh=[], newReg=NONE}
	    fun replace (opnd as I.Direct (sz, r')) = if CB.sameColor (r, r')
	        then (case spillLoc
		       of I.Direct (_, r) =>
			  (* I believe that this case occurrs when the mlrisc register allocator
			   * is doing something clever, as the spill location is clearly not a stack
			   * location. in any case, the size associated with the register r is bogus,
			   * which is why we use the size of the register being spilled. *)
			  I.Direct (sz, r)
			| _ => spillLoc)
	        else opnd
	      | replace opnd = opnd
	    fun operand (opnd, tmp) = let
	        fun replaceR (r', f) = if CB.sameColor (r, r')
	            then f tmp
	            else opnd
	        in
		  (case opnd
	            of I.Direct (opndSz, opndR) => replaceR (opndR, fn r' =>
	               I.Direct (opndSz, r'))
	             | I.Displace {base, disp, mem} => replaceR (base, fn r' =>
	               I.Displace {base=r', disp=disp, mem=mem})
	             | I.Indexed {base=NONE, index, scale, disp, mem} =>
	               replaceR (index, fn index' =>
	                 I.Indexed {base=NONE, index=index', scale=scale,
	                            disp=disp, mem=mem})
	             | I.Indexed {base=SOME b, index, scale, disp, mem} =>
	               if CB.sameColor (b, r)
			  then operand (I.Indexed {base=SOME tmp, index=index, scale=scale, disp=disp, mem=mem}, tmp)
		       else if CB.sameColor (index, r)
		          then I.Indexed {base=SOME b, index=tmp, scale=scale, disp=disp, mem=mem}
		       else opnd
	             | opnd => opnd
	           (* end case *))
	         end (* operand *)
	    fun operand' (I.Direct _, _) = spillLoc
	      | operand' (opnd, tmp) = operand (opnd, tmp)
	    (* assume that tmpR gets killed *)
	    fun withTmp' avail f = (case spillLoc
	        of I.Direct (_, tmpR) => if avail
	           then {code=[mark (f tmpR, an)], proh=[tmpR], newReg=SOME tmpR}
	           else {code=[mark (f tmpR, an)], proh=[], newReg=NONE}
	         | _ => let
	           val tmpR = newReg ()
	           in
	             {code=[I.move {mvOp=I.MOVQ, src=spillLoc,
	                            dst=I.Direct (64, tmpR)},
	                    mark (f tmpR, an)],
	              proh=[tmpR], newReg=SOME tmpR}
	           end
	        (* end case *))
	    val withTmp = withTmp' false
	    val withTmpAvail = withTmp' true
	    fun reloadCmp (cmp, lsrc, rsrc, an) = let
	        fun reload () = withTmp (fn tmp =>
	            cmp {lsrc=operand (lsrc, tmp), rsrc=operand (rsrc, tmp)})
	        in
	          if immedOrReg lsrc andalso immedOrReg rsrc
	            then let
	              val rsrc' = replace rsrc
	              val lsrc' = replace lsrc
	              in
	                if isMemory rsrc' andalso isMemory lsrc'
	                  then reload ()
	                  else done (cmp {lsrc=lsrc', rsrc=rsrc'}, an)
	              end
	            else reload ()
	        end (* reloadCmp *)
	    fun reloadTest (test, lsrc, rsrc, an) = let
	        fun reload () = withTmp (fn tmp =>
	            test {lsrc=operand (lsrc, tmp), rsrc=operand (rsrc, tmp)})
	        in
	          if immedOrReg lsrc andalso immedOrReg rsrc
	            then let
	              val rsrc' = replace rsrc
	              val lsrc' = replace lsrc
	              in
	                if isMemory rsrc'
	                  then if isMemory lsrc'
	                         then reload ()
	                         else done (test {lsrc=rsrc', rsrc=lsrc'}, an)
	                  else done (test {lsrc=lsrc', rsrc=rsrc'}, an)
	              end
	            else reload ()
	        end (* reloadTest *)
	    fun reloadBT (bitOp, lsrc, rsrc, an) =
		reloadCmp (fn {lsrc,rsrc} =>
		  I.BITOP {bitOp=bitOp,lsrc=lsrc,rsrc=rsrc}, lsrc, rsrc, an)
	    fun reloadPush(push, arg as I.Direct _, an) =
		done(push(replace arg), an)
	      | reloadPush(push, arg, an) =
		withTmpAvail(fn tmpR => push (operand(arg, tmpR)))
            in
              (case instr
                of I.JMP (opnd, labs) => withTmp (fn tmp =>
                   I.JMP (operand' (opnd, tmp), labs))
                 | I.JCC {opnd, cond} => withTmp (fn tmp =>
                   I.JCC {opnd=operand' (opnd, tmp), cond=cond})
                 | I.CALL {opnd, defs, uses, return, cutsTo, mem, pops} =>
                   withTmp (fn tmp =>
                     I.CALL {opnd=operand (opnd, tmp), defs=defs,
                             uses=C.rmvReg (r, uses), return=return, pops=pops,
                             cutsTo=cutsTo, mem=mem})
                 | I.MOVE {mvOp, src as I.Direct _, dst as I.Direct _} =>
                   done (I.MOVE {mvOp=mvOp, src=replace src, dst=dst}, an)
                 | I.MOVE {mvOp, src, dst as I.Direct _} => withTmpAvail (fn tmp =>
                   I.MOVE {mvOp=mvOp, src=operand (src, tmp), dst=dst})
                 | I.MOVE {mvOp, src as I.Direct _, dst} =>
                   if (Props.eqOpn (dst, spillLoc))
                      then {code=[], proh=[], newReg=NONE}
                      else withTmpAvail (fn tmp =>
                             I.MOVE {mvOp=mvOp, src=operand (src, tmp),
                                     dst=operand (dst, tmp)})
                 | I.MOVE {mvOp, src, dst} => withTmpAvail (fn tmp =>
                   I.MOVE {mvOp=mvOp, src=operand (src, tmp),
                           dst=operand (dst, tmp)})
                 | I.LEAL {r32, addr} => withTmpAvail (fn tmp =>
                   I.LEAL {r32=r32, addr=operand (addr, tmp)})
                 | I.LEAQ {r64, addr} => withTmpAvail (fn tmp =>
                   I.LEAQ {r64=r64, addr=operand (addr, tmp)})
                 | I.CMPQ {lsrc, rsrc} => reloadCmp (I.CMPQ, lsrc, rsrc, an)
		 | I.CMPL {lsrc, rsrc} => reloadCmp (I.CMPL, lsrc, rsrc, an)
		 | I.CMPW {lsrc, rsrc} => reloadCmp (I.CMPW, lsrc, rsrc, an)
		 | I.CMPB {lsrc, rsrc} => reloadCmp (I.CMPB, lsrc, rsrc, an)
		 | I.TESTQ {lsrc, rsrc} => reloadTest (I.TESTQ, lsrc, rsrc, an)
		 | I.TESTL {lsrc, rsrc} => reloadTest (I.TESTL, lsrc, rsrc, an)
		 | I.TESTW {lsrc, rsrc} => reloadTest (I.TESTW, lsrc, rsrc, an)
		 | I.TESTB {lsrc, rsrc} => reloadTest (I.TESTB, lsrc, rsrc, an)
		 | I.BITOP{bitOp,lsrc, rsrc} => reloadBT (bitOp, lsrc, rsrc, an)
		 | I.BINARY{binOp, src, dst} => withTmp (fn tmp =>
		   I.BINARY{binOp=binOp, src=operand(src, tmp),
			    dst=operand(dst, tmp)})
		 | I.CMOV{cond, src, dst} =>
		   if CB.sameColor(dst,r)
		     then error "CMOV"
		      else done (I.CMOV{cond=cond, src=spillLoc, dst=dst}, an)
		 | I.SHIFT {shiftOp, count as I.Direct _, src, dst} =>
		   withTmp (fn tmp =>
		     I.SHIFT {shiftOp=shiftOp, count=count,
		              src=operand (src, tmp),
		               dst=operand (dst, tmp)})
		 | I.CMPXCHG{lock,sz,src,dst} => withTmp(fn tmpR =>
		   I.CMPXCHG{lock=lock, sz=sz,
			     src=operand(src, tmpR),
			     dst=operand(dst, tmpR)})
		 | I.XCHG{lock,sz,src,dst} => withTmp(fn tmpR =>
		   I.XCHG{lock=lock, sz=sz,
			  src=operand(src, tmpR),
			  dst=operand(dst, tmpR)})
		 | I.XADD{lock,sz,src,dst} => withTmp(fn tmpR =>
		   I.XADD {lock=lock,sz=sz,
			   src=operand(src, tmpR),
			   dst=operand(dst, tmpR)})
		 | I.MULTDIV{multDivOp, src as I.Direct _} =>
		   done (I.MULTDIV{multDivOp=multDivOp, src=replace src}, an)
		 | I.MULTDIV{multDivOp, src} => withTmp(fn tmpR =>
		   I.MULTDIV{multDivOp=multDivOp, src=operand(src, tmpR)})
		 | I.MUL3{src1, src2, dst} => withTmp (fn tmpR =>
		   I.MUL3{src1=operand(src1, tmpR), src2=src2,
		          dst=if CB.sameColor(dst,r)
			      then error "reload:MUL3" else dst})
		 | I.MULQ3{src1, src2, dst} => withTmp (fn tmpR =>
		   I.MULQ3{src1=operand(src1, tmpR), src2=src2,
			   dst=if CB.sameColor(dst,r)
		               then error "reload:MULQ3" else dst})
		 | I.UNARY{unOp, opnd} => withTmpAvail (fn tmpR =>
		   I.UNARY{unOp=unOp, opnd=operand(opnd, tmpR)})
		 | I.SET{cond, opnd} => withTmpAvail (fn tmpR =>
		   I.SET{cond=cond, opnd=operand(opnd, tmpR)})
		 | I.PUSH arg => reloadPush (I.PUSH, arg, an)
		 | I.FMOVE {fmvOp, src, dst} => withTmpAvail (fn tmp =>
		   I.FMOVE {fmvOp=fmvOp, src=operand (src, tmp),
		            dst=operand (dst, tmp)})
		 | I.FCOM {comOp, dst, src} => withTmpAvail (fn tmp =>
		   I.FCOM {comOp=comOp, dst=dst, src=operand (src, tmp)})
		 | I.FBINOP {binOp, dst, src} => withTmpAvail (fn tmp =>
                   I.FBINOP {binOp=binOp, dst=dst, src=operand (src, tmp)})
		 | I.RDTSC => error "reload: RDTSC"
 		 | I.RDTSCP => error "reload: RDTSCP"
		 | _ => error "reloadR"
              (* end case *))
            end (* reload *)
        fun f (i, ans) = (case i
            of I.INSTR instr => reload (instr, ans)
             | I.ANNOTATION {a, i} => f (i, a :: ans)
             | I.LIVE lk => {code=[annotate (I.LIVE (rLiveKill (lk, r)), ans)],
                             proh=[], newReg=NONE}
             | _ => error "reloadR"
            (* end case *))
        in
          f (i, [])
        end (* reloadR *)

    fun reloadF (i, r, spillLoc) = let
        fun reload (instr, an) = let
            fun replace (opnd as I.FDirect r') =
                if CB.sameColor (r, r') then spillLoc else opnd
              | replace opnd = opnd
            val (sz, fmvOp) = fmvInstr instr
            in
              (case instr
                of I.FMOVE {fmvOp, src, dst=dst as I.FDirect _} =>
                   {code=[mark (I.FMOVE {fmvOp=fmvOp, src=replace src, dst=dst},
                           an)],
                    proh=[], newReg=NONE}
                 | I.FMOVE {fmvOp, src, dst} => let
                   val tmpR = newFreg ()
                   val tmp = I.FDirect tmpR
                   in
                     {code=[I.fmove {fmvOp=fmvOp, src=spillLoc, dst=tmp},
                            mark (I.FMOVE {fmvOp=fmvOp, src=tmp, dst=dst}, an)],
                      proh=[tmpR], newReg=SOME tmpR}
                   end
                (* treat bitwise operators differently, as they require that their source operand is 16-byte aligned *)
		 | I.FBINOP {binOp=binOp as (I.XORPS | I.XORPD | I.ANDPS | I.ANDPD | I.ORPS | I.ORPD), src, dst} => if CB.sameColor (r, dst)
                   then {code=[I.fmove {fmvOp=fmvOp, src=spillLoc, dst=I.FDirect dst},
			       mark (I.FBINOP {binOp=binOp, src=src, dst=dst}, an)],
		         proh=[], newReg=NONE}
                   else if floats16ByteAligned
                        then let
                          val tmpR = newFreg ()
                          val tmp = I.FDirect tmpR
                          in
                              {code=[I.fmove {fmvOp=fmvOp, src=spillLoc, dst=tmp},
				     mark (I.FBINOP {binOp=binOp, src=tmp, dst=dst}, an)],
		               proh=[tmpR], newReg=SOME tmpR}
                          end
                   else {code=[mark (I.FBINOP {binOp=binOp, src=replace src, dst=dst}, an)], proh=[], newReg=NONE}
                 | I.FBINOP {binOp, src, dst} => if CB.sameColor (r, dst)
 	           then {code=[I.fmove {fmvOp=fmvOp, src=spillLoc, dst=I.FDirect dst},
			       mark (I.FBINOP {binOp=binOp, src=src, dst=dst}, an)],
		         proh=[], newReg=NONE}
		   else {code=[mark (I.FBINOP {binOp=binOp, src=replace src, dst=dst}, an)], proh=[], newReg=NONE}
                 | I.FCOM {comOp, src, dst} => let
                   val tmpR = newFreg ()
                   val tmp = I.FDirect tmpR
		   val (src, dst) = if CB.sameColor (dst, r)
				       then (src, tmpR)
				       else (tmp, dst)
                   in
		       {code=[I.fmove {fmvOp=fmvOp, src=spillLoc, dst=tmp},
                             mark (I.FCOM {comOp=comOp, src=src, dst=dst}, an)],
                        proh=[tmpR], newReg=SOME tmpR}
                   end
                 | I.FSQRTS {dst, src} => if floats16ByteAligned
                   then let
                     val tmpR = newFreg ()
                     val tmp = I.FDirect tmpR
                     in
                        {code=[I.fmove {fmvOp=fmvOp, src=spillLoc, dst=tmp},
                               mark (I.FSQRTS {src=tmp, dst=dst}, an)],
			 proh=[tmpR], newReg=SOME tmpR}
                     end
                   else {code=[mark (I.FSQRTS {src=spillLoc, dst=dst}, an)], proh=[], newReg=NONE}
                 | I.FSQRTD {dst, src} => if floats16ByteAligned
                   then let
                     val tmpR = newFreg ()
                     val tmp = I.FDirect tmpR
                     in
                        {code=[I.fmove {fmvOp=fmvOp, src=spillLoc, dst=tmp},
                               mark (I.FSQRTD {src=tmp, dst=dst}, an)],
			 proh=[tmpR], newReg=SOME tmpR}
                     end
                   else {code=[mark (I.FSQRTD {src=spillLoc, dst=dst}, an)], proh=[], newReg=NONE}
                 | I.CALL {opnd, defs, uses, return, cutsTo, mem, pops} =>
                   {code=[mark (I.CALL {opnd=opnd, defs=C.rmvReg (r, defs),
                                        uses=uses, return=return, cutsTo=cutsTo,
                                        mem=mem, pops=pops}, an)],
                    proh=[], newReg=NONE}
                 | _ => error "reloadF"
               (* end case *))
            end (* reload *)
        fun f (i, ans) = (case i
            of I.INSTR i => reload (i, ans)
             | I.ANNOTATION {i, a} => f (i, a :: ans)
             | I.LIVE lk =>
               {code=[annotate (I.LIVE (fLiveKill (lk, r)), ans)],
                proh=[], newReg=NONE}
             | _ => error "reloadF.f"
            (* end case *))
        in
          f (i, [])
        end (* reloadF *)

    fun reload CB.GP = reloadR
      | reload CB.FP = reloadF
      | reload _ = error "reload"

  end (* AMD64SpillInstr *)
