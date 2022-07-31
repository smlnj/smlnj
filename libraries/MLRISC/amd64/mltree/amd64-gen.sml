(* amd64-gen.sml
 *
 * COPYRIGHT (c) 2016 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Translate MLRISC trees into AMD64 instructions.
 *)

functor AMD64Gen (

    structure I : AMD64INSTR
    structure MLTreeUtils : MLTREE_UTILS
	where T = I.T
    structure ExtensionComp : MLTREE_EXTENSION_COMP
        where I = I and T = I.T

    (* Take a number of bits and returns an rexp that points to a literal with the high bit set.
     * We need this literal value for floating-point negation and absolute value (at least
     * until SSE4).
     *)
    val signBit : int -> MLTreeUtils.T.rexp
    (* Same as signBit, except the high bit is zero and the low bits are 1s. *)
    val negateSignBit : int -> MLTreeUtils.T.rexp

   (* guaranteeing that floats are stored at 16-byte aligned addresses reduces the number of instructions *)
    val floats16ByteAligned : bool

   ) : MLTREECOMP = struct

    structure TS = ExtensionComp.TS
    structure T = I.T
    structure I = I
    structure CFG = ExtensionComp.CFG
    structure C = I.C
    structure CB = CellsBasis
    structure A = MLRiscAnnotations
    structure TRS = MLTreeSize (structure T = T
                                val intTy = 64)
    structure Shuffle = Shuffle (I)
    structure O = AMD64Opcodes (structure I = I)
    structure Gen = MLTreeGen (
            structure T = T
            structure Cells = C
            val intTy = 64
            val naturalWidths = [32, 64]
            datatype rep = SE | ZE | NEITHER
            val rep = NEITHER)

    structure W32 = Word32

    fun error msg = MLRiscErrorMsg.error ("AMD64Gen", msg)

    type instrStream = (I.instruction,C.cellset,CFG.cfg) TS.stream
    type mltreeStream = (T.stm,T.mlrisc list,CFG.cfg) TS.stream

  (* label where the shared trap is generated -- one per cluster *)
    val trapLabel = ref (NONE: (I.instruction * Label.label) option)

  (* flag to control merging of overflow traps *)
    val mergeTraps = ref true

    fun gpr (ty, r) = I.Direct (ty, r)
    val fpr = I.FDirect

    fun rax ty = I.Direct(ty,C.rax)
    fun rcx ty = I.Direct(ty,C.rcx)
    fun rdx ty = I.Direct(ty,C.rdx)

    fun signExtend ty = if ty = 64 then I.CDO else I.CDQ

    val readonly = I.Region.readonly

    val newReg  = C.newReg
    val newFreg = C.newFreg

    (* convert mlrisc to cellset: *)
    fun cellset mlrisc = let
	val addCCReg = CB.CellSet.add
        fun g([],acc) = acc
          | g(T.GPR(T.REG(_,r))::regs,acc)  = g(regs,C.addReg(r,acc))
          | g(T.FPR(T.FREG(_,f))::regs,acc) = g(regs,C.addFreg(f,acc))
          | g(T.CCR(T.CC(_,cc))::regs,acc)  = g(regs,addCCReg(cc,acc))
          | g(T.CCR(T.FCC(_,cc))::regs,acc)  = g(regs,addCCReg(cc,acc))
          | g(_::regs, acc) = g(regs, acc)
      in
	g(mlrisc, C.empty)
      end

  (* conversions to fixed-precision integers*)
    fun toInt32 i = T.I.toInt32(32, i)
    fun toInt64 i = T.I.toInt64(64, i)

(* 64BIT: workaround for compiler bug *)
    val minInt32 = Word64.toLargeIntX 0wxFFFFFFFF80000000
    val maxInt32 = Word64.toLargeInt 0wx80000000

  (* is an immediate operand representable as a signed 32-bit 2's complement value?
   * Note that values get sign extended when loaded into a 64-bit register.
   *)
    fun fitsIn32Bits (z : IntInf.int) = (minInt32 <= z) andalso (z < maxInt32)
(*
    fun fitsIn32Bits (z : IntInf.int) = (~0x80000000 <= z) andalso (z < 0x80000000)
*)

    fun move64 (src, dst) = I.move {mvOp=I.MOVABSQ, src=src, dst=dst}

    (* analyze for power-of-two-ness; this does not handle values that do not fit
     * in 32 bits.
     *)
    fun analyze i' = if fitsIn32Bits i'
	  then let
	    val i = toInt32 i'
	    in
	      let
	      val (isneg, a, w) = if i >= 0
		    then (false, i, T.I.toWord32 (32, i'))
		    else (true, ~i, T.I.toWord32 (32, T.I.NEG (32,  i')))
	      fun log2 (0w1, p) = p
		| log2 (w, p) = log2 (W32.>> (w, 0w1), p + 1)
	      in
		if w > 0w1 andalso W32.andb (w - 0w1, w) = 0w0
		  then SOME(i, isneg, a, T.LI (T.I.fromInt32 (32, log2 (w, 0))))
		  else NONE
	       end handle _ => NONE
	    end
	  else NONE
(* a 64-bit version that we might want to use in the future
    fun analyze i' = let
	  val i = toInt64 i'
	  val (isneg, a, w) = if i >= 0
		then (false, i, T.I.toWord64 (64, i'))
		else (true, ~i, T.I.toWord64 (64, T.I.NEG (64,  i')))
	       fun log2 (0w1, p) = p
		 | log2 (w, p) = log2 (W64.>> (w, 0w1), p + 1)
	  in
	    if w > 0w1 andalso W64.andb (w - 0w1, w) = 0w0
	      then (i, SOME (isneg, a, T.LI (T.I.fromInt64 (64, log2 (w, 0)))))
	      else (i, NONE)
	  end
	    handle _ => (toInt64 i', NONE)
*)

    (* translate MLTREE condition codes to amd64 condition codes *)
    fun cond T.LT = I.LT | cond T.LTU = I.B
      | cond T.LE = I.LE | cond T.LEU = I.BE
      | cond T.EQ = I.EQ | cond T.NE  = I.NE
      | cond T.GE = I.GE | cond T.GEU = I.AE
      | cond T.GT = I.GT | cond T.GTU = I.A
      | cond cc = error(concat["cond(", T.Basis.condToString cc, ")"])

      (* Is the expression zero? *)
    fun isZero (T.LI 0) = true
      | isZero (T.MARK(e, _)) = isZero e
      | isZero _ = false

    fun setZeroBit(T.ANDB _)     = true
      | setZeroBit(T.ORB _)      = true
      | setZeroBit(T.XORB _)     = true
      | setZeroBit(T.SRA _)      = true
      | setZeroBit(T.SRL _)      = true
      | setZeroBit(T.SLL _)      = true
      | setZeroBit(T.SUB _)      = true
      | setZeroBit(T.ADDT _)     = true
      | setZeroBit(T.SUBT _)     = true
      | setZeroBit(T.MARK(e, _)) = setZeroBit e
      | setZeroBit _             = false

    fun setZeroBit2(T.ANDB _)     = true
      | setZeroBit2(T.ORB _)      = true
      | setZeroBit2(T.XORB _)     = true
      | setZeroBit2(T.SRA _)      = true
      | setZeroBit2(T.SRL _)      = true
      | setZeroBit2(T.SLL _)      = true
      | setZeroBit2(T.ADD(_, _, _)) = true (* can't use leal! *)
      | setZeroBit2(T.SUB _)      = true
      | setZeroBit2(T.ADDT _)     = true
      | setZeroBit2(T.SUBT _)     = true
      | setZeroBit2(T.MARK(e, _)) = setZeroBit2 e
      | setZeroBit2 _             = false

    and isMemOpnd opnd = (case opnd
        of  I.Displace _ => true
          | I.Indexed _  => true
          | I.LabelEA _  => true
          | I.FDirect f  => true
          | _            => false
          (* end case *))

    fun selectInstructions (instrStream as TS.S.STREAM{
          emit=emitInstr, defineLabel, entryLabel, pseudoOp, annotation,
          getAnnotations, beginCluster, endCluster, exitBlock,
          comment, ...
	}) = let
	val emit = emitInstr o I.INSTR
	val emits = app emitInstr
	(* mark an expression with a list of annotations *)
	fun mark' (i, []) = emitInstr i
  	  | mark' (i, a::an) = mark'(I.ANNOTATION{i=i,a=a},an)
	(* annotate an expression and emit it *)
	fun mark (i, an) = mark' (I.INSTR i, an)
	(* annotated 64-bit move *)
	fun move64' (src, dst, an) = mark' (move64(src, dst), an)
	(* move with annotation *)
	fun move' (ty, dst as I.Direct (_, s), src as I.Direct (_, d), an) =
	      if CB.sameColor (s, d)
		then ()
		else mark' (I.COPY {k=CB.GP, sz=ty, src=[s], dst=[d], tmp=NONE}, an)
	  | move' (ty, I.Immed 0, dst as I.Direct _, an) =
	      mark' (I.binary {binOp=O.xorOp ty, src=dst, dst=dst}, an)
	  | move' (ty, src as I.Immed64 n, dst, an) = move64' (src, dst, an)
	  | move' (ty, src as I.ImmedLabel _ , dst as I.Direct _, an) = move64' (src, dst, an)
	  | move' (ty, src as I.ImmedLabel _ , dst, an) = let
	      val tmp = newReg ()
	      val tmpR = I.Direct (64, tmp)
	      in
		move64' (src, tmpR, an);
		emitInstr (I.move {mvOp=I.MOVQ, src=tmpR, dst=dst})
	      end
	  | move' (ty, src, dst, an) =
	      mark' (I.move {mvOp=O.movOp ty, src=src, dst=dst}, an)
	(* move without annotation *)
	fun move (ty, src, dst) = move' (ty, src, dst, [])

        fun zero (ty, dst) = emit (I.BINARY{binOp=O.xorOp ty, src=dst, dst=dst})

	fun copy (ty, [], [], an) = ()
	  | copy (ty, dst, src, an) = let
	    fun mvInstr {dst=I.Direct (_,rd), src=I.Direct (_,rs)} =
	        if CB.sameColor (rd, rs)
	          then []
	          else [I.COPY {k=CB.GP, sz=ty, dst=[rd], src=[rs], tmp=NONE}]
	      | mvInstr {dst, src} = [I.move {mvOp=O.movOp ty, src=src, dst=dst}]
	    val stms = Shuffle.shuffle {mvInstr=mvInstr, ea=fn r => gpr (ty, r)}
                         {tmp=SOME (I.Direct (ty, newReg ())), dst=dst, src=src}
	    in
	      emits stms
	    end (* copy *)

        (* Add an overflow trap *)
	fun trap () = if !mergeTraps
	      then let
		val jmp = (case !trapLabel
		       of NONE => let
			    val label = Label.label "trap" ()
			    val jmp = I.ANNOTATION{
				    i = I.jcc{cond=I.O, opnd=I.ImmedLabel(T.LABEL label)},
				    a = MLRiscAnnotations.BRANCHPROB Probability.unlikely
				  }
			    in
			      trapLabel := SOME(jmp, label);
			      jmp
			    end
			| SOME(jmp, _) => jmp
		      (* end case *))
		in
		  emitInstr jmp
		end (* trap *)
	      else let
		val label = Label.anon()
		val jmp = I.ANNOTATION{
			i = I.jcc{cond=I.NO, opnd = I.ImmedLabel(T.LABEL label)},
			a = MLRiscAnnotations.BRANCHPROB Probability.likely
		      }
		in
		  emitInstr jmp;
		(* signal an overflow exception (code 4) *)
		  emit(I.INT 0w4);
		  defineLabel label
		end

	exception EA

        fun address' ty (ea : T.rexp, mem) = let
            fun makeAddressingMode (NONE, NONE, _, disp) = disp
	      | makeAddressingMode (SOME base, NONE, _, disp) =
                  I.Displace{base=base, disp=disp, mem=mem}
              | makeAddressingMode (base, SOME index, scale, disp) =
                  I.Indexed{base=base, index=index, scale=scale, disp=disp, mem=mem}

  	    (* Keep building a bigger and bigger effective address expressions
	     * The input is a list of trees
	     * b -- base
	     * i -- index
	     * s -- scale
	     * d -- immed displacement
	     *)
	    fun doEA ([], b, i, s, d) = makeAddressingMode(b, i, s, d)
  	      | doEA (t::trees, b, i, s, d) = (case t
	        of T.LI n => if fitsIn32Bits n
		    then doEAImmed (trees, toInt32 n, b, i, s, d)
		    else error "doEA: immediate too large"
		 | T.CONST _ => doEALabel(trees, t, b, i, s, d)
		 | T.LABEL _ => doEALabel(trees, t, b, i, s, d)
		 | T.LABEXP le => doEALabel(trees, le, b, i, s, d)
		 | T.ADD(ty, t1, t2 as T.REG(_,r)) =>
		     doEA(t1::t2::trees, b, i, s, d)
		 | T.ADD(ty, t1, t2) => doEA(t1::t2::trees, b, i, s, d)
		 | T.SUB(ty, t1, T.LI n) =>
		     doEA(t1::T.LI(T.I.NEG(ty,n))::trees, b, i, s, d)
		 | T.SLL(ty, t1, T.LI n) => let
		   val n = T.I.toInt(ty, n)
                   in
	             case n
		       of 0 => displace(trees, t1, b, i, s, d)
	  		| 1 => indexed(trees, t1, t, 1, b, i, s, d)
	                | 2 => indexed(trees, t1, t, 2, b, i, s, d)
      	                | 3 => indexed(trees, t1, t, 3, b, i, s, d)
			| _ => displace(trees, t, b, i, s, d)
                   end
	         | t => displace(trees, t, b, i, s, d)
		(* esac *))

	  (* Add a 32-bit immed constant *)
            and doEAImmed (trees, 0, b, i, s, d) = doEA(trees, b, i, s, d)
 	      | doEAImmed (trees, n, b, i, s, I.Immed m) =
                  doEA(trees, b, i, s, I.Immed(n+m))
              | doEAImmed (trees, n, b, i, s, I.ImmedLabel le) =
                  doEA(trees, b, i, s,
                       I.ImmedLabel(T.ADD(ty,le,T.LI(T.I.fromInt32(ty, n)))))
              | doEAImmed (trees, n, b, i, s, _) = error "doEAImmed"

            (* Add a label expression.
	     * NOTE: Labels in the AMD64 can be 64 bits, but operands can only handle 32-bit constants.
	     * We have to spill label expressions to temporaries to be correct.
	     * TODO: eliminate the extra register-register move from genExpr
	     *)
            (* Added below pattern to stop infinite loop where (genExpr le)
               below called addition and addition called address again. *)
	    and doEALabel (trees, le, SOME base, i, s, I.Immed 0) = let
                val r = newReg ()
                val _ = expr (le, r, [])
                val _ = mark (I.BINARY{binOp=(O.addOp ty), src=operand ty (T.REG(ty,base)), dst=operand ty (T.REG(ty,r))}, [])
                in
                  doEA(trees, SOME r, i, s, I.Immed 0)
                end
	      | doEALabel (trees, le, b, i, s, d) = let
		val le = (case b
			    of NONE => le
			     | SOME base => T.ADD (ty, T.REG (ty, base), le)
                          (* end case *))
	        in
		  (case d
		    of I.Immed 0 => doEA(trees, SOME (genExpr le), i, s, I.Immed 0)
		     | I.Immed m => (doEA (trees,
		              SOME (genExpr (T.ADD (ty, le, T.LI(T.I.fromInt32(ty, m))))),
		         i, s, I.Immed 0)
		       handle Overflow => error "doEALabel: constant too large")
		     | I.ImmedLabel le' => doEA (trees,
		              SOME (genExpr (T.ADD (ty, le', le))),
		         i, s, I.Immed 0)
		     | _ => error "doEALabel"
                  (* end case *))
	        end

            (* generate code for tree and ensure that it is not in %rsp *)
            and exprNotRsp tree = let
		val r = genExpr tree
		in
		  if CB.sameColor(r, C.rsp) then let
		     val tmp = newReg()
	             in
		       move(ty, I.Direct (ty,r), I.Direct (ty,tmp));
		       tmp
		     end
                  else r
		end

            (* Add a base register *)
            and displace(trees, t, NONE, i, s, d) =  (* no base yet *)
		doEA(trees, SOME(genExpr t), i, s, d)
              | displace(trees, t, b as SOME base, NONE, _, d) = (* no index *)
                (* make t the index, but make sure that it is not %rsp! *)
		let val i = genExpr t
		in  if CB.sameColor(i, C.rsp) then
	               (* swap base and index *)
	               if CB.sameColor(base, C.rsp) then
			  doEA(trees, SOME i, b, 0, d)
		       else  (* base and index = %rsp! *)
			  let val index = newReg()
			  in
			     move(ty, I.Direct (ty,i), I.Direct (ty,index));
		             doEA(trees, b, SOME index, 0, d)
			  end
                    else doEA(trees, b, SOME i, 0, d)
		end
             | displace(trees, t, SOME base, i, s, d) = (* base and index *)
	       let val b = genExpr (T.ADD(ty,T.REG(ty,base),t))
	       in
	       	 doEA(trees, SOME b, i, s, d)
	       end

            (* Add an indexed register *)
            and indexed(trees, t, t0, scale, b, NONE, _, d) = (* no index yet *)
		doEA(trees, b, SOME(exprNotRsp t), scale, d)
              | indexed(trees, _, t0, _, NONE, i, s, d) = (* no base *)
  		doEA(trees, SOME(genExpr t0), i, s, d)
              | indexed(trees, _, t0, _, SOME base, i, s, d) = (*base and index*)
  		let val b = genExpr (T.ADD(ty, t0, T.REG(ty, base)))
		in
		  doEA(trees, SOME b, i, s, d)
		end
	  in
	    case doEA ([ea], NONE, NONE, 0, I.Immed 0)
	     of I.Immed _ => raise EA
	      | I.ImmedLabel le => I.Displace {base=genExpr le, disp=I.Immed 0, mem=mem} (* I.LabelEA le*)
	      | ea => ea
	  end (* address' *)

        and address (ea, mem) = address' 64 (ea, mem)

      (* load the label into a temp register. this operation is necessary, since most instructions
       * can only handle 32-bit immediates
       *)
	and loadLabel src = let
	      val tmp = newReg ()
	      val tmpR = I.Direct (64, tmp)
	      in
		emitInstr (move64 (src, tmpR));
		tmpR
	      end

        (* reduce an expression into an operand *)
        and operand ty (T.LI i) = if (fitsIn32Bits i)
	      then I.Immed (toInt32 i)
	      else let
		(* i is a 64-bit operand *)
		 val dstR = newReg ()
		 val dst = I.Direct (ty, dstR)
		 val i' = T.I.signed (64, i)
		 in
		   move (ty, I.Immed64 (Int64.fromLarge i'), dst);
		   dst
		 end
          | operand _ (x as (T.CONST _ | T.LABEL _)) = loadLabel (I.ImmedLabel x)
          | operand _ (T.LABEXP le) = loadLabel (I.ImmedLabel le)
          | operand _ (T.REG(ty,r)) = gpr (ty, r)
          | operand _ (T.LOAD(ty,ea,mem)) = address (ea, mem)
          | operand ty t = I.Direct(ty, genExpr t)

	and immedLabel lab = I.ImmedLabel(T.LABEL lab)

	(* ensure that the operand is either an immed or register *)
	and immedOrReg(ty, opnd as I.Displace _) = moveToReg (ty, opnd)
	  | immedOrReg(ty, opnd as I.Indexed _)  = moveToReg (ty, opnd)
	  | immedOrReg(ty, opnd as I.LabelEA _)  = moveToReg (ty, opnd)
	  | immedOrReg (ty, opnd)  = opnd

	and regOrMem (ty, opnd as (I.Immed _ | I.ImmedLabel _)) = moveToReg (ty, opnd)
	  | regOrMem (ty, opnd) = opnd

	and getExpr (exp as T.REG(_, rd)) = rd
          | getExpr exp = genExpr exp

	and moveToReg (ty, opnd) = let
	    val dst = I.Direct (ty, newReg ())
	    in
	      move (ty, opnd, dst);
	      dst
	    end

	and isImmediate(I.Immed _) = true
(*	  | isImmediate(I.ImmedLabel _) = true *)
	  | isImmediate _ = false

        (* generate an expression, and return its result in a register *)
        and genExpr (T.REG (_, r)) = r
          | genExpr e = let
	      val r = newReg ()
	      in
		expr (e, r, []);
		r
	      end

	and expr' (ty, e, dst, an) = let
              val dstOpnd = gpr (ty, dst)
	      fun equalDst (I.Direct (_,r)) = CB.sameColor(r, dst)
		| equalDst _ = false
	      fun dstMustBeReg f = f (dst, dstOpnd)
	      fun genLoad (mvOp, ea, mem) = dstMustBeReg (fn (_, dst) =>
		  mark (I.MOVE {mvOp=mvOp, src=address (ea, mem), dst=dst},an))
	      fun unknownExp exp = expr (Gen.compileRexp exp, dst, an)

	      (* Generate a unary operator *)
	      fun unary(ty, unOp, e) = let
		  val opnd = operand ty e
		  in
		    if isMemOpnd opnd then let
		       val tmp = I.Direct(ty, newReg())
		       in
			 move(ty, opnd, tmp);
			 move(ty, tmp, dstOpnd)
		       end
		    else move(ty, opnd, dstOpnd);
			 mark(I.UNARY{unOp=unOp ty, opnd=dstOpnd}, an)
		  end

	      fun genBinary (ty, binOp, opnd1, opnd2) =
		    if (isMemOpnd opnd1 orelse isMemOpnd opnd2) orelse
		       equalDst(opnd2) then let
		       val tmpR = newReg()
		       val tmp  = I.Direct (ty,tmpR)
		       in
			 move (ty, opnd1, tmp);
			 mark (I.BINARY{binOp=binOp ty, src=opnd2, dst=tmp}, an);
			 move (ty, tmp, dstOpnd)
		       end
		    else (move (ty, opnd1, dstOpnd);
			 mark(I.BINARY{binOp=binOp ty, src=opnd2, dst=dstOpnd}, an))
	      (* generate a binary operator that may commute *)
	      fun binaryComm (ty, binOp, e1, e2) = let
		  val (opnd1, opnd2) = (case (operand ty e1, operand ty e2)
		      of (x as I.Immed _, y)      => (y, x)
		       | (x as I.ImmedLabel _, y) => (y, x)
		       | (x, y as I.Direct _)     => (y, x)
		       | (x, y)                   => (x, y)
		      (* end case *))
		  in
		    genBinary(ty, binOp, opnd1, opnd2)
		  end
	      (* Generate a binary operator; non-commutative *)
	      fun binary(ty, binOp, e1, e2) =
		  genBinary(ty, binOp, operand ty e1, operand ty e2)
	      (* Add n to dst *)
	      fun addN (addOp, n) = let
		  val n = operand ty n
		  in
		    mark (I.BINARY{binOp=addOp, src=n, dst=dstOpnd}, an)
		  end
	      (* Generate addition *)
	      fun addition (ty, e1, e2) = (case e1
		  of T.REG(_,rs) =>
		     if CB.sameColor(rs,dst)
			then addN (O.addOp ty, e2)
			else addition1 (ty, e1,e2)
		   | _ => addition1(ty, e1,e2)
		  (* end case *))
	      and addition1 (ty, e1, e2) = (case e2
		  of T.REG(_,rs) =>
		     if CB.sameColor(rs,dst)
			then addN (O.addOp ty, e1)
			else addition2(ty, e1,e2)
		   | _ => addition2 (ty, e1,e2)
		  (* end case *))
	      and addition2 (32, e1, e2) = (
		    (* try *)
		    dstMustBeReg(fn (dstR, _) =>
		      mark(I.LEAL{r32=dstR, addr=address' 32 (e, readonly)}, an))
		    (* catch *)
		      handle EA => binaryComm(ty, O.addOp, e1, e2))
		| addition2 (64, e1, e2) = let
		    fun isBigImmed (T.LI n) = not(fitsIn32Bits n)
		      | isBigImmed _ = false
		    fun lea (dstR, _) = mark(I.LEAQ{r64=dstR, addr=address' 64 (e, readonly)}, an)
		    in
		      if isBigImmed e1 orelse isBigImmed e2
			then binaryComm(ty, O.addOp, e1, e2)
			else (dstMustBeReg lea) handle EA => binaryComm(ty, O.addOp, e1, e2)
		    end
              (* the shift amount must be a constant or in %rcx *)
              fun shift(ty, opcode, e1, e2) = let
                  val (opnd1, opnd2) = (operand ty e1, operand ty e2)
                  in
                    (case opnd2
                      of I.Immed _ => genBinary(ty, opcode, opnd1, opnd2)
                       | _ =>
                         if equalDst(opnd2) then
                         let val tmpR = newReg()
                             val tmp  = I.Direct (ty,tmpR)
                         in
                           move(ty, opnd1, tmp);
                           move(ty, opnd2, rcx ty);
                           mark(I.BINARY{binOp=opcode ty, src=rcx ty,
                                   dst=tmp},an);
                           move(ty, tmp, dstOpnd)
                         end
                         else
                           (move(ty, opnd1, dstOpnd);
                            move(ty, opnd2, rcx ty);
                            mark(I.BINARY{binOp=opcode ty, src=rcx ty,
                                 dst=dstOpnd},an))
		    (* end case *))
                  end

	      (* division with rounding towards negative infinity *)
	      fun divinf0 (ty, e1, e2) = let
		  val o1 = operand ty e1
		  val o2 = operand ty e2
		  val l = Label.anon ()
	      in
		  move (ty, o1, rax ty);
		  emit (signExtend ty);
		  mark (I.MULTDIV { multDivOp = O.idivOp ty,
		                     src = regOrMem (ty, o2) },
			an);
(* NOTE: on the x86-64, the IDIV instruction traps on overflow
		  if overflow then trap() else ();
*)
		  app emit [(O.cmpOp ty) { lsrc = rdx ty, rsrc = I.Immed 0 },
			    I.JCC { cond = I.EQ, opnd = immedLabel l },
			    I.BINARY { binOp = O.xorOp ty,
				       src = regOrMem (ty, o2),
				       dst = rdx ty },
			    I.JCC { cond = I.GE, opnd = immedLabel l },
			    I.UNARY { unOp = O.decOp ty, opnd = rax ty }];
		  defineLabel l;
		  move (ty, rax ty, dstOpnd)
	      end

	      (* Division by a power of two when rounding to neginf is the
	       * same as an arithmetic right shift. *)
	      fun divinf (ty, e1, e2 as T.LI n') = (case analyze n'
		     of NONE => divinf0 (ty, e1, e2)
		      | SOME(_, false, _, p) =>
		          shift (ty, O.sarOp, T.REG (ty, getExpr e1), p)
		      | SOME(_, true, _, p) => let
			  val reg = getExpr e1
			  in
			    emit(I.UNARY { unOp = O.negOp ty, opnd = I.Direct (ty,reg) });
			    shift (ty, O.sarOp, T.REG (ty, reg), p)
			  end
		    (* eed case *))
		| divinf (ty, e1, e2) = divinf0 (ty, e1, e2)

	      fun reminf0 (ty, e1, e2) = let
		  val o1 = operand ty e1
		  val o2 = operand ty e2
		  val l = Label.anon ()
	      in
		  move (ty, o1, rax ty);
		  emit (signExtend ty);
		  mark (I.MULTDIV { multDivOp = O.idiv1Op ty,
		                    src = regOrMem (ty, o2) },
			an);
		  app emit [(O.cmpOp ty) { lsrc = rdx ty, rsrc = I.Immed 0 },
			    I.JCC { cond = I.EQ, opnd = immedLabel l }];
		  move (ty, rdx ty, rax ty);
		  app emit [I.BINARY { binOp = O.xorOp ty,
				       src = regOrMem (ty, o2), dst = rax ty },
			    I.JCC { cond = I.GE, opnd = immedLabel l },
			    I.BINARY { binOp = O.addOp ty,
				       src = regOrMem (ty, o2), dst = rdx ty }];
		  defineLabel l;
		  move (ty, rdx ty, dstOpnd)
	      end

	      (* n mod (power-of-2) corrrsponds to a bitmask (AND).
	       * If the power is negative, then we must first negate
	       * the argument and then again negate the result. *)
	      fun reminf (ty, e1, e2 as T.LI n') = (case analyze n'
		     of NONE => reminf0 (ty, e1, e2)
		      | SOME(_, false, a, _) =>
		          binaryComm (ty, O.andOp, e1, T.LI (T.I.fromInt32 (ty, a - 1)))
		      | SOME(_, true, a, _) => let
			  val r1 = getExpr e1
			  val o1 = I.Direct (ty,r1)
			  in
			    emit (I.UNARY { unOp = O.negOp ty, opnd = o1 });
			    emit (I.BINARY { binOp = O.andOp ty,
					     src = I.Immed (a - 1),
					     dst = o1 });
			    unary (ty, O.negOp, T.REG (ty, r1))
			  end
		    (* end case *))
		| reminf (ty, e1, e2) = reminf0 (ty, e1, e2)

              (* Division or remainder: divisor must be in %rdx:%rax pair *)
              fun divrem (ty, signed, e1, e2, resultReg) = let
		    val (opnd1, opnd2) = (operand ty e1, operand ty e2)
		    val _ = move(ty, opnd1, rax ty)
		    val oper = if signed
			  then (emit(signExtend ty); O.idiv1Op ty)
			  else (zero (ty, rdx ty); O.div1Op ty)
		    in
		      mark(I.MULTDIV{multDivOp=oper, src=regOrMem (ty, opnd2)},an);
                      move(ty, resultReg, dstOpnd)
(* NOTE: on the x86-64, the IDIV instruction traps on overflow
                      if overflow then trap() else ()
*)
		    end

              (* Optimize the special case for division *)
              fun divide (ty, signed, e1, e2 as T.LI n') = (case analyze n'
		     of SOME(n, isneg, a, p) =>
		       if signed then
			   let val label = Label.anon ()
			       val reg1 = getExpr e1
			       val opnd1 = I.Direct (ty,reg1)
			   in
			       if isneg then
				   emit (I.UNARY { unOp = O.negOp ty,
						   opnd = opnd1 })
			       else if setZeroBit e1 then ()
			       else emit (O.cmpOp ty { lsrc = opnd1,
						   rsrc = I.Immed 0 });
			       emit (I.JCC { cond = I.GE,
					     opnd = immedLabel label });
			       emit (if a = 2 then
					 I.UNARY { unOp = O.incOp ty,
						   opnd = opnd1 }
				     else
					 I.BINARY { binOp = O.addOp ty,
						    src = I.Immed (a - 1),
						    dst = opnd1 });
			       defineLabel label;
			       shift (ty, O.sarOp, T.REG (ty, reg1), p)
			   end
		       else shift (ty, O.shrOp, e1, p)
		     | NONE =>
		       divrem(ty, signed, e1, e2, rax ty))
		| divide (ty, signed, e1, e2) =
		  divrem (ty, signed, e1, e2, rax ty)

	      (* rem never causes overflow *)
              fun rem (ty, signed, e1, e2 as T.LI n') = (case analyze n'
		     of SOME(n, isneg, a, _) =>
		       if signed then
			   (* The following logic should work uniformely
			    * for both isneg and not isneg.  It only uses
			    * the absolute value (a) of the divisor.
			    * Here is the formula:
			    *    let p be a power of two and a = abs(p):
			    *
			    *    x % p = x - ((x < 0 ? x + a - 1 : x) & (-a))
			    *
			    * (That's what GCC seems to do.)
			    *)
			   let val r1 = getExpr e1
			       val o1 = I.Direct (ty,r1)
			       val rt = newReg ()
			       val tmp = I.Direct (ty,rt)
			       val l = Label.anon ()
			   in
			       move (ty, o1, tmp);
			       if setZeroBit e1 then ()
			       else emit ((O.cmpOp ty) { lsrc = o1,
						   rsrc = I.Immed 0 });
			       emit (I.JCC { cond = I.GE,
					     opnd = immedLabel l });
			       emit (I.BINARY { binOp = O.addOp ty,
						src = I.Immed (a - 1),
						dst = tmp });
			       defineLabel l;
			       emit (I.BINARY { binOp = O.andOp ty,
						src = I.Immed (~a),
						dst = tmp });
			       binary (ty, O.subOp, T.REG (ty, r1), T.REG (ty, rt))
			   end
		       else
			   if isneg then
			       (* this is really strange... *)
			       divrem (ty, false, e1, e2, rdx ty)
			   else
(* 64BIT: FIXME what if n-1 does not fit in 32 bits? *)
			       binaryComm (ty, O.andOp, e1,
					   T.LI (T.I.fromInt32 (ty, n - 1)))
		     | NONE => divrem (ty, signed, e1, e2, rdx ty))
		| rem(ty, signed, e1, e2) =
                    divrem(ty, signed, e1, e2, rdx ty)


	      (* unsigned integer multiplication *)
              fun uMultiply0 (ty, e1, e2) =
                  (* note e2 can never be (I.Direct rdx) *)
                  (move(ty, operand ty e1, rax ty);
                   mark(I.MULTDIV{multDivOp=O.mul1Op ty,
                                  src=regOrMem(ty, operand ty e2)},an);
                   move(ty, rax ty, dstOpnd)
                  )

	      fun uMultiply (ty, e1, e2 as T.LI n') = (case analyze n'
		     of SOME(_, false, _, p) => shift (ty, O.shlOp, e1, p)
		      | NONE => uMultiply0 (ty, e1, e2))
		| uMultiply (ty, e1 as T.LI _, e2) = uMultiply (ty, e2, e1)
		| uMultiply (ty, e1, e2) = uMultiply0 (ty, e1, e2)

              (* signed integer multiplication:
               * The only forms that are allowed that also sets the
               * OF and CF flags are:
               *
               *          (dst)  (src1)  (src2)
               *      imul r64, r64/m64, imm8
               *          (dst)  (src)
               *      imul r64, imm8
               *      imul r64, imm32
               *      imul r64, r32/m64
               * Note: destination must be a register!
               *)
              fun multiply (ty, e1, e2) =
              dstMustBeReg(fn (dst, dstOpnd) =>
              let fun doit(i1 as I.Immed _, i2 as I.Immed _) =
                      (move(ty, i1, dstOpnd);
                       mark(I.BINARY{binOp=O.imulOp ty, dst=dstOpnd, src=i2},an))
                    | doit(rm, i2 as I.Immed _) = doit(i2, rm)
                    | doit(imm as I.Immed i, rm) =
                      (case ty
			of 32 => mark(I.MUL3{dst=dst, src1=rm, src2=i},an)
			 | 64 => mark(I.MULQ3{dst=dst, src1=rm, src2=i},an)
		      (* esac *))
                    | doit(r1 as I.Direct _, r2 as I.Direct _) =
                      (move(ty, r1, dstOpnd);
                       mark(I.BINARY{binOp=O.imulOp ty, dst=dstOpnd, src=r2},an))
                    | doit(r1 as I.Direct _, rm) =
                      (move(ty, r1, dstOpnd);
                       mark(I.BINARY{binOp=O.imulOp ty, dst=dstOpnd, src=rm},an))
                    | doit(rm, r as I.Direct _) = doit(r, rm)
                    | doit(rm1, rm2) =
                       if equalDst rm2 then
                       let val tmpR = newReg()
                           val tmp  = I.Direct (ty,tmpR)
                       in move(ty, rm1, tmp);
                          mark(I.BINARY{binOp=O.imulOp ty, dst=tmp, src=rm2},an);
                          move(ty, tmp, dstOpnd)
                       end
                       else
                         (move(ty, rm1, dstOpnd);
                          mark(I.BINARY{binOp=O.imulOp ty, dst=dstOpnd, src=rm2},an)
                         )
                  val (opnd1, opnd2) = (operand ty e1, operand ty e2)
              in  doit(opnd1, opnd2)
              end
              )

	      fun multiply_notrap (ty, e1, e2 as T.LI n') = (case analyze n'
		     of SOME(_, isneg, _, p) => let
			  val r1 = getExpr e1
			  val o1 = I.Direct (ty,r1)
			  in
			    if isneg
			      then emit (I.UNARY { unOp = O.negOp ty, opnd = o1 })
			      else ();
			    shift (ty, O.shlOp, T.REG (ty, r1), p)
			  end
		      | NONE => multiply (ty, e1, e2))
		| multiply_notrap (ty, e1 as T.LI _, e2) = multiply_notrap (ty, e2, e1)
		| multiply_notrap (ty, e1, e2) = multiply (ty, e1, e2)

	      (* NOTE: cmovcc encodes its operand lengths implicitly in the operand names: i.e.,
	       * cmove %rax, %rbx
	       * *)
	      fun cmovcc (tyCond, tyCmp, cc, t1, t2, y, n) = let
		  fun gen (dstR, _) = let
		      val _ = expr' (tyCond, n, dstR, [])  (* false branch *)
		      val src = regOrMem (tyCond, operand tyCond y)  (* yes branch (note the ordering ) *)
		      val cc = cmp (true, tyCmp, cc, t1, t2, []) (* compare *)
		      in
		        mark (I.CMOV {cond=cond cc, src=src, dst=dstR}, an)
		      end
	          in
		    dstMustBeReg gen
	          end

	      (* conditional move for float comparisons *)
              fun fcmovcc (tyCond, fty, cc, t1, t2, y, n) = dstMustBeReg (fn (dstR, _) => let
 		  val _ = expr' (tyCond, n, dstR, [])                 (* false branch *)
                  val src = regOrMem (tyCond, operand tyCond y)       (* true branch *)
                  fun j cc = mark (I.CMOV {cond=cc, src=src, dst=dstR}, an)
                  in
		      fbranch' (fty, cc, t1, t2, j)
		  end)

	    in
	      (case e
		of T.REG (ty, r) => move' (ty, gpr (ty, r), dstOpnd, an)
		 | T.LI z => if (fitsIn32Bits z)
		     then move' (ty, I.Immed(toInt32 z), dstOpnd, an)
		     else move64' (I.Immed64(toInt64 z), dstOpnd, an)
		 | (T.CONST _ | T.LABEL _) =>
                   move' (ty, I.ImmedLabel e, dstOpnd, an)
		 | T.LABEXP le => move' (ty, I.ImmedLabel le, dstOpnd, an)
		 (* arithmetic operations *)
		 | T.ADD(ty, e1, e2 as T.LI n) => (case n
		      of 1  => unary(ty, O.incOp, e1)
		       | ~1 => unary(ty, O.decOp, e1)
		       | _ => addition (ty, e1, e2)
		     (* end case *))
		 | T.ADD(ty, e1 as T.LI n, e2) => (case n
		      of  1 => unary(ty, O.incOp, e2)
		       | ~1 => unary(ty, O.decOp, e2)
		       | _ => addition (ty, e1, e2)
		     (* end case *))
		 | T.ADD(ty, e1, e2) => addition (ty, e1, e2)
		 | T.SUB(ty, e1, e2 as T.LI n) => (case n
		      of 0 => expr' (ty, e1, dst, an)
		       | 1 => unary(ty, O.decOp, e1)
		       | ~1 => unary(ty, O.incOp, e1)
		       | _ => binary(ty, O.subOp, e1, e2)
		     (* end case *))
		 | T.SUB(ty, e1 as T.LI n, e2) => if n = 0
		      then unary(ty, O.negOp, e2)
		      else binary(ty, O.subOp, e1, e2)
		 | T.SUB(ty, e1, e2) => binary(ty, O.subOp, e1, e2)
		 (* unsigned *)
		 | T.MULU(ty, x, y) => uMultiply(ty, x, y)
		 | T.DIVU(ty, x, y) => divide(ty, false, x, y)
		 | T.REMU(ty, x, y) => rem(ty, false, x, y)
		 (* signed *)
		 | T.MULS(ty, x, y) => multiply_notrap (ty, x, y)
		 | T.DIVS(T.DIV_TO_ZERO, ty, x, y) => divide(ty, true, x, y)
		 | T.DIVS(T.DIV_TO_NEGINF, ty, x, y) => divinf (ty, x, y)
		 | T.REMS(T.DIV_TO_ZERO, ty, x, y) => rem(ty, true, x, y)
		 | T.REMS(T.DIV_TO_NEGINF, ty, x, y) => reminf (ty, x, y)
		 (* trapping *)
		 | T.ADDT(ty, x, y) => (binaryComm(ty, O.addOp, x, y); trap())
		 | T.SUBT(ty, T.LI 0, y) => (unary(ty, O.negOp, y); trap())
		 | T.SUBT(ty, x, y) => (binary(ty, O.subOp, x, y); trap())
		 | T.MULT(ty, x, y) => (multiply (ty, x, y); trap ())
		 | T.DIVT(T.DIV_TO_ZERO, ty, x, y) => divide(ty, true, x, y)
		 | T.DIVT(T.DIV_TO_NEGINF, ty, x, y) => divinf (ty, x, y)
		 (* bitwise operations *)
		 | T.ANDB(ty, x, y) => binaryComm(ty, O.andOp, x, y)
		 | T.ORB(ty, x, y)  => binaryComm(ty, O.orOp, x, y)
		 | T.XORB(ty, x, y) => binaryComm(ty, O.xorOp, x, y)
		 | T.NOTB(ty, x)    => unary(ty, O.notOp, x)
		 | T.SRA(ty, x, y)  => shift(ty, O.sarOp, x, y)
		 | T.SRL(ty, x, y)  => shift(ty, O.shrOp, x, y)
		 | T.SLL(ty, x, y)  => shift(ty, O.shlOp, x, y)
		 (* loads *)
 		 | T.LOAD (8, ea, mem) => genLoad (O.loadZXOp (8, 64), ea, mem)
		 | T.LOAD (16, ea, mem) => genLoad (O.loadZXOp (16, 64), ea, mem)
		 | T.LOAD (32, ea, mem) => genLoad (I.MOVL, ea, mem)
		 | T.LOAD (64, ea, mem) => genLoad (I.MOVQ, ea, mem)
		 (* sign-extended loads *)
		 | T.SX (tTy, fTy, x) =>
		   mark (I.MOVE {mvOp=O.loadSXOp (fTy, tTy), src=regOrMem(fTy, operand fTy x), dst=I.Direct(tTy, dst)},an)
		 (* there is no movslq instruction, but movl suffices. *)
		 | T.ZX(64, 32, e) =>
		   mark (I.MOVE {mvOp=I.MOVL, src=operand 32 e, dst=I.Direct(32, dst)},an)
		 (* zero-extended loads *)
		 | T.ZX(tTy, fTy, e) =>
		   mark (I.MOVE {mvOp=O.loadZXOp (fTy, tTy), src=regOrMem(fTy, operand fTy e), dst=I.Direct(tTy, dst)},an)
		 | T.CVTF2I (ty, roundingMd, fty, fExp) => let
		  (* FIXME: handle the rounding mode *)
                   val mvOp = (case (fty, ty)
                       of (64, 32) => I.CVTSD2SI
			| (32, 32) => I.CVTSS2SI
			| (64, 64) => I.CVTSD2SIQ
			| (32, 64) => I.CVTSS2SIQ
                       (* end case *))
                   in
		       mark' (I.move {mvOp=mvOp, src=foperand(fty, fExp), dst=dstOpnd}, an)
                   end
		 | T.COND (tyCond, T.CMP (tyCmp, cc, t1, t2), y, n) =>
		   cmovcc (tyCond, tyCmp, cc, t1, t2, y, n)
		 | T.COND (tyCond, T.FCMP (fty, cc, t1, t2), y, n) =>
		   fcmovcc (tyCond, fty, cc, t1, t2, y, n)
		 | T.NEG (ty, x) => unary (ty, O.negOp, x)
		 | T.LET (s, e) => (stmt s; expr (e, dst, an))
		 | T.MARK (e, A.MARKREG f) => (f dst; expr' (ty, e, dst, an))
		 | T.MARK (e, a) => expr' (ty, e, dst, a :: an)
		 | T.PRED (e, c) => expr' (ty, e, dst, A.CTRLUSE c :: an)
		 | _ => raise Fail("Unsupported rexp: " ^ MLTreeUtils.rexpToString e)
	      (* end case *))
	    end (* expr' *)

	and expr (e, dst, an) = expr' (TRS.size e, e, dst, an)

	and fcopy (fty, ds, rs, an) = let
	    fun mvInstr {dst, src} =
	    	[I.fmove {fmvOp=O.fmovOp fty, dst=dst, src=src}]
	    (* eliminate unnecessary copies *)
	    val (ds, rs) = ListPair.unzip (List.filter (not o CB.sameColor) (ListPair.zip (ds, rs)))
	    val stms = Shuffle.shuffle {mvInstr=mvInstr, ea=fpr} {dst=ds, src=rs, tmp=NONE}
	    in
	      emits stms
	    end

	(* put a floating-point expression into a register *)
	and fexpToReg (fty, e) = (case e
	    of T.FREG (fty', r) => r
	     | e => let
	       val r = newFreg ()
	       in
	         fexpr (fty, r, e, []);
	         r
	       end
	    (* end case *))

	(* put a floating-point expression into an operand *)
	and foperand (fty, e) = (case e
	    of T.FLOAD (fty, ea, mem) => address (ea, mem)
	     | T.FREG (fty, r) => I.FDirect r
	     | e => I.FDirect (fexpToReg (fty, e))
	    (* end case *))

	and falignedOperand (fty, e) = if floats16ByteAligned
            then foperand (fty, e)
            else I.FDirect (fexpToReg (fty, e))

       (* SSE binary ops
	*         (src)      (dst)
	* binOp   freg/m64, freg
	*)
	and fbinop (fty, binOp, a, b, d, an) = let
           (* try to move memory operands to src rather than dst if binOp is commutative *)
	    val (a, b) = (case (binOp, a, b)
                of ( (I.ADDSS | I.ADDSD | I.MULSS | I.MULSD | I.XORPS | I.XORPD | I.ANDPS | I.ANDPD | I.ORPS | I.ORPD),
		     T.FLOAD _,
		     T.FREG _) => (b, a)
		 | _ => (a, b)
   	       (* end case *))
	    val src = foperand (fty, b)
	    in
	      fexpr (fty, d, a, []);
	      mark (I.FBINOP {binOp=binOp, dst=d, src=src}, an)
	    end

	and fsqrt (fty, d, a, an) = let
	    val s = falignedOperand (fty, a)
	    (* TODO: allow the source operand to be a memory location
	     * val aOpnd = foperand (fty, a)
	     *)
	    val oper = (case fty
	          of 32 => I.FSQRTS
	           | 64 => I.FSQRTD
	           | _ => error "fsqrt"
	          (* end case *))
	    in
	      mark (oper {src=s, dst=I.FDirect d}, an)
	    end

	and convertf2f (fromTy, toTy, e, d, an) = let
	    val fmvOp = (case (fromTy, toTy)
	    of (32, 64) => I.CVTSS2SD
	     | (64, 32) => I.CVTSD2SS
	     | _ => error "convertf2f"
	    (* end case *))
	    in
	      mark (I.FMOVE {fmvOp=fmvOp, dst=I.FDirect d, src=foperand (fromTy, e)}, an)
	    end (* convertf2f *)

	and converti2f (fty, ity, e, d, an) = let
	    val fmvOp = (case (ity, fty)
	                  of (32, 32) => I.CVTSI2SS
	                   | (32, 64) => I.CVTSI2SD
	                   | (64, 32) => I.CVTSI2SSQ
	                   | (64, 64) => I.CVTSI2SDQ
	                 (* end case *))
	    val src = regOrMem (ity, operand ity e)
	    in
	      mark (I.FMOVE {fmvOp=fmvOp, dst=I.FDirect d, src=src}, an)
	    end (* converti2f *)

	and fexpr (fty, d, e, an) = ( (*floatingPointUsed := true;*)
	     case e
	      of T.FREG (_, r) => fcopy (fty, [d], [r], an)
	       (* binary operators *)
	       | T.FADD (_, a, b) => fbinop (fty, O.faddOp fty, a, b, d, an)
	       | T.FSUB (_, a, b) => fbinop (fty, O.fsubOp fty, a, b, d, an)
	       | T.FMUL (_, a, b) => fbinop (fty, O.fmulOp fty, a, b, d, an)
	       | T.FDIV (_, a, b) => fbinop (fty, O.fdivOp fty, a, b, d, an)
	       (* unary operators *)
	       | T.FNEG (_, a) => let
		 val fop = (case fty
		     of 32 => I.XORPS
		      | 64 => I.XORPD
 	             (* end case *))
		 val r = newFreg ()
		 val s = falignedOperand (fty, a)
		 in
(* Two old versions of this code: pre-r3081 and r3081+
		     fload (fty, T.LABEL l, I.Region.memory, r, an);
		     fload (fty, T.ADD (64, T.REG (64, C.rsp), T.LI 208), I.Region.memory, r, an);
*)
                     fload (fty, signBit fty, I.Region.memory, r, an);
		     mark (I.FBINOP {binOp=fop, dst=r, src=s}, an);
		     fcopy (fty, [d], [r], an)
		 end
	       | T.FABS (_, a) => let
		 val fop = (case fty
		     of 32 => I.ANDPS
		      | 64 => I.ANDPD
 	             (* end case *))
		 val r = newFreg ()
		 val s = falignedOperand (fty, a)
		 in
(* Two old versions of this code: pre-r3081 and r3081+
		     fload (fty, T.LABEL l, I.Region.memory, r, an);
		     fload (fty, T.ADD (64, T.REG (64, C.rsp), T.LI 216), I.Region.memory, r, an);
*)
                     fload (fty, negateSignBit fty, I.Region.memory, r, an);
		     mark (I.FBINOP {binOp=fop, dst=r, src=s}, an);
		     fcopy (fty, [d], [r], an)
		 end
	       | T.FSQRT (fty, a) => fsqrt (fty, d, a, an)
	       (* conversions *)
	       | T.CVTF2F (fTy, tTy, e) => convertf2f (fTy, tTy, e, d, an)
	       | T.CVTI2F (fty, ty, e) => converti2f (fty, ty, e, d, an)
	       (* load *)
	       | T.FLOAD (fty, ea, mem) => fload (fty, ea, mem, d, an)
	       (* misc *)
	       | T.FMARK (e, A.MARKREG f) => (f d; fexpr (fty, d, e, an))
	       | T.FMARK (e, a) => fexpr (fty, d, e, a::an)
	       | T.FPRED (e, c) => fexpr (fty, d, e, A.CTRLUSE c::an)
	       | T.FEXT fexp => ExtensionComp.compileFext (reducer()) {e=fexp, fd=d, an=an}
	       | _ => error "fexpr"
	      (* end case *))

	and fload (fty, ea, mem, d, an) = mark (
	    I.FMOVE {fmvOp=O.fmovOp fty, dst=I.FDirect d,
	             src=address (ea, mem)}, an)

	and fstore (fty, ea, mem, e, an) = mark (
	    I.FMOVE {fmvOp=O.fmovOp fty, dst=address (ea, mem),
	             src=I.FDirect (fexpToReg (fty, e))}, an)

	and call (ea, flow, def, use, mem, cutsTo, an, pops) = let
	    fun return(set, []) = set
	      | return(set, a::an) = (case #peek A.RETURN_ARG a
	        of SOME r => return(CB.CellSet.add(r, set), an)
		 | NONE => return(set, an)
		(* end case *))
	    val dst = (case ea
		   of T.LABEL lab => I.ImmedLabel ea
		    | T.LABEXP le => I.ImmedLabel le
		    | _ => operand 64 ea
		  (* end case *))
	    in
	      mark(I.CALL{
		  opnd=dst, defs=cellset def, uses=cellset use,
		  return=return (C.empty, an), cutsTo=cutsTo, mem=mem,
		  pops=pops
		}, an)
	    end (* call *)

	and jmp (lexp as T.LABEL lab, labs, an) =
            mark (I.JMP (I.ImmedLabel lexp, [lab]), an)
	  | jmp (T.LABEXP le, labs, an) = mark (I.JMP (I.ImmedLabel le, labs), an)
          | jmp (ea, labs, an) = mark (I.JMP (operand 64 ea, labs), an)

      (* 8-bit store operations must use one of %al, %bl, %cl, or %dl as the source;
       * we chose %al (aka %rax).
       *)
	and doStore 8 (ea, d, mem, an) = let
	      val src = (case immedOrReg (8, operand 8 d)
		     of src as I.Direct(_, r) => if CB.sameColor(r, C.rax)
			  then src
			  else (move(64, src, rax 64); rax 8)
		      | src => src
		    (* end case *))
	      in
		move' (8, src, address (ea, mem), an)
	      end
	  | doStore ty (ea, d, mem, an) =
	      move' (ty, immedOrReg (ty, operand ty d), address (ea, mem), an)

	and binaryMem(ty, binOp, src, dst, mem, an) =
	    mark(I.BINARY{binOp=binOp, src=immedOrReg(ty, operand ty src),
				dst=address (dst,mem)}, an)
	and unaryMem(ty, unOp, opnd, mem, an) =
	    mark(I.UNARY{unOp=unOp, opnd=address (opnd,mem)}, an)
        and isOne (T.LI 1) = true
          | isOne _ = false

	and store (ty, ea, d, mem, opcodes : O.opcodes, doStore, an) = let
	      val {INC,DEC,ADD,SUB,NOT,NEG,SHL,SHR,SAR,OR,AND,XOR, ...} = opcodes
	      fun default () = doStore (ea, d, mem, an)
	      fun binary1 (t, t', unary, binary, ea', x) =
		  if t = ty andalso t' = ty then
		     if MLTreeUtils.eqRexp(ea, ea') then
			if isOne x then unaryMem(ty, unary, ea, mem, an)
			else binaryMem(ty, binary, x, ea, mem, an)
		      else default()
		  else default()
              fun unary(t,unOp, ea') =
                  if t = ty andalso MLTreeUtils.eqRexp(ea, ea') then
                     unaryMem(ty, unOp, ea, mem, an)
                  else default()
              fun binary(t,t',binOp,ea',x) =
                  if t = ty andalso t' = ty andalso
                     MLTreeUtils.eqRexp(ea, ea') then
                      binaryMem(ty, binOp, x, ea, mem, an)
                  else default()
              fun binaryCom1(t,unOp,binOp,x,y) =
             	  if t = ty then let
             	     fun again() = (case y
             	         of T.LOAD(ty',ea',_) =>
                            if ty' = ty andalso MLTreeUtils.eqRexp(ea, ea') then
                               if isOne x then unaryMem(ty, unOp, ea, mem, an)
                               else binaryMem(ty, binOp,x,ea,mem,an)
                            else default()
                          | _ => default()
                         (* end case *))
                     in
                       (case x
                         of T.LOAD(ty',ea',_) => if ty' = ty
                             andalso MLTreeUtils.eqRexp(ea, ea') then
                                if isOne y then unaryMem(ty, unOp, ea, mem, an)
                                else binaryMem(ty, binOp,y,ea,mem,an)
                             else again()
                          | _ => again()
                        (* end case *))
                     end
                   else default()
              fun binaryCom(t,binOp,x,y) = if t = ty then
                  let fun again() = (case y
                      of T.LOAD(ty',ea',_) =>
                         if ty' = ty andalso MLTreeUtils.eqRexp(ea, ea') then
                           binaryMem(ty, binOp,x,ea,mem,an)
                          else default()
                        | _ => default()
                      (* end case *))
                  in
                    (case x
                      of T.LOAD(ty',ea',_) =>
                         if ty' = ty andalso MLTreeUtils.eqRexp(ea, ea') then
     	                    binaryMem(ty, binOp,y,ea,mem,an)
                         else again()
                       | _ => again()
                    (* end case *))
                  end
                  else default()
	      in
		case d
		 of T.ADD(t,x,y) => binaryCom1(t,INC,ADD,x,y)
		  | T.SUB(t,T.LOAD(t',ea',_),x) => binary1(t,t',DEC,SUB,ea',x)
		  | T.ORB(t,x,y) => binaryCom(t,OR,x,y)
		  | T.ANDB(t,x,y) => binaryCom(t,AND,x,y)
		  | T.XORB(t,x,y) => binaryCom(t,XOR,x,y)
		  | T.SLL(t,T.LOAD(t',ea',_),x) => binary(t,t',SHL,ea',x)
		  | T.SRL(t,T.LOAD(t',ea',_),x) => binary(t,t',SHR,ea',x)
		  | T.SRA(t,T.LOAD(t',ea',_),x) => binary(t,t',SAR,ea',x)
		  | T.NEG(t,T.LOAD(t',ea',_)) => unary(t,NEG,ea')
		  | T.NOTB (t, T.LOAD(t',ea',_)) => unary(t,NOT,ea')
		  | _ => default ()
		(* end case *)
	      end (* store *)

	(* floating-point branch for
	 *   if (t1 fcc t2)
	 *      goto lab
	 *)
	and fbranch (fty, fcc, t1, t2, lab, an) = let
	      fun j cc = mark (I.JCC {cond=cc, opnd=immedLabel lab}, an)
	      in
		fbranch' (fty, fcc, t1, t2, j)
	      end

	and fbranch' (fty, fcc, x, y, j) = let
	    (* the condition code bits are set as follows:
	     *
	     *			P  Z  C
	     *   x < y		0  0  1
	     *   x > y		0  0  0
	     *   x = y		0  1  0
	     *   unordered	1  1  1
 	     *
	     * and the AMD64 condition tests are as follows:
	     *
	     *   A		-  0  0		-- implies ordered
	     *   AE		-  -  0
	     *   B		-  -  1
	     *   BE		-  1  -
	     *              or  -  -  1
	     *   EQ		-  1  -
	     *   NE		-  0  -
	     *   NP		0  -  -
	     *   P		1  -  -
	     *)
            fun branch fcc = (case fcc
		  of T.==   => orderedOnly I.EQ
		   | T.?<>  => (j I.P; j I.NE)
		   | T.?    => j I.P
		   | T.<=>  => j I.NP
		   | T.>    => j I.A
		   | T.?<=  => (j I.P; j I.BE)
		   | T.>=   => orderedOnly I.AE
		   | T.?<   => (j I.P; j I.BE)
		   | T.<    => orderedOnly I.B
		   | T.?>=  => (j I.P; j I.AE)
		   | T.<=   => orderedOnly I.BE
		   | T.?>   => (j I.P; j I.A)
		   | T.<>   => orderedOnly I.NE
		   | T.?=   => j I.EQ
		   | _      => error(concat["fbranch(", T.Basis.fcondToString fcc, ")"])
		 (* end case *))
	  (* branch on condition if ordered *)
(* FIXME: the emitted jump does not have annotations! *)
	    and orderedOnly fcc = let
		  val lab = Label.anon()
		  in
		    emit (I.JCC{cond=I.P, opnd=immedLabel lab});
		    j fcc;
                    defineLabel lab
		  end
	    (* compare for condition  (x op y)
	     *
	     *              (y)          (x)
	     * ucomiss/d    xmm1/m32,  xmm2
	     *)
               fun compare () = let
		   val x = foperand (fty, x)
		   val y = foperand (fty, y)
                   fun cmp (x, y, fcc) = (
                       emit (I.FCOM {comOp=O.ucomOp fty, src=y, dst=x});
                       fcc)
                   in
                     (case (x, y)
                       of (I.FDirect xReg, I.FDirect _) => cmp (xReg, y, fcc)
                        | (mem, I.FDirect yReg) =>
                          cmp (yReg, x, T.Basis.swapFcond fcc)
                        | (I.FDirect xReg, mem) => cmp (xReg, y, fcc)
                        | _ => let
                          val xReg = newFreg ()
                          val xTmp = I.FDirect xReg
                          in
                            emit (I.FMOVE {fmvOp=O.fmovOp fty, src=x, dst=xTmp});
                            cmp (xReg, y, fcc)
                          end
                     (* end case *))
                   end (* compare *)
	    in
	      branch (compare ())
	    end (* fbranch' *)

		(* %eflags <- src *)
	and moveToEflags src =
	    if CB.sameColor(src, C.eflags) then ()
	    else (move(32, I.Direct (32,src), rax 32); emit(I.LAHF))

	(* dst <- %eflags *)
	and moveFromEflags dst =
	    if CB.sameColor(dst, C.eflags) then ()
	    else (emit(I.SAHF); move(32, rax 32, I.Direct (32,dst)))

	(* Emit a test.
         *   The available modes are
         *      r/m, r
         *      r/m, imm
         * On selecting the right instruction: TESTQ/TESTL/TESTW/TESTB.
         * When anding an operand with a constant
         * that fits within 8 (or 16) bits, it is possible to use TESTB,
         * (or TESTW) instead of TESTL.   Because amd64 is little endian,
         * this works for memory operands too.  However, with TESTB, it is
         * not possible to use registers other than
         * AL, CL, BL, DL, and AH, CH, BH, DH.  So, the best way is to
         * perform register allocation first, and if the operand registers
         * are one of RAX, RCX, RBX, or RDX, replace the TESTL instruction
         * by TESTB.
         *)
        and test(ty, testopcode, a, b, an) = let
            val (_, opnd1, opnd2) = commuteComparison(ty, T.EQ, true, a, b)
              (* translate r, r/m => r/m, r *)
            val (opnd1, opnd2) =
                  if isMemOpnd opnd2 then (opnd2, opnd1) else (opnd1, opnd2)
            in
              mark (testopcode {lsrc=opnd1, rsrc=opnd2}, an)
            end

        (* generate a real comparison; return the real cc used *)
        and genCmp (ty, swapable, cc, a, b, an) = let
	      val (cc, opnd1, opnd2) = commuteComparison(ty, cc, swapable, a, b)
	      in
		  (case ty
		    of 8 => mark(I.CMPB{lsrc=opnd1, rsrc=opnd2}, an)
		     | 16 => mark(I.CMPW{lsrc=opnd1, rsrc=opnd2}, an)
		     | 32 => mark(I.CMPL{lsrc=opnd1, rsrc=opnd2}, an)
		     | 64 => mark(I.CMPQ{lsrc=opnd1, rsrc=opnd2}, an)
		  (* esac *));
		  cc
	      end

       (* Give a and b which are the operands to a comparison (or test)
	* Return the appropriate condition code and operands.
	*   The available modes are:
	*        r/m, imm
	*        r/m, r
	*        r,   r/m
	*)
	and commuteComparison (ty, cc, swapable, a, b) = let
	      val (opnd1, opnd2) = (operand ty a, operand ty b)
	      in  (* Try to fold in the operands whenever possible *)
	        case (isImmediate opnd1, isImmediate opnd2)
		 of (true, true) => (cc, moveToReg (ty, opnd1), opnd2)
		  | (true, false) => if swapable
		      then (T.Basis.swapCond cc, opnd2, opnd1)
		      else (cc, moveToReg (ty, opnd1), opnd2)
		  | (false, true) => (cc, opnd1, opnd2)
		  | (false, false) => (case (opnd1, opnd2)
		       of (_, I.Direct _) => (cc, opnd1, opnd2)
			| (I.Direct _, _) => (cc, opnd1, opnd2)
			| (_, _)          => (cc, moveToReg (ty, opnd1), opnd2)
		      (* end case *))
		(* end case *)
	      end  (* commuteComparison *)

	(* generate a condition code expression
	 * The zero is for setting the condition code!
	 * I have no idea why this is used.
	 *)
	and doCCexpr(T.CMP(ty, cc, t1, t2), dst, an) = (
	    cmp(false, ty, cc, t1, t2, an);
	    moveFromEflags dst)
 	  | doCCexpr(T.CC(cond,rs), dst, an) =
	    if CB.sameColor(rs,C.eflags) orelse CB.sameColor(dst,C.eflags)
	    then (moveToEflags rs; moveFromEflags dst)
	    else move'(64, I.Direct (64,rs), I.Direct (64,dst), an)
	  | doCCexpr(T.CCMARK(e,A.MARKREG f),dst,an) = (f dst; doCCexpr(e,dst,an))
	  | doCCexpr(T.CCMARK(e,a), dst, an) = doCCexpr(e,dst,a::an)
	  | doCCexpr(T.CCEXT e, cd, an) =
	    ExtensionComp.compileCCext (reducer()) {e=e, ccd=cd, an=an}
	  | doCCexpr _ = error "doCCexpr"

	(* Compare an expression with zero.
	 * On the amd64, TEST is superior to AND for doing the same thing,
	 * since it doesn't need to write out the result in a register.
	 *)
	and cmpWithZero(cc as (T.EQ | T.NE), e as T.ANDB(ty, a, b), an) =
		(case ty
		  of 8 => test(ty, I.TESTB, a, b, an)
		  | 16 => test(ty, I.TESTW, a, b, an)
		  | 32 => test(ty, I.TESTL, a, b, an)
		  | 64 => test(ty, I.TESTQ, a, b, an)
		  | _  => expr (e, newReg(), an);
		cc)
	 | cmpWithZero(cc, e, an) = (expr (e, newReg(), an); cc)

        (* generate a comparison and sets the condition code;
         * return the actual cc used.  If the flag swapable is true,
         * we can also reorder the operands.
         *)
        and cmp (swapable, ty, cc, t1, t2, an) = let
            (* == and <> can be always be reordered *)
            val swapable = swapable orelse cc = T.EQ orelse cc = T.NE
            in (* Sometimes the comparison is not necessary because
                * the bits are already set!
                *)
              if isZero t1 andalso setZeroBit2 t2 then
                 if swapable then
                    cmpWithZero(T.Basis.swapCond cc, t2, an)
                 else (* can't reorder the comparison! *)
                    genCmp(ty, false, cc, t1, t2, an)
	      else if isZero t2 andalso setZeroBit2 t1 then
		 cmpWithZero(cc, t1, an)
	      else genCmp(ty, swapable, cc, t1, t2, an)
            end

	and branch (T.CMP(ty, cc, t1, t2), lab, an) = let
            val cc = cmp (true, ty, cc, t1, t2, [])
            in
              mark (I.JCC{cond=cond cc, opnd=immedLabel lab}, an)
            end
          | branch (T.FCMP(fty, fcc, t1, t2), lab, an) =
            fbranch (fty, fcc, t1, t2, lab, an)
          | branch (ccexp, lab, an) = (
            doCCexpr(ccexp, C.eflags, []);
            mark(I.JCC{cond=cond(Gen.condOf ccexp), opnd=immedLabel lab}, an))

	and stmt' (s, an) = (case s
	    of T.MV (ty, d, e) => expr' (ty, e, d, an)
	     | T.FMV (fty, d, e) => fexpr (fty, d, e, an)
	     | T.CCMV (ccd, e) => doCCexpr (e, ccd, an)
	     | T.COPY (ty, dst, src) => copy (ty, dst, src, an)
	     | T.FCOPY (fty, dst, src) => fcopy (fty, dst, src, an)
	     | T.JMP (e, labs) => jmp (e, labs, an)
	     | T.CALL {funct, targets, defs, uses, region, pops, ...} =>
	       call (funct, targets, defs, uses, region, [], an, pops)
	     | T.FLOW_TO(T.CALL{funct, targets, defs, uses, region, pops, ...},
                         cutTo) =>
               call (funct, targets, defs, uses, region, cutTo, an, pops)
             | T.RET _ => mark (I.RET NONE, an)
             | T.STORE (ty, ea, d, mem) =>
               store (ty, ea, d, mem, O.opcodes ty, doStore ty, an)
             | T.FSTORE (fty, ea, e, mem) => fstore (fty, ea, mem, e, an)
             | T.BCC(cc, lab) => branch (cc, lab, an)
             | T.DEFINE l => defineLabel l
             | T.LIVE s => mark' (I.LIVE{regs=cellset s,spilled=C.empty},an)
             | T.KILL s => mark' (I.KILL{regs=cellset s,spilled=C.empty},an)
             | T.ANNOTATION(s, a) => stmt' (s, a::an)
             | T.EXT s =>
               ExtensionComp.compileSext (reducer ()) {stm=s, an=an}
             | s => app stmt (Gen.compileStm s)
	    (* end case *))
	and stmt s = stmt' (s, [])

	and beginCluster' _ = (
	    trapLabel := NONE;
	    beginCluster 0)

	and endCluster' a = (
	      case !trapLabel
	       of NONE => ()
		| SOME(_, lab) => (
		    defineLabel lab;
		  (* signal an overflow exception (code 4) *)
		    emit(I.INT 0w4))
              (* end case *);
            (* If floating point has been used allocate an extra
             * register just in case we didn't use any explicit register *)
	      ignore (newFreg ());
	      endCluster a)

	and ccExpr e = error "ccExpr"

	and reduceOpnd (I.Direct(ty, r)) = r
	  | reduceOpnd opnd = let
	      val dst = newReg()
	      in
		move(64, opnd, I.Direct(64, dst)); dst
	      end

	and reducer () = TS.REDUCER{
		reduceRexp    = genExpr,
		reduceFexp    = fn e => fexpToReg (64, e),
		reduceCCexp   = ccExpr,
		reduceStm     = stmt',
		operand       = operand 64,
		reduceOperand = reduceOpnd,
		addressOf     = fn e => address(e, I.Region.memory), (*XXX*)
		emit          = mark',
		instrStream   = instrStream,
		mltreeStream  = self()
	      }

	and self () = TS.S.STREAM {
	    beginCluster=beginCluster',
	    endCluster=endCluster',
	    emit=stmt,
	    pseudoOp=pseudoOp,
	    defineLabel=defineLabel,
	    entryLabel=entryLabel,
   	    comment=comment,
            annotation=annotation,
	    getAnnotations=getAnnotations,
            exitBlock=exitBlock o cellset}
	in
	  self ()
	end (* selectInstructions *)

  end (* AMD64Gen *)
