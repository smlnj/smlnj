(* amd64MC.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Machine code emitter for AMD64 architecture.
 *)

functor AMD64MCEmitter (

    structure Instr : AMD64INSTR
    structure Shuffle : AMD64SHUFFLE where I = Instr
    structure MLTreeEval : MLTREE_EVAL where T = Instr.T
    structure AsmEmitter : INSTRUCTION_EMITTER where I = Instr

  ) : MC_EMIT = struct
    structure I = Instr
    structure C = I.C
    structure W32 = Word32
    structure W8 = Word8
    structure W = LargeWord
    structure CB = CellsBasis

    val println = print o (fn s => s^"\n")
    val i2s = Int.toString
    val print_int = println o i2s

    val itow  = Word.fromInt
    val wtoi  = Word.toInt

    fun error msg = MLRiscErrorMsg.impossible ("AMD64MCEmitter: " ^ msg)
    fun unimplemented instr = MLRiscErrorMsg.impossible (concat[
	    "AMD64MCEmitter: ", instr, " unimplemented"
	  ])

  (*
   * Sanity check!
   *)
    val eax = 0   val esp = 4
    val ecx = 1   val ebp = 5
    val edx = 2   val esi = 6
    val ebx = 3   val edi = 7

    val lockPrefix : Word8.word = 0wxF0

    fun lexp le = Int32.fromInt (MLTreeEval.valueOf le)

    val toWord8 = Word8.fromLargeWord o LargeWord.fromLargeInt o Int32.toLarge
    val eBytes = Word8Vector.fromList
    fun eByte i = eBytes [W8.fromInt i]

    local
      val toLWord = (W.fromLargeInt o Int32.toLarge)
      fun shift (w,cnt) = W8.fromLargeWord(W.>>(w, cnt))
    in
    fun eShort i16 = let
	  val w = toLWord i16
	  in [
	    shift(w, 0w0), shift(w,0w8)
	  ] end
    fun eLong i32 = let
	  val w = toLWord i32
	  in [
	    shift(w, 0w0), shift(w,0w8), shift(w,0w16), shift(w,0w24)
	  ] end
    end (* local *)

    fun eLongLong i64 = let
	  val toLLWord = (Word64.fromLargeInt o Int64.toLarge)
	  val w = toLLWord i64
	  fun shift64 (w,cnt) = let
	      val shifted = Word64.>>(w, cnt)
	      val masked = Word64.andb(0w255, shifted)
	      in Word8.fromInt (Word64.toInt masked) end
	  in [shift64(w, 0w0),
	      shift64(w, 0w8),
	      shift64(w, 0w16),
	      shift64(w, 0w24),
	      shift64(w, 0w32),
	      shift64(w, 0w40),
	      shift64(w, 0w48),
	      shift64(w, 0w56)]
	  end

    fun eLongLongCut i64 = let
	  val toLLWord = (Word64.fromLargeInt o Int64.toLarge)
	  val w = toLLWord i64
	  fun shift64 (w,cnt) = let
		val shifted = Word64.>>(w, cnt)
		val masked = Word64.andb(0w255, shifted)
		in Word8.fromInt (Word64.toInt masked) end
	  in [
	    shift64(w, 0w0), shift64(w, 0w8), shift64(w, 0w16), shift64(w, 0w24)
	  ] end

    fun emitAMD64Instr instr = let
	  val error = fn msg => let
		val AsmEmitter.S.STREAM{emit,...} = AsmEmitter.makeStream []
		in
		  emit (I.INSTR instr); error msg
		end

	  datatype reg_or_opc = REG of int | OPCODE of int
	  fun rMask r = r mod 8
	  fun getRO (REG r) = rMask r
	    | getRO (OPCODE oc) = oc
	  val rNum' = rMask o CB.physicalRegisterNum
	  val rNum = CB.physicalRegisterNum
	  val fNum = CB.physicalRegisterNum
	  val isExtReg = (fn x => x > 7) o rNum
	  fun isExtReg' (REG r) = r > 7
	    | isExtReg' _ = false

	(* sizes of immediate operands *)
	  datatype size = Zero | Bits8 | Bits32
	  fun size i =
	      if i = 0 then Zero
	      else if Int32.<(i, 128) andalso Int32.<=(~128, i) then Bits8
	      else Bits32

	  fun immedOpnd (I.Immed i32) = i32
	    | immedOpnd (I.ImmedLabel le) = lexp le
	    | immedOpnd (I.LabelEA le) = lexp le
	    | immedOpnd _ = error "immedOpnd"

	  nonfix mod

	  fun scale (n, m) = Word.toIntX(Word.<<(Word.fromInt n, Word.fromInt m))
	  fun modrm {mod, reg, rm} = W8.fromInt(scale(mod,6) + scale(reg,3) + rm)
	  fun sib {ss, index, base} = W8.fromInt(scale(ss,6) + scale(index,3) + base)
	  fun eREXRegs (r, x, b) = let
	        val rb1 = if r then 0wx4 else 0wx0
		val rb2 = if x then rb1 + 0wx2 else rb1
		val rb3 = if b then rb2 + 0wx1 else rb2
		in
		  rb3
		end (* rex *)
	  fun eREX rb = 0wx40 + rb
	  fun eREX64 rb = eREX rb + 0wx8	(* sets REX.W *)

	  fun eImmedExt (_, I.Immed _) = error "eImmedExt: Immed"
	    | eImmedExt (_, I.Immed64 _) = error "eImmedExt: Immed64"
	    | eImmedExt (_, I.ImmedLabel _) = error "eImmedExt: ImmedLabel"
	    | eImmedExt (_, I.Relative _) = error "eImmedExt: Relative"
	    | eImmedExt (_, I.LabelEA _) = error "eImmedExt: LabelEA"
            | eImmedExt (r', I.Direct (_, r)) =
	      ( (isExtReg' r', false, isExtReg r),
		[modrm{mod=3, reg=getRO r', rm=rNum' r}] )
            | eImmedExt (r', I.FDirect r) =
	      ( (isExtReg' r', false, isExtReg r),
		[modrm{mod=3, reg=getRO r', rm=rNum' r}] )
	    | eImmedExt (r', I.Displace{base=base', disp, ...}) = let
	        val base = rNum' base'
		val immed = immedOpnd disp
		val rex = (isExtReg' r', false, isExtReg base')
		val r' = getRO r'
		fun displace(mod, eDisp) = if base = esp
		      then modrm{mod=mod, reg=r', rm=4}::
			sib{ss=0, index=4, base=esp}::eDisp immed
		      else modrm{mod=mod, reg=r', rm=base} :: eDisp immed
                in
		  (rex,
		   (case size immed
		     of Zero => if base = esp
			    then [modrm{mod=0, reg=r', rm=4}, sib{ss=0,index=4,base=esp}]
			  else if base = ebp
			    then [modrm{mod=1, reg=r', rm=ebp}, 0w0]
			    else [modrm{mod=0, reg=r', rm=base}]
		      | Bits8 => displace (1, fn i => [toWord8 i])
		      | Bits32 => displace (2, eLong)
		  (*esac*)) )
                end
	    | eImmedExt (r', I.Indexed{base=NONE, index, scale, disp, ...}) = let
                val rex = (isExtReg' r', isExtReg index, false)
		val r' = getRO r'
	        in
		  (rex,
		   (modrm{mod=0, reg=r', rm=4} ::
		    sib{base=5, ss=scale, index=rNum' index} ::
		    eLong (immedOpnd disp)) )
	        end
	    | eImmedExt (r', I.Indexed{base=SOME b, index, scale, disp, ...}) = let
	        val rex = (isExtReg' r', isExtReg index, isExtReg b)
		val r' = getRO r'
		val index = rNum' index
		val base = rNum' b
		val immed = immedOpnd disp
		fun indexed (mod, eDisp) =
		      modrm{mod=mod, reg=r', rm=4} ::
		      sib{ss=scale, index=index, base=base} :: eDisp immed
		in
		  (rex,
		   (case size immed
		     of Zero =>
			if base=ebp then
			    [modrm{mod=1, reg=r', rm=4},
			     sib{ss=scale, index=index, base=5}, 0w0]
			else
			    [modrm{mod=0, reg=r', rm=4},
			     sib{ss=scale, index=index, base=base}]
		      | Bits8 => indexed(1, fn i => [toWord8 i])
		      | Bits32 => indexed(2, eLong)
		   (*esac*)) )
		end

	  fun encode32' (bytes, r', opnd) = let
	        val (rex, e) = eImmedExt (r', opnd)
	        in
		  case eREXRegs rex
		   of 0w0 => bytes @ e
		    | rexByte => (eREX rexByte) :: bytes @ e
		  (* esac *)
	        end (* encode32' *)
	  fun encode64' (bytes, r', opnd) = let
	        val (rex, e) = eImmedExt (r', opnd)
	        in
		  (eREX64 (eREXRegs rex)) :: bytes @ e
	        end (* encode64' *)
	  fun encode' 32 = encode32'
	    | encode' _ = encode64'
	  fun encode32 (byte1, r', opnd) = eBytes (encode32' ([byte1], r', opnd))
	  fun encode64 (byte1, r', opnd) = eBytes (encode64' ([byte1], r', opnd))
	  fun encode sz = if sz = 64 then encode64 else encode32
	  fun encodeReg32 (byte1, r, opnd) = encode32 (byte1, REG(rNum r), opnd)
	  fun encodeReg64 (byte1, r, opnd) = encode64 (byte1, REG(rNum r), opnd)
	  fun encodeReg sz = if sz = 64 then encodeReg64 else encodeReg32
	  fun encodeLongImm32 (byte1, r', opnd, i) =
	        eBytes ((encode32' ([byte1], r', opnd)) @ eLong i)
	  fun encodeLongImm64 (byte1, r', opnd, i) =
	        eBytes ((encode64' ([byte1], r', opnd)) @ eLong i)
	  fun encodeLongImm sz = if sz = 64 then encodeLongImm64 else encodeLongImm32
	  fun encodeShortImm32 (byte1, r', opnd, w) =
	        eBytes ((encode32' ([byte1], r', opnd)) @ eShort w)
	  fun encodeShortImm64 (byte1, r', opnd, w) =
	        eBytes ((encode64' ([byte1], r', opnd)) @ eShort w)
	  fun encodeShortImm sz = if sz = 64 then encodeShortImm64 else encodeShortImm32
	  fun encodeByteImm32 (byte1, r', opnd, b) =
	        eBytes ((encode32' ([byte1], r', opnd)) @ [toWord8 b])
	  fun encodeByteImm64 (byte1, r', opnd, b) =
	        eBytes ((encode64' ([byte1], r', opnd)) @ [toWord8 b])
	  fun encodeByteImm sz = if sz = 64
		then encodeByteImm64
		else encodeByteImm32
	  fun encodeST (byte1, opc, STn) = let
	        fun reg {opc, reg} = W8.fromInt (scale (opc,3) + reg)
	        in
		  eBytes [byte1, reg{opc=opc,reg=fNum STn}]
		end

	  (* arith: only 5 cases need be considered for each size:
	   *  dst,   src	op/en
	   *  --------------------------
	   *  EAX,   imm32	I
	   *  r/m32, imm32	MI
	   *  r/m32, imm8	MI
	   *  r/m32, r32	MR
	   *  r32,   r/m32	RM
	   *)
	  fun arith (sz : int, opc1 : Word8.word, opc2 : reg_or_opc) = let
		fun f (I.ImmedLabel le, dst) = f(I.Immed(lexp le), dst)
		  | f (I.LabelEA le, dst) = f(I.Immed(lexp le), dst)
		  | f (I.Immed i, dst) = (case size i
			 of Bits32 => (case dst
			       of I.Direct (_, r) => if CB.physicalRegisterNum r = eax
				    then if sz = 32
				      then eBytes (W8.fromInt (8 * (getRO opc2) + 5) :: eLong i)
				      else eBytes (eREX64 0w0 :: W8.fromInt(8 * (getRO opc2) + 5)
						  :: eLong i)
				    else encodeLongImm sz (0wx81, opc2, dst, i)
			        | _ => encodeLongImm sz (0wx81, opc2, dst, i)
			      (*esac*))
			  | _ => encodeByteImm sz (0wx83, opc2, dst, i) (* 83 /digit ib *)
			(*esac*))
		  | f(src, I.Direct(_, r)) = encodeReg sz (opc1+0w3, r, src)
		  | f(I.Direct(_, r), dst) = encodeReg sz (opc1+0w1, r, dst)
		  | f _ = error "arith"
		in
		  f
		end (* arith *)

	  fun condCode cond = (case cond
		 of I.EQ => 0w4      | I.NE => 0w5
		  | I.LT => 0w12     | I.LE => 0w14
		  | I.GT => 0w15     | I.GE => 0w13
		  | I.A  => 0w7      | I.AE => 0w3
		  | I.B  => 0w2      | I.BE => 0w6
		  | I.C  => 0w2      | I.NC => 0w3
		  | I.P  => 0wxa     | I.NP => 0wxb
		  | I.O  => 0w0      | I.NO => 0w1
		(*esac*))

	  (* test:  the following cases need be considered:
	   *  lsrc,  rsrc
	   *  -----------
	   *  AL,    imm8  opc1 A8
	   *  EAX,   imm32 opc1 A9
	   *  r/m8,  imm8  opc2 F6/0 ib
	   *        r/m32, imm32 opc2 F7/0 id
	   *        r/m8,  r8    opc3 84/r
	   *        r/m32, r32   opc3 85/r
	   *)
	  fun test (sz, I.ImmedLabel le, lsrc) = test(sz, I.Immed(lexp le), lsrc)
	    | test (sz, I.LabelEA le, lsrc) = test(sz, I.Immed(lexp le), lsrc)
	    | test (sz, I.Immed i, lsrc) = (case (lsrc, i >= 0 andalso i < 255)
		 of (I.Direct (_, r), false) => if CB.physicalRegisterNum r = eax
		      then eBytes(0wxA9 :: eLong i)
		      else encodeLongImm sz (0wxF7, OPCODE 0, lsrc, i)
		  | (_, false)  => encodeLongImm sz (0wxF7, OPCODE 0, lsrc, i)
		  | (I.Direct (_, r), true) => let (* 8 bit *)
		      val r = CB.physicalRegisterNum r
		      in
			if r = eax
			  then eBytes[0wxA8, toWord8 i]
			else if r < 4
			      (* unfortunately, only CL, DL, BL can be encoded *)
			  then encodeByteImm sz (0wxF6, OPCODE 0, lsrc, i)
			else if sz = 8
			  then error "test.8"
			  else encodeLongImm sz (0wxF7, OPCODE 0, lsrc, i)
		      end
		 | (_, true) => encodeByteImm sz (0wxF6, OPCODE 0, lsrc, i)
		(* end case *))
	    | test (8, rsrc as I.Direct (_, r), lsrc) = if rNum r < 4
		then encodeReg32 (0wx84, r, lsrc)
		else error "test.8"
	    | test (sz, I.Direct (_, r), lsrc) = encodeReg sz (0wx85, r, lsrc)
	    | test _ = error "test"

	  fun movsd (byte3, r, opnd) =
	        eBytes (0wxf2::encode32'([0wxf, byte3], REG (rNum r), opnd))

          (* DEBUG print instructions in stdout
          fun makestream s = let
              fun write f slice = let
                  val t = f slice
                  val _ = s := (!s)^t
                  in
                  String.size t
                  end
              val writer =
                  TextPrimIO.WR
                  {
                   name = "stringout",
                   chunkSize = 512,
                   writeVec = SOME (write CharVectorSlice.vector),
                   writeArr = SOME (write CharArraySlice.vector),
                   writeVecNB = NONE,
                   writeArrNB = NONE,
                   block = NONE,
                   canOutput = NONE,
                   getPos = NONE,
                   setPos = NONE,
                   endPos = NONE,
                   verifyPos = NONE,
                   close = (fn () => ()),
                   ioDesc = NONE
                  }
              val stream =
                  TextIO.StreamIO.mkOutstream (writer, IO.NO_BUF)
              in
              TextIO.mkOutstream stream
              end

          fun instrstring () =
              let
              val s = ref ""
              val stream = makestream s
              val _ =
                  AsmStream.withStream stream
                  (fn _ => let
                      val AsmEmitter.S.STREAM{emit,...} =
                          AsmEmitter.makeStream []
                      in
                      emit (I.INSTR instr) end) ()
              val _ = TextIO.closeOut stream
              in !s end

	  val _ = print (instrstring ())
          *)

	  in
	    case instr
	     of I.NOP => eByte 0x90
	      | I.JMP(I.Relative i, _) => (
		  case size (Int32.fromInt(i-2))
		   of Bits32 => eBytes (0wxe9 :: eLong (Int32.fromInt (i-5)))
		    | _ => eBytes [0wxeb, Word8.fromInt(i-2)]
		  (*esac*))
	      | I.JMP(opnd, _) => let
                  val ty = (case opnd of I.Direct(ty,_) => ty | _ => ~1)
                  in
                    if ty = 64
                      then let
                        fun encodejmp (bytes, r', opnd) = let
			      val (rex, e) = eImmedExt (r', opnd)
			      in
				case eREXRegs rex
				 of 0w0 => bytes @ e
				  | rexByte => (eREX rexByte) :: bytes @ e
				(* end case *)
			      end
                        in
                          eBytes(encodejmp([0wxff], OPCODE 4, opnd))
                        end
                      else encode32(0wxff, OPCODE 4, opnd)
                  end
	      | I.JCC{cond, opnd=I.Relative i} => let
	          val code = condCode cond
		  val i' = Int32.fromInt i
	          in
		    case size (i'-2)
		     of Bits32 => eBytes(0wx0f :: Word8.+(0wx80, code) :: eLong(i'-6))
		      | _ => eBytes[Word8.+(0wx70,code), Word8.fromInt(i-2)]
		    (* end case *)
	          end
	      | I.CALL{opnd=I.Relative i,...} =>
	          eBytes (0wxe8 :: eLong (Int32.fromInt (i-5)))
	      | I.CALL{opnd, ...} => encode32 (0wxff, OPCODE 2, opnd)
	      | I.ENTER{src1, src2} => unimplemented "ENTER"
	      | I.LEAVE => eByte 0xc9
	      | I.RET NONE => eByte 0xc3
	      | I.MOVE{mvOp, src, dst} => let
		(* utility to compute rex and adjusted register *)
		  fun rexReg r = if r < 8
			then (0wx48, r)
			else (0wx49, r - 8)
		(* emit basic MOV operation *)
		  fun mov sz = (case (src, dst)
		         of (I.Immed i, I.Direct (_, r)) => (case sz
			       of 32 => eBytes (Word8.+(0wxb8, Word8.fromInt(rNum r))::eLong i)
				| 64 => let
				    val (rex, reg) = rexReg(rNum r)
				    in
				      eBytes(rex::0wxc7::Word8.+(0wxc0, Word8.fromInt reg)::eLong i)
				    end
				| _ => raise Fail "impossible"
			      (* end case *))
			  | (I.Immed i, _) => encodeLongImm sz (0wxc7, OPCODE 0, dst, i)
			  | (I.Immed64 i, I.Direct (_, r)) => if sz = 32
			      then let
				val (start, reg) = if rNum r < 8
				      then ([], rNum r)
				      else ([0wx41], rNum r - 8)
				in
				  eBytes(start@Word8.+(0wxb8, Word8.fromInt reg)::eLongLongCut i)
				end
			      else let
				val (rex, reg) = rexReg(rNum r)
				in
				  eBytes(rex::Word8.+(0wxb8, Word8.fromInt reg)::eLongLong i)
				end
			  | (I.Immed64 i, _) => error " Immed64 _"
			  | (I.ImmedLabel le, dst) =>
			      encodeLongImm sz (0wxc7, OPCODE 0, dst, lexp le)
			  | (I.LabelEA le, dst) => error "MOVL: LabelEA"
			  | (src, dst) => arith(sz, 0wx88, OPCODE 0) (src, dst)
			(* end case *))
		(* zero and sign-extension moves for 16/32-bit results *)
		  fun extend (opc, r) = eBytes (encode32' ([0wx0f, opc], REG(rNum r), src))
		(* zero and sign-extension moves for 64-bit results *)
		  fun extend64 (opc, r) = eBytes (encode64' ([0wx0f, opc], REG(rNum r), src))
		  in
		    case (mvOp, src, dst)
		     of (I.MOVQ, _, _) => mov 64
		      | (I.MOVL, _, _) => mov 32
(* NOTE: there is a comment by Allen in the x86 code generator stating that there is a
 * bug in the ML code generator related to sign extension, thus we ignore the range
 * check and just store the low 8 bits.
 *)
		      | (I.MOVB, I.Immed i, _) => encodeByteImm32 (0wxc6, OPCODE 0, dst, i)
(*
		      | (I.MOVB, I.Immed i, _) => (case size i
			   of Bits32 => error "MOVE: MOVB: imm8"
			    | _ => encodeByteImm32 (0wxc6, OPCODE 0, dst, i)
			  (* end case *))
*)
		      | (I.MOVB, I.Direct(_, r), _) => encodeReg32 (0wx88, r, dst)
		      | (I.MOVB, _, I.Direct(_, r)) => encodeReg32 (0wx8a, r, src)
		      | (I.MOVW, _, I.Direct(_, r)) =>
			  eBytes (0wx66 :: encode32' ([0wx89], REG (rNum r), dst))
		      | (I.MOVW, _, _) => unimplemented "MOVW"
		      | (I.MOVABSQ, I.Immed64 i, I.Direct(_, r)) => let
			  val (rex, reg) = rexReg (rNum r)
			  in
			    eBytes(rex :: (0wxb8+Word8.fromInt reg) :: eLongLong i)
			  end
		      | (I.MOVABSQ, I.ImmedLabel labexp, I.Direct(_ ,r)) => let
			  val (byte1, p) = rexReg(rNum r)
			  val byte2 = 0wxb8 + Word8.fromInt p
			  val byten = eLong (lexp labexp) (* FIXME: should be 64 bits *)
			  val hilong = if (lexp labexp) < 0
				then [0wxff, 0wxff, 0wxff, 0wxff]
				else eLong 0
			  in
			    eBytes ([byte1, byte2] @ byten @ hilong)
			  end
		      | (I.MOVABSQ, _, _) => unimplemented "MOVABSQ"
		      | (I.MOVSWQ, _, I.Direct(_, r)) => extend64 (0wxbf, r)
		      | (I.MOVZWQ, _, I.Direct(_, r)) => extend64 (0wxb7, r)
		      | (I.MOVSWL, _, I.Direct(_, r)) => extend (0wxbf, r)
		      | (I.MOVZWL, _, I.Direct(_, r)) => extend (0wxb7, r)
		      | (I.MOVSBQ, _, I.Direct(_, r)) => extend64 (0wxbe, r)
		      | (I.MOVZBQ, _, I.Direct(_, r)) => extend64 (0wxb6, r)
		      | (I.MOVSBL, _, I.Direct(_, r)) => extend (0wxbe, r)
		      | (I.MOVZBL, _, I.Direct(_, r)) => extend (0wxb6, r)
		      | (I.MOVSLQ, _, I.Direct(_, r)) => extend64 (0wx63, r)
		      | (I.CVTSD2SI, _, I.Direct(_, r)) => unimplemented "CVTSD2SI"
		      | (I.CVTSS2SI, _, I.Direct(_, r)) => unimplemented "CVTSS2SI"
		      | (I.CVTSD2SIQ, _, I.Direct(_, r)) => unimplemented "CVTSD2SIQ"
		      | (I.CVTSS2SIQ, _, I.Direct(_, r)) => unimplemented "CVTSS2SIQ"
		      | _ => error "MOVE"
		    (* end case *)
		  end
	      | I.LEAL{r32, addr} => encodeReg32(0wx8d, r32, addr)
	      | I.LEAQ{r64, addr} => encodeReg64(0wx8d, r64, addr)
	      | I.CMPQ{lsrc, rsrc} => arith(64, 0wx38, OPCODE 7) (rsrc, lsrc)
	      | I.CMPL{lsrc, rsrc} => arith(32, 0wx38, OPCODE 7) (rsrc, lsrc)
	      | I.CMPW _ => unimplemented "CMPW"
	      | I.CMPB _ => unimplemented "CMP"
	      | I.TESTQ{lsrc, rsrc} => test(64, rsrc, lsrc)
	      | I.TESTL{lsrc, rsrc} => test(32, rsrc, lsrc)
	      | I.TESTW _ => unimplemented "TESTW"
	      | I.TESTB{lsrc, rsrc} => test(8, rsrc, lsrc)
	      | I.BITOP{bitOp, lsrc, rsrc} => let
		  fun encode sz = (case lsrc
			 of I.Immed n =>
			      if ((0 <= n) andalso (n < sz))
				then let
				  val (rex, [modRM]) = eImmedExt (OPCODE 4, rsrc)
				  val ib = Word8.fromLargeInt(Int32.toLarge n)
				  in
				    if (sz = 64)
				      then [eREX64 (eREXRegs rex), 0wx0f, 0wxba, modRM, ib]
				      else [0wx0f, 0wxba, modRM, ib]
				  end
				else error "BITOP: invalid bit position"
			  | I.Direct(_, r) => let
			      val (rex, suffix) = eImmedExt (REG(rNum r), rsrc)
			      in
				if (sz = 64)
				  then eREX64 (eREXRegs rex) :: 0wx0f :: 0wxba :: suffix
				  else 0wx0f :: 0wxba :: suffix
			      end
			(* end case *))
		  in
		    case bitOp
		     of I.BTW => unimplemented "BTW"
		      | I.BTL => eBytes (encode 32)
		      | I.BTQ => eBytes (encode 64)
		      | I.LOCK_BTW => eBytes (lockPrefix :: encode 32)
		      | I.LOCK_BTL => eBytes (lockPrefix :: encode 64)
		    (* end case *)
		  end
	      | I.BINARY{binOp, src, dst} => let
		  fun shift (sz, code) = (case src
			 of I.Immed 1 => encode sz (0wxd1, OPCODE code, dst)
			  | I.Immed n => encodeByteImm sz (0wxc1, OPCODE code, dst, n)
			  | I.Direct(_, r) => if rNum r <> ecx
			      then error "shift: Direct"
			      else encode sz (0wxd3, OPCODE code, dst)
(*			  | I.MemReg _ => shift(code, memReg src) *)
			  | _  => error "shift"
		       (*esac*))
		(* signed integer multiplication
		 *   dst <- src * dst		[REX.W] 0F AF /r
		 *   dst <- src * immed8	[REX.W] 6B /r ib
		 *   dst <- src * immed32	[REX.W] 69 /r id
		 *)
		  fun imul sz = (case (src, dst)
			 of (I.Immed i, I.Direct(_, dstR)) => (case size i
			       of Bits32 => encodeLongImm sz (0wx69, REG (rNum dstR), dst, i)
			        | _ => encodeByteImm sz (0wx6b, REG (rNum dstR), dst, i)
			      (* end case *))
			  | (_, I.Direct(_, dstR)) =>
			      eBytes (encode' sz ([0wx0f, 0wxaf], REG (rNum dstR), src))
			  | _ => error "imul"
			(* end case *))
		  in
		    case binOp
		     of I.ADDQ => arith(64, 0w0, OPCODE 0) (src, dst)
		      | I.SUBQ => arith(64, 0wx28, OPCODE 5) (src, dst)
		      | I.ANDQ => arith(64, 0wx20, OPCODE 4) (src, dst)
		      | I.ORQ  => arith(64, 0w8, OPCODE 1) (src, dst)
		      | I.XORQ => arith(64, 0wx30, OPCODE 6) (src, dst)
		      | I.SHLQ => shift(64, 4)
		      | I.SARQ => shift(64, 7)
		      | I.SHRQ => shift(64, 5)
		      | I.IMULQ => imul 64
		      | I.ADCQ => unimplemented "ADCQ"
		      | I.SBBQ => unimplemented "SBBQ"
		      | I.ADDL => arith(32, 0w0, OPCODE 0) (src, dst)
		      | I.SUBL => arith(32, 0wx28, OPCODE 5) (src, dst)
		      | I.ANDL => arith(32, 0wx20, OPCODE 4) (src, dst)
		      | I.ORL  => arith(32, 0w8, OPCODE 1) (src, dst)
		      | I.XORL => arith(32, 0wx30, OPCODE 6) (src, dst)
		      | I.SHLL => shift(32, 4)
		      | I.SARL => shift(32, 7)
		      | I.SHRL => shift(32, 5)
		      | I.IMULL => imul 32
		      | I.ADCL => unimplemented "ADCL"
		      | I.SBBL => unimplemented "SBBL"
		      | I.ADDW => unimplemented "ADDW"
		      | I.SUBW => unimplemented "SUBW"
		      | I.ANDW => unimplemented "ANDW"
		      | I.ORW => unimplemented "ORW"
		      | I.XORW => unimplemented "XORW"
		      | I.SHLW => unimplemented "SHLW"
		      | I.SARW => unimplemented "SARW"
		      | I.SHRW => unimplemented "SHRW"
		      | I.IMULW => unimplemented "IMULW"
		      | I.ADDB => unimplemented "ADDB"
		      | I.SUBB => unimplemented "SUBB"
		      | I.ANDB => unimplemented "ANDB"
		      | I.ORB => unimplemented "ORB"
		      | I.XORB => unimplemented "XORB"
		      | I.SHLB => unimplemented "SHLB"
		      | I.SARB => unimplemented "SARB"
		      | I.SHRB => unimplemented "SHRB"
		      | I.IMULB => unimplemented "IMULB"
		      | I.BTSW => unimplemented "BTSW"
		      | I.BTCW => unimplemented "BTCW"
		      | I.BTRW => unimplemented "BTRW"
		      | I.BTSL => unimplemented "BTSL"
		      | I.BTCL => unimplemented "BTCL"
		      | I.BTRL => unimplemented "BTRL"
		      | I.ROLW => unimplemented "ROLW"
		      | I.RORW => unimplemented "RORW"
		      | I.ROLL => unimplemented "ROLL"
		      | I.RORL => unimplemented "RORL"
		      | I.XCHGB => unimplemented "XCHGB"
		      | I.XCHGW => unimplemented "XCHGW"
		      | I.XCHGL => unimplemented "XCHGL"
		      | I.LOCK_ADCW => unimplemented "LOCK_ADCW"
		      | I.LOCK_ADCL => unimplemented "LOCK_ADCL"
		      | I.LOCK_ADDW => unimplemented "LOCK_ADDW"
		      | I.LOCK_ADDL => unimplemented "LOCK_ADDL"
		      | I.LOCK_ANDW => unimplemented "LOCK_ANDW"
		      | I.LOCK_ANDL => unimplemented "LOCK_ANDL"
		      | I.LOCK_BTSW => unimplemented "LOCK_BTSW"
		      | I.LOCK_BTSL => unimplemented "LOCK_BTSL"
		      | I.LOCK_BTRW => unimplemented "LOCK_BTRW"
		      | I.LOCK_BTRL => unimplemented "LOCK_BTRL"
		      | I.LOCK_BTCW => unimplemented "LOCK_BTCW"
		      | I.LOCK_BTCL => unimplemented "LOCK_BTCL"
		      | I.LOCK_ORW => unimplemented "LOCK_ORW"
		      | I.LOCK_ORL => unimplemented "LOCK_ORL"
		      | I.LOCK_SBBW => unimplemented "LOCK_SBBW"
		      | I.LOCK_SBBL => unimplemented "LOCK_SBBL"
		      | I.LOCK_SUBW => unimplemented "LOCK_SUBW"
		      | I.LOCK_SUBL => unimplemented "LOCK_SUBL"
		      | I.LOCK_XORW => unimplemented "LOCK_XORW"
		      | I.LOCK_XORL => unimplemented "LOCK_XORL"
		      | I.LOCK_XADDB => unimplemented "LOCK_XADDB"
		      | I.LOCK_XADDW => unimplemented "LOCK_XADDW"
		      | I.LOCK_XADDL => unimplemented "LOCK_XADDL"
		    (* end case *)
	          end
	      | I.SHIFT{shiftOp, src, dst, count} => (case shiftOp
		   of I.SHLDL => unimplemented "SHLDL"
		    | I.SHRDL => unimplemented "SHRDL"
		  (* end case *))
	      | I.MULTDIV{multDivOp, src} => let
	          val (mulOp, sz) = (case multDivOp
			 of I.MULL1 => (4, 32) | I.IDIVL1 => (7, 32) | I.DIVL1 => (6, 32)
			  | I.MULQ1 => (4, 64) | I.IDIVQ1 => (7, 64) | I.DIVQ1 => (6, 64)
			  | I.IMULL1 => error "imull1"
			  | I.IMULQ1 => error "imulq1"
		       (* esac *))
		  in
		    encode sz (0wxf7, OPCODE mulOp, src)
		  end
	      | I.MUL3{dst, src1, src2=i} => (case src1
		   of I.Immed _ => error "mul3: Immed"
		    | I.ImmedLabel _ => error "mul3: ImmedLabel"
		    | _ => (case size i
			 of Bits32 => encodeLongImm32(0wx69, REG (rNum dst), src1, i)
			  | _ => encodeByteImm32(0wx6b, REG (rNum dst), src1, i)
		       (*esac*))
		  (*esac*))
	      | I.MULQ3{dst, src1, src2=i} => (case src1
		   of I.Immed _ => error "mul3: Immed"
		    | I.ImmedLabel _ => error "mul3: ImmedLabel"
		    | _ => (case size i
			 of Bits32 => encodeLongImm64(0wx69, REG (rNum dst), src1, i)
			  | _ => encodeByteImm64(0wx6b, REG (rNum dst), src1, i)
			(*esac*))
		  (*esac*))
	      | I.UNARY{unOp, opnd} => let
		  fun lock code = eBytes(lockPrefix :: code)
		  in
		    case unOp
		     of I.DECQ => encode64 (0wxff, OPCODE 1, opnd)
		      | I.INCQ => encode64 (0wxff, OPCODE 0, opnd)
		      | I.NEGQ => encode64 (0wxf7, OPCODE 3, opnd)
		      | I.NOTQ => encode64 (0wxff, OPCODE 2, opnd)
		      | I.DECL => encode32 (0wxff, OPCODE 1, opnd)
		      | I.NEGL => encode32 (0wxff, OPCODE 3, opnd)
		      | I.INCL => encode32 (0wxff, OPCODE 0, opnd)
		      | I.NOTL => encode32 (0wxff, OPCODE 2, opnd)
		      | I.DECW => unimplemented "DECW"
		      | I.NEGW => unimplemented "NEGW"
		      | I.INCW => unimplemented "INCW"
		      | I.NOTW => unimplemented "NOTW"
		      | I.DECB => unimplemented "DECB"
		      | I.NEGB => unimplemented "NEGB"
		      | I.INCB => unimplemented "INCB"
		      | I.NOTB => unimplemented "NOTB"
		      | I.LOCK_DECQ => lock (encode64' ([0wxff], OPCODE 1, opnd))
		      | I.LOCK_INCQ => lock (encode64' ([0wxff], OPCODE 0, opnd))
		      | I.LOCK_NEGQ => lock (encode64' ([0wxf7], OPCODE 3, opnd))
		      | I.LOCK_NOTQ => lock (encode64' ([0wxff], OPCODE 2, opnd))
		    (* esac *)
		  end
	      | I.SET{cond,opnd} =>
	          eBytes (encode32' ([0wx0f, Word8.+(0wx90,condCode cond)], REG 0, opnd))
	      | I.CMOV{cond,src,dst} =>
		  eBytes (encode32' ([0wx0f, Word8.+(condCode cond,0wx40)], REG (rNum dst), src))
	      | I.PUSH(I.Immed i) => (case size i
		   of Bits32 => eBytes(0wx68 :: eLong i)
		    | _ => eBytes [0wx6a, toWord8 i]
		  (* esac *))
	      | I.PUSH(I.Direct(_, r)) => let
		  val r = rNum r
		  in
		    if (r < 8)
		      then eByte (0x50 + r)
		      else eBytes [0wx41, 0wx50 + Word8.fromInt(r - 8)]
		  end
(* TODO: check that PUSH is correct for other operands *)
	      | I.PUSH opnd => encode32 (0wxff, OPCODE 6, opnd)
	      | I.PUSHFQ => eByte 0x9c
	      | I.POPFQ => eByte 0x9d
	      | I.POP(I.Direct(_, r)) => let
		  val r = rNum r
		  in
		    if (r < 8)
		      then eByte (0x58 + r)
		      else eBytes [0wx41, 0wx58 + Word8.fromInt(r - 8)]
		  end
(* TODO: check that POP is correct for other operands *)
	      | I.POP opnd => encode32 (0wx8f, OPCODE 0, opnd)
	      | I.CDQ => eByte 0x99
	      | I.CDO => eBytes [0wx48, 0wx99]
	      | I.INT b => eBytes [0wxcd, b]
	      | I.FMOVE {fmvOp=I.MOVSD, dst=I.FDirect r, src=src} => movsd(0wx10, r, src)
	      | I.FMOVE {fmvOp=I.MOVSD, dst=dst, src=I.FDirect r} => movsd(0wx11, r, dst)
              | I.FMOVE {fmvOp, dst, src} => (case fmvOp
		   of I.CVTSI2SD => error "CVTSI2SD not implemented"
		    | I.CVTSI2SDQ => let
			val I.FDirect r = dst
			in
			  eBytes([0wxf2] @ encode64'([0wxf,0wx2a],REG (fNum r),src))
			end
		    | I.CVTSD2SS => (case dst
		         of (I.FDirect r) =>
			      eBytes([0wxf2] @ encode32'([0wxf,0wx5a],REG (fNum r),src))
			  | _ => error "CVTSD2SS"
			(* end case *))
		    | I.CVTSS2SD => (case dst
			 of (I.FDirect r) =>
			      eBytes([0wxf3] @ encode32'([0wxf,0wx5a],REG (fNum r),src))
			  | _ => error "CVTSD2SD"
			(* end case *))
		    | I.MOVSS => (case src
			 of (I.FDirect r) =>
			      eBytes([0wxf3] @ encode32'([0wxf,0wx11],REG (fNum r),dst))
			  | _ => error "MOVSS"
			(* end case *))
		    | _ => error "FMOVE"
		  (* end case *))
              | I.FBINOP {binOp, src, dst} => let
		  fun encode2 (op1, op2) = eBytes(encode32'([op1, op2], REG(fNum dst), src))
		  fun encode (op1, op2, op3) =
			eBytes(op1 :: encode32'([op2, op3], REG(fNum dst), src))
		  in
		    case binOp
		     of I.ADDSS => encode (0wxf3, 0wx0f, 0wx58)
		      | I.ADDSD => encode (0wxf2, 0wx0f, 0wx58)
		      | I.SUBSS => encode (0wxf3, 0wx0f, 0wx5c)
		      | I.SUBSD => encode (0wxf2, 0wx0f, 0wx5c)
		      | I.MULSS => encode (0wxf3, 0wx0f, 0wx59)
		      | I.MULSD => encode (0wxf2, 0wx0f, 0wx59)
		      | I.DIVSS => encode (0wxf3, 0wx0f, 0wx5e)
		      | I.DIVSD => encode (0wxf2, 0wx0f, 0wx5e)
		      | I.XORPS => encode2 (0wx0f, 0wx57)
		      | I.XORPD => encode (0wx66, 0wx0f, 0wx57)
		      | I.ANDPS => encode2 (0wx0f, 0wx54)
		      | I.ANDPD => encode (0wx66, 0wx0f, 0wx54)
		      | I.ORPS => encode2 (0wx0f, 0wx56)
		      | I.ORPD => encode (0wx66, 0wx0f, 0wx56)
		    (* end case *)
		  end
	      | I.FCOM {comOp, src, dst} => (case comOp
		   of I.COMISS => unimplemented "COMISS"
		    | I.COMISD => unimplemented "COMISD"
		    | I.UCOMISS => unimplemented "UCOMISS"
		    | I.UCOMISD => eBytes([0wx66] @ encode32'([0wxf,0wx2e],REG (fNum dst),src))
		  (* end case *))
	      | I.FSQRTS {dst, src} => let
		  val I.FDirect r = dst
		  in
		    eBytes([0wxf3] @ encode32'([0wxf,0wx51],REG (fNum r),src))
		  end
	      | I.FSQRTD {dst, src} => let
		  val I.FDirect r = dst
		  in
		    eBytes([0wxf2] @ encode32'([0wxf,0wx51],REG (fNum r),src))
		  end
	      | I.SAHF => eByte 0x9e
	      | I.LFENCE => eBytes [0wx0f, 0wxae, 0wxe8]
	      | I.MFENCE => eBytes [0wx0f, 0wxae, 0wxf0]
	      | I.SFENCE => eBytes [0wx0f, 0wxae, 0wxf8]
	      | I.PAUSE => eBytes [0wxf3, 0wx90]
	      | I.XCHG _ => unimplemented "XCHG"
	      | I.CMPXCHG _ => unimplemented "CMPXCHG"
	      | I.XADD _ => unimplemented "XADD"
	      | I.RDTSC => unimplemented "RDTSC"
	      | I.RDTSCP => unimplemented "RDTSCP"
	      | I.LAHF => eByte 0x9f
	      | _ => error "emitInstr"
	    (* esac *)
	  end (* emitAMD64Instr *)

    fun emitInstr (I.LIVE _) = Word8Vector.fromList []
      | emitInstr (I.KILL _) = Word8Vector.fromList []
      | emitInstr (I.COPY{k, dst, src, tmp, ...}) = (case k
	   of CB.GP => emitInstrs (Shuffle.shuffle {tmp=tmp, dst=dst, src=src})
	    | CB.FP => emitInstrs (Shuffle.shufflefp {tmp=tmp, dst=dst, src=src})
	    | _ => error "COPY"
	  (*esac*))
      | emitInstr (I.INSTR instr) = emitAMD64Instr instr
(* NOTE: the general consensus on the internet is that Intel hardware ignores the
 * branch hint prefixes and just relies on dynamic techniques.  Therefore, we
 * comment this code out for now.
      | emitInstr (I.ANNOTATION{i, a}) = (case #peek MLRiscAnnotations.BRANCH_PROB a
	   of SOME prob => let
		val prob = Probability.toReal prob
		fun emit (I.ANNOTATION{i, ...}) = emit i
		  | emit (I.INSTR(instr as I.JCC _)) = let
		      val code = emitInstr i
		      in
			if prob < 0.5
			  then Word8Vector.prepend (0wx2e, code) (* add not-taken hint *)
			else if prob > 0.5
			  then Word8Vector.prepend (0wx3e, code) (* add taken hint *)
			  else code
		      end
		  | emit _ = error "bogus BRANCH_PROB annotation"
		in
		  emit i
		end
	    | NONE => emitInstr i
	  (* end case *))
 *)
      | emitInstr (I.ANNOTATION{i, a}) = emitInstr i

    and emitInstrs instrs = Word8Vector.concat(map emitInstr instrs)

  end (* AMD64MCEmitter *)
