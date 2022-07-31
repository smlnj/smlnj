(*
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 * IMPORTANT NOTE: 
 *   Integer registers are numbered from 0 - 31 (0-7 are physical)
 *   Floating point registers are numbered from 32-63 (32-39 are physical)
 *)
functor X86MCEmitter
  (structure Instr : X86INSTR
   structure Shuffle : X86SHUFFLE where I = Instr
   structure MLTreeEval : MLTREE_EVAL where T = Instr.T
   structure MemRegs : MEMORY_REGISTERS where I = Instr
   val memRegBase : CellsBasis.cell option
   structure AsmEmitter : INSTRUCTION_EMITTER where I = Instr) : MC_EMIT = 
struct
  structure I = Instr
  structure C = I.C
  structure Const = I.Constant
  structure W32 = Word32
  structure W8 = Word8
  structure W = LargeWord
  structure CB = CellsBasis
  structure LE = MLTreeEval 

  val itow  = Word.fromInt
  val wtoi  = Word.toInt

  fun error msg = MLRiscErrorMsg.impossible ("X86MCEmitter." ^ msg)

  (*
   * Sanity check!
   *)

  val eax = 0   val esp = 4   
  val ecx = 1   val ebp = 5
  val edx = 2   val esi = 6   
  val ebx = 3   val edi = 7

  val opnd16Prefix = 0x66

  fun const c = Int32.fromInt(Const.valueOf c)
  fun lexp le = Int32.fromInt(LE.valueOf le)

  val toWord8 = Word8.fromLargeWord o LargeWord.fromLargeInt o Int32.toLarge
  val eBytes = Word8Vector.fromList 
  fun eByte i = eBytes [W8.fromInt i]
  local 
    val toLWord = (W.fromLargeInt o Int32.toLarge) 
    fun shift (w,cnt) = W8.fromLargeWord(W.>>(w, cnt))
  in
    fun eShort i16 = let
      val w = toLWord i16
    in [shift(w, 0w0), shift(w,0w8)]
    end
    fun eLong i32 = let
      val w = toLWord i32
    in [shift(w, 0w0), shift(w,0w8), shift(w,0w16), shift(w,0w24)]
    end
  end

  fun emitInstrs(instrs) = Word8Vector.concat(map emitInstr instrs)

  and emitX86Instr(instr: I.instr) = let
    val error = 
        fn msg =>
           let val AsmEmitter.S.STREAM{emit,...} = AsmEmitter.makeStream []
           in  emit (I.INSTR instr); error msg end

    val rNum = CB.physicalRegisterNum 
    val fNum = CB.physicalRegisterNum 

    fun memReg r = MemRegs.memReg{reg=r, base=Option.valOf memRegBase}

    datatype size = Zero | Bits8 | Bits32
    fun size i = 
      if i = 0 then Zero
      else if Int32.<(i, 128) andalso Int32.<=(~128, i) then Bits8 
      else Bits32

    fun immedOpnd(I.Immed(i32)) = i32
      | immedOpnd(I.ImmedLabel le) = lexp le
      | immedOpnd(I.LabelEA le) = lexp le
      | immedOpnd _ = error "immedOpnd"

    nonfix mod    

    fun scale(n, m) = Word.toIntX(Word.<<(Word.fromInt n, Word.fromInt m))
    fun modrm{mod, reg, rm} = W8.fromInt(scale(mod,6) + scale(reg,3) + rm)
    fun sib{ss, index, base} = W8.fromInt(scale(ss,6) + scale(index,3) + base)
    fun reg{opc, reg} = W8.fromInt(scale(opc,3) + reg)

    fun eImmedExt(opc, I.Direct r) = [modrm{mod=3, reg=opc, rm=rNum r}]
      | eImmedExt(opc, opn as I.MemReg _) = eImmedExt(opc, memReg opn)
      | eImmedExt(opc, I.Displace{base, disp, ...}) = let
          val base = rNum base                (* XXX rNum may be done twice *)
          val immed = immedOpnd disp
          fun displace(mod, eDisp) = 
            if base=esp then 
              modrm{mod=mod, reg=opc, rm=4}::
              sib{ss=0, index=4, base=esp}::eDisp immed
            else
              modrm{mod=mod, reg=opc, rm=base} :: eDisp immed
        in
          case size immed
          of Zero => 
              if base=esp then 
               [modrm{mod=0, reg=opc, rm=4}, sib{ss=0,index=4,base=esp}]
             else if base=ebp then
               [modrm{mod=1, reg=opc, rm=ebp}, 0w0]
             else 
               [modrm{mod=0, reg=opc, rm=base}]
           | Bits8 => displace(1, fn i => [toWord8 i])
           | Bits32 => displace(2, eLong)
          (*esac*)
        end
      | eImmedExt(opc, I.Indexed{base=NONE, index, scale, disp, ...}) = 
         (modrm{mod=0, reg=opc, rm=4} ::
          sib{base=5, ss=scale, index=rNum index} :: 
          eLong(immedOpnd disp))
      | eImmedExt(opc, I.Indexed{base=SOME b, index, scale, disp, ...}) = let
          val index = rNum index
          val base = rNum b
          val immed = immedOpnd disp
          fun indexed(mod, eDisp) = 
            modrm{mod=mod, reg=opc, rm=4} ::
              sib{ss=scale, index=index, base=base} :: eDisp immed
        in
          case size immed
          of Zero => 
             if base=ebp then 
               [modrm{mod=1, reg=opc, rm=4},
                  sib{ss=scale, index=index, base=5}, 0w0]
             else
               [modrm{mod=0, reg=opc, rm=4}, 
                  sib{ss=scale, index=index, base=base}]
           | Bits8 => indexed(1, fn i => [toWord8 i])
           | Bits32 => indexed(2, eLong)
          (*esac*)
        end
      | eImmedExt(opc, opnd as I.FDirect f) = eImmedExt(opc, memReg opnd)
      | eImmedExt(_, I.Immed _) = error "eImmedExt: Immed"
      | eImmedExt(_, I.ImmedLabel _) = error "eImmedExt: ImmedLabel"
      | eImmedExt(_, I.Relative _) = error "eImmedExt: Relative"
      | eImmedExt(_, I.LabelEA _) = error "eImmedExt: LabelEA"
      | eImmedExt(_, I.FPR _) = error "eImmedExt: FPR"
      | eImmedExt(_, I.ST _) = error "eImmedExt: ST"

       (* Short hands for various encodings *)
    fun encode(byte1, opc, opnd) = eBytes(byte1 :: eImmedExt(opc, opnd))
    fun encodeST(byte1, opc, STn) = eBytes[byte1, reg{opc=opc,reg=fNum STn}]
    fun encode2(byte1, byte2, opc, opnd) = 
        eBytes(byte1 :: byte2 :: eImmedExt(opc, opnd))
    fun encodeReg(byte1, reg, opnd) = encode(byte1, rNum reg, opnd)
    fun encodeLongImm(byte1, opc, opnd, i) =
         eBytes(byte1 :: (eImmedExt(opc, opnd) @ eLong i))
    fun encodeShortImm(byte1, opc, opnd, w) =
         eBytes(byte1 :: (eImmedExt(opc, opnd) @ eShort w))
    fun encodeByteImm(byte1, opc, opnd, b) =
         eBytes(byte1 :: (eImmedExt(opc, opnd) @ [toWord8 b]))

    fun condCode cond =
        (case cond
          of I.EQ => 0w4      | I.NE => 0w5
           | I.LT => 0w12     | I.LE => 0w14
           | I.GT => 0w15     | I.GE => 0w13
           | I.A  => 0w7      | I.AE => 0w3
           | I.B  => 0w2      | I.BE => 0w6
           | I.C  => 0w2      | I.NC => 0w3
           | I.P  => 0wxa     | I.NP => 0wxb
           | I.O  => 0w0      | I.NO => 0w1
        (*esac*))

    (* arith: only 5 cases need be considered:
     *  dst,   src
     *  -----------
     *  EAX,   imm32
     *        r/m32, imm32
     *  r/m32, imm8
     *        r/m32, r32
     *  r32,   r/m32
     *)
    fun arith(opc1, opc2) = let
      fun f(I.ImmedLabel le, dst) = f(I.Immed(lexp le), dst)
        | f(I.LabelEA le, dst) = f(I.Immed(lexp le), dst)
        | f(I.Immed(i), dst) = 
          (case size i
            of Bits32 => 
               (case dst
                of I.Direct r =>
                    if CB.physicalRegisterNum r = eax then 
                      eBytes(W8.fromInt(8*opc2 + 5) :: eLong(i))
                    else 
                      encodeLongImm(0wx81, opc2, dst, i)
                 | _ => encodeLongImm(0wx81, opc2, dst, i)
               (*esac*))
             | _ => encodeByteImm(0wx83, opc2, dst, i) (* 83 /digit ib *)
          (*esac*))
        | f(src, I.Direct r) = encodeReg(opc1+0w3, r, src)
        | f(I.Direct r, dst) = encodeReg(opc1+0w1, r, dst)
        | f _ = error "arith.f"
    in f
    end

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
    fun test(bits, I.ImmedLabel le, lsrc) = test(bits, I.Immed(lexp le), lsrc)
      | test(bits, I.LabelEA le, lsrc) = test(bits, I.Immed(lexp le), lsrc)
      | test(bits, I.Immed(i), lsrc) =
         (case (lsrc, i >= 0 andalso i < 255) of 
           (I.Direct r, false) => 
             if CB.physicalRegisterNum r = eax then eBytes(0wxA9 :: eLong i) 
             else encodeLongImm(0wxF7, 0, lsrc, i)
         | (_, false)  => encodeLongImm(0wxF7, 0, lsrc, i)
         | (I.Direct r, true) =>  (* 8 bit *)
           let val r = CB.physicalRegisterNum r
           in  if r = eax then eBytes[0wxA8, toWord8 i]
               else if r < 4 then 
                    (* unfortunately, only CL, DL, BL can be encoded *)
                  encodeByteImm(0wxF6, 0, lsrc, i)
               else if bits = 8 then error "test.8" 
               else encodeLongImm(0wxF7, 0, lsrc, i)
           end
         | (_, true) => encodeByteImm(0wxF6, 0, lsrc, i)
         )
      | test(8, rsrc as I.Direct r, lsrc) =
         if rNum r < 4 then encodeReg(0wx84, r, lsrc)
         else error "test.8"
      | test(32, I.Direct r, lsrc) = encodeReg(0wx85, r, lsrc)
      | test _ = error "test"

  in
    case instr
    of I.NOP => eByte 0x90
     | I.JMP(I.Relative i, _) => ((let
         fun shortJmp() = eBytes[0wxeb, Word8.fromInt(i-2)]
       in
        case size(Int32.fromInt (i-2))
        of Bits32 => eBytes(0wxe9 :: eLong(Int32.fromInt(i-5)))
         | _ => shortJmp()
        (*esac*)
       end
       ) handle e => (print "JMP\n"; raise e))
     | I.JMP(opnd, _) => encode(0wxff, 4, opnd)
     | I.JCC{cond, opnd=I.Relative i} => 
       let val code = condCode cond
       in  case size (Int32.fromInt(i-2))
            of Bits32 => 
               eBytes(0wx0f :: Word8.+(0wx80,code) :: eLong(Int32.fromInt(i-6)))
             | _ => 
                eBytes[Word8.+(0wx70,code), Word8.fromInt(i-2)]
       end 
     | I.CALL{opnd=I.Relative i,...} => eBytes(0wxe8::eLong(Int32.fromInt(i-5)))
     | I.CALL{opnd, ...} => encode(0wxff, 2, opnd)
     | I.RET NONE => eByte 0xc3
     (* integer *)
     | I.MOVE{mvOp=I.MOVL, src, dst} => 
       let fun mv(I.Immed(i), I.Direct r) =
                 eBytes(Word8.+(0wxb8, Word8.fromInt(rNum r))::eLong(i))
             | mv(I.Immed(i), _) = encodeLongImm(0wxc7, 0, dst, i)
             | mv(I.ImmedLabel le,dst) = mv(I.Immed(lexp le),dst)
             | mv(I.LabelEA le,dst) = error "MOVL: LabelEA"
             | mv(src as I.MemReg _, dst) = mv(memReg src, dst)
             | mv(src, dst as I.MemReg _) = mv(src, memReg dst)  
             | mv(src,dst) = arith(0wx88, 0) (src, dst)
       in  mv(src,dst) end
     | I.MOVE{mvOp=I.MOVW, src, dst} => let
	  fun immed16 i = Int32.<(i, 32768) andalso Int32.<=(~32768, i)
	  fun prefix v = Word8Vector.concat[eByte(opnd16Prefix), v]
	  fun mv(I.Immed(i), _) = 
	      (case dst
	       of I.Direct r => 
		  if immed16 i then 
		    prefix(eBytes(W8.+(0wxb8, W8.fromInt(rNum r)):: eShort(i)))
		  else error "MOVW: Immediate too large"
		| _ => prefix(encodeShortImm(0wxc7, 0, dst, i))
	      (*esac*))
	    | mv(src as I.MemReg _, dst) = mv(memReg src, dst)
	    | mv(src, dst as I.MemReg _) = mv(src, memReg dst)
	    | mv(src, dst) = prefix(arith(0wx88, 0) (src, dst))
       in mv(src, dst)
       end
     | I.MOVE{mvOp=I.MOVB, dst, src=I.Immed(i)} =>
          encodeByteImm(0wxc6, 0, dst, i)
       (* 2007/02/20 AKL: just store the low order 8 bits.  
          Forget about checking for the range.  This is because of
          sign-extension issues in sml/nj's code generator.
       (case size i
         of Bits32 => error "MOVE: MOVB: imm8"
          | _ => encodeByteImm(0wxc6, 0, dst, i)
       (*esac*))
       *)
     | I.MOVE{mvOp=I.MOVB, dst, src=I.Direct r} => encodeReg(0wx88, r, dst)
     | I.MOVE{mvOp=I.MOVB, dst=I.Direct r, src} => encodeReg(0wx8a, r, src)
     | I.MOVE{mvOp, src=I.Immed _, ...} => error "MOVE: Immed"
     | I.MOVE{mvOp, src, dst=I.Direct r} =>
       let val byte2 = 
               case mvOp of
                 I.MOVZBL => 0wxb6 
               | I.MOVZWL => 0wxb7 
               | I.MOVSBL => 0wxbe 
               | I.MOVSWL => 0wxbf 
               | _ => error "MOV[SZ]X"
       in  eBytes(0wx0f :: byte2 :: eImmedExt(rNum r, src)) end
     | I.MOVE _ => error "MOVE"
     | I.CMOV{cond,src,dst} => 
       let val cond = condCode cond
       in  eBytes(0wx0f :: Word8.+(cond,0wx40) :: eImmedExt(rNum dst, src))
       end
     | I.LEA{r32, addr} => encodeReg(0wx8d, r32, addr)
     | I.CMPL{lsrc, rsrc} => arith(0wx38, 7) (rsrc, lsrc)
     | (I.CMPW _ | I.CMPB _) => error "CMP"
     | I.TESTL{lsrc, rsrc} => test(32, rsrc, lsrc)
     | I.TESTB{lsrc, rsrc} => test(8, rsrc, lsrc)
     | I.TESTW _ => error "TEST"
     | I.BINARY{binOp, src, dst} => let
         fun shift(code, src) = 
            (case src
             of I.Immed (1) => encode(0wxd1, code, dst)
              | I.Immed (n) => encodeByteImm(0wxc1, code, dst, n)
              | I.Direct r => 
                 if rNum r <> ecx then  error "shift: Direct"
                 else encode(0wxd3, code, dst)
              | I.MemReg _ => shift(code, memReg src)
              | _  => error "shift"
             (*esac*))
       in
         case binOp
          of I.ADDL => arith(0w0, 0) (src, dst)
           | I.SUBL => arith(0wx28, 5) (src, dst)
           | I.ANDL => arith(0wx20, 4) (src, dst)
           | I.ORL  => arith(0w8, 1) (src, dst)
           | I.XORL => arith(0wx30, 6) (src, dst)
           | I.SHLL => shift(4,src)
           | I.SARL => shift(7,src)
           | I.SHRL => shift(5,src)
           | I.IMULL => 
            (case (src, dst) 
             of (I.Immed(i), I.Direct dstR) =>
                 (case size i
                  of Bits32 => encodeLongImm(0wx69, rNum dstR, dst, i)
                   | _ => encodeByteImm(0wx6b, rNum dstR, dst, i)
                 )
              | (_, I.Direct dstR) => 
                 eBytes(0wx0f::0wxaf::(eImmedExt(rNum dstR, src)))
              | _ => error "imull"
            )
	   | _ => error "binary"
       end
     | I.MULTDIV{multDivOp, src} => let
         val mulOp = 
             (case multDivOp of
		  I.MULL1 => 4 | I.IDIVL1 => 7 | I.DIVL1 => 6
		| I.IMULL1 => error "imull1")
       in encode(0wxf7, mulOp, src)
       end
     | I.MUL3{dst, src1, src2=i} => 
       (case src1 
        of I.Immed _ => error "mul3: Immed"
         | I.ImmedLabel _ => error "mul3: ImmedLabel"
         | _ => 
           (case size i
            of Bits32 => encodeLongImm(0wx69, rNum dst, src1, i)
             | _ => encodeByteImm(0wx6b, rNum dst, src1, i)
            (*esac*))
        (*esac*))
     | I.UNARY{unOp, opnd} => 
       (case unOp
        of I.DECL => 
            (case opnd
             of I.Direct d => eByte(0x48 + rNum d)
              | _ => encode(0wxff, 1, opnd)
             (*esac*))
         | I.INCL =>
            (case opnd
             of I.Direct d => eByte(0x40 + rNum d)
              | _ => encode(0wxff, 0, opnd)
             (*esac*))
         | I.NEGL => encode(0wxf7, 3, opnd)
         | I.NOTL => encode(0wxf7, 2, opnd)
	 | _ => error "UNARY is not in DECL/INCL/NEGL,NOTL"
        (*esac*))
     | I.SET{cond,opnd} => 
         eBytes(0wx0f :: Word8.+(0wx90,condCode cond) :: eImmedExt(0, opnd))
     | I.PUSHL(I.Immed(i)) => 
       (case size i 
        of Bits32 => eBytes(0wx68 :: eLong(i))
         | _ => eBytes[0wx6a, toWord8 i]
        (*esac*))
     | I.PUSHL(I.Direct r) => eByte(0x50+rNum r)
     | I.PUSHL opnd => encode(0wxff, 6, opnd)
     | I.POP(I.Direct r) => eByte(0x58+rNum r)
     | I.POP(opnd) => encode(0wx8f, 0, opnd)
     | I.CDQ => eByte(0x99)
     | I.INTO => eByte(0xce)

     (* floating *)
     | I.FBINARY{binOp, src=I.ST src, dst=I.ST dst} =>    
       let val src = W8.fromInt(fNum src)
           val dst = W8.fromInt(fNum dst)
           val (opc1, opc2) =
            case (src, dst) of
              (_, 0w0) => 
                (case binOp 
                 of I.FADDL  => (0wxd8, 0wxc0 + src)
                  | I.FMULL  => (0wxd8, 0wxc8 + src)
                  | I.FSUBRL => (0wxd8, 0wxe8 + src)
                  | I.FSUBL  => (0wxd8, 0wxe0 + src) (* gas XXX *)
                  | I.FDIVRL => (0wxd8, 0wxf8 + src)
                  | I.FDIVL  => (0wxd8, 0wxf0 + src) (* gas XXX *)
                  | _        => error "FBINARY:pop:src=%st(n),dst=%st"
                )
            | (0w0, _) =>
                (case binOp
                 of I.FADDP  => (0wxde, 0wxc0 + dst)
                  | I.FMULP  => (0wxde, 0wxc8 + dst)
                  | I.FSUBRP => (0wxde, 0wxe8 + dst) (* gas XXX *)
                  | I.FSUBP  => (0wxde, 0wxe0 + dst)
                  | I.FDIVRP => (0wxde, 0wxf8 + dst) (* gas XXX *)
                  | I.FDIVP  => (0wxde, 0wxf0 + dst)

                  | I.FADDL  => (0wxdc, 0wxc0 + dst)
                  | I.FMULL  => (0wxdc, 0wxc8 + dst)
                  | I.FSUBRL => (0wxdc, 0wxe8 + dst) (* gas XXX *)
                  | I.FSUBL  => (0wxdc, 0wxe0 + dst)
                  | I.FDIVRL => (0wxdc, 0wxf8 + dst) (* gas XXX *)
                  | I.FDIVL  => (0wxdc, 0wxf0 + dst)

		  | _ => error "FBINARY (0w0,_)"
                )
            | (_, _) => error "FBINARY (src, dst) non %st(0)"
       in  eBytes[opc1, opc2]
       end
     | I.FBINARY{binOp, src, dst=I.ST dst} => 
       if CB.physicalRegisterNum dst = 0 then
       let
         val (opc, code) = 
           (case binOp of 
                I.FADDL  => (0wxdc, 0) 
              | I.FMULL  => (0wxdc, 1) 
              | I.FCOML  => (0wxdc, 2) 
              | I.FCOMPL => (0wxdc, 3) 
              | I.FSUBL  => (0wxdc, 4) 
              | I.FSUBRL => (0wxdc, 5) 
              | I.FDIVL  => (0wxdc, 6)
              | I.FDIVRL => (0wxdc, 7)
              | I.FADDS  => (0wxd8, 0) 
              | I.FMULS  => (0wxd8, 1) 
              | I.FCOMS  => (0wxd8, 2) 
              | I.FCOMPS => (0wxd8, 3) 
              | I.FSUBS  => (0wxd8, 4) 
              | I.FSUBRS => (0wxd8, 5) 
              | I.FDIVS  => (0wxd8, 6)
              | I.FDIVRS => (0wxd8, 7)
              | _ =>  error "FBINARY:pop:dst=%st"
           (*esac*))
       in encode(opc, code, src)
       end
       else error "FBINARY"
     | I.FIBINARY{binOp, src} => 
       let val (opc, code) =
             case binOp of
               I.FIADDL  => (0wxda, 0)
             | I.FIMULL  => (0wxda, 1)
             | I.FICOML  => (0wxda, 2)
             | I.FICOMPL => (0wxda, 3)
             | I.FISUBL  => (0wxda, 4)
             | I.FISUBRL => (0wxda, 5)
             | I.FIDIVL  => (0wxda, 6)
             | I.FIDIVRL => (0wxda, 7)
             | I.FIADDS  => (0wxde, 0)
             | I.FIMULS  => (0wxde, 1)
             | I.FICOMS  => (0wxde, 2)
             | I.FICOMPS => (0wxde, 3)
             | I.FISUBS  => (0wxde, 4)
             | I.FISUBRS => (0wxde, 5)
             | I.FIDIVS  => (0wxde, 6)
             | I.FIDIVRS => (0wxde, 7)
       in  encode(opc, code, src) end
     | I.FUNARY unOp =>
        eBytes[0wxd9, 
             case unOp 
               of I.FABS => 0wxe1 
                | I.FCHS => 0wxe0
                | I.FSQRT => 0wxfa
                | I.FSIN => 0wxfe
                | I.FCOS => 0wxff
                | I.FPTAN => 0wxf2
                | I.FPATAN => 0wxf3
                | I.FDECSTP => 0wxf6
                | I.FINCSTP => 0wxf7
		| _ => error "FUNARY"
               ]
     | I.FXCH{opnd} => encodeST(0wxd9, 25, opnd)

     | I.FUCOM(I.ST n) => encodeST(0wxdd, 28, n)
     | I.FUCOMP(I.ST n) => encodeST(0wxdd, 29, n)
     | I.FUCOMPP => eBytes[0wxda, 0wxe9]
     | I.FCOMI(I.ST n) => encodeST(0wxdb, 0x1e, n)
     | I.FCOMIP(I.ST n) => encodeST(0wxdf, 0x1e, n)
     | I.FUCOMI(I.ST n) => encodeST(0wxdb, 0x1d, n)
     | I.FUCOMIP(I.ST n) => encodeST(0wxdf, 0x1d, n)

     | I.FSTS opnd  => encode(0wxd9, 2, opnd)
     | I.FSTL(I.ST n) => encodeST(0wxdd, 26, n)
     | I.FSTL opnd  => encode(0wxdd, 2, opnd)

     | I.FSTPS opnd => encode(0wxd9, 3, opnd)
     | I.FSTPL(I.ST n) => encodeST(0wxdd, 27, n)
     | I.FSTPL opnd => encode(0wxdd, 3, opnd)
     | I.FSTPT opnd => encode(0wxdb, 7, opnd)

     | I.FLD1   => eBytes[0wxd9,0wxe8]
     | I.FLDL2T => eBytes[0wxd9,0wxe9]
     | I.FLDL2E => eBytes[0wxd9,0wxea]
     | I.FLDPI  => eBytes[0wxd9,0wxeb]
     | I.FLDLG2 => eBytes[0wxd9,0wxec]
     | I.FLDLN2 => eBytes[0wxd9,0wxed]
     | I.FLDZ   => eBytes[0wxd9,0wxee]
     | I.FLDS opnd => encode(0wxd9, 0, opnd)

     | I.FLDL(I.ST n) => encodeST(0wxd9, 24, n)
     | I.FLDL opnd => encode(0wxdd, 0, opnd)

     | I.FILD opnd => encode(0wxdf, 0, opnd)
     | I.FILDL opnd => encode(0wxdb, 0, opnd)
     | I.FILDLL opnd => encode(0wxdf, 5, opnd)

     | I.FNSTSW => eBytes[0wxdf, 0wxe0]

     (* misc *)
     | I.SAHF => eByte(0x9e)
     | _ => error "emitInstr"
  end 
  and emitInstr (I.LIVE _) = Word8Vector.fromList []
    | emitInstr (I.KILL _) = Word8Vector.fromList []
    | emitInstr(I.COPY{k, dst, src, tmp, ...}) = 
      (case k 
       of CB.GP => emitInstrs(Shuffle.shuffle {tmp=tmp, dst=dst, src=src})
	| CB.FP => emitInstrs(Shuffle.shufflefp {tmp=tmp, dst=dst, src=src})
	| _ => error "COPY"
      (*esac*))
    | emitInstr (I.INSTR instr) = emitX86Instr instr
    | emitInstr (I.ANNOTATION{i,...}) = emitInstr i

end
