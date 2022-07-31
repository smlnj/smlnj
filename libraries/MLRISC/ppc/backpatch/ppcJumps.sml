functor PPCJumps 
  (structure Instr: PPCINSTR
   structure Shuffle : PPCSHUFFLE where I = Instr
   structure MLTreeEval : MLTREE_EVAL where T = Instr.T
  ) : SDI_JUMPS = 
struct
  structure I = Instr
  structure C = I.C
  structure Const = I.Constant
  structure CB = CellsBasis

  fun error msg = MLRiscErrorMsg.error("PPCJumps",msg)

  val warn_long_branch =
      MLRiscControl.mkFlag ("ppc-warn-long-branch",
			    "whether to warn about long form of branch")

  val branchDelayedArch = false

  fun isSdi(I.ANNOTATION{i,...}) =isSdi i
    | isSdi(I.LIVE _)		  = true
    | isSdi(I.KILL _)		  = true
    | isSdi(I.COPY _)		  = true
    | isSdi(I.INSTR instr) = let
	fun operand(I.LabelOp _) = true
	  | operand _ = false
      in
	case instr
	of I.L{d, ...} => operand d
	 | I.LF{d, ...} => operand d
	 | I.ST{d, ...} => operand d
	 | I.STF{d, ...} => operand d
	 | I.ARITHI{im, ...} => operand im
	 | I.ROTATEI{sh, ...} => operand sh
	 | I.COMPARE{rb, ...} => operand rb
	 | I.TW{si, ...} => operand si
	 | I.TD{si, ...} => operand si
	 | I.BC{addr, ...} => operand addr
	 | _ => false
      end


  (* max Size is not used for the PPC span dependency analysis. *)
  fun maxSize _ = error "maxSize"

  fun minSize(I.LIVE _)		  = 0
    | minSize(I.KILL _)		  = 0
    | minSize(I.COPY _)		  = 0
    | minSize(I.ANNOTATION{i,...}) = minSize i
    | minSize _ = 4

  fun sdiSize(I.ANNOTATION{i, ...}, labmap, loc) = sdiSize(i, labmap, loc)
    | sdiSize(I.LIVE _, _, _) = 0
    | sdiSize(I.KILL _, _, _) = 0
    | sdiSize(I.COPY{k=CB.GP, src, dst, tmp, ...}, _, _) =
        4 * length(Shuffle.shuffle{tmp=tmp, dst=dst, src=src})
    | sdiSize(I.COPY{k=CB.FP, src, dst, tmp, ...}, _, _) = 
	4 * length(Shuffle.shufflefp{src=src, dst=dst, tmp=tmp})
    | sdiSize(I.INSTR instr, labmap, loc) = let
	fun signed16 n = ~32768 <= n andalso n < 32768
	fun signed12 n = ~2048 <= n andalso n < 2048
	fun signed14 n = ~8192 <= n andalso n < 8192
	fun unsigned16 n = 0 <= n andalso n < 65536
	fun unsigned5 n = 0 <=n andalso n < 32

	fun operand(I.LabelOp le, inRange, lo, hi) = 
	     if inRange(MLTreeEval.valueOf le) then lo else hi
	  | operand _ = error "sdiSize:operand"
      in
	case instr
	of I.L{ld=(I.LBZ | I.LHZ | I.LHA | I.LWZ),d,...} => 
	      operand(d, signed16, 4, 8)
	 | I.L{d,...} => operand(d, signed12, 4, 8)
	 | I.LF{ld=(I.LFS | I.LFD), d, ...} => operand(d, signed16, 4, 8)
	 | I.LF{d, ...} => operand(d, signed12, 4, 8)
	 | I.ST{st=(I.STB | I.STH | I.STW), d, ...} => operand(d, signed16, 4, 8)
	 | I.ST{d, ...} => operand(d, signed12, 4, 8)
	 | I.STF{st=(I.STFS | I.STFD), d, ...} => operand(d, signed16, 4, 8)
	 | I.STF{d, ...} => operand(d, signed12, 4, 8)
	 | I.ARITHI{oper, im, ...} => 
	   (case oper
	    of I.ADDI => operand(im, signed16, 4, 8)
	     | (I.ADDIS | I.SUBFIC | I.MULLI) => operand(im, signed16, 4, 12)
	     | (I.ANDI_Rc | I.ANDIS_Rc | I.ORI | I.ORIS | I.XORI | I.XORIS) => 
		   operand(im, unsigned16, 4, 12)
	     | (I.SRAWI | I.SRADI) => operand(im, unsigned5, 4, 12)
	    (*esac*))
	 | I.ROTATEI{sh, ...} => error "sdiSize:ROTATE"
	 | I.COMPARE{cmp, rb, ...} => 
	   (case cmp
	    of I.CMP => operand(rb, signed16, 4, 12)
	     | I.CMPL => operand(rb, unsigned16, 4, 12)
	   (*esac*))
	 | I.BC{addr=I.LabelOp lexp, ...} => 
	    if signed14((MLTreeEval.valueOf lexp - loc) div 4) then 4 else 8
	| _ => error "sdiSize"
      end
    | sdiSize _ = error "sdiSize"

  fun valueOf(I.LabelOp lexp) = MLTreeEval.valueOf lexp
    | valueOf _ = error "valueOf"

  fun split opnd = let
    val i = valueOf opnd
    val w = Word.fromInt i
    val hi = Word.~>>(w, 0w16)
    val lo = Word.andb(w, 0w65535)
    val (high,low) = 
      if lo <  0w32768 then (hi, lo) else (hi+0w1, lo-0w65536)
  in (Word.toIntX high, Word.toIntX low)
  end

  fun cnv I.ADDI    = I.ADD
    | cnv I.SUBFIC  = I.SUBF 
    | cnv I.MULLI   = I.MULLW 
    | cnv I.ANDI_Rc = I.AND 
    | cnv I.ORI     = I.OR 
    | cnv I.XORI    = I.XOR 
    | cnv I.SRAWI   = I.SRAW 
    | cnv I.SRADI   = I.SRAD 
    | cnv _         = error "cnv"

  fun expand(I.ANNOTATION{i, ...}, size, pos) = expand(i, size, pos)
    | expand(I.LIVE _, _, _) = []
    | expand(I.KILL _, _, _) = []
    | expand(I.COPY{k=CB.GP, src, tmp, dst, ...}, _, _)  = 
       Shuffle.shuffle{src=src, dst=dst, tmp=tmp}
    | expand(I.COPY{k=CB.FP, src, tmp, dst, ...}, _, _)  = 
       Shuffle.shufflefp{src=src, dst=dst, tmp=tmp}
    | expand(instr as I.INSTR i, size, pos) = 
      (case i
        of I.L{ld, rt, ra, d, mem} =>
	   (case size
	    of 4 => [I.l{ld=ld, rt=rt, ra=ra, d=I.ImmedOp(valueOf d), mem=mem}]
	     | 8 => let
		 val (hi,lo) = split d
	       in
		 [I.arithi{oper=I.ADDIS, rt=C.asmTmpR, ra=ra, im=I.ImmedOp hi},
		  I.l{ld=ld, rt=rt, ra=C.asmTmpR, d=I.ImmedOp lo, mem=mem}]
	       end
	     | _ => error "expand:L"
	   (*esac*))
	 | I.LF{ld, ft, ra, d, mem} =>
	   (case size
	    of 4 => [I.lf{ld=ld, ft=ft, ra=ra, d=I.ImmedOp(valueOf d), mem=mem}]
	     | 8 => let
		 val (hi,lo) = split d
	       in
		 [I.arithi{oper=I.ADDIS, rt=C.asmTmpR, ra=ra, im=I.ImmedOp hi},
		  I.lf{ld=ld, ft=ft, ra=C.asmTmpR, d=I.ImmedOp lo, mem=mem}]
	       end
	     | _ => error "expand:LF"
	   (*esac*))
	 | I.ST{st, rs, ra, d, mem} =>
	   (case size 
	    of 4 => [I.st{st=st, rs=rs, ra=ra, d=I.ImmedOp(valueOf d), mem=mem}]
	     | 8 => let
		   val (hi,lo) = split d
		 in
		   [I.arithi{oper=I.ADDIS, rt=C.asmTmpR, ra=ra, im=I.ImmedOp hi},
		    I.st{st=st, rs=rs, ra=C.asmTmpR, d=I.ImmedOp lo, mem=mem}]
		 end
	     | _ => error "expand:ST"
	   (*esac*))
	 | I.STF{st, fs, ra, d, mem} =>
	   (case size 
	    of 4 => [I.stf{st=st, fs=fs, ra=ra, d=I.ImmedOp(valueOf d), mem=mem}]
	     | 8 => let
		   val (hi,lo) = split d
		 in
		   [I.arithi{oper=I.ADDIS, rt=C.asmTmpR, ra=ra, im=I.ImmedOp hi},
		    I.stf{st=st, fs=fs, ra=C.asmTmpR, d=I.ImmedOp lo, mem=mem}]
		 end
	     | _ => error "expand:STF"
	   (*esac*))
	 | I.ARITHI{oper, rt, ra, im} => 
	   (case size
	    of 4 => [I.arithi{oper=oper, rt=rt, ra=ra, im=I.ImmedOp(valueOf im)}]
	     | 8 => let val (hi, lo) = split im (* must be ADDI *)
		    in [I.arithi{oper=I.ADDIS, rt=rt, ra=ra, im=I.ImmedOp hi},
			I.arithi{oper=I.ADDI, rt=rt, ra=rt, im=I.ImmedOp lo}]
		    end
	     | 12 => 
	       let val (hi,lo) = split im
	       in [I.arithi{oper=I.ADDIS, rt=C.asmTmpR, ra=C.Reg CellsBasis.GP 0, 
			    im=I.ImmedOp hi},
		   I.arithi{oper=I.ADDI,rt=C.asmTmpR,ra=C.asmTmpR,im=I.ImmedOp lo},
		   I.arith{oper=cnv oper, rt=rt, ra=ra, rb=C.asmTmpR, OE=false, 
			   Rc=(oper = I.ANDI_Rc)}]
	       end
	     | _ => error "ARITHI"
	   (*esac*))
	 | I.BC{bo, bf, bit, fall, addr, LK} => 
	   (case size
	     of 4 => [instr]
	      | 8 => let
		  val newBO = 
		    (case bo 
		     of I.TRUE => I.FALSE
		      | I.FALSE => I.TRUE
		      | I.ALWAYS => error "expand:newBO:BC"
		      | I.COUNTER{eqZero, cond} => error "expand:newBO:COUNTER"
		    (*esac*))
		in 
		  if !warn_long_branch then
		      print("emiting long form of branch"  ^ "\n")
		  else ();
		 [I.bc{bo=newBO, bf=bf, bit=bit, addr=fall, fall=fall, LK=false},
		  I.b{addr=addr, LK=LK}]
		end
	      | _ => error "expand:BC"
	  (*esac*))
	 (* The other span dependent instructions are not generated *)
	 | I.COMPARE _ => error "expand:COMPARE"
	 | _ => error "expand"
     (*esac*))
    | expand _ = error "expand"
end
