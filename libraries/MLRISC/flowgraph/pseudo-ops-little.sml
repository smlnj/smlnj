(* pseudo-ops-little.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Subset of pseudo-ops functions that are little endian sensitive
 *)

functor PseudoOpsLittle (

    structure T : MLTREE
    structure MLTreeEval : MLTREE_EVAL
			   where T = T
    val icache_alignment : int		(* cache line size *)
    val max_alignment : int option	(* maximum alignment for internal labels *)
    val nop: {sz:int, en:Word32.word}	(* encoding for noop *)

  ) : PSEUDO_OPS_ENDIAN = struct

  structure W = Word
  structure T = T
  structure PB = PseudoOpsBasisTyp
  type 'a pseudo_op = (T.labexp, 'a) PB.pseudo_op

  fun error msg = MLRiscErrorMsg.error ("PseudoOpsLittle.", msg)

  val >> = Word.>>
  val ~>> = Word.~>>
  val & = Word.andb
  infix >>  ~>>   &

  (* return loc aligned at bndry *)
  fun align(loc, bndry) = let
	val mask = W.fromInt bndry - 0w1
	in W.toIntX(W.andb(W.fromInt loc + mask, W.notb mask))
	end

  (* bytes of padding required *)
  fun padding(loc, bndry) = align(loc, bndry) - loc

  fun pow2(x, 0) = x
    | pow2(x, n) = pow2(x * 2, n-1)

  fun bytesIn sz = Int.quot(sz, 8)

  fun sizeOf(pOp, loc) = (case pOp
	 of PB.ALIGN_SZ n => padding(loc, pow2(1, n))
	  | PB.ALIGN_ENTRY => padding(loc, icache_alignment)
	  | PB.ALIGN_LABEL => let
	      val pad = padding(loc, icache_alignment)
	      in
		case max_alignment
		 of NONE => pad
		  | SOME m => if pad <= m then pad else 0
	      end
	  | PB.INT{sz, i} => length i * bytesIn sz
	  | PB.ASCII s => String.size s
	  | PB.ASCIIZ s => String.size s + 1
	  | PB.SPACE(sz)  => sz
	  | PB.FLOAT{sz, f} => length f * bytesIn sz
	  | PB.EXT _ => error "sizeOf: EXT"
	  | _ => 0
	(*esac*))


  fun emitValue {pOp, loc, emit} = let
	val itow  = W.fromInt
	fun emitByte n = emit(Word8.fromLargeWord(W.toLargeWord n))
	fun emitWord n = (emitByte(n & 0w255); emitByte((n >> 0w8) & 0w255))
	fun emitLongX n = let
	      val w = itow n
	      in
		emitWord(w & 0w65535);
		emitWord(w ~>> 0w16)
	      end
	fun emitQuadX n = (
	      emitLongX n;
	      if n < 0 then emitLongX ~1 else emitLongX 0)
	local
	  val {sz, en} = nop
	  val toWord = W.fromLargeInt o Word32.toLargeIntX
	in
	fun emitNop () = (case sz
	       of 1 => emitByte (toWord en)
		| 2 => emitWord (toWord en)
		| 4 => (emitWord(toWord(Word32.andb(en, 0w65535)));
			emitWord(toWord(Word32.>>(en, 0w16))))
		| n => error ("emitNop: sz = " ^ Int.toString n)
	      (* end case *))
	fun insertNops 0 = ()
	  | insertNops n = if n >= sz
	      then (emitNop(); insertNops(n-sz))
	      else error "insertNops"
	end (* local *)
	fun align(loc, bndry) = let
	      val bndry = Word.fromInt bndry
	      val mask = bndry - 0w1
	      in
		case W.andb(itow(loc), mask)
		 of 0w0 => ()
		  |  w => let
		       val padSz = (bndry - w)
		     in insertNops(Word.toInt padSz)
		     end
		(*esac*)
	      end
	val {ccexp, rexp} = MLTreeEval.eval {
		const = IntInf.fromInt o T.Constant.valueOf,
		label = Label.addrOf
	      }
	in
	  case pOp
	   of PB.ALIGN_SZ n  => insertNops(sizeOf(pOp, loc))
	    | PB.ALIGN_ENTRY => insertNops(sizeOf(pOp, loc))
	    | PB.ALIGN_LABEL => insertNops(sizeOf(pOp, loc))
	    | PB.INT{sz, i}  => let
		val ints = map (IntInf.toInt o rexp) i
		in
		  case sz
		   of  8 => app (emitByte o itow) ints
		    | 16 => app (emitWord o itow) ints
		    | 32 => app emitLongX ints
		    | 64 => app emitQuadX ints
		    |  _ => error "emitValue: INT 64"
		  (*esac*)
		end
	    | PB.ASCII s => app (emit o Word8.fromInt o Char.ord) (String.explode s)
	    | PB.ASCIIZ s => (emitValue{pOp=PB.ASCII s, loc=loc, emit=emit}; emit 0w0)
	    | PB.FLOAT{sz, f} => error "emitValue: FLOAT - not implemented"
	    | PB.EXT _ => error "emitValue: EXT"
	    | PB.SPACE _ => error "emitValue: SPACE"
	    | _ => ()
	end (* emitValue *)

end
