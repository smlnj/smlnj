(* word8.sml
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

structure Word8Imp : WORD =
  struct

    structure W8 = InlineT.Word8
    structure LW = LargeWordImp

    type word = Word8.word		(* tagged word *)

    val wordSize = 8
    val wordSizeW = 0w8

    val toInt   : word -> int = W8.toInt
    val toIntX  : word -> int = W8.toIntX
    val fromInt : int -> word = W8.fromInt

    val toLarge : word -> LargeWord.word = W8.toLarge
    val toLargeX = W8.toLargeX
    val fromLarge = W8.fromLarge

  (* same as above, but deprecated *)
    val toLargeWord = toLarge
    val toLargeWordX = toLargeX
    val fromLargeWord = fromLarge

    val toLargeInt  : word -> LargeInt.int = W8.toLargeInt
    val toLargeIntX : word -> LargeInt.int = W8.toLargeIntX
    val fromLargeInt: LargeInt.int -> word = W8.fromLargeInt

    val << : word * Word.word -> word = W8.chkLshift
    val >> : word * Word.word -> word = W8.chkRshiftl
    val ~>> : word * Word.word -> word = W8.chkRshift

    val orb  : word * word -> word = W8.orb
    val xorb : word * word -> word = W8.xorb
    val andb : word * word -> word = W8.andb
    val notb : word -> word = W8.notb

    val op * : word * word -> word = op *
    val op + : word * word -> word = op +
    val op - : word * word -> word = op -
    val op div : word * word -> word = op div
    val op mod : word * word -> word = op mod

    fun compare (w1, w2) =
	  if (W8.<(w1, w2)) then LESS
	  else if (W8.>(w1, w2)) then GREATER
	  else EQUAL
    val op > : word * word -> bool = op >
    val op >= : word * word -> bool = op >=
    val op < : word * word -> bool = op <
    val op <= : word * word -> bool = op <=

    val ~ : word -> word = ~
    val min : word * word -> word = W8.min
    val max : word * word -> word = W8.max

    fun fmt radix = let
	  val fmt' = NumFormat.fmtWord radix
	  in
	    fn b => fmt' (InlineT.Word.fromLarge (InlineT.Word8.toLarge b))
	  end
    val toString = fmt StringCvt.HEX

    fun scan radix = let
	  val scanLarge = NumScan32.scanWord radix
	  fun scan getc cs = (case (scanLarge getc cs)
		 of NONE => NONE
		  | (SOME(w, cs')) => if InlineT.Word32.>(w, 0w255)
		      then raise Overflow
		      else SOME(fromLargeWord(InlineT.Word32.toLarge w), cs')
		(* end case *))
	  in
	    scan
	  end
    val fromString = PreBasis.scanString (scan StringCvt.HEX)

  (* added for Basis Library proposal 2026-001 *)
    val rotateL  : word * Word.word -> word = W8.rotateL
    val rotateR : word * Word.word -> word = W8.rotateR

    val countZeros = W8.cntZeros
    val countOnes = W8.cntOnes

    val countLeadingZeros = W8.cntLeadingZeros
    val countLeadingOnes = W8.cntLeadingOnes

    val countTrailingZeros = W8.cntTrailingZeros
(*
    val countTrailingOnes = W8.cntTrailingOnes
*)

    val isPowerOf2 = W8.isPowOf2
    val ceilLog2 = W8.ceilLog2

  end  (* structure Word8 *)
