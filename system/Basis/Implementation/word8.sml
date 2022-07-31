(* word8.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
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

    val toLargeInt  : word -> LargeInt.int = LW.toLargeInt o toLarge
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

    fun fmt radix = (NumFormat32.fmtWord radix) o InlineT.Word32.fromLarge o toLargeWord
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

  (* added for Basis Library proposal 2016-001 *)

    fun popCount w = let
        (* pop count of each 2 bits into those 2 bits *)
          val w = w - W8.andb(W8.rshiftl(w, 0w1), 0wx55)
        (* pop count of each 4 bits into those 4 bits *)
          val w = W8.andb(w, 0wx33) + W8.andb(W8.rshiftl(w, 0w2), 0wx33)
        (* pop count of each 8 bits into those 8 bits *)
          val w = w + W8.rshiftl(w, 0w4)
	  in
          (* mask out result *)
	    W8.toIntX (W8.andb(w, 0wx0F))
	  end

  end  (* structure Word8 *)
