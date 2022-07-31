(* word.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Default word structure (31 bits) for 32-bit targets.
 *)

structure WordImp : WORD =
  struct
    structure Word = InlineT.Word

    type word = word

    val wordSize = 31

    val toLarge   : word -> LargeWord.word = Word.toLarge
    val toLargeX  : word -> LargeWord.word = Word.toLargeX
    val fromLarge : LargeWord.word -> word = Word.fromLarge

  (* same as above, but deprecated *)
    val toLargeWord = toLarge
    val toLargeWordX = toLargeX
    val fromLargeWord = fromLarge

    val toLargeInt : word -> LargeInt.int = Word.toLargeInt
    val toLargeIntX  : word -> LargeInt.int = Word.toLargeIntX
    val fromLargeInt : LargeInt.int -> word = Word.fromLargeInt

    val toInt   : word -> int = Word.toInt
    val toIntX  : word -> int = Word.toIntX
    val fromInt : int -> word = Word.fromInt

    val orb  : word * word -> word = Word.orb
    val xorb : word * word -> word = Word.xorb
    val andb : word * word -> word = Word.andb
    val notb : word -> word = Word.notb

    val op * : word * word -> word = Word.*
    val op + : word * word -> word = Word.+
    val op - : word * word -> word = Word.-
    val op div : word * word -> word = Word.div
    val op mod : word * word -> word = Word.mod

    val <<  : word * word -> word = Word.chkLshift
    val >>  : word * word -> word = Word.chkRshiftl
    val ~>> : word * word -> word = Word.chkRshift

    fun compare (w1, w2) =
	  if (Word.<(w1, w2)) then LESS
	  else if (Word.>(w1, w2)) then GREATER
	  else EQUAL
    val op > : word * word -> bool = Word.>
    val op >= : word * word -> bool = Word.>=
    val op < : word * word -> bool = Word.<
    val op <= : word * word -> bool = Word.<=

    val ~ : word -> word = ~
    val min : word * word -> word = Word.min
    val max : word * word -> word = Word.max

    fun fmt radix = (NumFormat32.fmtWord radix) o Word.toWord32
    val toString = fmt StringCvt.HEX

    fun scan radix = let
	  val scanLarge = NumScan32.scanWord radix
	  fun scan getc cs = (case (scanLarge getc cs)
		 of NONE => NONE
		  | (SOME(w, cs')) => if InlineT.Word32.>(w, 0wx7FFFFFFF)
		      then raise Overflow
		      else SOME(Word.fromWord32 w, cs')
		(* end case *))
	  in
	    scan
	  end
    val fromString = PreBasis.scanString (scan StringCvt.HEX)

  (* added for Basis Library proposal 2016-001 *)

    fun popCount w = W32PopCount.popCount (Word.toWord32 w)

  end  (* structure WordImp *)
