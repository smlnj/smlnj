(* word32.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Word32Imp : WORD =
  struct
    structure W32 = InlineT.Word32

    type word = Word32.word

    val wordSize = 32

    val toLarge   : word -> LargeWord.word = W32.toLarge
    val toLargeX  : word -> LargeWord.word = W32.toLargeX
    val fromLarge : LargeWord.word -> word = W32.fromLarge

  (* same as above, but deprecated *)
    val toLargeWord = toLarge
    val toLargeWordX = toLargeX
    val fromLargeWord = fromLarge

    val toLargeInt    = W32.toLargeInt
    val toLargeIntX   = W32.toLargeIntX
    val fromLargeInt  = W32.fromLargeInt

    val toInt   : word -> int = W32.toInt
    val toIntX  : word -> int = W32.toIntX
    val fromInt : int -> word = W32.fromInt

    val orb  : word * word -> word = W32.orb
    val xorb : word * word -> word = W32.xorb
    val andb : word * word -> word = W32.andb
    val notb : word -> word = W32.notb

    val op * : word * word -> word = W32.*
    val op + : word * word -> word = W32.+
    val op - : word * word -> word = W32.-
    val op div : word * word -> word = W32.div
    val op mod : word * word -> word = W32.mod

    fun compare (w1, w2) =
	  if (W32.<(w1, w2)) then LESS
	  else if (W32.>(w1, w2)) then GREATER
	  else EQUAL
    val op > : word * word -> bool = W32.>
    val op >= : word * word -> bool = W32.>=
    val op < : word * word -> bool = W32.<
    val op <= : word * word -> bool = W32.<=

    val <<   = W32.chkLshift
    val >>   = W32.chkRshiftl
    val ~>>  = W32.chkRshift

    val ~   : word -> word = ~
    val min : word * word -> word = W32.min
    val max : word * word -> word = W32.max

    val fmt = NumFormat32.fmtWord
    val toString = fmt StringCvt.HEX

    val scan = NumScan32.scanWord
    val fromString = PreBasis.scanString (scan StringCvt.HEX)

  (* added for Basis Library proposal 2016-001 *)

    val popCount = W32PopCount.popCount

  end  (* structure Word32 *)
