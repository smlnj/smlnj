(* word64.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Implementation of Word64 for 32-bit targets.
 *)

structure Word64Imp : sig

    include WORD

    val extern : word -> Word32.word * Word32.word
    val intern : Word32.word * Word32.word -> word

  end = struct

    structure W64 = InlineT.Word64
    structure W32 = Word32Imp

    type word = Word64.word	(* from Basis/TypesOnly *)

    fun unimplemented _ = raise Fail "unimplemented"

    val extern = W64.extern
    val intern = W64.intern

    val wordSize = 64

    val toLarge  = W64.toLarge
    val toLargeX = W64.toLargeX
    val fromLarge = W64.fromLarge

  (* same as above, but deprecated *)
    val toLargeWord = toLarge
    val toLargeWordX = toLargeX
    val fromLargeWord = fromLarge

    val toInt = W64.toInt
    val toIntX = W64.toIntX
    val fromInt = W64.fromInt

    val toLargeInt = W64.toLargeInt
    val toLargeIntX = W64.toLargeIntX
    val fromLargeInt = W64.fromLargeInt

    val op * : word * word -> word = W64.*
    val op + : word * word -> word = W64.+
    val op - : word * word -> word = W64.-
    val op div : word * word -> word = W64.div
    val op mod : word * word -> word = W64.mod

    val orb  : word * word -> word = W64.orb
    val xorb : word * word -> word = W64.xorb
    val andb : word * word -> word = W64.andb
    val notb : word -> word = W64.notb

    val <<   = W64.chkLshift
    val >>   = W64.chkRshiftl
    val ~>>  = W64.chkRshift

    fun compare (w1, w2) =
	  if (W64.<(w1, w2)) then LESS
	  else if (W64.>(w1, w2)) then GREATER
	  else EQUAL

    val op > : word * word -> bool = W64.>
    val op >= : word * word -> bool = W64.>=
    val op < : word * word -> bool = W64.<
    val op <= : word * word -> bool = W64.<=

    val ~   : word -> word = ~
    val min : word * word -> word = W64.min
    val max : word * word -> word = W64.max

    val fmt = NumFormat64.fmtWord
    val toString = fmt StringCvt.HEX

    val scan = NumScan64.scanWord
    val fromString = PreBasis.scanString (scan StringCvt.HEX)

  (* added for Basis Library proposal 2016-001 *)

    fun popCount w = let
	  val (hi, lo) = extern w
	  in
	    InlineT.Int.fast_add(W32PopCount.popCount hi, W32PopCount.popCount lo)
	  end

  end
