(* word.sig
 *
 * COPYRIGHT (c) 2016 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature WORD_2004 =
  sig
    eqtype word

    val wordSize : int

    val toLarge   : word -> LargeWord.word
    val toLargeX  : word -> LargeWord.word
    val fromLarge : LargeWord.word -> word

  (* same as above, but deprecated *)
    val toLargeWord   : word -> LargeWord.word
    val toLargeWordX  : word -> LargeWord.word
    val fromLargeWord : LargeWord.word -> word

    val toLargeInt   : word -> LargeInt.int
    val toLargeIntX  : word -> LargeInt.int
    val fromLargeInt : LargeInt.int -> word

    val toInt   : word -> int
    val toIntX  : word -> int
    val fromInt : int -> word

    val orb  : word * word -> word
    val xorb : word * word -> word
    val andb : word * word -> word
    val notb : word -> word

    val << : (word * Word.word) -> word
    val >> : (word * Word.word) -> word
    val ~>> : (word * Word.word) -> word

    val + : word * word -> word
    val - : word * word -> word
    val * : word * word -> word
    val div : word * word -> word
    val mod : word * word -> word

    val compare : (word * word) -> order
    val >  : word * word -> bool
    val >= : word * word -> bool
    val <  : word * word -> bool
    val <= : word * word -> bool

    val ~   : word -> word
    val min : (word * word) -> word
    val max : (word * word) -> word

    val scan :
	  StringCvt.radix -> (char, 'a) StringCvt.reader
	    -> (word, 'a) StringCvt.reader
    val fromString : string -> word option

    val fmt : StringCvt.radix -> word -> string
    val toString   : word -> string

  end

signature WORD_2026 =
  sig
    include WORD_2004

    val rotateL : word * Word.word -> word
    val rotateR : word * Word.word -> word

    val countZeros : word -> int
    val countOnes : word -> int

    val countLeadingZeros : word -> int
    val countLeadingOnes : word -> int

    val countTrailingZeros : word -> int
(*
    val countTrailingOnes : word -> int
*)

    val isPowerOf2 : word -> bool
    val ceilLog2 : word -> Word.word

  end

signature WORD = WORD_2026
