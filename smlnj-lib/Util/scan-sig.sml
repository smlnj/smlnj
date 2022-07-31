(* scan-sig.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * C-style conversions from string representations.
 *
 * TODO: replace the fmt_item type with a datatype that reflects the more
 * limited set of possible results.  Also replace the shared implementation
 * of format-string processing with processing that is specific to scanning.
 *)

signature SCAN =
  sig

    datatype fmt_item
      = ATOM of Atom.atom
      | LINT of LargeInt.int
      | INT of Int.int
      | LWORD of LargeWord.word
      | WORD of Word.word
      | WORD8 of Word8.word
      | BOOL of bool
      | CHR of char
      | STR of string
      | REAL of Real.real
      | LREAL of LargeReal.real
      | LEFT of (int * fmt_item)	(* left justify in field of given width *)
      | RIGHT of (int * fmt_item)	(* right justify in field of given width *)

    exception BadFormat			(* bad format string *)

    val sscanf : string -> string -> fmt_item list option
    val scanf  : string -> (char, 'a) StringCvt.reader
	  -> (fmt_item list, 'a) StringCvt.reader

  end (* SCAN *)
