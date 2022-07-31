(* format-sig.sml
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * Formatted conversion to and from strings.
 *
 * AUTHOR:  John Reppy
 *	    AT&T Bell Laboratories
 *	    Murray Hill, NJ 07974
 *	    jhr@research.att.com
 *)

signature FORMAT =
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
    exception BadFmtList		(* raised on specifier/item type mismatch *)

    val format  : string -> fmt_item list -> string
    val formatf : string -> (string -> unit) -> fmt_item list -> unit

  end (* FORMAT *)
