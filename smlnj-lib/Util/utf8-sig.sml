(* utf8-sig.sml
 *
 * COPYRIGHT (c) 2020 John Reppy (http://www.cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Routines for working with UTF8 encoded strings.
 *)

signature UTF8 =
  sig

    type wchar = word

    val maxCodePoint : wchar	(* = 0wx0010FFFF *)

    (* raised by some operations when applied to incomplete strings. *)
    exception Incomplete

    (* raised when invalid Unicode/UTF8 characters are encountered in certain
     * situations.
     *)
    exception Invalid

  (** Character operations **)

    (* convert a character reader to a wide-character reader; the reader
     * raises `Incomplete` when the end of input is encountered in the middle
     * of a multi-byte encoding and `Invalid` when an invalid UTF8 encoding
     * is encountered.
     *)
    val getu : (char, 'strm) StringCvt.reader -> (wchar, 'strm) StringCvt.reader

    (* return the UTF8 encoding of a wide character; raises Invalid if the
     * wide character is larger than the maxCodePoint, but does not do any
     * other validity checking.
     *)
    val encode : wchar -> string

    val isAscii : wchar -> bool
    val toAscii : wchar -> char		(* truncates to 7-bits *)
    val fromAscii : char -> wchar	(* truncates to 7-bits *)

    (* return a printable string representation of a wide character; raises
     * Invalid if the wide character is larger than the maxCodePoint, but
     * does not do any other validity checking.
     *)
    val toString : wchar -> string

  (** String operations **)

    val size : string -> int
	(* return the number of Unicode characters in a string *)

    val size' : substring -> int
	(* return the number of Unicode characters in a substring *)

    val explode : string -> wchar list
	(* return the list of wide characters that are encoded by a string *)
    val implode : wchar list -> string
	(* return the UTF-8 encoded string that represents the list of
	 * Unicode code points.
	 *)

    val map : (wchar -> wchar) -> string -> string
	(* map a function over the Unicode characters in the string *)
    val app : (wchar -> unit) -> string -> unit
	(* apply a function to the Unicode characters in the string *)
    val fold : ((wchar * 'a) -> 'a) -> 'a -> string -> 'a
	(* fold a function over the Unicode characters in the string *)
    val all : (wchar -> bool) -> string -> bool
    val exists : (wchar -> bool) -> string -> bool

  end
