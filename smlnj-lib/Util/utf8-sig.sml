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

    val maxCodePoint : wchar            (* = 0wx0010FFFF *)
    val replacementCodePoint : wchar    (* = 0wxFFFD *)

    (* raised by some operations when applied to incomplete strings. *)
    exception Incomplete

    (* raised when invalid Unicode/UTF8 characters are encountered in certain
     * situations.
     *)
    exception Invalid

    (* what to do when there is an incomplete UTF-8 sequence *)
    datatype decode_strategy
      (* raise `Incomplete` when the end of input is encountered in the
       * middle of an otherwise acceptable multi-byte encoding,
       * and raise `Invalid` when an invalid byte is encountered.
       * This is the default decoding option.
       *)
      = STRICT
      (* replace invalid input with the REPLACEMENT CHARACTER (U+FFFD)
       * using the W3C standard substitution of maximal subparts algorithm
       * https://www.unicode.org/versions/Unicode16.0.0/core-spec/chapter-3/#G66453
       *)
      | REPLACE

  (** Character operations **)

    (* convert a character reader to a wide-character reader; the reader
     * uses the given decoding strategy to handle invalid input.
     *)
    val getWC : decode_strategy
          -> (char, 'strm) StringCvt.reader
          -> (wchar, 'strm) StringCvt.reader

    (* convert a character reader to a wide-character reader; the reader
     * raises `Incomplete` when the end of input is encountered in the middle
     * of a multi-byte encoding and `Invalid` when an invalid UTF8 encoding
     * is encountered.  This function is equivalent to `getWC STRICT`.
     *)
    val getWCStrict : (char, 'strm) StringCvt.reader -> (wchar, 'strm) StringCvt.reader

    (* same as getWCStrict *)
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

    (* return the number of Unicode characters in a string *)
    val size : string -> int

    (* return the number of Unicode characters in a substring *)
    val size' : substring -> int

    (* return the list of wide characters that are encoded by a string *)
    val explode : string -> wchar list
    (* return the UTF-8 encoded string that represents the list of
     * Unicode code points.
     *)
    val implode : wchar list -> string

    (* map a function over the Unicode characters in the string *)
    val map : (wchar -> wchar) -> string -> string
    (* apply a function to the Unicode characters in the string *)
    val app : (wchar -> unit) -> string -> unit
    (* fold a function over the Unicode characters in the string *)
    val fold : ((wchar * 'a) -> 'a) -> 'a -> string -> 'a
    (* test the predicate against the Unicode characters in the string and
     * return `true` if it is `true` for all of the characters.
     *)
    val all : (wchar -> bool) -> string -> bool
    (* test the predicate against the Unicode characters in the string and
     * return `true` if it is `true` for at least one of the characters.
     *)
    val exists : (wchar -> bool) -> string -> bool

  end
