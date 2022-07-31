(* list-format-sig.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature LIST_FORMAT =
  sig

    val fmt : {
	    init : string,
	    sep : string,
	    final : string,
	    fmt : 'a -> string
	  } -> 'a list -> string
	(* given an initial string (init), a separator (sep), a terminating
	 * string (final), and an item formating function (fmt), return a list
	 * formatting function.  The list ``[a, b, ..., c]'' gets formated as
	 * ``init ^ (fmt a) ^ sep ^ (fmt b) ^ sep ^ ... ^ sep ^ (fmt c) ^ final.''
	 *)

    val listToString : ('a -> string) -> 'a list -> string
	(* formats a list in SML style (i.e., init="[", sep=",", final="]"). *)

    val scan : {
	    init : string,
	    sep : string,
	    final : string,
	    scan : (char, 'b) StringCvt.reader -> ('a, 'b) StringCvt.reader
	  } -> (char, 'b) StringCvt.reader -> ('a list, 'b) StringCvt.reader
	(* given an expected initial string, a separator, a terminating
	 * string, and an item scanning function, return a function that
	 * scans a string for a list of items.  Whitespace is ignored.
	 * If the input string has the incorrect syntax, then NONE is returned.
	 *)

  end; (* LIST_FORMAT *)
