(* regexp-sig.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Main signature for regular expressions.
 *)

signature REGEXP =
  sig

    type regexp
	(* the type of a compiled regular expression
	 *)

  (* a match specifies the position (as a stream) and the length of the match *)
    type 'a match = {pos : 'a, len : int} MatchTree.match_tree

    exception CannotParse
	(* raised by compileString and match on syntax errors *)

    val compile : (char,'a) StringCvt.reader -> (regexp, 'a) StringCvt.reader
	(* read an external representation of a regular expression from a stream
	 *)

    val compileString : string -> regexp
	(* read an external representation of a regular expression from a string
	 *)

    val find : regexp -> (char,'a) StringCvt.reader -> ('a match, 'a) StringCvt.reader
        (* scan the stream for the first occurence of the regular expression
	 *)

    val prefix : regexp -> (char,'a) StringCvt.reader -> ('a match, 'a) StringCvt.reader
        (* attempt to match the stream at the current position with the
	 * regular expression
	 *)

    val match : (string * ('a match -> 'b)) list
	  -> (char,'a) StringCvt.reader -> ('b, 'a) StringCvt.reader
        (* attempt to match the stream at the current position with one
	 * of the external representations of regular expressions and trigger
	 * the corresponding action
	 *)

  end
