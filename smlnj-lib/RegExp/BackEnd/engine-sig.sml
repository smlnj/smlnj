(* engine-sig.sml
 *
 * COPYRIGHT (c) 2008 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature REGEXP_ENGINE =
  sig

    type regexp
	(* the type of a compiled regular expression
	 *)

  (* a match specifies the position (as a stream) and the length of the match *)
    type 'a match = {pos : 'a, len : int} MatchTree.match_tree

    val compile : RegExpSyntax.syntax -> regexp
	(* compile a regular expression from the abstract syntax
	 *)

    val find : regexp -> (char,'a) StringCvt.reader -> ('a match, 'a) StringCvt.reader
	(* scan the stream for the first occurence of the regular expression.  The call
	 *
	 *    find re getc strm
	 *
	 * returns NONE if the end of stream is reached without a match.  Otherwise it
	 * returns SOME(match, strm'), where match is the match-tree for the match and
	 * strm' is the stream following the match.
	 *)

    val prefix : regexp ->(char,'a) StringCvt.reader -> ('a match, 'a) StringCvt.reader
	(* attempt to match the stream at the current position with the
	 * regular expression
	 *)

    val match : (RegExpSyntax.syntax * ('a match -> 'b)) list
		  -> (char,'a) StringCvt.reader -> ('b, 'a) StringCvt.reader
	(* attempt to the match the stream at the current position with one of
	 * the abstract syntax representations of regular expressions and trigger
	 * the corresponding action
	 *)

  end
