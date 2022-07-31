(* parser-comb-sig.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *
 * Parser combinators over readers.  These are modeled after the Haskell
 * combinators of Hutton and Meijer.  The main difference is that they
 * return a single result, instead of a list of results.  This means that
 * "or" is a committed choice; once one branch succeeds, the others will not
 * be enabled.  While this is somewhat limiting, for many applications it
 * will not be a problem.  For more substantial parsing problems, one should
 * use ML-Yacc and/or ML-Lex.
 *)

signature PARSER_COMB =
  sig

    type ('a, 'strm) parser =
	  (char, 'strm) StringCvt.reader -> ('a, 'strm) StringCvt.reader

    val result : 'a -> ('a, 'strm) parser

    val failure : ('a, 'strm) parser

    val wrap : (('a, 'strm) parser * ('a -> 'b)) -> ('b, 'strm) parser

    val seq : (('a, 'strm) parser * ('b, 'strm) parser) -> (('a * 'b), 'strm) parser
    val seqWith : (('a * 'b) -> 'c)
	  -> (('a, 'strm) parser * ('b, 'strm) parser)
	    -> ('c, 'strm) parser

    val bind : (('a, 'strm) parser * ('a -> ('b, 'strm) parser))
	  -> ('b, 'strm) parser

    val eatChar : (char -> bool) -> (char, 'strm) parser

    val char   : char -> (char, 'strm) parser
    val string : string -> (string, 'strm) parser

    val skipBefore : (char -> bool) -> ('a, 'strm) parser -> ('a, 'strm) parser

    val or : (('a, 'strm) parser * ('a, 'strm) parser) -> ('a, 'strm) parser
    val or' : ('a, 'strm) parser list -> ('a, 'strm) parser

    val zeroOrMore : ('a, 'strm) parser -> ('a list, 'strm) parser
    val oneOrMore  : ('a, 'strm) parser -> ('a list, 'strm) parser

    val option : ('a, 'strm) parser -> ('a option, 'strm) parser
    val join   : ('a option, 'strm) parser -> ('a, 'strm) parser

    val token : (char -> bool) -> (string, 'strm) parser
	  (* parse a token consisting of characters satisfying the predicate.
	   * If this succeeds, then the resulting string is guaranteed to be
	   * non-empty.
	   *)

  end;
