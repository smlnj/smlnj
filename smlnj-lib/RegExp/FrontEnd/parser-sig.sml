(* parser-sig.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature REGEXP_PARSER =
  sig

    val scan : (char, 'a) StringCvt.reader
	          -> (RegExpSyntax.syntax, 'a) StringCvt.reader
	(* read an external representation of a regular expression
	 * from the stream and return an abstract syntax representation
	 *)

  end

