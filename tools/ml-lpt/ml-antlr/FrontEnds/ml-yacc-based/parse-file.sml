(* parse-file.sml
 *
 * COPYRIGHT (c) 2006 
 * John Reppy (http://www.cs.uchicago.edu/~jhr)
 * Aaron Turon (http://www.cs.uchicago.edu/~adrassi)
 * All rights reserved.
 *
 * Driver for the parser.
 *)

structure ParseFile = 
  struct

  (* glue together the lexer and parser *)
    structure LLKLrVals = MLYLrValsFun (structure Token = LrParser.Token)
    structure LLKLex = MLYLexFn (structure Tok = LLKLrVals.Tokens)
    structure LLKParser = JoinWithArg(
      structure ParserData = LLKLrVals.ParserData
      structure Lex = LLKLex
      structure LrParser = LrParser)

  (* parse a file, returning a parse tree *)
    fun parse' (startGrm, filename) = let
	  val _ = Err.status ("parsing " ^ filename)
	  val file = TextIO.openIn filename
	  fun get n = TextIO.inputN (file, n)
	  val lexer = LLKParser.makeLexer get (Err.lexErr filename)
	  in
	    #1(LLKParser.parse
		 (15, lexer, Err.parseErr filename, 
		  (Err.parseErr filename, startGrm, parse')))
	      before TextIO.closeIn file
	  end

    fun parse filename = parse'(GrammarSyntax.mkGrammar(), filename)

  end