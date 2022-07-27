(* ml-lex-input.sml
 *
 * COPYRIGHT (c) 2005
 * John Reppy (http://www.cs.uchicago.edu/~jhr)
 * Aaron Turon (adrassi@gmail.com)
 * All rights reserved.
 *
 * Driver for ml-lex input format.
 *)

structure MLLexInput : INPUT =
  struct

    structure MLLexLrVals =
      MLLexLrValsFun(structure Token = LrParser.Token)
    structure MLLexLex =
      MLLexLexFun(structure Tok = MLLexLrVals.Tokens)
    structure MLLexParser =
      Join(structure ParserData = MLLexLrVals.ParserData
           structure Lex = MLLexLex
           structure LrParser = LrParser)

    fun parseFile fname = let
	  val anyErrors = ref false
	  fun parseErr (msg, line, _) = (
		print(concat["** Syntax error [", fname, ":", Int.toString line, "]: ", msg, "\n"]);
		anyErrors := true)
	  val strm = TextIO.openIn fname
	  val lexer = MLLexParser.makeLexer (fn n => TextIO.inputN (strm, n))
	  val (spec, _) = MLLexParser.parse(15, lexer, parseErr, ())
	  in
	    TextIO.closeIn strm;
	    if !anyErrors then NONE else SOME spec
	  end

  end
