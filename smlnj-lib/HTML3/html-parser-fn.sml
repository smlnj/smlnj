(* html-parser-fn.sml
 *
 * COPYRIGHT (c) 1996 AT&T REsearch.
 *
 * This glues the lexer and parser together.
 *)

functor HTMLParserFn (Err : HTML_ERROR) : sig

    val parseFile : string -> HTML.html

  end = struct

    structure TIO = TextIO

    structure HTMLAttrs = HTMLAttrsFn(Err)
    structure HTMLLrVals = HTMLLrValsFn(
	structure Token = LrParser.Token
	structure HTMLAttrs = HTMLAttrs)
    structure Lex = HTMLLexFn(
	structure Err = Err
	structure Tokens = HTMLLrVals.Tokens
	structure HTMLAttrs = HTMLAttrs)
    structure Parser = JoinWithArg(
	structure Lex= Lex
	structure LrParser = LrParser
	structure ParserData = HTMLLrVals.ParserData)
    structure CheckHTML = CheckHTMLFn(Err)

    fun parseFile fname = let
	(* build a context to hand to the HTMLAttrs build functions. *)
	  fun ctx lnum = {file = SOME fname, line=lnum}
	  fun lexError (msg, lnum, _) =
		Err.lexError {file = SOME fname, line = lnum} msg
	  fun syntaxError (msg, lnum, _) =
		Err.syntaxError {file = SOME fname, line = lnum} msg
	  val inStrm = TIO.openIn fname
	  fun close () = TIO.closeIn inStrm
	  val lexer = Parser.makeLexer (fn n => TIO.inputN(inStrm, n))
		(lexError, SOME fname)
	  val (result, _) = Parser.parse (
		15,	(* lookahead *)
		lexer,
		syntaxError,
		ctx)
	  in
	    CheckHTML.check (ctx 0) result
	      handle ex => (close(); raise ex)
	    close();
	    result
	  end

  end;
