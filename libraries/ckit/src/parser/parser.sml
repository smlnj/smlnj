(* Copyright (c) 1998 by Lucent Technologies *)

structure Parser : PARSER = 
struct

  (* instantiate parser structures *)
  structure LrVals = LrValsFun(structure Token = LrParser.Token)

  structure TokTable = TokenTable(structure Tokens = LrVals.Tokens)

  structure CLex = CLexFun(structure Tokens = LrVals.Tokens
			   structure TokTable = TokTable)

  structure P = JoinWithArg(structure ParserData = LrVals.ParserData
			    structure Lex = CLex
			    structure LrParser = LrParser)

  fun parseFile errState f = 
    let val _ = TypeDefs.reset()

	val sourceMap = SourceMap.newmap{srcFile=f}

	fun lexErr (p1, p2, msg) =
	  Error.error (errState, SourceMap.location sourceMap (p1, p2), msg)
	fun lexWarn (p1, p2, msg) =
	  Error.warning (errState, SourceMap.location sourceMap (p1, p2), msg)
	fun parseErr (msg, p1, p2) =
	  Error.error (errState, SourceMap.location sourceMap (p1, p2), msg)

	fun inputc instrm i = TextIO.inputN(instrm,i)

	val lexArg = {comLevel = ref 0,
		      sourceMap = sourceMap,
		      charlist = ref ([] : string list),
		      stringstart = ref 0,
		      errWarn = {err=lexErr, warn = lexWarn}
		      }
	val instrm = TextIO.openIn f
	val lookahead = 15

	val lexer = LrParser.Stream.streamify (CLex.makeLexer (inputc instrm) lexArg)
	val (res,_) = P.parse(lookahead, lexer, parseErr, sourceMap) 
	val _ = TextIO.closeIn instrm
     in res
    end
    handle P.ParseError =>
	(TextIO.output(Error.errStream errState,"ParseError raised\n");
	 [])

end (* structure Parser *)
