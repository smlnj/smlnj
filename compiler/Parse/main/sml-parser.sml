(* sml-parser.sml
 *
 * COPYRIGHT (c) 2016 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This parser recognizes the Successr ML language (plus some SML/NJ extensions)
 *)

structure SMLParser : SMLNJ_PARSER =
  struct

    structure LrVals = SMLLrValsFun(structure Token = LrParser.Token)
    structure Lex = SMLLexFun(structure Tokens = LrVals.Tokens)
    structure P = JoinWithArg(
	structure ParserData = LrVals.ParserData
	structure Lex = Lex
	structure LrParser = LrParser)

  (* the following two functions are also defined in build/computil.sml *)
    val addLines = Stats.addStat(Stats.makeStat "Source Lines")

    open ErrorMsg

    datatype parseResult = datatype ParseResult.parseResult

    val dummyEOF = LrVals.Tokens.EOF(0,0)
    val dummySEMI = LrVals.Tokens.SEMICOLON(0,0)

    fun parse (source : Source.inputSource) = let
	  val {sourceStream, errConsumer, interactive, sourceMap, anyErrors, ...} = source
          val err = ErrorMsg.error source
	  val complainMatch = ErrorMsg.matchErrorString source
	  fun parseerror (s, p1, p2) = err (p1, p2) COMPLAIN s nullErrorBody
	  val lexarg = {
		  comLevel = ref 0,
		  sourceMap = sourceMap,
		  charlist = ref (nil : string list),
		  stringtype = ref false,
		  stringstart = ref 0,
		  err = err,
		  brack_stack = ref (nil: int ref list)
		}
	  val doprompt = ref true
	  val prompt = ref (!ParserControl.primaryPrompt)
	  fun inputc_sourceStream _ = TextIO.input(sourceStream)
	  exception AbortLex
	  fun getline k = (
		if !doprompt
		  then (
		    if !anyErrors then raise AbortLex else ();
		    if !(#comLevel lexarg) > 0 orelse !(#charlist lexarg) <> nil
		      then Control_Print.say (!ParserControl.secondaryPrompt)
		      else Control_Print.say (!prompt);
		    Control_Print.flush();
		    doprompt := false)
		  else ();
		let val s = inputc_sourceStream k
		in
		  doprompt := ((String.sub(s, size s - 1) = #"\n") handle _ => false);
		  s
		end)
	  val lexer = Lex.makeLexer (if interactive then getline else inputc_sourceStream) lexarg
	  val lexer' = ref(LrParser.Stream.streamify lexer)
	  val lookahead = if interactive then 0 else 30
	  fun oneparse () = let
	        val _ = prompt := !ParserControl.primaryPrompt
		val (nextToken,rest) = LrParser.Stream.get(!lexer')
		in
		  if P.sameToken(nextToken,dummySEMI)
		    then (lexer' := rest; oneparse ())
		  else if P.sameToken(nextToken,dummyEOF)
		    then EOF
		    else let
		      val _ = prompt := !ParserControl.secondaryPrompt;
		      val initialLinePos = SourceMap.lastLinePos sourceMap
		      val (result, lexer'') = P.parse(lookahead, !lexer', parseerror, err)
		      val linesRead = SourceMap.newlineCount sourceMap
			    (initialLinePos, SourceMap.lastLinePos sourceMap)
		      in
			addLines linesRead;
			lexer' := lexer'';
			if !anyErrors then ERROR else PARSE result
		      end
		end handle LrParser.ParseError => ABORT
			 | AbortLex => ABORT
		(* oneparse *)
	  in
	    fn () => (anyErrors := false; oneparse ())
	  end

  end
