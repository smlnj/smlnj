(* ml-ulex-input.sml
 *
 * COPYRIGHT (c) 2005
 * John Reppy (http://www.cs.uchicago.edu/~jhr)
 * Aaron Turon (adrassi@gmail.com)
 * All rights reserved.
 *
 * Driver for ml-ulex input format.
 *)

structure MLULexInput : INPUT =
  struct

    structure SP = AntlrStreamPos
    structure L = MLULexLex
    structure P = MLULexParseFn(L)

    fun parseFile fname = let
	  val fstrm = TextIO.openIn fname
	  val strm = L.streamifyInstream fstrm
	  val sm = SP.mkSourcemap' fname
	  val lex = L.lex sm
	  val (spec, strm', errors, {errs}) = P.parse lex strm
	  fun errMsg ty (pos, err) = TextIO.output (TextIO.stdErr, String.concat [
		  SP.toString sm pos, ty, ": ", err, "\n"
		])
	  in
	    TextIO.closeIn fstrm;
	    if (null errors andalso null errs)
	      then spec
	      else (
		app (errMsg " Syntax error")
		  (map (fn (p, e) => (p, AntlrRepair.actionToString MLULexTokens.toString e)) errors);
		app (errMsg "") (map (fn ((p, _), e) => (p, e)) errs);
		NONE)
	  end

  end
