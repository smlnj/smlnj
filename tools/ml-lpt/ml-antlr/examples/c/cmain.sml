structure Main = 
  struct

    structure P = Parser(ListLexer)

    fun parse(strm) = let
      fun inputN n = (case TextIO.inputLine strm
		       of SOME s => s
			| NONE => "")
      val lexer = Mlex.makeLexer inputN
      fun lex() = (case lexer()
		    of Tok.EOF => [Tok.EOF]
		     | t => t :: lex())
    in
      #1 (P.parser (lex()))
    end

    fun errMsg l = 
	  TextIO.output(TextIO.stdErr, String.concat l)

    fun main (_, [file]) = (
	  parse (TextIO.openIn file);
	  OS.Process.success)
	  handle ex => (
	    errMsg [
		"uncaught exception ", General.exnName ex,
		" [", exnMessage ex, "]\n"
	      ];
	    List.app (fn s => errMsg ["  raised at ", s, "\n"]) (SMLofNJ.exnHistory ex);
	    OS.Process.failure)

  end
