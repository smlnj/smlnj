(* parse.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * $Log$
 * Revision 1.2  2000/06/01 18:33:42  monnier
 * bring revisions from the vendor branch to the trunk
 *
 * Revision 1.1.1.8  1999/04/17 18:56:04  monnier
 * version 110.16
 *
 * Revision 1.1.1.1  1997/01/14 01:38:00  george
 *   Version 109.24
 *
 * Revision 1.1.1.2  1997/01/11  18:52:32  george
 *   ml-burg Version 109.24
 *
 * Revision 1.2  1996/02/26  15:02:06  george
 *    print no longer overloaded.
 *    use of makestring has been removed and replaced with Int.toString ..
 *    use of IO replaced with TextIO
 *
 * Revision 1.1.1.1  1996/01/31  16:01:25  george
 * Version 109
 * 
 *)
structure Parse = 
struct
  structure BurgLrVals = BurgLrValsFun(structure Token = LrParser.Token)
  structure BurgLex    = BurgLexFun(structure Tokens = BurgLrVals.Tokens)
  structure BurgParser = Join(structure ParserData = BurgLrVals.ParserData
			      structure Lex 	   = BurgLex
			      structure LrParser   = LrParser)

  fun parse stream = 
    let
      val lexer = BurgParser.makeLexer(fn n => TextIO.inputN(stream,n))
      fun error(msg,i:int,_) = 
	    TextIO.output(TextIO.stdOut,
			  "Error: line " ^ Int.toString i ^ ", " ^ msg ^ "\n")
    in
	 BurgParser.parse(30,lexer,error,()) 
	    before
	        BurgLex.UserDeclarations.resetState()
    end

  fun reset () =
    BurgLex.UserDeclarations.resetState()
    
end

