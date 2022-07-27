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

    structure P = SpecParseFn(SpecLex)

    fun parse filename = let
	  val _ = Err.status ("parsing " ^ filename) 
	  val file = TextIO.openIn filename
          val sm = AntlrStreamPos.mkSourcemap' filename
	  val strm = SpecLex.streamifyInstream file
(*  performance testing 
fun go 0 = ()
  | go n = (ignore (P.parse (SpecLex.lex (AntlrStreamPos.mkSourcemap())) (filename, sm) strm);
	    go (n - 1))
val _ = go 1000
*)
	  val (res, strm', errs, _) = P.parse (SpecLex.lex sm) (filename, sm) strm
	  fun doErr err = Err.errMsg ["Syntax error ",
			    AntlrRepair.repairToString SpecTokens.toString sm err]
          in 
            app doErr errs;
            TextIO.closeIn file;
            res  
          end

  end