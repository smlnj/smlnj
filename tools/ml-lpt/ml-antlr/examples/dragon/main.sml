structure Main = 
  struct

    structure L = Mlex
    structure P = ParseFn(L)

    fun errMsg l = 
	  TextIO.output(TextIO.stdErr, String.concat l)

    fun parse strm = let
	  val s = Mlex.streamifyInstream strm
	  val sm = AntlrStreamPos.mkSourcemap()
	  val (SOME p, s', errors) = P.parse (L.lex sm) s before TextIO.closeIn strm
	  fun doErr err = print ("Syntax error " ^ 
			    AntlrRepair.repairToString Tokens.toString sm err ^ "\n")
	  in
            app doErr errors;
	    p
	  end

    fun main (_, [file]) = (
	  print ("\n -- ECHO -- \n\n" ^ (parse (TextIO.openIn file)));
	  print "\n\n";
	  OS.Process.success)
	  handle ex => (
	    errMsg [
		"uncaught exception ", General.exnName ex,
		" [", exnMessage ex, "]\n"
	      ];
	    List.app (fn s => errMsg ["  raised at ", s, "\n"]) (SMLofNJ.exnHistory ex);
	    OS.Process.failure)

  end
