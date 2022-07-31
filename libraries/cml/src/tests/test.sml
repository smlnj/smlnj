structure Test = 
struct

  val prog = "/bin/ls"

  fun doit () = let
	val proc = Unix.execute(prog, [])
	val (fin,fout) = Unix.streamsOf proc
	fun echo () = (case TextIO.inputLine fin
	       of "" => ()
		| s => (TextIO.output(TextIO.stdOut, s); echo())
	      (* end case *))
        in
	  TextIO.closeOut fout;
	  echo ();
	  TextIO.closeIn fin;
	  ignore(Unix.reap proc);
	  ()
        end

  fun run () = RunCML.doit(doit, SOME(Time.fromMilliseconds 100))

end

