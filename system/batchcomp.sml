local
    fun main _ = let
	fun quit () = OS.Process.exit OS.Process.success
	fun one [] = "ok"
	  | one ["m"] = Bool.toString (CMB.make ())
	  | one ["m", f] = Bool.toString (CMB.make' (SOME f))
	  | one ["r"] = (CMB.reset (); "reset")
	  | one ["?", v] =
	    getOpt (Option.map Int.toString (#get (CMB.symval v) ()),
		    "not set")
	  | one ["=", v] = (#set (CMB.symval v) NONE; v ^ " set to NONE")
	  | one ["=", v, n] =
	    (case Int.fromString n of
		 x as SOME _ => (#set (CMB.symval v) x; v ^ " set to " ^ n)
	       | NONE => "bad number syntax")
	  | one ["q"] = quit ()
	  | one _ = "invalid command"
	fun loop () =
	    case TextIO.inputLine TextIO.stdIn of
		"" => quit ()
	      | line => (TextIO.output (TextIO.stdOut,
					one (String.tokens Char.isSpace line) ^
					"\n");
			 loop ())
    in
	SMLofNJ.Internals.resetTimers ();
	Compiler.Stats.reset ();
	loop ()
	handle exn =>
	    (TextIO.output (TextIO.stdOut,
			    General.exnMessage exn ^ "\n");
	     quit ())
    end
in
    val _ = SMLofNJ.exportFn ("batchcomp", main)
end
