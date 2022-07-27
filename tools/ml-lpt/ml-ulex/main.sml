(* main.sml
 *
 * COPYRIGHT (c) 2005
 * John Reppy (http://www.cs.uchicago.edu/~jhr)
 * Aaron Turon (adrassi@gmail.com)
 * All rights reserved.
 *
 * Driver for ml-ulex.
 *)

structure Main =
  struct

    structure RE = RegExp
    structure Lex = LexGen
    structure LO = LexOutputSpec

  (* print debug messages, etc to stdErr *)
    fun errPrint msg = TextIO.output (TextIO.stdErr, String.concat msg)

    val name = "ml-ulex"

  (* count the total number of DFA states *)
    fun numStates (LO.Spec{dfa, ...}) = List.length dfa

    fun status s = errPrint ["[", name, ": ", s, "]\n"]

    fun frontEnd () = let
	  val fname = !Options.fname
          val _ = if (String.size fname = 0)
		  then (
		    errPrint [
			"No input file specified\n  usage:  ",
			name, " ", Options.args, " file\n"
		      ];
		    OS.Process.exit OS.Process.failure)
		  else ()
	  val _ = status "parsing"
	  in
            if !Options.lexCompat
	      then MLLexInput.parseFile fname
	      else MLULexInput.parseFile fname
	  end

    fun compile inSpec = let
	  val inSpec = if (!Options.beTest)
		       then LexSpec.emptyActions inSpec
		       else inSpec
	  val _ = status "DFA gen"
	  val outSpec = Lex.gen inSpec
	  val _ = errPrint [" ", Int.toString (numStates outSpec), " states in full DFA\n"]
	  val _ = if !Options.dump
		  then (
		    status "DFA dump";
		    DumpOutput.output (outSpec, !Options.fname))
		  else ()
	  in
	    outSpec
	  end

    fun backEnd outSpec = (
	  if !Options.dot
	    then (
	      status "DOT gen";
	      DotOutput.output (outSpec, !Options.fname))
	    else ();
	  status "SML gen";
	  if (numStates outSpec > 150 andalso !Options.beMode = Options.BySize)
	  orelse !Options.beMode = Options.TableBased
	    then SMLTblOutput.output (outSpec, !Options.fname)
	    else SMLFunOutput.output (outSpec, !Options.fname);
	  if !Options.match
	    then (
	      errPrint ["-- Interactive matching (blank line to quit) --\n"];
	      Match.output (outSpec, !Options.fname))
	    else ())

    fun mlULex () = (case frontEnd ()
	   of SOME inSpec => (backEnd (compile inSpec); OS.Process.success)
	    | NONE => OS.Process.failure
	  (* end case *))

    fun main (_, args) = let
	  val _ = List.app Options.procArg args
	  in
	    mlULex()
          end
	    handle Options.Usage msg => (
		TextIO.output(TextIO.stdErr, concat[
		    "** ", msg, "\nusage: ml-ulex ", Options.args, "\n"
		  ]);
		OS.Process.exit OS.Process.failure)
	    | ex => (
	      TextIO.output(TextIO.stdErr, concat[
		  "** uncaught exception ", General.exnName ex,
		  " [", General.exnMessage ex, "]\n"
	        ]);
	      app (fn s => TextIO.output(TextIO.stdErr, concat[
		  "  raised at ", s, "\n"
	        ]))
	        (SMLofNJ.exnHistory ex);
	      OS.Process.exit OS.Process.failure)

  end
