(* gen-template-struct.sml
 *
 * COPYRIGHT (c) 2009 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This is a helper program for converting the template files into SML
 * structures.  The usage is:
 *
 *	gen-template-struct struct-name template-file output-file
 *
 * where struct-name is the name of the structure to be generated,
 * template-file is the source file for the string, and output-file
 * is the generated file.  The generated structure will have the
 * following signature:
 *
 *	structure <struct-name> : sig
 *	    val template : ExpandFile.template
 *	  end
 *)

structure Gen =
  struct

    structure TIO = TextIO

    fun header {fileName, structName} = String.concat [
	    "(* ", fileName, "\n",
	    " *\n\
	    \ * COPYRIGHT (c) 2009 The Fellowship of SML/NJ (http://www.smlnj.org)\n\
	    \ * All rights reserved.\n\
	    \ *\n\
	    \ * !!! WARNING: this file is generated; do not edit !!!\n\
	    \ *)\n\
	    \\n\
	    \structure ", structName, " : sig val template : ExpandFile.template end =\n\
	    \  struct\n\
	    \\n\
	    \    val template = ExpandFile.mkTemplateFromList ["
	  ]

    val tail = "\
	  \\n\
	  \          ]\n\
	  \\n\
	  \  end\n\
	  \"

    fun gen {outputFile, structName, templateFile} = let
	  val src = TIO.openIn templateFile
		handle ex => (
		  TIO.output(TIO.stdErr, concat[
		      "Error: unable to open template file \"",
		      templateFile, "\"\n"
		    ]);
		  raise ex)
	  val dst = TIO.openOut outputFile
		handle ex => (
		  TIO.output(TIO.stdErr, concat[
		      "Error: unable to open output file \"",
		      outputFile, "\"\n"
		    ]);
		  raise ex)
	  fun done () = (TIO.closeIn src; TIO.closeOut dst)
	  fun copy first = (case TIO.inputLine src
		 of NONE => ()
		  | SOME line => (
		      if (not first) then TIO.output (dst, ",") else ();
		      TIO.output (dst, "\n            \"");
		      TIO.output (dst, String.toString line);
		      TIO.output (dst, "\"");
		      copy false)
		(* end case *))
	  in
	    ((* try *)
	      TIO.output (dst, header {
		  fileName=outputFile, structName=structName
		});
	      copy true;
	      TIO.output (dst, tail)
	    handle ex => (
	      TIO.output(TIO.stdErr, "Error copying file\n");
	      done(); raise ex));
	    done()
	  end

    fun main (_, [structName, templateFile, outputFile]) = (
	  (* try *)
	    (gen {
		outputFile = outputFile,
		structName = structName,
		templateFile = templateFile
	      };
	    OS.Process.success)
	  handle ex => OS.Process.failure)
      | main (cmd, _) = (
	  TIO.output(TIO.stdErr, concat [
	      "usage: ", cmd, " struct-name template-file output-file\n"
	    ]);
	  OS.Process.failure)

  end

