(* html4-test.sml
 *
 * COPYRIGHT (c) 2014 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Test =
  struct

    val parsetreeStreamToString =
	  HTML4Utils.mkParsetreeStreamToString HTML4Tokens.toString

    fun handleFile outS fileName = let
	  val inStream = TextIO.openIn fileName
	  val concrete_pt_opt = HTML4Parser.parseStream inStream
	  val pt_visit_strm = (case concrete_pt_opt
		 of SOME concrete_pt => HTML4Utils.parsetreeToVisitationStream concrete_pt
		  | NONE => HTML4Utils.StreamNil
		(* end case *))
	  val (_, htmlOpt) = HTML4Parser.htmlFromParseStream pt_visit_strm
	      handle HTML4Parser.IllFormedHTMLParseStream (strm, SOME msg) =>
		     (HTML4Parser.printVisitationStream strm;
		      print (msg ^ "\n\n"); (strm, NONE))
	  in
	    TextIO.closeIn inStream;
	    TextIO.output(outS, concat["<!-- ******************** start ", fileName, " ******************** -->\n"]);
	    case htmlOpt
	     of SOME html => HTML4Print.prHTML {
		    putc = fn c => TextIO.output1(outS, c),
		    puts = fn s => TextIO.output(outS, s)
		  } html
	      | NONE => TextIO.output (outS, parsetreeStreamToString pt_visit_strm)
	    (* end case *);
	    TextIO.output(outS, concat["<!-- ******************** end ", fileName, " ******************** -->\n"])
	  end

    fun main (_, args) = let
	  val outS = TextIO.openOut "html4-test.out"
	  in
	    (List.app (handleFile outS) args; TextIO.closeOut outS; OS.Process.success)
	      handle ex => (
		  TextIO.closeOut outS;
		  print(concat["uncaught exception: ", exnMessage ex, "\n"]);
		  List.app (fn s => print(concat["  ", s, "\n"])) (SMLofNJ.exnHistory ex);
		  OS.Process.failure)
	  end

    val tests = [
	    "tests/abbr.html",
	    "tests/dir.html",
	    "tests/edit.html",
	    "tests/elements.html",
	    "tests/entities.html",
	    "tests/forms.html",
	    "tests/framebody.html",
	    "tests/frames.html",
	    "tests/id.html",
	    "tests/objaudio.html",
	    "tests/objects.html",
	    "tests/objvideo.html",
	    "tests/quote.html",
	    "tests/scripts.html",
	    "tests/spchars.html",
	    "tests/tables.html",
	    "tests/template.html",
	    "tests/test001.html",
	    "tests/test002.html"
	  ]

  end

(* ______________________________________________________________________
   End of html4-test.sml
   ______________________________________________________________________ *)
