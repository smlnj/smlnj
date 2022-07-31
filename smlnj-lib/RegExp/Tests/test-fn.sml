(* test-fn.sml
 *
 * COPYRIGHT (c) 2008 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

functor TestFn (
    val engineName : string
    structure RE : REGEXP
  ) = struct

    structure M = MatchTree

    fun getc s i = if (i < String.size s) then SOME(String.sub(s, i), i+1) else NONE

    fun test (name, re, data) = let
	  val _ = print(concat["  ", name, ": "])
	  val re = RE.compileString re handle ex => (print "compile failed\n"; raise ex)
	  in
	    case ((RE.find re (getc data) 0) handle ex => NONE)
	     of NONE => print "match failed\n"
	      | SOME(M.Match({pos, len}, _), _) =>
		  print(concat[
		      "match at ", Int.toString pos, " = \"",
		      String.toString(String.substring(data, pos, len)), "\"\n"
		    ])
	    (* end case *)
	  end
	    handle _ => ()

    fun doTests () = (
	  print(concat["  testing ", engineName, "\n"]);
	  test ("01", "[0-9]+", "abc123xyz");
	  test ("02", "^[0-9]+", "abc123def\n987xyz");
	  test ("03", "[0-9]+$", "abc123def\n987xyz456");
	  test ("04", "[0-9]+$", "987xyz456\nabc123");
	  test ("05", "^$", "");
	  test ("06", ".", "a");
	  test ("07", "^foo$", "foo");
	  test ("08", "^...$", "foo");
	  test ("09", "^.*$", "foo");
	  test ("10", "^.*foo@bar\\.com$", "foo@bar.com");
	  test ("11", "(abc)","abc");
	  test ("12", "\\(abc\\)","(abc)");
	  test ("13", "(abc){2,4}$", "abcabc");
	  test ("14", "(abc){2,4}$", "abcabcabc");
	  test ("15", "(abc){2,4}$", "abcabcabcabc");
	  print "** tests done\n")

  end
