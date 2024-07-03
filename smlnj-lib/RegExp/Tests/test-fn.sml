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

    datatype result
      = NoMatch
      | Error of exn
      | Match of {pos : int, len : int, next : int}

    fun find (re, strm) = ((
          case (RE.find re strm 0)
           of NONE => NoMatch
            | SOME(M.Match({pos, len}, _), n) =>
                if (pos + len <> n)
                  then raise Fail "invalid next position"
                  else Match{pos=pos, len=len, next=n}
          (* end case *))
            handle ex => Error ex)

    fun test (name, re, data) = let
	  val _ = print(concat["  ", name, ": "])
	  val re = RE.compileString re handle ex => (print "compile failed\n"; raise ex)
	  in
	    case find (re, getc data)
	     of NoMatch => print "match failed\n"
              | Error exn => print(concat["Error: ", General.exnMessage exn, "\n"])
              | Match{pos, len, next} =>
		  print(concat[
		      "match at ", Int.toString pos, " = \"",
		      String.toString(String.substring(data, pos, len)), "\"; next = ",
                      Int.toString next, "\n"
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
          test ("16", "[true]+", "truexxx");
          test ("17", "true", "truexxx");
	  (* tests from https://github.com/smlnj/smlnj/issues/289 *)
	  test ("18a", "BA{3,5}B", "BAAB"); (* should fail *)
	  test ("18b", "BA{3,5}B", "BAAAB"); (* should match *)
	  test ("18c", "BA{3,5}B", "BAAAAB"); (* should match *)
	  test ("18d", "BA{3,5}B", "BAAAAAB"); (* should match *)
	  test ("18e", "BA{3,5}B", "BAAAAAAB"); (* should fail *)
          ())

  end
