(* sexp-tests.sml
 *
 * COPYRIGHT (c) 2011 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Author: Damon Wang (with modifications by John Reppy)
 *
 * Some test cases for the SExp library  11 May 2011 by Damon Wang
 *)

structure TEST : sig

  exception ERROR of string option (* an error message *)

  datatype outcome = PASS of string                     (* test name *)
                   | FAIL of (string * exn)             (* test name, FAIL instance *)
                   | PARTIAL of (string * outcome list) (* name, results *)

  datatype testcase = CASE of { name : string, test : unit -> unit }
                    | SUITE of { name : string, tests : testcase list }

  val run : testcase -> outcome
  val count : outcome -> (int * int)     (* number passed, number run *)
  val summary : outcome -> string

  (* args: (msg, cond) where
  * msg is a description to be printed on failure
  * cond is a boolean---false means failure *)
  val assert : (string * bool) -> unit
  (* args: (cond) *)
  val assert' : bool -> unit

  (* args: (msg, name, func) where
  * msg is a description to be printed on failure
  * name is the exnName of the exception to expect
  * func is the function which should raise the exception *)
  val throws : (string * string * (unit -> unit)) -> unit
  (* args: (name, func) *)
  val throws' : (string * (unit -> unit )) -> unit

end = struct

  exception ERROR of string option (* an error message *)

  datatype outcome = PASS of string                     (* test name *)
                   | FAIL of (string * exn)             (* test name, FAIL instance *)
                   | PARTIAL of (string * outcome list) (* name, results *)

  datatype testcase = CASE of { name : string, test : unit -> unit }
                    | SUITE of { name : string, tests : testcase list }

  fun run (SUITE {name, tests}) = PARTIAL(name, List.map run tests)
    | run (CASE {name, test}) = (test(); PASS(name)) handle e => FAIL(name, e)

  fun addVec ((a, b), (c, d)) = (a + c, b + d)

  fun count (PASS _) = (1, 1)
    | count (FAIL _) = (0, 1)
    | count (PARTIAL (_, results)) = let
      in
        List.foldl addVec (0,0) (List.map count results)
      end

  fun assert (msg, cond) = if not cond then raise ERROR (SOME msg) else ()

  fun assert' cond = if not cond then raise ERROR NONE else ()

  fun throws (msg, name, func) = let
    fun wrongExn e = raise ERROR (SOME (String.concat [msg, 
      "---expected exception '", name, "' but got '", exnName e, "' with msg\n",
      exnMessage e]))
    fun noExn () = raise ERROR (SOME (String.concat [msg, 
      "---expected exception '", name, "' but got nothing"]))
  in
    if (func (); true) handle e =>
      if exnName e = name
      then false
      else (wrongExn e; false)
    then noExn ()
    else ()
  end

  fun throws' (name, func) = throws ("", name, func)

  local
    fun splitMsg msg = String.fields (fn c => EQUAL = Char.compare (#"\n", c)) msg
    fun indent lines = List.map (fn s => "\t" ^ s) lines
    fun summary' (PASS name) : (string list * (int * int)) = 
        ([ String.concat ["     ", name, "\n"] ], (1, 1))
      | summary' (FAIL (name, ERROR msg)) = 
        (List.concat [ [ String.concat ["FAIL ", name ] ], 
                      (case msg 
                         of SOME msg => indent 
                           (List.concat [["\n"], (splitMsg msg), ["\n"] ])
                          | NONE => ["\n"]) ], 
         (0, 1))
      | summary' (FAIL (name, e)) = 
        (List.concat [ [ String.concat ["FAIL ", name, " with external error ",
                                        exnName e, "\n" ] ], 
                      indent (splitMsg (exnMessage e)), 
                      [ "\n"] ], 
         (0, 1))
      | summary' (PARTIAL (name, results)) = let
          val (lines, counts) = ListPair.unzip (List.map summary' results)
          val indented = indent (List.concat lines)
          val (n_passed, n_run) = List.foldl addVec (0, 0) counts
        in
          (String.concat [ "[ ", Int.toString n_passed, " / ", Int.toString n_run, 
          " ] ", name, "\n"] :: indented, (n_passed, n_run))
        end
  in
    fun summary results = String.concat (#1 (summary' results))
  end

end

structure SExpTests : sig
  val run : unit -> unit
end = struct

  structure P = SExpParser
  structure S = SExp

  val assert = TEST.assert
  val assert' = TEST.assert'
  val throws' = TEST.throws'

  fun pS str = hd (P.parse (TextIO.openString str))

  val tests = TEST.SUITE{name="parsing", tests=[

    TEST.SUITE{name="bool", tests=[
      TEST.CASE{name="true", test=fn () =>
        assert' (S.same(pS "#t", S.BOOL true)) },
      TEST.CASE{name="false", test=fn () =>
        assert' (S.same(pS "#f", S.BOOL false))} ] },

    TEST.SUITE{name="int", tests=[
      TEST.CASE{name="negative", test=fn () =>
        assert' (S.same(pS "-1", S.INT ~1)) },
      TEST.CASE{name="zero", test=fn () =>
        assert' (S.same(pS "0", S.INT 0)) },
      TEST.CASE{name="positive", test=fn () =>
        assert' (S.same(pS "1", S.INT 1)) },
      TEST.CASE{name="32-bit signed max", test=fn () =>
        assert' (S.same(pS "2147483647", S.INT 2147483647)) },
      TEST.CASE{name="32-bit signed min", test=fn () =>
        assert' (S.same(pS "-2147483648", S.INT ~2147483648)) },
      TEST.CASE{name="bigger than 32-bit", test=fn () =>
        assert' (S.same(pS "3147483647", S.INT 3147483647)) },
      TEST.CASE{name="leading plus", test=fn () =>
        assert' (S.same(pS "+1", S.INT 1)) },
      TEST.CASE{name="hex", test=fn () =>
        assert' (S.same(pS "0xdeadbeef", S.INT 0xdeadbeef)) },
      TEST.CASE{name="positive hex", test=fn () =>
        assert' (S.same(pS "+0xdeadbeef", S.INT 0xdeadbeef)) },
      TEST.CASE{name="negative hex", test=fn () =>
        assert' (S.same(pS "-0xdeadbeef", S.INT ~0xdeadbeef)) }
                                  ]},

    TEST.SUITE{name="float", tests=[
      TEST.CASE{name="decimal", test=fn () =>
        assert' (S.same(pS "1.0", S.FLOAT 1.0))},
      TEST.CASE{name="exponent", test=fn () => 
        assert' (S.same(pS "1e2", S.FLOAT 100.0))},
      TEST.CASE{name="decimal and exponent", test=fn () =>
        assert' (S.same(pS "1.2e2", S.FLOAT 120.0))},
      TEST.CASE{name="negative", test=fn () =>
        assert' (S.same(pS "-1.0", S.FLOAT ~1.0))},
      TEST.CASE{name="negative exponent", test=fn () =>
        assert' (S.same(pS "1.0e-2", S.FLOAT 0.01))},
      TEST.CASE{name="zero first digit", test=fn () =>
        assert' (S.same(pS "0.1", S.FLOAT 0.1))} ]},

    TEST.SUITE{name="string", tests=[
      TEST.CASE{name="empty", test=fn () =>
        assert' (S.same(pS "\"\"", S.STRING "")) },
      TEST.CASE{name="characters", test=fn () => 
        assert' (S.same(pS "\"foo\"", S.STRING "foo")) },
      TEST.CASE{name="escapes", test=fn () =>
        assert' (S.same(pS "\" \\\\ \\\" \\/ \\b \\f \\n \\r \\t \"",
        S.STRING " \\ \" / \b \f \n \r \t ")) } ]},

    TEST.SUITE{name="comments", tests=[
      TEST.CASE{name="empty", test=fn () =>
        assert' (S.same(pS "/* */1", S.INT 1))},
      TEST.CASE{name="with text", test=fn () =>
        assert' (S.same(pS "/* 1 */1", S.INT 1))},
      TEST.CASE{name="multiline allowed", test=fn () =>
        assert' (S.same(pS "/* 1 \n 1 */1", S.INT 1))},
      TEST.CASE{name="cannot be nested", test=fn ()=>
        throws' ("Fail", fn () => (pS "/* 0 /* 1 */ 2 */ 3"; ())) }
                                    ]},

    TEST.SUITE{name="lists", tests=[
      TEST.CASE{name="empty", test=fn () =>
        assert' (S.same(pS "()", S.LIST []))},
      TEST.CASE{name="empty with spaces", test=fn () =>
        assert' (S.same(pS "( )", S.LIST []))},
      TEST.CASE{name="one int", test=fn () =>
        assert' (S.same(pS "(1)", S.LIST [ (S.INT 1) ]))},
      TEST.CASE{name="one int with space", test=fn () =>
        assert' (S.same(pS "( 1)", S.LIST [ (S.INT 1) ]))},
      TEST.CASE{name="one int with spaces", test=fn () =>
        assert' (S.same(pS "( 1 )", S.LIST [ (S.INT 1) ]))},
      TEST.CASE{name="with empty list ", test=fn () =>
        assert' (S.same(pS "( () )", S.LIST [ (S.LIST []) ]))},
      TEST.CASE{name="nested", test=fn () =>
        assert' (S.same(pS "( ( 1 ) )", S.LIST [ S.LIST [ S.INT 1 ] ]))},
      TEST.CASE{name="two elements", test=fn () =>
        assert' (S.same(pS "( 1 2 )", S.LIST [ S.INT 1, S.INT 2]))},
      TEST.CASE{name="three elements", test=fn () =>
        assert' (S.same(pS "( 1 2 3 )", S.LIST [ S.INT 1, S.INT 2]))},
      TEST.CASE{name="mixed elements", test=fn () =>
        assert' (S.same(pS "( 1 2.5 \"foo\" (2))", 
        S.LIST [ S.INT 1, S.FLOAT 2.5, S.STRING "foo", S.LIST [ S.INT 2 ] ]))},
      TEST.CASE{name="brackets", test=fn () =>
        assert' (S.same(pS "[ 1 2 3 ]", S.LIST [ S.INT 1, S.INT 2]))},
      TEST.CASE{name="braces", test=fn () =>
        assert' (S.same(pS "{ 1 2 3 }", S.LIST [ S.INT 1, S.INT 2]))},
      TEST.CASE{name="mixed delimiters", test=fn () =>
        assert' (S.same(pS "{ [ ( ) ] }", S.LIST [ S.LIST [ S.LIST [] ] ]))},
      TEST.CASE{name="delimiters must match", test=fn () =>
        throws' ("Fail", fn () => (pS "(]"; ())) },
      TEST.CASE{name="alternative separaters", test=fn () =>
        assert' (S.same(pS "(1;2,3)", S.LIST [ S.INT 1, S.INT 2, S.INT 3])) }
                                               ]},

    TEST.SUITE{name="symbols", tests=[
      TEST.CASE{name="characters", test=fn () =>
        assert' (S.same(pS "ab", S.SYMBOL (Atom.atom "ab")))},
      TEST.CASE{name="trailing digits", test=fn () =>
        assert' (S.same(pS "x0", S.SYMBOL (Atom.atom "x0")))},
      TEST.CASE{name="quoted", test=fn ()=>
        assert' (S.same(pS "'0", S.SYMBOL (Atom.atom "0")))}
    ]},

    TEST.SUITE{name="bugs", tests=[
	TEST.CASE{name="bug01", test= fn () =>
	    assert' (S.same(
	      pS "(set pi 3.141592653589793 :documentation \"The value of $\\pi$.\")",
	      S.LIST[
		  S.SYMBOL(Atom.atom "set"), S.SYMBOL(Atom.atom "pi"),
		  S.FLOAT 3.141592653589793, S.SYMBOL(Atom.atom ":documentation"),
		  S.STRING "The value of $\\pi$."
		]))
	  }
      ]}
  ]}
    
  fun run () = TextIO.print (TEST.summary (TEST.run tests))
end

(* SExpTests.run (); *)
