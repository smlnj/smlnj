(* dump-output.sml
 *
 * COPYRIGHT (c) 2005 
 * John Reppy (http://www.cs.uchicago.edu/~jhr)
 * Aaron Turon (adrassi@gmail.com)
 * All rights reserved.
 *
 * Dump (to stderr) the complete DFA
 *)

structure DumpOutput : OUTPUT = 
  struct

    structure RE = RegExp
    structure LO = LexOutputSpec

    fun pr s = TextIO.output(TextIO.stdOut, s)
    fun prl ss = pr(concat ss)

    fun nameOf (LO.State{id, ...}) = "Q" ^ Int.toString id

    fun prState (s as LO.State{id, label, final, next, ...}) = let
          val name = (case final
		       of [] => nameOf s
			| id::_ => concat[nameOf s, " (act ", Int.toString id, ")"]
		      (* end case *))
	  fun prEdge (symSet, st) = prl[
		  "  -- ", RE.toString (RE.mkSymSet symSet), " --> ", nameOf st, "\n"
		]
	  fun prRE re = prl[" ", RE.toString re, "\n"]
          in
            prl[name, ": "(*, RE.toString label*), "\n"];
	    Vector.app prRE label;
	    List.app prEdge (!next);
	    pr "\n"
          end

    fun dumpDFA states = 
	  (List.app prState states;
	   pr (Int.toString (List.length states));
	   pr " states\n\n")


    fun outSS (label, ss) = prl ["Start state: ", label, " => ", nameOf ss, "\n"]

    fun output (spec, _) = let
          val LO.Spec {dfa, startStates, ...} = spec
          in
            dumpDFA dfa;
	    pr "\n";
	    List.app outSS startStates
	  end

  end
