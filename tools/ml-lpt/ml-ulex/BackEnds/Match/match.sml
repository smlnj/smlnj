(* match.sml
 *
 * COPYRIGHT (c) 2005 
 * John Reppy (http://www.cs.uchicago.edu/~jhr)
 * Aaron Turon (adrassi@gmail.com)
 * All rights reserved.
 *
 * A simple match "backend" that runs the produced state machine directly
 * on stdin.  Treats end of line as end of input.  Note that a match only
 * occurs if the machine is in an accepting state after consuming the 
 * complete input; in particular, the input is meant to represent a single
 * token, and the machine does not restart until the end of input.
 *
 ****************************************************************
 * NOTE!     NOT UNICODE COMPATIBLE
 ****************************************************************
 *)

structure Match : OUTPUT = 
  struct

    structure SIS = RegExp.SymSet
    structure LO = LexOutputSpec

    fun match (LO.State{id, label, final, next, ...}, []) = final
      | match (LO.State{id, label, final, next, ...}, sym::r) = let
	  fun goto [] = []
	    | goto ((syms, s)::r') = 
	        if SIS.member (syms, sym) 
		then match(s, r) 
		else goto r'
	  in
	    goto (!next)
	  end

    fun matchLoop states = (case TextIO.inputLine (TextIO.stdIn)
	  of NONE => ()
	   | SOME "\n" => ()
	   | SOME s => let
	       val chars = List.rev (List.tl (List.rev (String.explode s)))
	       val syms = List.map (Word.fromInt o Char.ord) chars
	       val q0 as LO.State {label, ...} = List.hd states
	       val _ = case match (q0, syms)
			of [] => print "-- No match --\n"
			 | i::_ => 
			     (print "-- Match: ";
			      print (RegExp.toString (Vector.sub (label, i)));
			      print " --\n")
	       in
	       (* continue I/O loop *)
	         matchLoop states
	       end
	 (* end case *))

    fun output (LO.Spec {dfa, ...}, _) = 
	  matchLoop(dfa)

  end
