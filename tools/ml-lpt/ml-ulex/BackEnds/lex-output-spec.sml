(* lex-output-spec.sml
 *
 * COPYRIGHT (c) 2005 
 * John Reppy (http://www.cs.uchicago.edu/~jhr)
 * Aaron Turon (adrassi@gmail.com)
 * All rights reserved.
 *
 * Specification produced by LexGen
 *)

structure LexOutputSpec = 
  struct

    datatype dfa_state
      = State of {
	  id : int,
	  startState : bool,
	  label : RegExp.re Vector.vector,
	  final : int list,	(* action vector indices *)
	  next :  (RegExp.sym_set * dfa_state) list ref
	}

    fun sameState (State {id = id1, ...}, State {id = id2, ...}) =
	  id1 = id2

    type action = string

    datatype spec = Spec of {
	decls : string,
	header : string,
	arg : string,
	actions : action vector,
	dfa : dfa_state list,
	startStates : (string * dfa_state) list,
	eofRules : (string * action) list
      }

  end
