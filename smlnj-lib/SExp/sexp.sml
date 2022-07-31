(* sexp.sml
 *
 * COPYRIGHT (c) 2011 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Author: Damon Wang (with modifications by John Reppy)
 *
 * This is the tree representation of a SExp data as produced/consumed
 * by the tree parser.
 *)

structure SExp =
  struct

    datatype value
      = SYMBOL of Atom.atom
      | BOOL of bool
      | INT of IntInf.int
      | FLOAT of real
      | STRING of string
      | QUOTE of value
      | LIST of value list

    fun same (SYMBOL a, SYMBOL b) = Atom.same (a, b)
      | same (BOOL a, BOOL b) = (a = b)
      | same (INT a, INT b) = (a = b)
      | same (FLOAT a, FLOAT b) = Real.==(a, b)
      | same (STRING a, STRING b) = (a = b)
      | same (QUOTE a, QUOTE b) = same(a, b)
      | same (LIST a, LIST b) = ListPair.allEq same (a, b)
      | same _ = false

    fun compare (a, b) = (case (a, b)
	   of (SYMBOL a, SYMBOL b) => Atom.compare(a, b)
	    | (SYMBOL _, _) => LESS
	    | (_, SYMBOL _) => GREATER
	    | (BOOL a, BOOL b) =>
		if (a = b) then EQUAL
		else if a then LESS
		else GREATER
            | (BOOL _, _) => GREATER
            | (_, BOOL _) => LESS
	    | (INT a, INT b) => IntInf.compare (a, b)
	    | (INT _, _) => LESS
	    | (_, INT _) => GREATER
	    | (FLOAT a, FLOAT b) => Real.compare(a, b)
	    | (FLOAT _, _) => LESS
	    | (_, FLOAT _) => GREATER
	    | (STRING a, STRING b) => String.compare(a, b)
	    | (STRING _, _) => LESS
	    | (_, STRING _) => GREATER
	    | (QUOTE a, QUOTE b) => compare(a, b)
	    | (QUOTE _, _) => LESS
	    | (_, QUOTE _) => GREATER
	    | (LIST a, LIST b) => List.collate compare (a, b)
	  (* end case *))

  end
