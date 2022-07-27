(* prod.sml
 *
 * COPYRIGHT (c) 2006
 * John Reppy (http://www.cs.uchicago.edu/~jhr)
 * Aaron Turon (http://www.cs.uchicago.edu/~adrassi)
 * All rights reserved.
 *
 * Utility functions for the prod datatype.
 *)

structure Prod =
  struct

    datatype prod = datatype LLKSpec.prod

    fun id (PROD{id, ...}) = id
    fun lhs (PROD{lhs, ...}) = lhs
    fun items (PROD{rhs, ...}) = !rhs
    fun itemBindings (PROD{rhsBindings, ...}) = 
	  (fn (x, _) => x) (ListPair.unzip rhsBindings)
    fun itemYields (PROD{rhsBindings, ...}) = 
	  (fn (_, y) => y) (ListPair.unzip rhsBindings)
    fun action (PROD{action, ...}) = action
    fun pred (PROD{pred, ...}) = pred

    fun span (PROD{loc, ...}) = loc

    fun name (PROD{name, ...}) = name
    fun fullName p = name p
(*    fun fullName p = (case Nonterm.parent (lhs p)
          of SOME p' => String.concat [
	       fullName p', "_",
	       Nonterm.name (lhs p'), "_",
	       name p]
	   | NONE => name p)
*)

    fun toString p = concat[
	    Nonterm.qualName (lhs p), " ::= ",
	    String.concatWith " " (List.map Item.toString (items p))
	  ]

    fun compare (p1, p2) = Int.compare(id p1, id p2)

    fun canTry (PROD{try = true, ...}) = true
      | canTry _ = false
(*
      | canTry (PROD{rhs, ...}) = 
	  List.exists (fn (LLKSpec.SEM_PRED _) => true
			| _ => false)
	              rhs
*)

    fun bindingsAtAction p = let
          val (leftBindings, formals) = case Nonterm.parent (lhs p)
		of NONE => (AtomSet.empty,
			    AtomSet.addList (AtomSet.empty,
			      Nonterm.formals (lhs p)))
		 | SOME p' => let
		     fun isProdItm (itm) = (case Item.nt itm
                           of SOME nt => Nonterm.same (nt, lhs p)
			    | NONE => false)
		     val prodItm = valOf (List.find isProdItm (items p'))
		     in
		       Item.bindingsLeftOf (prodItm, p')
		     end
	  in
	    (AtomSet.addList (leftBindings, map Atom.atom (itemBindings p)), formals)
	  end

    structure Set = RedBlackSetFn (
      struct
	type ord_key = prod
	val compare = compare
      end)

    structure Map = RedBlackMapFn (
      struct
	type ord_key = prod
	val compare = compare
      end)

    fun sortProds prods = 
	  ListMergeSort.sort 
	    (fn (x, y) => compare (x, y) = GREATER)
	    prods

  end
