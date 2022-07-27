(* compute-nullable.sml
 *
 * COPYRIGHT (c) 2006
 * John Reppy (http://www.cs.uchicago.edu/~jhr)
 * Aaron Turon (http://www.cs.uchicago.edu/~adrassi)
 * All rights reserved.
 *
 * Computation of nullable test on nonterminals.
 *)

structure ComputeNullable : sig

    datatype result = Result of {
	nullable : LLKSpec.nonterm -> bool,
	nullableItem : LLKSpec.item -> bool,
	nullableItems : LLKSpec.item list -> bool
      }

    val compute : LLKSpec.prod list -> result

  end = struct

    datatype result = Result of {
	nullable : LLKSpec.nonterm -> bool,
	nullableItem : LLKSpec.item -> bool,
	nullableItems : LLKSpec.item list -> bool
      }

    fun nullableItem ns item = (case item
	  of (LLKSpec.TOK _) => false
	   | (LLKSpec.NONTERM nt) => Nonterm.Set.member(ns, nt)
	   | (LLKSpec.CLOS _) => true
	   | (LLKSpec.POSCLOS nt) => Nonterm.Set.member(ns, nt)
	   | (LLKSpec.OPT _) => true
	  (* end case *))

    fun nullableItems ns = List.all (nullableItem ns)

    fun compute (rules : LLKSpec.prod list) = let
	  fun doRule (LLKSpec.PROD{lhs, rhs, ...}, ns) =
		if nullableItems ns rhs
		  then Nonterm.Set.add(ns, lhs)
		  else ns
	  fun loopToFixedPt ns = let
		val ns' = List.foldl doRule ns rules
		in
		  if Nonterm.Set.numItems ns' > Nonterm.Set.numItems ns
		    then loopToFixedPt ns'
		    else ns
		end
	(* compute the set of nullable nonterminals *)
	  val ns = loopToFixedPt Nonterm.Set.empty
	  in Result {
	       nullable = fn nt => Nonterm.Set.member(ns, nt),
	       nullableItem = nullableItem ns,
	       nullableItems = nullableItems ns
	     }
	  end

  end
