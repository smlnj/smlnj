(* compute-follow1.sml
 *
 * COPYRIGHT (c) 2006
 * John Reppy (http://www.cs.uchicago.edu/~jhr)
 * Aaron Turon (http://www.cs.uchicago.edu/~adrassi)
 * All rights reserved.
 *)

structure ComputeFollow1 : sig

    datatype result = Result of {
	followOf : LLKSpec.nonterm -> Token.Set.set
      }

    val compute : LLKSpec.prod list 
		* ComputeNullable.result 
		* ComputeFirst1.result
		-> result

  end = struct

    structure TSet = Token.Set
    structure NMap = Nonterm.Map

    datatype result = Result of {
	followOf : LLKSpec.nonterm -> Token.Set.set
      }

    fun getNT (LLKSpec.TOK _) = NONE
      | getNT (LLKSpec.NONTERM nt) = SOME nt
      | getNT (LLKSpec.CLOS nt) = SOME nt
      | getNT (LLKSpec.POSCLOS nt) = SOME nt
      | getNT (LLKSpec.OPT nt) = SOME nt

    fun compute (rules, nres, fres) = let
	  val ComputeNullable.Result 
		{nullable, nullableItem, nullableItems} = nres
	  val ComputeFirst1.Result 
		{firstOf, firstOfItem, firstOfItems} = fres
	  fun followOf nm nt =
	        Option.getOpt (NMap.find (nm, nt), TSet.empty)
	  fun doRule (LLKSpec.PROD{lhs, rhs, ...}, (nm, changed)) = let
		val lhsToks = followOf nm lhs
		fun iter (nm, changed, []) = (nm, changed)
		  | iter (nm, changed, item::prec) = let
		      val (nm', changed') = 
			  case getNT item
			   of SOME nt => let
				val toks = followOf nm nt 
				val toks' = TSet.union (lhsToks, toks)
				val changed' = changed
				      orelse (TSet.numItems toks' > TSet.numItems toks)
				val nm' = NMap.insert (nm, nt, toks')
			        in  
				  (nm', changed')
			        end
			    | NONE => (nm, changed)
		      in
		        if nullableItem item then
			  iter (nm', changed', prec)
			else (nm', changed')
	              end
		in 
	          iter (nm, false, List.rev rhs)
	        end
	  fun loopToFixedPt nm = let
		val (nm', changed) = List.foldl doRule (nm, false) rules
		in if changed then loopToFixedPt nm' else nm'
		end
	  fun initRule (LLKSpec.PROD{lhs, rhs, ...}, nm) = let
	        fun iter (nm, []) = nm
		  | iter (nm, item::rest) = (case getNT item
		      of SOME nt => let
			   val toks = followOf nm nt
			   val toks' = TSet.union (toks, firstOfItems rest)
			   val nm' = NMap.insert (nm, nt, toks')
			   in
		             iter (nm', rest)
			   end
		       | NONE => iter (nm, rest)
		     (* end case *))
		in 
	          iter (nm, rhs)
		end
	  val nm = loopToFixedPt (List.foldl initRule NMap.empty rules)
	  in Result {
	       followOf = followOf nm 
	      }
	  end

  end

