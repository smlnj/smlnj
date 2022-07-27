(* compute-first1.sml
 *
 * COPYRIGHT (c) 2005 John Reppy (http://www.cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure ComputeFirst1 : sig

    datatype result = Result of {
        firstOf : LLKSpec.nonterm -> Token.Set.set,
	firstOfItem : LLKSpec.item -> Token.Set.set,
	firstOfItems : LLKSpec.item list -> Token.Set.set
      }

    val compute : LLKSpec.prod list 
		* ComputeNullable.result
		-> result

  end = struct

    structure TSet = Token.Set
    structure NMap = Nonterm.Map
    structure P = Predict

    datatype result = Result of {
        firstOf : LLKSpec.nonterm -> Token.Set.set,
	firstOfItem : LLKSpec.item -> Token.Set.set,
	firstOfItems : LLKSpec.item list -> Token.Set.set
      }

    fun compute (rules, nres) = let
	  val ComputeNullable.Result 
		{nullable, nullableItem, nullableItems} = nres
	  fun firstOf nm nt = 
	        Option.getOpt (NMap.find (nm, nt), TSet.empty)
	  fun firstOfItem nm item = (case item
		of (LLKSpec.TOK tok)     => P.mkTok tok
		 | (LLKSpec.NONTERM nt)  => firstOf nm nt
		 | (LLKSpec.CLOS nt)     => firstOf nm nt
		 | (LLKSpec.POSCLOS nt)  => firstOf nm nt
		 | (LLKSpec.OPT nt)      => firstOf nm nt
	       (* end case *))
	  and firstOfItems nm items = let
	        fun iter (fs, []) = fs
		  | iter (fs, item::rest) = let
		      val fs' = TSet.union (fs, firstOfItem nm item)
		      in if nullableItem item 
			 then iter (fs', rest)
			 else fs'
		      end
                in iter (TSet.empty, items)
                end
	  fun doRule (LLKSpec.PROD{lhs, rhs, ...}, (nm, changed)) = let
		val toks = firstOf nm lhs
		val toks' = TSet.union (toks, firstOfItems nm rhs)
		val nm' = NMap.insert (nm, lhs, toks')
		in 
	          (nm', changed 
			  orelse (TSet.numItems toks' > TSet.numItems toks))
	        end
	  fun loopToFixedPt nm = let
		val (nm', changed) = List.foldl doRule (nm, false) rules
		in if changed then loopToFixedPt nm' else nm'
		end
	  val nm = loopToFixedPt NMap.empty
	  in Result {
               firstOf = firstOf nm,
	       firstOfItem = firstOfItem nm,
	       firstOfItems = firstOfItems nm
	     }
	  end

  end
