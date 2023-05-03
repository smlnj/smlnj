(* Basics/symbol/numericlabel.sml *)
(* Copyright 2022 The Fellowship of SML/NJ *)

structure NumericLabel : NUMERIC_LABEL =
struct

local (* top local *)

    structure S = Symbol

    structure NLArray =
      DynamicArrayFn
	(struct
	   open Array
	   type array = S.symbol option array
	   type vector = S.symbol option vector
	   type elem = S.symbol option
	 end)

    (* memoize numeric labels -- to save on duplicate symbols? *)
    val labelArray =
	NLArray.tabulate (50, (fn i => SOME (S.labSymbol (Int.toString i))), NONE)

in

    type numericLabel = S.symbol

    (* numericLabel : int -> numericLabel *)
    fun numericLabel (i: int) : numericLabel =
	case NLArray.sub (labelArray, i)
	  of NONE =>
	       let val newlabel = S.labSymbol (Int.toString i)
		in NLArray.update (labelArray, i, SOME newlabel);
		   newlabel
	       end
	   | SOME label => label

    (* numericLabelIsInt : numericLabel * int -> bool *)
    fun numericLabelIsInt (label: numericLabel, n: int) : bool =
	S.name label = Int.toString n

    (* checkTupleLabels : S.symbol list -> bool *)
    fun checkTupleLabels (labels : S.symbol list) =
	  let fun checkLabels (labels, n) =
		  (case labels
		    of nil => true
		     | label :: rest =>
		       if numericLabelIsInt (label, n)
		       then checkLabels (rest, n+1)
		       else false)
	   in checkLabels (labels, 1)
	  end  	    

end (* top local *)
end (* structure NumericLabel *)

(* Notes:
   [DBM, 2022.10.14] Should numeric labels (for tuples as records) be symbols? Or should they
    have some special type?  If so, then
 
       (3,4) <> {1 = 3, 2 = 4} ?

*)
