(* bsearch-fn.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Binary searching on sorted monomorphic arrays.
 *)

functor BSearchFn (A : MONO_ARRAY) : sig

    structure A : MONO_ARRAY

    val bsearch : (('a * A.elem) -> order)
	  -> ('a * A.array) -> (int * A.elem) option
	(* binary search on ordered monomorphic arrays. The comparison function
	 * cmp embeds a projection function from the element type to the key
	 * type.
	 *)

  end = struct

    structure A = A

  (* binary search on ordered monomorphic arrays. The comparison function
   * cmp embeds a projection function from the element type to the key
   * type.
   *)
    fun bsearch cmp (key, arr) = let
	  fun look (lo, hi) =
                if hi >= lo then let
		  val m = lo + (hi - lo) div 2
		  val x = A.sub(arr, m)
		  in
	 	    case cmp(key, x)
		     of LESS => look(lo, m-1)
		      | EQUAL => (SOME(m, x))
		      | GREATER => look(m+1, hi)
		    (* end case *)
		  end
                else NONE
	  in
	    look (0, A.length arr - 1)
	  end

  end; (* BSearch *)
