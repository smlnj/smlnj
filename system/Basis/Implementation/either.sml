(* either.sml
 *
 * COPYRIGHT (c) 2015 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * The Either structure is a propsed SML Basis Library extension (proposal 2015-002).
 *)

structure Either : EITHER =
  struct

    datatype ('left, 'right) either = INL of 'left | INR of 'right

    fun isLeft (INL _) = true
      | isLeft (INR _) = false
    fun isRight (INL _) = false
      | isRight (INR _) = true

    fun asLeft (INL x) = SOME x
      | asLeft (INR _) = NONE
    fun asRight (INL _) = NONE
      | asRight (INR x) = SOME x

    fun map (fl, fr) sum = (case sum
	   of INL x => INL(fl x)
	    | INR x => INR(fr x)
	  (* end case *))

    fun mapLeft f sum = (case sum
	   of INL x => INL(f x)
	    | INR x => INR x
	  (* end case *))

    fun mapRight f sum = (case sum
	   of INL x => INL x
	    | INR x => INR(f x)
	  (* end case *))

    fun app (fl, fr) sum = (case sum
	   of INL x => fl x
	    | INR x => fr x
	  (* end case *))

    fun appLeft f sum = (case sum
	   of INL x => f x
	    | INR x => ()
	  (* end case *))

    fun appRight f sum = (case sum
	   of INL x => ()
	    | INR x => f x
	  (* end case *))

    fun fold (fl, fr) init sum = (case sum
	   of INL x => fl (x, init)
	    | INR x => fr (x, init)
	  (* end case *))

    fun proj (INL x) = x
      | proj (INR x) = x

    fun partition sums = let
	  fun lp ([], ls, rs) = (List.rev ls, List.rev rs)
	    | lp ((INL x)::sums, ls, rs) = lp (sums, x::ls, rs)
	    | lp ((INR x)::sums, ls, rs) = lp (sums, ls, x::rs)
	  in
	    lp (sums, [], [])
	  end

  end
