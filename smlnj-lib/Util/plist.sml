(* plist.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Property lists using Stephen Weeks's implementation.
 *)

structure PropList :> PROP_LIST =
  struct

    type holder = exn list ref

    fun newHolder() : holder = ref []

    fun hasProps (ref []) = false
      | hasProps _ = true

    fun clearHolder r = (r := [])

    fun sameHolder (r1 : holder, r2) = (r1 = r2)

    fun mkProp () = let
	  exception E of 'a
	  fun cons (a, l) = E a :: l
	  fun peek [] = NONE
	    | peek (E a :: _) = SOME a
	    | peek (_ :: l) = peek l
	  fun delete [] = []
	    | delete (E a :: r) = r
	    | delete (x :: r) = x :: delete r
	  in
	    { cons = cons, peek = peek, delete = delete }
	  end

    fun mkFlag () = let
	  exception E
	  fun peek [] = false
	    | peek (E :: _) = true
	    | peek (_ :: l) = peek l
	  fun set (l, flg) = let
		fun set ([], _) = if flg then E::l else l
		  | set (E::r, xs) = if flg then l else List.revAppend(xs, r)
		  | set (x::r, xs) = set (r, x::xs)
		in
		  set (l, [])
		end
	  in
	    { set = set, peek = peek }
	  end

    fun newProp (selHolder : 'a -> holder, init : 'a -> 'b) = let
	  val {peek, cons, delete} = mkProp()
	  fun peekFn a = peek(!(selHolder a))
	  fun getF a = let
		val h = selHolder a
		in
		  case peek(!h)
		   of NONE => let val b = init a in h := cons(b, !h); b end
		    | (SOME b) => b
		  (* end case *)
		end
	  fun clrF a = let
		val h = selHolder a
		in
		  h := delete(!h)
		end
	  fun setFn (a, x) = let
		val h = selHolder a
		in
		  h := cons(x, delete(!h))
		end
	  in
	    {peekFn = peekFn, getFn = getF, clrFn = clrF, setFn = setFn}
	  end

    fun newFlag (selHolder : 'a -> holder) = let
	  val {peek, set} = mkFlag()
	  fun getF a = peek(!(selHolder a))
	  fun setF (a, flg) = let
		val h = selHolder a
		in
		  h := set(!h, flg)
		end
	  in
	    {getFn = getF, setFn = setF}
	  end

  end

