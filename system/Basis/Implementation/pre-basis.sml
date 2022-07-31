(* pre-basis.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This contains definitions of various Basis types that are
 * abstract but need to be concrete to the basis implementation.
 * It also has some ultility functions.
 *
 *)

structure PreBasis =
  struct

    local
      val op - = InlineT.Int.-
      val op + = InlineT.Int.+
      val op < = InlineT.Int.<
    in


  (* the time type is abstract in the time structure, but other modules need
   * access to it.  Here we open the type-only Time structure to expose the
   * representation.
   *)
    open Time


  (***************************************************************************
   * These definitions are part of the StringCvt structure, but are defined here
   * so that they can be used in other basis modules.
   *)

    fun scanString scanFn s = let
	  val n = InlineT.CharVector.length s
	  fun getc i =
	    if (i < n) then SOME(InlineT.CharVector.sub(s, i), i+1) else NONE
	  in
	    case (scanFn getc 0)
	     of NONE => NONE
	      | SOME(x, _) => SOME x
	    (* end case *)
	  end

    fun skipWS (getc : 'a -> (char * 'a) option) = let
	  fun isWS (#" ") = true
	    | isWS (#"\t") = true
	    | isWS (#"\n") = true
	    | isWS _ = false
	  fun lp cs = (case (getc cs)
		 of (SOME(c, cs')) => if (isWS c) then lp cs' else cs
		  | NONE => cs
		(* end case *))
	  in
	    lp
	  end

  (* get n characters from a character source (this is not a visible part of
   * StringCvt.
   *)
    fun getNChars (getc : 'a -> (char * 'a) option) (cs, n) = let
	  fun rev ([], l2) = l2
	    | rev (x::l1, l2) = rev(l1, x::l2)
	  fun get (cs, 0, l) = SOME(rev(l, []), cs)
	    | get (cs, i, l) = (case getc cs
		 of NONE => NONE
		  | (SOME(c, cs')) => get (cs', i-1, c::l)
		(* end case *))
	  in
	    get (cs, n, [])
	  end

    end (* local *)

  end
