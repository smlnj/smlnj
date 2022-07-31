(* pre-string.sml
 *
 * COPYRIGHT (c) 2015 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Some common operations that are used by both the String and
 * Substring structures, as well as by the pervasive environment.
 *)

local
    infix 7  * /  mod  div
    infix 6 ^ + -
    infix 3 := o
    infix 4 > < >= <= = <>
    infixr 5 :: @
    infix 0 before

    open PrimTypes
in

structure PreString =
  struct

    local
      open PrePervasive

      structure C = InlineT.Char

      val op + = InlineT.Int.+
      val op - = InlineT.Int.-
      val op * = InlineT.Int.*
      val op quot = InlineT.Int.quot
      val op < = InlineT.Int.<
      val op <= = InlineT.Int.<=
      val op > = InlineT.Int.>
      val op >= = InlineT.Int.>=
(*      val op = = InlineT.= *)
      val unsafeSub = InlineT.CharVector.sub
      val unsafeUpdate = InlineT.CharVector.update
      val unsafeCreate = Assembly.A.create_s
      val maxSize = Core.max_length
      val size = InlineT.CharVector.length
    in

  (* allocate an uninitialized string of given length (with a size check) *)
    fun create n = if (InlineT.Int.ltu(maxSize, n))
	  then raise Core.Size
	  else unsafeCreate n

  (* a vector of single character strings *)
    val chars = let
	  fun next i = if (i <= C.maxOrd)
	        then let
		  val s = unsafeCreate 1
		  in
		    unsafeUpdate(s, 0, C.chr i);  s :: next(i+1)
		  end
	        else []
          in
	    Assembly.A.create_v(C.maxOrd+1, next 0)
          end

    fun unsafeSubstring (_, _, 0) = ""
      | unsafeSubstring (s, i, 1) =
	  InlineT.PolyVector.sub (chars, InlineT.cast (unsafeSub (s, i)))
      | unsafeSubstring (s, i, n) = let
	  val ss = unsafeCreate n
	  fun copy j = if (j = n)
		then ()
		else (unsafeUpdate(ss, j, unsafeSub(s, i+j)); copy(j+1))
	  in
	    copy 0; ss
	  end

  (* concatenate a pair of non-empty strings *)
    fun concat2 (x, y) = let
	  val xl = size x and yl = size y
	  val ss = create(xl+yl)
	  fun copyx n = if (n = xl)
		then ()
		else (unsafeUpdate(ss, n, unsafeSub(x, n)); copyx(n+1))
	  fun copyy n = if (n = yl)
		then ()
		else (unsafeUpdate(ss, xl+n, unsafeSub(y,n)); copyy(n+1))
	  in
	    copyx 0; copyy 0;
	    ss
	  end

  (* given a reverse order list of strings and a total length, return
   * the concatenation of the list.
   *)
    fun revConcat (0, _) = ""
      | revConcat (1, lst) = let
	  fun find ("" :: r) = find r
	    | find (s :: _) = s
	    | find _ = "" (** impossible **)
	  in
	    find lst
	  end
      | revConcat (totLen, lst) = let
	  val ss = create totLen
	  fun copy ([], _) = ()
	    | copy (s::r, i) = let
		val len = size s
		val i = i - len
		fun copy' j = if (j = len)
		      then ()
		      else (
			unsafeUpdate(ss, i+j, unsafeSub(s, j));
			copy'(j+1))
		in
		  copy' 0;
		  copy (r, i)
		end
	  in
	    copy (lst, totLen);  ss
	  end

  (* map a translation function across the characters of a substring *)
    fun translate (tr, s, i, n) = let
	  val stop = i+n
	  fun mkList (j, totLen, lst) = if (j < stop)
		then let val s' = tr (unsafeSub (s, j))
		  in
		    mkList (j+1, totLen + size s', s' :: lst)
		  end
		else revConcat (totLen, lst)
	  in
	    mkList (i, 0, [])
	  end

  (* implode a non-empty list of characters into a string where both the
   * length and list are given as arguments.
   *)
    fun implode (len, cl) = let
	  val ss = create len
	  fun copy ([], _) = ()
	    | copy (c::r, i) = (InlineT.CharVector.update(ss, i, c); copy(r, i+1))
	  in
	    copy (cl, 0); ss
	  end

  (* implode a reversed non-empty list of characters into a string where both the
   * length and list are given as arguments.
   *)
    fun revImplode (len, cl) = let
	  val ss = create len
	  fun copy ([], _) = ()
	    | copy (c::r, i) = (InlineT.CharVector.update(ss, i, c); copy(r, i-1))
	  in
	    copy (cl, len-1); ss
	  end

    fun isPrefix (s1, s2, i2, n2) = let
	  val n1 = size s1
	  fun eq (i, j) =
		(i >= n1)
		orelse ((unsafeSub(s1, i) = unsafeSub(s2, j)) andalso eq(i+1, j+1))
	  in
	    (n2 >= n1) andalso eq (0, i2)
	  end

    fun collate cmpFn (s1, i1, n1, s2, i2, n2) = let
	  val (n, order) =
		if (n1 = n2) then (n1, EQUAL)
		else if (n1 < n2) then (n1, LESS)
		else (n2, GREATER)
	  fun cmp' i = if (i = n)
		then order
		else let
		  val c1 = unsafeSub(s1, i1+i)
		  val c2 = unsafeSub(s2, i2+i)
		  in
		    case (cmpFn(c1, c2))
		     of EQUAL => cmp' (i+1)
		      | order => order
		    (* end case *)
		  end
	  in
	    cmp' 0
	  end

    fun cmp (s1, i1, n1, s2, i2, n2) = let
	  fun cmpFn (c1, c2) =
		if (c1 = c2) then EQUAL
		else if (C.>(c1, c2)) then GREATER
		else LESS
	  in
	    collate cmpFn (s1, i1, n1, s2, i2, n2)
	  end

    (* Knuth-Morris-Pratt String Matching
     *
     * val kmp : string -> string * int * int -> int option
     * Find the first string within the second, starting at and
     * ending before the given positions.
     * Return the starting position of the match
     * or the given ending position if there is no match. *)
    fun kmp pattern =
	let val psz = size pattern
	    val next = InlineT.PolyArray.array (psz, ~1)

	    fun pat x = unsafeSub (pattern, x)
	    fun nxt x = InlineT.PolyArray.sub (next, x)
	    fun setnxt (i, x) = InlineT.PolyArray.update (next, i, x)

	    (* trying to fill next at position p (> 0) and higher;
	     * invariants: x >= 0
             *             pattern[0..x) = pattern[p-x..p)
	     *             for i in [0..p) :
	     *                 pattern[i] <> pattern[next[i]]
	     *                 pattern[0..next[i]) = pattern[i-next[i]..i) *)
	    fun fill (p, x) = if p >= psz then ()
			      else if pat x = pat p then dnxt (p, nxt x, x + 1)
			      else dnxt (p, x, nxt x + 1)
	    and dnxt (p, x, y) = (setnxt (p, x); fill (p + 1, y))

	    (* Once next has been initialized, it serves the following purpose:
	     * Suppose we are looking at text position t and pattern position
	     * p.  This means that all pattern positions < p have already
	     * matched the text positions that directly precede t.
	     * Now, if the text[t] matches pattern[p], then we simply advance
	     * both t and p and try again.
	     * However, if the two do not match, then we simply
	     * try t again, but this time with the pattern position
	     * given by next[p].
	     * Success is when p reaches the end of the pattern, failure is
	     * when t reaches the end of the text without p having reached the
	     * end of the pattern. *)
	    fun search (text, start, tsz) =
		let fun txt x = unsafeSub (text, x)
		    fun loop (p, t) =
			if p >= psz then t - psz
			else if t >= tsz then tsz
			else if p < 0 orelse pat p = txt t then loop (p+1, t+1)
			else loop (nxt p, t)
		in loop (0, start)
		end
	in fill (1, 0); search
	end

    end (* local *)
  end; (* PreString *)
end
