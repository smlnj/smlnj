(* string-cvt.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure StringCvt : STRING_CVT =
  struct

    datatype radix = BIN | OCT | DEC | HEX
    datatype realfmt
      = EXACT
      | SCI of int option
      | FIX of int option
      | GEN of int option
    type ('a, 'b) reader = 'b -> ('a * 'b) option

    val op + = InlineT.Int.+
    val op - = InlineT.Int.-
    val op < = InlineT.Int.<
    val op > = InlineT.Int.>

    local
      fun fillStr (c, s, i, n) = let
	    val stop = i+n
	    fun fill j = if (j < stop)
		  then (InlineT.CharVector.update(s, j, c); fill(j+1))
		  else ()
	    in
	      fill i
	    end
      fun cpyStr (src, srcLen, dst, start) = let
	    fun cpy (i, j) = if (i < srcLen)
		  then (
		    InlineT.CharVector.update(dst, j, InlineT.CharVector.sub(src, i));
		    cpy (i+1, j+1))
		  else ()
	    in
	      cpy (0, start)
	    end
    in
    fun padLeft padChr wid s = let
	  val len = InlineT.CharVector.length s
	  val pad = wid - len
	  in
	    if (pad > 0)
	      then let
		val s' = PreString.create wid
		in
		  fillStr (padChr, s', 0, pad);
		  cpyStr (s, len, s', pad);
		  s'
		end
	      else s
	  end
    fun padRight padChr wid s = let
	  val len = InlineT.CharVector.length s
	  val pad = wid - len
	  in
	    if (pad > 0)
	      then let
		val s' = PreString.create wid
		in
		  fillStr (padChr, s', len, pad);
		  cpyStr (s, len, s', 0);
		  s'
		end
	      else s
	  end
    end (* local *)

    fun revImplode (0, _) = ""
      | revImplode (n, chars) = PreString.revImplode(n, chars)

    fun splitl pred getc rep = let
	  fun lp (n, chars, rep) = (case (getc rep)
		 of NONE => (revImplode(n, chars), rep)
		  | SOME(c, rep') => if (pred c)
		      then lp(n+1, c::chars, rep')
		      else (revImplode(n, chars), rep)
		(* end case *))
	  in
	    lp (0, [], rep)
	  end
    fun takel pred getc rep = let
	  fun lp (n, chars, rep) = (case (getc rep)
		 of NONE => revImplode(n, chars)
		  | SOME(c, rep') => if (pred c)
		      then lp(n+1, c::chars, rep')
		      else revImplode(n, chars)
		(* end case *))
	  in
	    lp (0, [], rep)
	  end
    fun dropl pred getc = let
	  fun lp rep = (case (getc rep)
		 of NONE => rep
		  | SOME(c, rep') => if (pred c) then lp rep' else rep
		(* end case *))
	  in
	    lp
	  end
    val skipWS = PreBasis.skipWS

  (* the cs type is the type used by scanString to represent a stream of
   * characters; we use the current index in the string being scanned.
   *)
    type cs = int
    val scanString = PreBasis.scanString

  end


