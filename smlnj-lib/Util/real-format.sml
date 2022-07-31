(* real-format.sml
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories.
 *
 * Basic real to string conversions.  This module is use internally, but is
 * not part of the exported library interface.  It duplicates code in the
 * SML/NJ boot directory, but it is more portable not to rely on it.
 *
 * AUTHOR:  Emden Gansner & John Reppy
 *	    AT&T Bell Laboratories
 *	    Murray Hill, NJ 07974
 *	    erg@ulysses.att.com & jhr@research.att.com
 *)

structure RealFormat : sig

  (* Low-level real to string conversion routines. For F and E format, the precision
   * specifies the number of fractional digits with 0's appended if necessary.
   * For G format, precision specifies the number of significant digits, but
   * trailing 0's in the fractional part are dropped.
   *)
    val realFFormat : (real * int) -> {sign : bool, mantissa : string}
    val realEFormat : (real * int) -> {sign : bool, mantissa : string, exp : int}
    val realGFormat : (real * int)
	  -> {sign : bool, whole : string, frac : string, exp : int option}

  end = struct

    exception BadPrecision
	(* raised by real to string conversions, if the precision is < 0. *)

    fun zeroLPad (s, w) = StringCvt.padLeft #"0" w s
    fun zeroRPad (s, w) = StringCvt.padRight #"0" w s

  (* convert an integer between 0..9 to a single digit *)
    fun mkDigit (i : int) = String.sub("0123456789", i)

  (* decompose a non-zero real into a list of at most maxPrec significant digits
   * (the first digit non-zero), and integer exponent. The return value
   *   (a::b::c..., exp)
   * is produced from real argument
   *   a.bc... * (10 ^^ exp)
   * If the list would consist of all 9's, the list consisting of 1 followed by
   * all 0's is returned instead.
   *)
    val maxPrec = 15
    fun decompose (f, e, precisionFn) = let
	  fun scaleUp (x, e) = if (x < 1.0) then scaleUp(10.0*x, e-1) else (x, e)
	  fun scaleDn (x, e) = if (x >= 10.0) then scaleDn(0.1*x, e+1) else (x, e)
	  fun mkdigits (f, 0) = ([], if f < 5.0 then 0 else 1)
	    | mkdigits (f, i) = let 
		val d = floor f
		val (digits, carry) = mkdigits (10.0 * (f - real d), i - 1)
		val (digit, c) = (case (d, carry)
		       of (9, 1) => (0, 1)
			| _ => (d + carry, 0)
		      (* end case *))
		in
		  (digit::digits, c)
		end
	  val (f, e) = if (f < 1.0)
		  then scaleUp (f, e)
		else if (f >= 10.0)
		  then scaleDn (f, e)
		  else (f, e)
	  val (digits, carry) =
		mkdigits(f, Int.max(0, Int.min(precisionFn e, maxPrec)))
	  in
	    case carry
	     of 0 => (digits, e)
	      | _ => (1::digits, e+1)
          end

    fun realFFormat (r, prec) = let
	  fun pf e = e + prec + 1
	  fun rtoa (digits, e) = let
		fun doFrac (_, 0, l) = implode(rev l)
		  | doFrac ([], p, l) = doFrac([], p-1, #"0"::l)
		  | doFrac (hd::tl, p, l) = doFrac(tl, p-1, (mkDigit hd) :: l)
		fun doWhole ([], e, l) = if e >= 0
			then doWhole ([], e-1, #"0" :: l)
		      else if prec = 0
			then implode(rev l)
			else doFrac ([], prec, #"." :: l)
		  | doWhole (arg as (hd::tl), e, l) = if e >= 0
			then doWhole(tl, e-1, (mkDigit hd) :: l)
		      else if prec = 0
			then implode(rev l)
			else doFrac(arg, prec, #"." :: l)
		fun doZeros (n, 0, l) = implode(rev l)
		  | doZeros (1, p, l) = doFrac(digits, p, l)
		  | doZeros (n, p, l) = doZeros(n-1, p-1, #"0" :: l)
		in
		  if (e >= 0)
		    then doWhole(digits, e, [])
		  else if (prec = 0)
		    then "0"
		    else doZeros (~e, prec, [#".", #"0"])
		end
	  in
	    if (prec < 0) then raise BadPrecision else ();
	    if (r < 0.0)
	      then {sign = true, mantissa = rtoa(decompose(~r, 0, pf))}
	    else if (r > 0.0)
	      then {sign=false, mantissa = rtoa(decompose(r, 0, pf))}
	    else if (prec = 0)
	      then {sign=false, mantissa = "0"}
	      else {sign=false, mantissa = zeroRPad("0.", prec+2)}
	  end (* realFFormat *)

    fun realEFormat (r, prec) = let
	  fun pf _ = prec + 1
	  fun rtoa (sign, (digits, e)) = let
		fun mkRes (m, e) = {sign = sign, mantissa = m, exp = e}
		fun doFrac (_, 0, l)  = implode(rev l)
		  | doFrac ([], n, l) = zeroRPad(implode(rev l), n)
		  | doFrac (hd::tl, n, l) = doFrac (tl, n-1, (mkDigit hd) :: l)
		in
		  if (prec = 0)
		    then mkRes(String.str(mkDigit(hd digits)), e)
		    else
		      mkRes(doFrac(tl digits, prec, [#".", mkDigit(hd digits)]), e)
		end
	  in
	    if (prec < 0) then raise BadPrecision else ();
	    if (r < 0.0)
	      then rtoa (true, decompose(~r, 0, pf))
	    else if (r > 0.0)
	      then rtoa (false, decompose(r, 0, pf))
	    else if (prec = 0)
	      then {sign = false, mantissa = "0", exp = 0}
	      else {sign = false, mantissa = zeroRPad("0.", prec+2), exp=0}
	  end (* realEFormat *)

    fun realGFormat (r, prec) = let
	  fun pf _ = prec
	  fun rtoa (sign, (digits, e)) = let
		fun mkRes (w, f, e) = {sign = sign, whole = w, frac = f, exp = e}
		fun doFrac [] = []
		  | doFrac (0::tl) = (case doFrac tl
		       of [] => []
			| rest => #"0" :: rest
		      (* end case *))
		  | doFrac (hd::tl) = (mkDigit hd) :: (doFrac tl)
		fun doWhole ([], e, wh) =
		      if e >= 0
			then doWhole([], e-1, #"0"::wh)
			else mkRes(implode(rev wh), "", NONE)
		  | doWhole (arg as (hd::tl), e, wh) =
		      if e >= 0
			then doWhole(tl, e-1, (mkDigit hd)::wh)
			else mkRes(implode(rev wh), implode(doFrac arg), NONE)
		in
		  if (e < ~4) orelse (e >= prec)
		    then mkRes(
		      String.str(mkDigit(hd digits)),
		      implode(doFrac(tl digits)), SOME e)
		  else if e >= 0
		    then doWhole(digits, e, [])
		    else let
		      val frac = implode(doFrac digits)
		      in
			mkRes("0", zeroLPad(frac, (size frac) + (~1 - e)), NONE)
		      end
		end
	  in
	    if (prec < 1) then raise BadPrecision else ();
	    if (r < 0.0)
	      then rtoa(true, decompose(~r, 0, pf))
	    else if (r > 0.0)
	      then rtoa(false, decompose(r, 0, pf))
	      else {sign=false, whole="0", frac="", exp=NONE}
	  end (* realGFormat *)

  end (* RealFormat *)
