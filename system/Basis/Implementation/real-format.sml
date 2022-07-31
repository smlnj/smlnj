(* real-format.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Code for converting from real (IEEE 64-bit floating-point) to string.
 * This ought to be replaced with David Gay's conversion algorithm.
 *
 *)

structure RealFormat : sig

    val fmtReal : StringCvt.realfmt -> LargeReal.real -> string

  end = struct

    structure String = StringImp

    infix 4 == !=

    val op +  = InlineT.Real64.+
    val op -  = InlineT.Real64.-
    val op *  = InlineT.Real64.*
    val op /  = InlineT.Real64./
    val op ~  = InlineT.Real64.~
    val op <  = InlineT.Real64.<
    val op >  = InlineT.Real64.>
    val op >= = InlineT.Real64.>=
    val op == = InlineT.Real64.==
    val op != = InlineT.Real64.!=

    val real  = InlineT.Real64.from_int
    val floor = InlineT.Real64.floor

    val op ^  = String.^
    val implode = String.implode
    val concat = String.concat
    val size = String.size

    structure I = InlineT.Int
    fun inc i = I.+(i, 1)
    fun dec i = I.-(i, 1)
    fun min (i, j) = if I.<(i, j) then i else j
    fun max (i, j) = if I.>(i, j) then i else j

    val atoi = (NumFormat32.fmtInt StringCvt.DEC) o InlineT.Int32.fromInt

    fun zeroLPad (s, wid) = StringCvt.padLeft #"0" wid s
    fun zeroRPad (s, wid) = StringCvt.padRight #"0" wid s

    fun mkDigit d = InlineT.CharVector.sub("0123456789abcdef", d)

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
	  fun scaleUp (x, e) =
		if (x < 1.0) then scaleUp(10.0*x, dec e) else (x, e)
	  fun scaleDn (x, e) =
		if (x >= 10.0) then scaleDn(0.1*x, inc e) else (x, e)
	  fun mkdigits (f, 0, odd) = ([], if f < 5.0 then 0
					  else if f > 5.0 then 1
					  else odd)
	    | mkdigits (f, i, _) = let
		val d = floor f
		val (digits, carry) = mkdigits (10.0 * (f - real d), dec i,
						I.mod(d,2))
		val (digit, c) = (case (d, carry)
		       of (9, 1) => (0, 1)
			| _ => (I.+(d, carry), 0)
		      (* end case *))
		in
		  (digit::digits, c)
		end
	  val (f, e) = if (f < 1.0)
		  then scaleUp (f, e)
		else if (f >= 10.0)
		  then scaleDn (f, e)
		  else (f, e)
	  val (digits, carry) = mkdigits(f, max(0, min(precisionFn e, maxPrec)),0)
	  in
	    case carry
	     of 0 => (digits, e)
	      | _ => (1::digits, inc e)
	    (* end case *)
          end

    fun realFFormat (r, prec) = let
	  fun pf e = I.+(e, inc prec)
	  fun rtoa (digits, e) = let
		fun doFrac (_, 0, n, l) = PreString.revImplode(n, l)
		  | doFrac ([], p, n, l) = doFrac([], dec p, inc n, #"0"::l)
		  | doFrac (hd::tl, p, n, l) =
		      doFrac(tl, dec p, inc n, (mkDigit hd) :: l)
		fun doWhole ([], e, n, l) = if I.>=(e, 0)
			then doWhole ([], dec e, inc n, #"0" :: l)
		      else if prec = 0
			then PreString.revImplode(n, l)
			else doFrac ([], prec, inc n, #"." :: l)
		  | doWhole (arg as (hd::tl), e, n, l) = if I.>=(e, 0)
			then doWhole(tl, dec e, inc n, (mkDigit hd) :: l)
		      else if prec = 0
			then PreString.revImplode(n, l)
			else doFrac(arg, prec, inc n, #"." :: l)
		fun doZeros (_, 0, n, l) = PreString.revImplode(n, l)
		  | doZeros (1, p, n, l) = doFrac(digits, p, n, l)
		  | doZeros (e, p, n, l) = doZeros(dec e, dec p, inc n, #"0" :: l)
		in
		  if I.>=(e, 0)
		    then doWhole(digits, e, 0, [])
		  else if (prec = 0)
		    then "0"
		    else doZeros (I.~ e, prec, 2, [#".", #"0"])
		end
	  in
	    if I.<(prec, 0) then raise General.Size else ();
	    if (r < 0.0)
	      then {sign = "~", mantissa = rtoa(decompose(~r, 0, pf))}
	    else if (r > 0.0)
	      then {sign="", mantissa = rtoa(decompose(r, 0, pf))}
	    else {
		sign = if (InlineT.Real64.signBit r) then "~" else "",
		mantissa = if (prec = 0) then "0" else zeroRPad("0.", I.+(prec, 2))
	      }
	  end (* realFFormat *)

    fun realEFormat (r, prec) = let
	  fun pf _ = inc prec
	  fun rtoa (sign, (digits, e)) = let
		fun mkRes (m, e) = {sign = sign, mantissa = m, exp = e}
		fun doFrac (_, 0, l)  = implode(List.rev l)
		  | doFrac ([], n, l) = zeroRPad(implode(List.rev l), n)
		  | doFrac (hd::tl, n, l) = doFrac (tl, dec n, (mkDigit hd) :: l)
		in
		  if (prec = 0)
		    then mkRes(String.str(mkDigit(List.hd digits)), e)
		    else mkRes(
		      doFrac(List.tl digits, prec, [#".", mkDigit(List.hd digits)]), e)
		end
	  in
	    if I.<(prec, 0) then raise General.Size else ();
	    if (r < 0.0)
	      then rtoa ("~", decompose(~r, 0, pf))
	    else if (r > 0.0)
	      then rtoa ("", decompose(r, 0, pf))
	    else {
		sign = if (InlineT.Real64.signBit r) then "~" else "",
		mantissa = if (prec = 0) then "0" else zeroRPad("0.", I.+(prec, 2)),
		exp = 0
	      }
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
		      if I.>=(e, 0)
			then doWhole([], dec e, #"0"::wh)
			else mkRes(implode(List.rev wh), "", NONE)
		  | doWhole (arg as (hd::tl), e, wh) =
		      if I.>=(e, 0)
			then doWhole(tl, dec e, (mkDigit hd)::wh)
			else mkRes(implode(List.rev wh), implode(doFrac arg), NONE)
		in
		  if I.<(e, ~4) orelse I.>=(e, prec)
		    then mkRes(
		      String.str(mkDigit(List.hd digits)),
		      implode(doFrac(List.tl digits)), SOME e)
		  else if I.>=(e, 0)
		    then doWhole(digits, e, [])
		    else let
		      val frac = implode(doFrac digits)
		      in
			mkRes("0", zeroLPad(frac, I.+(size frac, I.-(~1, e))), NONE)
		      end
		end
	  in
	    if I.<(prec, 1) then raise General.Size else ();
	    if (r < 0.0)
	      then rtoa("~", decompose(~r, 0, pf))
	    else if (r > 0.0)
	      then rtoa("", decompose(r, 0, pf))
	    else if (InlineT.Real64.signBit r)
	      then {sign="~", whole="0", frac="", exp=NONE}
	      else {sign="", whole="0", frac="", exp=NONE}
	  end (* realGFormat *)

    fun fmtInfNan x = if (x != x) then "nan"
	  else if (x == Real64Values.posInf) then "inf"
          else "~inf"

    fun isFinite r = (Real64Values.negInf < r) andalso (r < Real64Values.posInf)

  (* convert a real number to a string of the form [~]d.dddE[~]dd, where
   * the precision (number of fractional digits) is specified by the
   * second argument.
   *)
    fun realToSciStr prec r = if isFinite r
	  then let
	    val {sign, mantissa, exp} = realEFormat (r, prec)
	    in
	      (* minimum size exponent string, no padding *)
	      concat[sign, mantissa, "E", atoi exp]
	    end
	  else fmtInfNan r

  (* convert a real number to a string of the form [~]ddd.ddd, where
   * the precision (number of fractional digits) is specified by the
   * second argument.
   *)
    fun realToFixStr prec r = if isFinite r
	  then let
	    val {sign, mantissa} = realFFormat (r, prec)
	    in
	      sign^mantissa
	    end
	  else fmtInfNan r

    fun realToGenStr prec r = if isFinite r
	  then let
	    val {sign, whole, frac, exp} = realGFormat(r, prec)
	    val (frac,expStr) = (case exp
		   of NONE => if (frac = "")
			then (".0", "")
			else ("." ^ frac, "")
		    | (SOME e) => let
			val expStr = if I.<(e, 0)
			      then "E~" ^ zeroLPad(atoi(I.~ e), 2)
			      else "E" ^ zeroLPad(atoi e, 2)
			in
			  ((if (frac = "") then "" else ("." ^ frac)), expStr)
			end
		  (* end case *))
	    in
	      concat[sign, whole, frac, expStr]
	    end
	  else fmtInfNan r

  (* Note that the SML Basis Library specification requires the Size exception to be
   * raised as soon as Real.fmt is applied to an invalid format specifier.
   *)
    fun checkPrec p = if I.<(p, 0) then raise Size else p

    fun fmtReal (StringCvt.SCI NONE) = realToSciStr 6
      | fmtReal (StringCvt.SCI(SOME prec)) = realToSciStr (checkPrec prec)
      | fmtReal (StringCvt.FIX NONE) = realToFixStr 6
      | fmtReal (StringCvt.FIX(SOME prec)) = realToFixStr (checkPrec prec)
      | fmtReal (StringCvt.GEN NONE) = realToGenStr 12
      | fmtReal (StringCvt.GEN(SOME prec)) = realToGenStr (checkPrec prec)
      | fmtReal StringCvt.EXACT =
	  raise Fail "RealFormat: fmtReal: EXACT not supported"

  end
