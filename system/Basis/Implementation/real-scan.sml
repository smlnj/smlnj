(* real-scan.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure RealScan : sig

    val scanReal : (char, 'a) StringCvt.reader -> (real, 'a) StringCvt.reader
(* REAL32: should be to LargeReal.real *)

  end = struct

    structure W = InlineT.Word
    structure R = InlineT.Real64
    structure U = ScanUtil

    val iadd = InlineT.Int.fast_add

  (* codes copied from ScanUtil *)
    val plusCode : word = 0w129		(* code for #"+" *)
    val minusCode : word = 0w130	(* code for #"-" and #"~" *)
    val ptCode : word = 0w131		(* code for #"." *)
    val eCode : word = 0w14		(* code for #"e" and #"E" *)

  (* scan a string of decimal digits (starting with d), and return their
   * value as a real number.  Also return the number of digits, and the
   * rest of the stream.
   *)
    fun fscan10 getc (d, cs) = let
	  fun wordToReal d = R.from_int(W.toIntX d)
	  fun scan (accum, n, cs) = (case (getc cs)
		 of (SOME(c, cs')) => let val d = U.code c
		      in
			if (d <= 0w9)
			  then scan(R.+(R.*(10.0, accum), wordToReal d), iadd(n, 1), cs')
			  else SOME(accum, n, cs)
		      end
		  | NONE => SOME(accum, n, cs)
		(* end case *))
	  in
	    if (d <= 0w9) then scan(wordToReal d, 1, cs) else NONE
	  end

    local
      val negTbl = #[
	      1.0E~0, 1.0E~1, 1.0E~2, 1.0E~3, 1.0E~4,
	      1.0E~5, 1.0E~6, 1.0E~7, 1.0E~8, 1.0E~9
	    ]
      val posTbl = #[
	      1.0E0, 1.0E1, 1.0E2, 1.0E3, 1.0E4,
	      1.0E5, 1.0E6, 1.0E7, 1.0E8, 1.0E9
	    ]
    in
    fun scaleUp (r, exp) = if R.==(r, 0.0)
	  then r
	  else let
	    fun lp (r, 0) = r
	      | lp (r, exp) = if R.==(Real64Values.negInf, r)
		  orelse R.==(Real64Values.posInf, r)
		    then r
		  else if (exp < 10)
		    then (R.*(r, InlineT.PolyVector.sub(posTbl, exp)))
		    else lp (R.*(1.0E10, r), exp - 10)
	    in
	      lp (r, exp)
	    end
    fun scaleDown (r, 0) = r
      | scaleDown (r, exp) = if R.==(r, 0.0)
	    then r
	  else if (exp < 10)
	    then (R.*(r, InlineT.PolyVector.sub(negTbl, exp)))
	    else scaleDown (R.*(1.0E~10, r), exp - 10)
    end (* local *)

  (* scan special reals: "inf"|"infinity"|"nan".
   * Note that the names are case insesitive.
   *)
    local
      (* suffixes *)
      val nfChrs = [#"n",#"f"]                          (* [i]nf *)
      val inityChrs = [#"i",#"n",#"i",#"t",#"y"]        (* [inf]inity *)
      val anChrs = [#"a", #"n"]                         (* [n]an *)
    in
    fun scanSpecial getc (neg, cs) = let
          fun match (cs, []) = SOME cs
            | match (cs, c::cr) = (case getc cs
                 of SOME(c', cs') => if (c = Char.toLower c')
                      then match (cs', cr)
                      else NONE
                  | NONE => NONE
                (* end case *))
          fun infinity cs = if neg
                then SOME(Real64Values.negInf, cs)
                else SOME(Real64Values.posInf, cs)
          fun nan cs = if neg
                then SOME(Real64Values.negNaN, cs)
                else SOME(Real64Values.posNaN, cs)
          in
            case getc cs
             of SOME((#"I" | #"i"), cs') => (case match (cs', nfChrs)
                   of SOME cs'' => (case match (cs'', inityChrs)
                         of SOME cs''' => infinity cs'''
                          | NONE => infinity cs''
                        (* end case *))
                    | NONE => NONE
                  (* end case *))
              | SOME((#"N" | #"n"), cs') => (case match (cs', anChrs)
                   of SOME cs' => nan cs'
                    | NONE => NONE
                  (* end case *))
              | _ => NONE
            (* end case *)
          end
    end (* local *)

  (* scan the optional sign *)
    fun scanSign getc cs = (case getc cs
           of SOME(#"~", cs) => (true, cs)
            | SOME(#"-", cs) => (true, cs)
            | SOME(#"+", cs) => (false, cs)
            | _ => (false, cs)
          (* end case *))

  (* scanning real literals from strings.  If the number is too large, it should
   * be represented by +/- infinity.
   *)
    fun scanReal getc cs = let
          val scanSign = scanSign getc
          val scanSpecial = scanSpecial getc
	  fun scan10 cs = (case (getc cs)
		 of (SOME(c, cs)) => fscan10 getc (U.code c, cs)
		  | NONE => NONE
		(* end case *))
	  fun getFrac rest = (case (scan10 rest)
		 of SOME(frac, n, rest) => SOME(scaleDown(frac, n), rest)
		  | NONE => NONE
		(* end case *))
	  fun negate (true, num) = R.~ num
	    | negate (false, num) = num
	(* scan the exponent; return a triple (optExp, overflow, rest), where
	 * optExp is the integer value of the exponent (NONE for no exponent),
	 * overflow will be true if the exponent overflowed, and rest is the
	 * unconsumed part of the character stream.
	 *)
	  fun scanExp cs = (case (getc cs)
		 of SOME(c, cs) => let
		      val d = U.code c
		    (* get the digits of the exponent *)
		      fun scan (cs, digits) = (case (getc cs)
			     of SOME(c, cs') => let val d = U.code c
				  in
				    if (d <= 0w9)
				      then scan (cs', W.toIntX d :: digits)
				      else (digits, cs)
				  end
			      | NONE => (digits, cs)
			    (* end case *))
		     (* convert digits to integer exponent *)
		      fun digitsToInt [] = 0
			| digitsToInt (d::digits) = iadd(d, 10 * digitsToInt digits)
		      in
			if (d <= 0w9)
			  then let
			    val (digits, rest) = scan (cs, [W.toIntX d])
			    in
			      (SOME(digitsToInt digits), false, rest)
				handle Overflow => (NONE, true, rest)
			    end
			  else (NONE, false, cs)
		      end
		  | NONE => (NONE, false, cs)
		(* end case *))
	  fun getExp (neg, num, cs) = (case (getc cs)
		 of (SOME(c, cs1)) =>
		      if (U.code c = eCode)
		        then (case (getc cs1)
			   of SOME(c, cs2) => let
			      (* get the sign of the exponent *)
				val codeC = U.code c
				val (negExp, cs3) =
				      if (codeC = minusCode) then (true, cs2)
				      else if (codeC = plusCode) then (false, cs2)
				      else (false, cs1)  (* no sign *)
				val (optExp, overflow, cs4) = scanExp cs3
				in
				  if R.!=(num, 0.0)
				    then (case (optExp, overflow)
				       of (_, true) => if negExp
					    then SOME(negate(neg, 0.0), cs4)
					    else SOME(negate(neg, Real64Values.posInf), cs4)
					| (SOME exp, _) => let
					    val num = negate(neg, num)
					    in
					      if negExp
						then SOME(scaleDown(num, exp), cs4)
						else SOME(scaleUp(num, exp), cs4)
					    end
					| (NONE, _) => SOME(negate(neg, num), cs)
				      (* end case *))
				    else SOME(negate(neg, 0.0), cs)
				end
			    | NONE => SOME(negate(neg, num), cs)
			  (* end case *))
			else SOME(negate(neg, num), cs)
		  | NONE => SOME(negate(neg, num), cs)
		(* end case *))
          val cs = StringCvt.skipWS getc cs
          val (neg, cs) = scanSign cs
	  in
            case getc cs
             of SOME(#".", rest) => (case getFrac rest
                   of SOME(frac, rest) => getExp(neg, frac, rest)
                    | NONE => NONE (* initial point not followed by digit *)
                  (* end case *))
              | SOME(c, rest) => if Char.isDigit c
                  then (
                  (* get whole number part *)
                    case fscan10 getc (U.code c, rest)
                     of SOME(whole, _, rest) => (case (getc rest)
                           of SOME(#".", rest') => (
                              (* whole part followed by point, get fraction *)
                                case getFrac rest'
                                 of SOME(frac, rest'') => (* fraction exists *)
                                     getExp(neg, R.+(whole, frac), rest'')
                                  | NONE =>
                                     (* no fraction -- point terminates num *)
                                     SOME(negate(neg, whole), rest)
                                (* end case *))
                            | _ => getExp(neg, whole, rest)
                         (* end case *))
                     | NONE => NONE (* ASSERT: this case can't happen *)
		   (* end case *))
                  else scanSpecial (neg, cs)
              | NONE => NONE
            (* end case *)
          end

  end
