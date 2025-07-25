(* frep-to-string.sml
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 * Internal conversions for creating strings from IEEEReal.decimal_approx
 * values.  Note that we assume that the inputs are well formed; i.e.,
 *
 *      - the list of digits only contain values in the range [0..9]
 *
 *      - the list of digits is normalized; i.e., there are no trailing
 *        zeros.
 *
 *      - if the class is ZERO, then the list of digits is empty and the
 *        exponent is 0.  Likewise, if the list of digits is empty, then
 *        the class should be ZERO.
 *
 * The interpretation of a IEEEReal.decimal_approx normal or sub-normal
 * number with sign "s", digits "d_1", ..., "d_n", and exponent "e" is
 *
 *      (-1)^s * 0.d_1 ... d_n * 10^e
 *)

structure FRepToString : sig

    (* for implementing Real.fmt *)
    val fmt : StringCvt.realfmt -> FloatRep.float_rep -> string

    (* for implementing Real.toString *)
    val toString : FloatRep.float_rep -> string

    (* for implementing IEEEReal.toString *)
    val toExactString : FloatRep.float_rep -> string

  end = struct

    structure String = StringImp
    structure Int = IntImp

(* should be
    (* fast add/subtract avoiding the overflow test *)
    infix -- ++
    fun x -- y = InlineT.Int.fast_sub(x, y)
    fun x ++ y = InlineT.Int.fast_add(x, y)

    val upd = InlineT.CharVector.update
*)

    datatype realfmt = datatype StringCvt.realfmt
    datatype float_rep = datatype FloatRep.float_rep

    (* wrapper to handle infinities and NaNs *)
    fun wrapper _ (Inf false) = "inf"
      | wrapper _ (Inf true) = "~inf"
      | wrapper _ (NaN _) = "nan"
      | wrapper toS (Zero sgn) = toS (sgn, 0, [], 0)
      | wrapper toS (Normal{sign, nDigits, digits, exp}) =
          toS (sign, nDigits, digits, exp)
      | wrapper toS (Subnormal{sign, nDigits, digits, exp}) =
          toS (sign, nDigits, digits, exp)

    val digits = #["0","1","2","3","4","5","6","7","8","9"]
    fun getDigit d = Vector.sub(digits, d)

    fun prependDigits ([], suffix) = suffix
      | prependDigits (d::dr, suffix) = getDigit d :: prependDigits(dr, suffix)

    (* produce a string of `n` zeros *)
    fun zeros n = CharVector.tabulate(n, fn _ => #"0")
(* should be
    fun zeros n = let
          val s = Assembly.A.create_s n
          fun fill i =
              if i < n then (upd (s, i, #"0"); fill (i ++ 1))
              else ()
          in
            fill 0; s
          end
 *)

    fun prependZeros (0, frags) = frags
      | prependZeros (n, frags) = zeros n :: frags

    fun addSign (false, frags) = frags
      | addSign (true, frags) = "~" :: frags

    (* `roundAndNormalize (ds, m)` rounds the normalized sequence of digits `ds` to `m`
     * digits and returns the triple `(co, ds', nz)`, where
     *
     *    - `ds` is a normalized sequence of `n` digits
     *    - `m <= n` specifies the desired number of digits in the result
     *    - `co` is `true` if the result of rounding produces a carry out
     *    - `ds'` is the normalized result that has `m - nz` digits
     *    - `nz` is the number of trailing zeros removed from the result of
     *      rounding.
     *)
    fun roundAndNormalize (digits, m) = let
          (* given the list of digits in reverse order (lsd first), trim the zeros.
           * we return the remaining digits in msd first order with the number of
           * trimmed zeros.
           *)
          fun trimTrailingZeros ([], nz) = (false, [], nz)
            | trimTrailingZeros (0::ds, nz) = trimTrailingZeros (ds, nz+1)
            | trimTrailingZeros (ds, nz) = (false, List.rev ds, nz)
          fun trim (0, [], ds) = trimTrailingZeros (ds, 0)
            | trim (_, [], _) = raise Fail "impossible"
            | trim (0, d::dr, ds) = if (d < 5)
                  then trimTrailingZeros (ds, 0)
                else if (d > 5)
                  then roundWithCarry (true, ds, [], 0)
                else if null dr
                  then trimTrailingZeros (ds, 0)  (* round down *)
                  else roundWithCarry (true, ds, [], 0)
            | trim (m, d::dr, ds) = trim (m-1, dr, d::ds)
          and roundWithCarry (carry, [], ds', nz) = (carry, ds', nz)
            | roundWithCarry (true, d::ds, ds', nz) = let
                val d' = d+1
                in
                  if (d' <= 9)
                    then roundWithCarry (false, ds, d'::ds', nz)
                  else if null ds'
                    then roundWithCarry (true, ds, ds', nz+1) (* normalize *)
                    else roundWithCarry (true, ds, 0::ds', nz)
                end
            | roundWithCarry (false, d::ds, ds', nz) =
                (false, List.revAppend(ds, d::ds'), nz)
          in
            trim (m, digits, [])
          end

    (* `round (ds, m)` rounds the normalized sequence of digits `ds` to `m`
     * digits and returns the pair `(co, ds')`, where
     *
     *    - `ds` is a normalized sequence of `n` digits
     *    - `m <= n` specifies the desired number of digits in the result
     *    - `co` is `true` if the result of rounding produces a carry out
     *    - `ds'` is the normalized result that has `m` digits
     *
     * Note that unlike `roundAndNormalize`, the result sequence `ds'` can
     * have trailing zeros.
     *)
    fun round (digits, m) = let
          fun trim (0, [], ds) = (false, List.rev ds)
            | trim (_, [], _) = raise Fail "impossible"
            | trim (0, d::dr, ds) = if (d < 5)
                  then (false, List.rev ds)
                else if (d > 5)
                  then roundWithCarry (ds, [])
                else if null dr
                  then (false, List.rev ds) (* round down *)
                  else roundWithCarry (ds, [])
            | trim (m, d::dr, ds) = trim (m-1, dr, d::ds)
          and roundWithCarry ([], ds') = (true, ds')
            | roundWithCarry (d::ds, ds') = let
                val d' = d+1
                in
                  if (d' <= 9)
                    then (false, List.revAppend(ds, d'::ds'))
                    else roundWithCarry (ds, 0::ds')
                end
          in
            trim (m, digits, [])
          end

    (* convert to scientific notation: "[~]?[0-9].[0-9]+(E[~]?[0-9]+)?", where `prec`
     * specifies the digits to the right of the decimal point.
     *)
    fun toSci prec (sign, nDigits, digits, exp) = let
          (* adjust exponent for "dddd." => "d.ddd" conversion *)
          val nFracDigits = Int.max(0, nDigits - 1)
          val exp = exp + nFracDigits
          in
            case (prec, digits)
             of (0, []) => if sign then "~0E0" else "0E0"
              | (_, []) => if sign
                  then String.concat["~0.", zeros prec, "E0"]
                  else String.concat["0.", zeros prec, "E0"]
              | (0, _) => (case roundAndNormalize (digits, 1)
                   of (false, digits, nz) => let
                        val frags = [Int.toString exp]
                        val frags = if (nz > 0)
                              then "0E" :: frags
                              else prependDigits(digits, "E" :: frags)
                        in
                          if sign
                            then String.concat("~" :: frags)
                            else String.concat frags
                        end
                    | (true, _, _) => if sign
                        then "~1E" ^ Int.toString(exp+1)
                        else "1E" ^ Int.toString(exp+1)
                  (* end case *))
              | (_, d::dr) => let
                (* round if number of digits to the right of the decimal is
                 * greater than `prec`
                 *)
                val (whole, nFracDigits, frac, exp) = if (nFracDigits <= prec)
                      then (d, nFracDigits, dr, exp)
                      else (case round (digits, prec+1)
                         of (false, d::ds) => (d, prec, ds, exp)
                          | (true, ds) => (1, prec, List.take(ds, prec), exp+1)
                          | _ => raise Fail "impossible"
                        (* end case *))
                val frags = ["E", Int.toString exp]
                val frags = if (nFracDigits < prec)
                      then zeros (prec - nFracDigits) :: frags
                      else frags
                val frags = getDigit whole :: "." :: prependDigits(frac, frags)
                in
                  String.concat (addSign (sign, frags))
                end
            (* end case *)
          end

    fun insertDecimal (_, digits, 0, _) = prependDigits (digits, [])
      | insertDecimal (nDigits, digits, nFrac, prec) = let
          val (w, f) = List.splitAt (digits, nDigits - nFrac)
          val frags = if (nFrac < prec)
                then [zeros (prec - nFrac)]
                else []
          in
            prependDigits (w, "." :: prependDigits (f, frags))
          end

    (* convert to fixed-point notation: "[~]?[0-9]+.[0-9]+?", where `prec` specifies
     * the number of digits to appear after the decimal point.
     *)
    fun toFix prec (sign, 0, _, _) = (case (prec, sign)
           of (0, false) => "0"
            | (0, true) => "~0"
            | (_, false) => "0." ^ zeros prec
            | (_, true) => "~0." ^ zeros prec
          (* end case *))
      | toFix prec (sign, nDigits, digits, exp) =
          if (exp >= 0)
            then let
              val frags = if prec > 0 then [".", zeros prec] else []
              val frags = if exp > 0 then zeros exp :: frags else frags
              val frags = prependDigits(digits, frags)
              in
                String.concat (addSign (sign, frags))
              end
            else let (* exp < 0 *)
              (* the number of places to shift to the right, which also gives
               * the position of the rightmost digit relative to the decimal
               * point.
               *)
              val rShift = ~exp
              (* position of the leftmost digit relative to the decimal point.
               * Values >= 0 are the number of zeros between the decimal and
               * the most significant fractional digit.  Values < 0 represent
               * the number of digits to the left of the decimal.
               *)
              val lPos = rShift - nDigits
              (* We know that 0 < nDigits, 0 < rShift, and lPos < rShift.  The
               * possible cases are:
               *
               *   a) lPos < 0 < rShift <= prec -- ddd.ddd no rounding
               *   b) lPos < 0 <= prec < rShift -- ddd.ddd round to
               *                                   `nDigits - (rShift - prec)` digits
               *   c) 0 = lPos = prec < rShift  -- d round to 0 digits; d is carry
               *   d) 0 = lPos < prec < rShift  -- x.dddd round to `prec` digits;
               *                                   x is carry
               *   e) 0 = lPos < prec = rShift  -- 0.dddd
               *   f) 0 = lPos < rShift < prec  -- 0.dd00; `prec-rShift` trailing zeros
               *   g) 0 < lPos < prec < rShift  -- 0.00dd round to `prec - lPos` digits
               *   h) 0 < lPos < rShift <= prec -- 0.00dd with possible trailing zeros
               *   i) 0 < prec = lPos < rShift  -- 0.000d round to 0 digits; d is carry
               *   j) 0 = prec < lPos < rShift  -- 0
               *   k) 0 < prec < lPos < rShift  -- 0.0000 `prec` zeros
               *)
              val frags = if (lPos < 0)
                      then if (rShift <= prec)
                        (* (a) `lPos < 0 < rShift <= prec` *)
                        then insertDecimal(nDigits, digits, rShift, prec)
                        (* (b) `lPos < 0 <= prec < rShift` *)
                        else let
                          val nSigDigits = nDigits - (rShift - prec)
                          val (co, ds) = round (digits, nSigDigits)
                          val (nSigDigits, digits) = if co
                                then (nSigDigits + 1, 1::ds)
                                else (nSigDigits, ds)
                          in
                            insertDecimal(nSigDigits, digits, prec, prec)
                          end
                    else if (lPos = 0)
                      then if (prec < rShift)
                        then let
                          val (co, ds) = round (digits, prec)
                          in
                            if (prec = 0)
                              (* (c) `0 = lPos = prec < rShift` *)
                              then if co then ["1"] else ["0"]
                              (* (d) `0 = lPos < prec <= rShift` *)
                              else let
                                val frags = prependDigits(ds, [])
                                in
                                  if co then "1." :: frags else "0." :: frags
                                end
                          end
                      else if (prec = rShift)
                        (* (e) `0 = lPos < prec = rShift` *)
                        then "0." :: prependDigits(digits, [])
                        (* (f) `0 = lPos < rShift < prec` *)
                        else "0." :: prependDigits(digits, [zeros(prec - rShift)])
                    (* 0 < lPos *)
                    else if (lPos < prec)
                      then if (prec < rShift)
                        (* (g) `0 < lPos < prec < rShift` *)
                        then let
                          val nSigDigits = prec - lPos
                          val (co, ds) = round (digits, nSigDigits)
                          val (digits, lPos) = if co
                                then (1 :: ds, lPos-1)
                                else (ds, lPos)
                          in
                            "0." :: prependZeros(lPos, prependDigits(digits, []))
                          end
                        (* (h) `0 < lPos < rShift <= prec` *)
                        else let
                          val frags = if (rShift < prec)
                                then [zeros(prec - rShift)]
                                else []
                          in
                            "0." :: prependZeros(lPos, prependDigits(digits, frags))
                          end
                    else if (lPos = prec)
                      (* (i) `0 < prec = lPos < rShift` *)
                      then let
                        val (co, _) = round (digits, 0)
                        in
                          if co
                            then "0." :: prependZeros(prec-1, ["1"])
                            else ["0.", zeros prec]
                        end
                    (* prec < lPos < rShift *)
                    else if (prec = 0)
                      (* (j) `0 = prec < lPos < rShift` *)
                      then ["0"]
                      (* (k) `0 < prec < lPos < rShift` *)
                      else ["0.", zeros prec]
              in
                String.concat (addSign (sign, frags))
              end

    (* return the number of characters required to represent the exponent *)
    fun expSize 0 = 0
      | expSize e = let
          (* count the "E" and sign *)
          val (n, e) = if (e < 0) then (2, ~e) else (1, e)
          in
            if (n < 10) then n+1
            else if (n < 100) then n+2
            else if (n < 1000) then n+3
            else if (n < 10000) then n+4
            else n+5 (* covers max exponent for 128-bit floats *)
          end

    fun toGen nSigDigits (sign, nDigits, digits, exp) = (
          case (sign, digits)
           of (false, []) => "0"
            | (true, []) => "~0"
            | _ => let
                (* first we round to the maximum number of significant digits; note
                 * that rounding can produce trailing zeros, which are dropped.
                 *)
                val (nDigits, digits, exp) = if (nSigDigits < nDigits)
                      then let (* round to the number of significant digits and normalize *)
                        (* adjust the exponent for the implicit right shift *)
                        val exp = exp + (nDigits - nSigDigits)
                        in
                          case roundAndNormalize (digits, nSigDigits)
                           of (false, ds, nz) => (nSigDigits-nz, ds, exp+nz)
                            | (true, ds, nz) => let
                                val nSigDigits = nSigDigits - nz (* = length ds *)
                                val exp = exp + nz
                                in
                                  if (nSigDigits > 1)
                                    then (nSigDigits, 1 :: List.take(ds, nSigDigits-1), exp)
                                    else (1, [1], exp)
                                end
                          (* end case*)
                        end
                      else (nDigits, digits, exp)
                (* compute the size (w/o sign) when formatting in fixed-point notation *)
                val fixSize = if (exp >= 0)
                        (* <whole> <zeros> *)
                        then nDigits + exp
                      else if (~exp >= nDigits)
                        (* "0" "." <zeros> <frac> *)
                        then 2 - exp (* note that exp < 0 *)
                        (* <whole> "." <frac> *)
                        else nDigits + 1
                (* compute the size (w/o sign) when formatting in scientific notation *)
                val sciSize = if (nDigits = 1)
                      (* "<digit> "E" <exp> *)
                      then 1 + expSize exp
                      (* "<digit> "." <frac> "E" <exp> *)
                      else nDigits + 1 + expSize(nDigits - 1)
                in
                  if (fixSize <= sciSize)
                    then let (* fixed-point notation *)
                      val frags = if (exp > 0)
                              then prependDigits(digits, [zeros exp])
                            else if (exp = 0)
                              then prependDigits(digits, [])
                            else if (~exp > nDigits)
                              then "0." :: zeros(~exp - nDigits)
                                :: prependDigits(digits, [])
                            else if (~exp = nDigits)
                              then "0." :: prependDigits(digits, [])
                              else let
                                val (w, f) = List.splitAt (digits, nDigits + exp)
                                in
                                  prependDigits(w, "." :: prependDigits(f, []))
                                end
                      val frags = if sign then "~" :: frags else frags
                      in
                        String.concat frags
                      end
                    else let (* scientific notation *)
                      val exp = exp + (nDigits - 1) (* adjust exponent *)
                      val frags = ["E", Int.toString exp]
                      val frags = (case digits
                             of [d] => getDigit d :: frags
                              | d::frac => getDigit d :: "." :: prependDigits(frac, frags)
                              | [] => raise Fail "impossible"
                            (* end case *))
                      val frags = if sign then "~" :: frags else frags
                      in
                        String.concat frags
                      end
                end
          (* end case *))

    (* convert to an "exact" decimal representation of the form
     * "[~]?0.[0-9]+(E[0-9]+)?"
     *)
    fun toExact (sign, nDigits, digits, exp) = (
          case (sign, digits)
           of (false, []) => "0.0"
            | (true, []) => "~0.0"
            | _ => let
                (* adjust `exp` so that digits are to the right of the decimal *)
                val exp = exp + nDigits
                (* build up list of fragments from right to left *)
                val frags = if (exp <> 0) then ["E", Int.toString exp] else []
                val frags = List.foldr
                      (fn (d, frags) => getDigit d :: frags)
                        frags digits
                val frags = if sign then "~0." :: frags else "0." :: frags
                in
                  String.concat frags
                end
          (* end case *))

    fun fmt mode = (case mode
           of SCI NONE => wrapper(toSci 6)
            | SCI(SOME p) => if (p < 0) then raise Size else wrapper(toSci p)
            | FIX NONE => wrapper(toFix 6)
            | FIX(SOME p) => if (p < 0) then raise Size else wrapper(toFix p)
            | GEN NONE => wrapper(toGen 12)
            | GEN(SOME p) => if (p < 1) then raise Size else wrapper(toGen p)
            | EXACT => wrapper toExact
          (* end case *))

    val toString = wrapper (toGen 12)

    val toExactString = wrapper toExact

  end
