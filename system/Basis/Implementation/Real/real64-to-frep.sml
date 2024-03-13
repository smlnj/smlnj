(* real64-to-frep.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Conversion of 64-bit IEEE floating-point numbers to the `FloatRep.float_rep`
 * representation.  This code is an implementation of the algorithm from Section 2
 * of the paper:
 *
 *      Ryũ: Fast Float-to-String Conversion
 *      by Ulf Adams
 *      PLDI 2018
 *
 * This algorithm uses arbitrary precision arithmetic; the paper also describes the
 * faster "ryũ" algorithm that uses 64-bit fixed-point arithmetic, which we might
 * want to implement at some time in the future.
 *
 * The original port of the C code was done by Skye Soss.
 *)

structure Real64ToFRep : sig

    val cvt : real -> FloatRep.float_rep

  end = struct

    structure W = InlineT.Word
    structure W64 = InlineT.Word64
    structure Int = IntImp
    structure IntInf = IntInfImp

    datatype float_class = datatype IEEEReal.float_class

    val toBits = InlineT.Real64.toBits

    (* some useful constants *)
    val nExpBits = 0w11                 (* # of exponent bits in double-precision real *)
    val nFracBits = 0w52                (* # of stored fractional (aka mantissa) bits *)
    val bias = 1023                     (* exponent bias for normalized numbers *)

    (* to unify normal and subnormal numbers, we change the representation to
     *
     *    (-1)^s * frac * 2^exp
     *
     * where "frac" is an integer and "exp" is the adjusted exponent.  The following
     * constants are used to compute the adjusted exponent:
     *)
    val normalExpAdjust = bias + W.toInt nFracBits
    val subnormalExp = 1 - bias - W.toInt nFracBits
    (* value of implicit "1" bit for normalized numbers *)
    val implicitBit = W64.lshift(0w1, 0w52)

    (* Given 0≤a≤b≤c, returns (d, e) such that:
     * - a ≤ d ≤ c
     * - a < d*10^e < c
     * - e is maximal
     * - The resulting value for d is closest to b as possible
     * - The result respects the acceptBounds flag
     *)
    fun computeShortest (a, b, c, acceptBounds) = let
          fun iter1 (i, a, b, c, digit, allAZero, allBZero) = let
                val (aq10, ar10) = IntInf.quotRem (a, 10)
                val cq10 = IntInf.quot (c, 10)
                in
                  if (aq10 < cq10)
                    then let
                      val allAZero = allAZero andalso (ar10 = 0)
                      val allBZero = allBZero andalso (digit = 0)
                      val (bq10, br10) = IntInf.quotRem (b, 10)
                      in
                        iter1 (i+1, aq10, bq10, cq10, br10, allAZero, allBZero)
                      end
                    else (i, a, b, c, digit, allAZero, allBZero)
                end
          val (i, a, b, c, digit, allAZero, allBZero) =
                iter1 (0, a, b, if acceptBounds then c - 1 else c, 0, true, true)
          fun iter2 (i, a, b, c, digit, allBZero) = let
                val (aq10, ar10) = IntInf.quotRem (a, 10)
                in
                  if (ar10 = 0)
                    then let
                      val cq10 = IntInf.quot(c, 10)
                      val allBZero = allBZero andalso (digit = 0)
                      val (bq10, br10) = IntInf.quotRem (b, 10)
                      in
                        iter2 (i+1, aq10, bq10, cq10, br10, allBZero)
                      end
                    else (i, a, b, c, digit, allBZero)
                end
          val (i, a, b, c, digit, allBZero) = if (acceptBounds andalso allAZero)
                then iter2 (i, a, b, c, digit, allBZero)
                else (i, a, b, c, digit, allBZero)
          val isTie = (digit = 5) andalso allBZero
          val breakTieDown = (IntInf.andb(b, 1) = 0)
          val wantRoundDown = (digit < 5) orelse (isTie andalso breakTieDown)
          val roundDown = (wantRoundDown andalso (allAZero orelse a <> b))
                orelse (b+1 > c)
          in
            if roundDown then (b, i) else (b+1, i)
          end

    (* convert an IntInf.int to a number and list of digits *)
    fun toDigits n = let
          fun lp (0, nd, ds) = (nd, ds)
            | lp (n, nd, ds) = let
                val (n, d) = IntInf.quotRem (n, 10)
                in
                  lp (n, nd+1, (Int.fromLarge d)::ds)
                end
          in
            lp (n, 0, [])
          end

    val signBitMask = W64.lshift(0w1, 0w63)
    val fracMask = W64.lshift(0w1, nFracBits) - 0w1
    val expMask = W64.lshift(0w1, nExpBits) - 0w1

    fun cvt r = let
          (* decompose the 64-bit float into sign, exponent, and fractional parts *)
          val bits = toBits r
          val sign = W64.andb(bits, signBitMask) <> 0w0
          val expBits = W64.andb(W64.rshiftl(bits, nFracBits), expMask)
          val fracBits = W64.andb(bits, fracMask)
          (* given the float f = (-1)^s * frac * 2^exp, convert to decimal
           * representation.
           *)
          fun toDecimal (sign, frac, exp) = let
                (* Determine the interval of information-preserving outputs *)
                val e2 = exp - 2
                val v = W64.lshift(frac, 0w2)
                val u = if (fracBits = 0w0 andalso expBits > 0w1)
                      then v - 0w1
                      else v - 0w2
                val w = v + 0w2
                (* convert (u, v, w)×2^e₂ to a decimal power base:
                 * (a, b, c)×10^e₁₀ == (u, v, w)×2^e₂
                 *)
                val (e10, a, b, c) = if e2 >= 0
                      then let
                        val x = W.fromInt e2
                        fun shift y = IntInf.<<(W64.toLargeInt y, x)
                        in
                          (0, shift u, shift v, shift w)
                        end
                      else let
                        val x = IntInf.pow (5, ~e2)
                        fun scale y = W64.toLargeInt y * x
                        in
                          (e2, scale u, scale v, scale w)
                        end
                (* find the shortest, correctly rounded decimal representation *)
                val acceptBounds = (W64.andb (fracBits, 0w1) = 0w0)
                val (ds, e) = computeShortest (a, b, c, acceptBounds)
                val (nDigits, digits) = toDigits ds
                in {
                  sign = sign,
                  nDigits = nDigits,
                  digits = digits,
                  exp = e + e10
                } end (* toDecimal *)
          in
            case expBits
             of 0w0 => if (fracBits = 0w0)
                  then FloatRep.Zero sign
                  else (* subnormal *)
                    FloatRep.Subnormal(toDecimal (sign, fracBits, subnormalExp))
              | 0wx7ff => if (fracBits = 0w0)
                  then FloatRep.Inf sign
                  else FloatRep.NaN sign
              | _ => let (* normal *)
                  val exp = W.toInt(W.fromLarge expBits) - normalExpAdjust
                  in
                    FloatRep.Normal(toDecimal (sign, fracBits + implicitBit, exp))
                  end
            (* end case *)
          end

  end
