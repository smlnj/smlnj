(* frep-to-real64.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Conversion of the `FloatRep.float_rep` representation to
 * 64-bit IEEE floating-point numbers.
 *
 * This code is a port of the `s2d_n` function in the Ryũ library by Ulf Adams.
 * The original C source code for that library is available at
 *
 *      https://github.com/ulfjack/ryu
 *)

structure FRepToReal64 : sig

    val cvt : FloatRep.float_rep -> real

  end = struct

    structure W = InlineT.Word
    structure W64 = InlineT.Word64
    structure Int = IntImp

    datatype float_rep = datatype FloatRep.float_rep

(* the following should be in the Unsafe structure *)
    (* bitcast a Word64.word to a Real64.real *)
    fun fromBits (b : Word64.word) : real = let
          val r : real ref = InlineT.cast(ref b)
          in
            !r
          end

(* the following should be part of the WORD signature *)
    (* count the leading zeros in a Word64.word value *)
    fun nlz (w : Word64.word) : word = let
          fun step (x, n, k) = let
                val y = W64.rshiftl(x, k)
                in
                  if (y <> 0w0) then (n - k, y) else (n, x)
                end
          val (n, x) = step (w, 0w64, 0w32)
          val (n, x) = step (x, n, 0w16)
          val (n, x) = step (x, n, 0w8)
          val (n, x) = step (x, n, 0w4)
          val (n, x) = step (x, n, 0w2)
          in
            if (W64.rshiftl(x, 0w1) <> 0w0)
              then n - 0w2
              else n - W.fromLarge x
          end

    val kMantissaBits = 52
    val kExpBits = 11
    val kExpBias = 1023

    val kPow5InvBitCount = 125
    val kPow5BitCount = 125

    val pow5InvSplit2Tbl : (Word64.word * Word64.word) vector = #[
            (                    0w1, 0w2305843009213693952 ),
            (  0w5955668970331000884, 0w1784059615882449851 ),
            (  0w8982663654677661702, 0w1380349269358112757 ),
            (  0w7286864317269821294, 0w2135987035920910082 ),
            (  0w7005857020398200553, 0w1652639921975621497 ),
            ( 0w17965325103354776697, 0w1278668206209430417 ),
            (  0w8928596168509315048, 0w1978643211784836272 ),
            ( 0w10075671573058298858, 0w1530901034580419511 ),
            (   0w597001226353042382, 0w1184477304306571148 ),
            (  0w1527430471115325346, 0w1832889850782397517 ),
            ( 0w12533209867169019542, 0w1418129833677084982 ),
            (  0w5577825024675947042, 0w2194449627517475473 ),
            ( 0w11006974540203867551, 0w1697873161311732311 ),
            ( 0w10313493231639821582, 0w1313665730009899186 ),
            ( 0w12701016819766672773, 0w2032799256770390445 )
          ]

    val pow5InvOffsetTbl : Word64.word vector = #[
            0wx54544554, 0wx04055545, 0wx10041000, 0wx00400414, 0wx40010000,
            0wx41155555, 0wx00000454, 0wx00010044, 0wx40000000, 0wx44000041,
            0wx50454450, 0wx55550054, 0wx51655554, 0wx40004000, 0wx01000001,
            0wx00010500, 0wx51515411, 0wx05555554, 0wx50411500, 0wx40040000,
            0wx05040110, 0wx00000000
          ]

    val pow5Split2Tbl : (Word64.word * Word64.word) vector = #[
            (                    0w0, 0w1152921504606846976 ),
            (                    0w0, 0w1490116119384765625 ),
            (  0w1032610780636961552, 0w1925929944387235853 ),
            (  0w7910200175544436838, 0w1244603055572228341 ),
            ( 0w16941905809032713930, 0w1608611746708759036 ),
            ( 0w13024893955298202172, 0w2079081953128979843 ),
            (  0w6607496772837067824, 0w1343575221513417750 ),
            ( 0w17332926989895652603, 0w1736530273035216783 ),
            ( 0w13037379183483547984, 0w2244412773384604712 ),
            (  0w1605989338741628675, 0w1450417759929778918 ),
            (  0w9630225068416591280, 0w1874621017369538693 ),
            (   0w665883850346957067, 0w1211445438634777304 ),
            ( 0w14931890668723713708, 0w1565756531257009982 )
          ]

    val pow5OffsetTbl : Word64.word vector = #[
            0wx00000000, 0wx00000000, 0wx00000000, 0wx00000000, 0wx40000000,
            0wx59695995, 0wx55545555, 0wx56555515, 0wx41150504, 0wx40555410,
            0wx44555145, 0wx44504540, 0wx45555550, 0wx40004000, 0wx96440440,
            0wx55565565, 0wx54454045, 0wx40154151, 0wx55559155, 0wx51405555,
            0wx00000105
          ]

    (* powers of 5 from 5^0 to 5^25 *)
    val pow5TblSz = 26
    val pow5Tbl = let
          fun gen (0, _) = []
            | gen (i, n) = n :: gen(i-1, 0w5 * n)
          in
            Vector.fromList(gen(pow5TblSz, 0w1))
          end

    (* assume w <> 0w0 *)
    fun floorLog2 (w : Word64.word) = W.toIntX(0w63 - nlz w)

    (* returns log_2(5^e) for 0 < e <= 3528 and 1 for e = 0. *)
    fun log2Pow5 e = W.toIntX(W.rshiftl(W.fromInt e * 0w1217359, 0w19))

    fun pow5Bits e = W.toIntX(W.rshiftl(W.fromInt e * 0w1217359, 0w19) + 0w1)

    (* returns ceil(log_2(5^e)) for 0 < e <= 3528 and 1 for e = 0. *)
    fun ceilLog2Pow5 e = log2Pow5 e + 1

    fun pow5Factor (value : Word64.word) = let
          (* 5 * m_inv_5 = 1 (mod 2^64) *)
          val m_inv_5 : Word64.word = 0w14757395258967641293
          (* #{ n | n = 0 (mod 2^64) } = 2^64 / 5 *)
          val n_div_5 : Word64.word = 0w3689348814741910323
          fun lp (value, count) = let
                val value = value * m_inv_5
                in
                  if (value > n_div_5)
                    then count
                    else lp (value, count+0w1)
                end
          in
            lp (value, 0w0)
          end

    (* returns true if value is divisible by 5^p *)
    fun multipleOfPowerOf5 (value : Word64.word, p : word) = (pow5Factor value >= p)

    (* returns true if value is divisible by 2^p *)
    fun multipleOfPowerOf2 (value, p) = (W64.andb(value, W64.lshift(0w1, p)-0w1) = 0w0)

    (* 64x64 --> 128 bit unsigned multiplication *)
    fun umul128 (a, b) = let
          fun lo32 x = W64.andb(x, 0wxffffffff)
          fun hi32 x = W64.rshiftl(x, 0w32)
          val aLo = lo32 a
          val aHi = hi32 a
          val bLo = lo32 b
          val bHi = hi32 b
          val b00 = aLo * bLo
          val b01 = aLo * bHi
          val b10 = aHi * bLo
          val b11 = aHi * bHi
          val b00Lo = lo32 b00
          val b00Hi = hi32 b00
          val mid1 = b10 + b00Hi
          val mid1Lo = lo32 mid1
          val mid1Hi = hi32 mid1
          val mid2 = b01 + mid1Lo
          val mid2Lo = lo32 mid2
          val mid2Hi = hi32 mid2
          val pHi = b11 + mid1Hi + mid2Hi
          val pLo = W64.orb(W64.lshift(mid2Lo, 0w32),  b00Lo)
          in
            (pHi, pLo)
          end

    (* returns the low 64 bits from shifting the 128-bit number (hi, lo) by dist
     * bits.  We assume that 0 < dist < 64.
     *)
    fun shiftRight128 (lo, hi, dist) =
          W64.orb (W64.lshift(hi, 0w64 - dist), W64.rshiftl(lo, dist))

    fun mulShift64 (m : Word64.word, mul : (Word64.word * Word64.word), j : word) = let
          val (hi1, _) = umul128 (m, #1 mul)
          val (hi2, lo2) = umul128 (m, #2 mul)
          val sum = hi1 + lo2
          val hi2 = if (sum < hi1) then hi2+0w1 (* overflow into hi2 *) else hi2
          in
            shiftRight128 (sum, hi2, j - 0w64)
          end

    (* computes 5^i as a 128-bit result *)
    fun computePow5 i = let
          val base = i div pow5TblSz
          val base2 = base * pow5TblSz
          val offset = i - base2
          val mul = Vector.sub(pow5Split2Tbl, base)
          in
            if (offset = 0)
              then mul
              else let
                val m = W.toLarge(Vector.sub(pow5Tbl, offset))
                val (hi1, lo1) = umul128 (m, #1 mul)
                val (hi2, lo2) = umul128 (m, #2 mul)
                val sum = hi1 + lo2
                val hi2 = if (sum < hi1) then hi2+0w1 else hi2
                val delta = W.fromInt(pow5Bits i - pow5Bits base2)
                val i' = W.fromInt i
                in (
                  shiftRight128(lo1, sum, delta) +
                    W64.andb(
                      W64.rshiftl(
                        Vector.sub(pow5OffsetTbl, W.toIntX(i' div 0w16)),
                        W.lshift(i' mod 0w16, 0w1)),
                      0w3),
                  shiftRight128(sum, hi2, delta)
                ) end
          end

    (* computes 5^{-i} as a 128-bit result *)
    fun computeInvPow5 i = let
          val base = (i + pow5TblSz - 1) div pow5TblSz
          val base2 = base * pow5TblSz
          val offset = base2 - i
          val mul = Vector.sub(pow5InvSplit2Tbl, base) (* 1 / 5^{base2} *)
          in
            if offset = 0
              then mul
              else let
                val m = W.toLarge(Vector.sub(pow5Tbl, offset))
                val (hi1, lo1) = umul128(m, #1 mul - 0w1)
                val (hi2, lo2) = umul128(m, #2 mul)
                val sum = hi1 + lo2
                val hi2 = if sum < hi1 then hi2+0w1 (* overflow into hi2 *) else hi2
                val delta = W.fromInt(pow5Bits base2 - pow5Bits i)
                val i' = W.fromInt i
                in (
                  shiftRight128(lo1, sum, delta) + 0w1 +
                    W64.andb(
                      W64.rshiftl(
                        Vector.sub(pow5InvOffsetTbl, W.toIntX(i' div 0w16)),
                        W.lshift(i' mod 0w16, 0w1)),
                      0w3),
                  shiftRight128(sum, hi2, delta)
                ) end
          end

    fun fromDecimalRep (f : FloatRep.decimal_rep) = let
          fun sumDigits ([], accum) = accum
            | sumDigits (d::ds, accum) = sumDigits (ds, W64.fromInt d + 0w10*accum)
          val m10 = sumDigits (#digits f, 0w0)
          val e10 = #exp f
          (* Convert to binary float m2 * 2^e2, while retaining information
           * about whether the conversion was exact (trailingZeros).
           *)
(*+DEBUG**
          val _ = print(concat[
                  "m10 * 10^e10 = ", W64.fmt StringCvt.DEC m10,
                  " * 10^", Int.toString e10, "\n"
                ])
**-DEBUG*)
          val (m2, e2, trailingZeros) = if (e10 >= 0)
                then let
                  (* The length of m * 10^e in bits is:
                   *   log2(m10 * 10^e10) =
                   *       log2(m10) + e10 log2(10) = log2(m10) + e10 + e10 * log2(5)
                   *
                   * We want to compute the kMantissaBits + 1 top-most bits (+1 for
                   * the implicit leading one in IEEE format). We therefore choose
                   * a binary output exponent of
                   *   log2(m10 * 10^e10) - (kMantissaBits + 1).
                   *
                   * We use floor(log2(5^e10)) so that we get at least this many
                   * bits; better to have an additional bit than not have enough.
                   *)
                  val e2 = floorLog2 m10 + e10 + log2Pow5 e10 - (kMantissaBits + 1)
                  (* We now compute [m10 * 10^e10 / 2^e2] = [m10 * 5^e10 / 2^(e2-e10)].
                   * To that end, we use the DOUBLE_POW5_SPLIT table.
                   *)
                  val j = e2 - e10 - ceilLog2Pow5 e10 + kPow5BitCount
                  val m2 = mulShift64 (m10, computePow5 e10, W.fromInt j)
                  (* We also compute if the result is exact; i.e.,
                   *   [m10 * 10^e10 / 2^e2] == m10 * 10^e10 / 2^e2.
                   * This can only be the case if 2^e2 divides m10 * 10^e10, which
                   * in turn requires that the largest power of 2 that divides
                   * m10 + e10 is greater than e2. If e2 is less than e10, then
                   * the result must be exact. Otherwise we use the existing
                   * multipleOfPowerOf2 function.
                   *)
                  val trailingZeros = (e2 < e10)
                        orelse ((e2 - e10 < 64)
                          andalso multipleOfPowerOf2(m10, W.fromInt(e2 - e10)))
                  in
                    (m2, e2, trailingZeros)
                  end
                else let (* e10 < 0 *)
                  val e2 = floorLog2 m10 + e10 - ceilLog2Pow5(~e10) - (kMantissaBits + 1)
                  val j = e2 - e10 + ceilLog2Pow5(~e10) - 1 + kPow5InvBitCount
(*+DEBUG**
                  val pow5 = computeInvPow5 (~e10)
                  val _ = (
                        print(concat["j = ", Int.toString j, "\n"]);
                        print(concat[
                            "pow5 = (", W64.fmt StringCvt.DEC (#1 pow5),
                            ", ", W64.fmt StringCvt.DEC (#2 pow5), ")\n"
                          ]))
**-DEBUG*)
                  val m2 = mulShift64(m10, computeInvPow5(~e10), W.fromInt j)
                  val trailingZeros = multipleOfPowerOf5(m10, W.fromInt(~e10))
                  in
                    (m2, e2, trailingZeros)
                  end
(*+DEBUG**
          val _ = print(concat[
                  "m2 * 2^e2 = ", W64.fmt StringCvt.DEC m2,
                  " * 2^", Int.toString e2, "\n"
                ])
**-DEBUG*)
          (* compute the final IEEE exponent *)
          val ieee_e2 = Int.max(0, e2 + kExpBias + floorLog2 m2)
          (* We need to figure out how much we need to shift m2. The tricky part is
           * that we need to take the final IEEE exponent into account, so we need
           * to reverse the bias and also special-case the value 0.
           *)
          val shift = if (ieee_e2 = 0) then 1 else ieee_e2
          val shift = shift - e2 - kExpBias - kMantissaBits
          (* assert(shift >= 0) *)
(*+DEBUG**
          val _ = print(concat["shift = ", Int.toString shift, "\n"])
**-DEBUG*)
          val shift = W.fromInt shift
(*+DEBUG**
          val _ = (
                print(concat["ieee_e2 = ", Int.toString ieee_e2, "\n"]);
                print(concat["shift = ", W.fmt StringCvt.DEC shift, "\n"]))
**-DEBUG*)
          (* We need to round up if the exact value is more than 0.5 above the
           * value we computed. That's equivalent to checking if the last removed
           * bit was 1 and either the value was not just trailing zeros or the result
           * would otherwise be odd.
           *
           * We need to update trailingZeros given that we have the exact output
           * exponent ieee_e2 now.
           *)
          val trailingZeros = trailingZeros
                andalso (W64.andb(m2, W64.lshift(0w1, shift - 0w1) - 0w1) = 0w0)
          val lastRemovedBit = W64.andb(W64.rshiftl(m2, shift-0w1), 0w1)
          val roundUp = (lastRemovedBit <> 0w0)
                andalso (not trailingZeros
                  orelse (W64.andb(W64.rshiftl(m2, shift), 0w1) <> 0w0));
          val ieee_m2 = W64.rshiftl(m2, shift) + (if roundUp then 0w1 else 0w0);
(*+DEBUG**
          val _ = (
                print(concat["roundUp = ", Bool.toString roundUp, "\n"]);
                print(concat["ieee_e2 = ", Int.toString ieee_e2, "\n"]))
**-DEBUG*)
          (* assert(ieee_m2 <= (1 << (kMantissaBits + 1))) *)
          val ieee_m2 = W64.andb(ieee_m2, W64.lshift(0w1, W.fromInt kMantissaBits) - 0w1)
          val ieee_e2 = if (ieee_m2 = 0w0) andalso roundUp
                (* Due to how the IEEE represents +/-Infinity, we don't need to
                 * check for overflow here.
                 *)
                then ieee_e2+1
                else ieee_e2
          (* build the bit representation *)
          val bits = if #sign f then W64.lshift(0w1, W.fromInt kExpBits) else 0w0
          val bits = W64.orb(bits, W64.fromInt ieee_e2)
          val bits = W64.orb(W64.lshift(bits, W.fromInt kMantissaBits), ieee_m2)
          in
            fromBits bits
          end

    val posInf = fromBits 0wx7FF0000000000000
    val negInf = fromBits 0wx7FF0000000000000
    val posNaN = fromBits 0wx7FF8000000000000
    val negNaN = fromBits 0wxFFF8000000000000
    val posZero = fromBits 0wx0000000000000000
    val negZero = fromBits 0wx8000000000000000

    fun cvt (Inf false) = posInf
      | cvt (Inf true) = negInf
      | cvt (NaN false) = posNaN
      | cvt (NaN true) = negNaN
      | cvt (Zero false) = posZero
      | cvt (Zero true) = negZero
      | cvt (Normal f) = fromDecimalRep f
      | cvt (Subnormal f) = fromDecimalRep f

  end
