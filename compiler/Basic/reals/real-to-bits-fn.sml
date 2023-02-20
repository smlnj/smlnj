(* real-to-bits-fn.sml
 *
 * This code was derived from the RealConst functor in the SML/NJ sources
 * (base/compiler/MiscUtil/bignums/realconst.sml).
 *
 * COPYRIGHT (c) 2018 John Reppy (http://cs.uchicago.edu/~jhr)
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 * This code is part of the SML Compiler Utilities, which can be found at
 *
 *      https://github.com/JohnReppy/sml-compiler-utils
 *)

(* the parameters for an IEEE floating-point representation.  Consider a binary
 * floating point number
 *
 *      r = (-1)^s * 2^{exp} * b_0 . b_1 b_2 ... b_{p-1}
 *
 * where 's' is the sign bit, 'exp' is the exponent, and 'p' is the precision of the
 * significand (aka mantissa).  The IEEE encoding represents this number as follows:
 *
 *      [s | E | b_1 b_2 ... b_{p-1} ]
 *
 * where
 *  - biased exponent E = e + bias (w bits)
 *  - significand T = b_1 b_2 b_3 ... b_{p-1}
 *  - the bias = 2^{w-1} - 1.
 *
 * The width of the representation is w+p.
 *
 * For normal numbers, 'b_0' is assumed to be 1 and the exponent 'e' range from 1-bias
 * to bias.
 *
 * NaNs and Infs are encoded with an exponent of 2^w - 1 (i.e., all 1s).  Specifically,
 * we have the following encodings:
 *
 *  +inf                [ 0 | 1{w} | 0{p-1} ]
 *  -inf                [ 1 | 1{w} | 0{p-1} ]
 *  Quiet NaN           [ s | 1{w} | 1 b_2 ... b_{p-1} ]
 *  Signaling NaN       [ s | 1{w} | 0 b_2 ... b_{p-1} ]  -- at least on of the b_i must be 1
 *
 * Subnormal numbers are encoded with an exponent of 0 and a non-zero mantissa.
 *
 *      r = (-1)^s * 2^{-bias+1} * 0 . b_1 b_2 ... b_{p-1}
 *)
signature IEEE_FLOAT_PARAMS =
  sig

  (* the total number of bits in the representation; this value should be
   * a multiple of 8.
   *)
    val width : int

  (* the number of bits in the significand; this value is one larger than
   * the width of the mantissa field, since it includes the redundant bit.
   *)
    val significant : int

  (* the exponent bias *)
    val bias : int

  end

functor RealToBitsFn (FP : IEEE_FLOAT_PARAMS) : REAL_TO_BITS =
  struct

    structure W = Word
    structure W8 = Word8
    structure W8A = Word8Array

  (* convert a Word.word to a Word8.word *)
    fun w2b w = W8.fromLargeWord(W.toLargeWord w)
    fun b2w b = W.fromLargeWord(W8.toLargeWord b)

  (* representation width of the literal type *)
    val width = FP.width

  (* number of bits in exponent *)
    val expWidth = FP.width - FP.significant

  (* unbiased min and max exponents *)
    val maxExp = Word.toIntX(Word.<<(0w1, W.fromInt(expWidth-1)) - 0w1)
    val minExp = 1 - maxExp

    val bias = FP.bias (* biased exponents range from 1<=exp<maxExp *)

  (* check for consistency; assume that 16-bit floats are the smallest *)
    val _ = (
          if (width < 16) orelse (width mod 8 <> 0)
            then raise Fail "FloatToBitsFn: invalid width"
            else ();
          if (FP.significant < 11) orelse (width-5 < FP.significant)
            then raise Fail "FloatToBitsFn: invalid significant"
            else ();
          if (FP.bias <> maxExp)
            then raise Fail "FloatToBitsFn: invalid bias"
            else ())

  (* number of bytes needed to represent a float *)
    val numBytes = FP.width div 8

  (* number of bytes in the representation that contain at least one bit
   * of the mantissa.  Note that `significant` is the width of mantissa bits + 1.
   *)
    val numMantBytes = (FP.significant + 6) div 8

  (* number of bytes in the representation that contain at least one bit
   * of the exponent.
   *)
    val numExpBytes = (expWidth + 8) div 8

  (* index of the first mantissa byte *)
    val firstMantByte = numBytes - numMantBytes

  (* shift amount needed to align exponent with its position in the target.  E.g.,
   * the IEEE 32-bit exponent is 11 bits (bits 1--11), so we need to left shift
   * four to get it aligned.
   *)
    val expShift = Word.fromInt(8 * numExpBytes - (expWidth + 1))

  (* bit layout for mantissa; each element has the byte index, the start bit
   * (numbered from the MSB to LSB), and the width of the bits for that byte.
   * For example, the IEEE 32-bit float, which has a 24 bit significand (23
   * bits + 1), has the layout
   *    {byte = 1, start = 1, width = 7},
   *    {byte = 2, start = 8, width = 8},
   *    {byte = 3, start = 16, width = 8}
   *)
    val fracLayout : {byte : int, start : int, width : int} list = let
          val firstByte = numBytes - numMantBytes
          val firstBitWidth = 8 - ((expWidth + 1) mod 8)
          fun layout 0 = {byte = firstByte, start = 1, width = firstBitWidth}
            | layout i = {
                  byte = firstByte + i,
                  start = firstBitWidth + 1 + (i-1) * 8,
                  width = 8
                }
          in
            List.tabulate (numMantBytes, layout)
          end

  (* Use more than the required precision, then round at the end.  We need bits
   * for the mantissa, plus bits for the bias, plus bits for one additional decimal
   * digit.
   *)
    val precision = FP.significant + expWidth + 4

  (* the number of bits needed to represent a positive IntInf.int *)
    fun numBitsForInt n = IntInf.log2 n + 1

  (* A float is a WHOLE "fraction" and an exponent base TWO. *)
    type float = {frac : IntInf.int, exp : int}

  (* round a float to n significant binary digits *)
    fun round (float as {frac, exp}, n) = let
          val shift = numBitsForInt frac + 1 - n
          in
            if shift <= 0
              then float
              else {
                  frac = if (IntInf.andb(frac, IntInf.<<(1, W.fromInt(shift-1))) = 0)
                      then IntInf.~>>(frac, W.fromInt shift)
                      else IntInf.~>>(frac, W.fromInt shift) + 1,
                  exp = exp + shift
                }
          end

  (* float values ten and one tenth, to the correct precision. *)
    val ten : float = {frac = 5, exp = 1}
    val tenth : float = let
          fun mk 1 = {frac = 1, exp = ~4}
            | mk n = let
                val {frac, exp} = mk (n-1)
                val tenthBit = (case n mod 4
                       of 0 => 0
                        | 1 => 1
                        | 2 => 1
                        | _ => 0
                      (* end case *))
                val f = 2 * frac + tenthBit
                val e = exp - 1
                in
                  {frac = f, exp = e}
                end
          in
            round (mk (precision+1), precision)
          end

  (* Multiplies two floats together to the correct precision *)
    fun mult ({frac=f1, exp=e1} : float, {frac=f2, exp=e2}) =
          round ({frac = f1 * f2, exp = e1 + e2}, precision)

    local
    (* a cache of powers of 10 *)
      datatype cache = C of float option array ref
      fun newCache f0 = let
            val arr = Array.array(16, NONE)
            in
              Array.update (arr, 0, SOME f0);
              C(ref arr)
            end
      fun grow (C(cache as ref arr), sz) = let
            val sz = Int.max(sz+1, 2*Array.length arr)
            val newArr = Array.array(sz, NONE)
            in
              Array.appi (fn (i, f) => Array.update(newArr, i, f)) arr;
              cache := newArr
            end
      fun access (cache as C(ref arr), n) =
            if (n < Array.length arr)
              then let
                fun get i = (case Array.sub(arr, i)
                       of NONE => let
                            val prev = get (i-1)
                            val flt = mult(prev, prev)
                            in
                              Array.update(arr, n, SOME flt);
                              flt
                            end
                        | SOME flt => flt
                      (* end case *))
                in
                  get n
                end
              else (grow (cache, n); access (cache, n))
      val pos10 = newCache ten          (* 10^2^n *)
      val neg10 = newCache tenth        (* 10^-2^n *)
(* FIXME: we should check for too large exponents to avoid having the table blow up on bad inputs *)
      fun pow10_2 0 = {frac = 1, exp = 0}
        | pow10_2 n = if (n > 0) then access(pos10, n - 1) else access(neg10, ~n - 1)
    in
    fun raiseToPower (f, 0) = f
      | raiseToPower (f, e) = let
          val (sign, e) = if (e < 0) then (~1, ~e) else (1, e)
          fun power (f, p) = mult (f, pow10_2 (sign * p))
          fun raisep (f, 0w0, _) = f
            | raisep (f, e, p) =
              if W.andb(e, 0w1) = 0w1
                then raisep (power(f, p), W.>>(e, 0w1), p+1)
                else raisep(f, W.>>(e, 0w1), p+1)
          in
            raisep (f, W.fromInt(abs e), 1)
          end
    end (* local *)

  (* Take an IntInf.int representing the fractional part of a float and return a
   * function that will generate the mantissa bits.  The function is called with
   * two integers (start,width), and returns a byte representing the bits of frac
   * from start to start+width-1, where bits are numbered from MSB to LSB.  We
   * assume that `0 < frac`, `0 <= start`, and `0 < width <= 8`.
   *)
    fun makebits 0 = (fn _ => 0w0)
      | makebits frac = let
        (* the number of bits needed to represent frac *)
          val s = numBitsForInt frac
        (* mask for high bit of frac *)
          val highBit = IntInf.<<(1, W.fromInt(s-1))
        (* loop to generate bits: `i` is loop bound (starts at width), `fracBit` is
         * is the bit mask for testing bits in frac, `bit` is the corresponding bit
         * being tested, and `bits` is the accumulated bit values.
         *)
          fun getBits (i, fracBit, bit, bits) =
                if (0 < i) andalso (fracBit > 0)
                  then let
                    val fracBit' = IntInf.~>>(fracBit, 0w1)
                    val bit' = W.>>(bit, 0w1)
                    in
                      if (IntInf.andb(frac, fracBit) = 0)
                        then getBits (i-1, fracBit', bit', bits)
                        else getBits (i-1, fracBit', bit', W.orb(bits, bit))
                    end
                  else w2b bits
          fun mk (start, width) = let
                val fracBit = IntInf.~>>(highBit, W.fromInt start)
                val bit = W.<<(0w1, W.fromInt(width-1))
                in
                  getBits (width, fracBit, bit, 0w0)
                end
          in
            mk
          end

  (* allocate a byte array and set the sign and exponent fields *)
    fun mkSignAndExp (isNeg, exp) = let
        (* allocate and initialize space for the result *)
          val bytes = W8A.array(numBytes, 0w0)
        (* set the modify the i'th byte by or'ing in b *)
          fun orb (i, b) = W8A.update(bytes, i, W8.orb(W8A.sub(bytes, i), b))
        (* start by setting the sign bit *)
          val _ = if isNeg then W8A.update(bytes, 0, 0wx80) else ();
        (* process the exponent *)
          val alignedExp = Word.<<(exp, expShift)
          fun doExpBytes i = if (i < numExpBytes)
                then (
                  orb (i, w2b (Word.>>(alignedExp, 0w8*Word.fromInt(numExpBytes-i-1))));
                  doExpBytes (i+1))
                else ()
          in
            doExpBytes 0;
            bytes
          end

  (* build the byte-vector representation, where isNeg denotes the sign, exp is the word
   * representation of the biased exponent, and frac is the IntInf.int representation of
   * the mantissa.
   *)
    fun pack (isNeg, exp, frac : IntInf.int) = let
        (* allocate and initialize space for the result *)
          val bytes = mkSignAndExp (isNeg, exp)
        (* set the modify the i'th byte by or'ing in b *)
          fun orb (i, b) = W8A.update(bytes, i, W8.orb(W8A.sub(bytes, i), b))
        (* process the mantissa *)
          val makebits = makebits frac
          val _ = List.app
                (fn {byte, start, width} => orb (byte, makebits (start, width)))
                  fracLayout
          in
          (* return the immutable vector representation *)
            W8A.toVector bytes
          end

  (* build the byte-vector representation for a non-normal representation (i.e., sub-normal
   * numbers, infinities, or NaNs), where isNeg denotes the sign, exp is the special
   * exponent value, and frac is the IntInf.int representation of the mantissa.
   *)
    fun packSpecial (isNeg, exp, frac : IntInf.int) = let
        (* allocate and initialize space for the result *)
          val bytes = mkSignAndExp (isNeg, exp)
        (* set the modify the i'th byte by or'ing in b *)
          fun orb (i, b) = W8A.update(bytes, i, W8.orb(W8A.sub(bytes, i), b))
        (* fill in the mantissa bits *)
          fun lp (mant, ix) = if (mant > 0)
                then (
                  orb (ix, W8.fromLargeInt mant); (* grab low 8 bits of mantissa *)
                  lp (IntInf.~>>(mant, 0w8), ix - 1))
                else ()
          in
            lp (frac, numBytes-1);
          (* return the immutable vector representation *)
            W8A.toVector bytes
          end

    val specialExp = Word.<<(0w1, Word.fromInt expWidth) - 0w1

    fun zero isNeg = pack(isNeg, 0w0, 0)
    val posInf = packSpecial (false, specialExp, 0)
    val negInf = packSpecial (true, specialExp, 0)
    val quietNaN = packSpecial (false, specialExp, 1)  (* quiet NaN with 0 payload *)

    fun classify lit = (case RealLit.toRep lit
           of RealLit.PosInf => IEEEReal.INF
            | RealLit.NegInf => IEEEReal.INF
            | RealLit.QNaN => IEEEReal.NAN
            | RealLit.Flt{isNeg, digits=[], exp} => IEEEReal.ZERO
            | RealLit.Flt{isNeg, digits, exp} => let
              (* convert the digits to a IntInf.int and adjust the exponent *)
                val (frac_10, exp_10) = let
                      fun doDigit (d, (m, e)) = (IntInf.fromInt d + 10*m, e-1)
                      val (frac, exp) = List.foldl doDigit (0, exp) digits
                      in
                        (frac, IntInf.toInt exp)
                      end
              (* convert to base 2 *)
                val flt = raiseToPower (round({frac=frac_10, exp=0}, precision), exp_10)
                val {frac, exp} = round(flt, FP.significant+1)
              (* adjust exp for size of fraction *)
                val exp = exp + numBitsForInt frac - 1
                in
                  if (exp < minExp)
                    then let
                      val diff = Word.fromInt(minExp - exp)
                      val frac = IntInf.~>>(frac, diff)
                      in
                        if frac > 0
                          then IEEEReal.SUBNORMAL
                          else IEEEReal.ZERO
                      end
                  else if (maxExp < exp)
                    then IEEEReal.INF
                    else IEEEReal.NORMAL
                end
          (* end case *))

    fun toBits lit = (case RealLit.toRep lit
           of RealLit.PosInf => (posInf, IEEEReal.INF)
            | RealLit.NegInf => (negInf, IEEEReal.INF)
            | RealLit.QNaN => (quietNaN, IEEEReal.NAN)
            | RealLit.Flt{isNeg, digits=[], exp} => (zero isNeg, IEEEReal.ZERO)
            | RealLit.Flt{isNeg, digits, exp} => let
              (* convert the digits to a IntInf.int and adjust the exponent *)
                val (frac_10, exp_10) = let
                      fun doDigit (d, (m, e)) = (IntInf.fromInt d + 10*m, e-1)
                      val (frac, exp) = List.foldl doDigit (0, exp) digits
                      in
                        (frac, IntInf.toInt exp)
                      end
              (* convert to base 2 *)
                val flt = raiseToPower (round({frac=frac_10, exp=0}, precision), exp_10)
                val {frac, exp} = round(flt, FP.significant+1)
              (* adjust exp for size of fraction *)
                val exp = exp + numBitsForInt frac - 1
                in
                  if (exp < minExp)
                    then let
                      val diff = Word.fromInt(minExp - exp)
                      val frac = IntInf.~>>(frac, diff)
                      in
                        if frac > 0
                          then (packSpecial (isNeg, 0w0, frac), IEEEReal.SUBNORMAL)
                          else (zero isNeg, IEEEReal.ZERO)
                      end
                  else if (maxExp < exp)
                    then raise Overflow
                    else (pack (isNeg, W.fromInt(exp + maxExp), frac), IEEEReal.NORMAL)
                end
          (* end case *))

  end;

(*
structure IEEEFloat16Params : IEEE_FLOAT_PARAMS =
  struct
    val width = 16
    val significant = 11
    val bias = 15
  end;
*)

(*
structure IEEEFloat32Params : IEEE_FLOAT_PARAMS =
  struct
    val width = 32
    val significant = 24
    val bias = 127
  end;
*)

structure IEEEFloat64Params : IEEE_FLOAT_PARAMS =
  struct
    val width = 64
    val significant = 53
    val bias = 1023
  end;

(*
structure IEEEFloat128Params : IEEE_FLOAT_PARAMS =
  struct
    val width = 128
    val significant = 113
    val bias = 16383
  end;
*)

(*
structure IEEEFloat256Params : IEEE_FLOAT_PARAMS =
  struct
    val width = 256
    val significant = 237
    val bias = 262143
  end;
*)
