(* real-to-bits.sig
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

signature REAL_TO_BITS =
  sig

  (* the number of bits in the representation *)
    val width : int

  (* classify a real literal based on its representation as an IEEE float of
   * the given precision.  This function will return ZERO on numbers that are too
   * small to represent and will return INF for numbers that are too large.  In these
   * cases, one can use RealLit.isZero and RealLit.isFinite functions to further
   * classify the number.
   *)
    val classify : RealLit.t -> IEEEReal.float_class

  (* convert a real literal to its IEEE binary representation; we also
   * return the IEEE classification of the value.  The resulting vector is in
   * big-endian layout (i.e., the sign bit will be the MSB of the first byte).
   * This function raises the Overflow exception when the literal is too large
   * to represent.
   *)
    val toBits : RealLit.t -> Word8Vector.vector * IEEEReal.float_class

    val zero : bool -> Word8Vector.vector
    val negInf : Word8Vector.vector
    val posInf : Word8Vector.vector
    val quietNaN : Word8Vector.vector

  end
