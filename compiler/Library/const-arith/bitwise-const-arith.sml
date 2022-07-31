(* bitwise-const-arith.sml
 *
 * Operations for constant-folding bitwise operations on constant integers.
 *
 * COPYRIGHT (c) 2017 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
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

structure BitwiseConstArith : BITWISE_CONST_ARITH =
  struct

  (* we use arbitrary-precision integers to represent constant values *)
    type t = IntInf.int

  (* bit-widths are represented as integers *)
    type width = int

    fun pow2 w = IntInf.<<(1, Word.fromInt w)

    fun narrow (wid, n) = IntInf.andb(n, pow2 wid - 1)

    fun bAnd (wid, a, b) = narrow (wid, IntInf.andb(a, b))
    fun bOr (wid, a, b) = narrow (wid, IntInf.orb(a, b))
    fun bXor (wid, a, b) = narrow (wid, IntInf.xorb(a, b))
    fun bNot (wid, a) = narrow (wid, IntInf.xorb(a, pow2 wid - 1))

  end
