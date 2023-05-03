(* unsigned-const-arith-sig.sml
 *
 * Operations for constant-folding unsigned operations on constant integers.
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

signature UNSIGNED_CONST_ARITH =
  sig

  (* we use arbitrary-precision integers to represent constant values *)
    type t = IntInf.int

  (* bit-widths are represented as integers *)
    type width = int

  (* narrow an unsigned value to the range 0..2^WID^-1; depending on the semantics
   * of the implementation, this function may raise Overflow on values that are
   * outside the range -2^(WID-1)^..2^(WID)^-1.
   *)
    val uNarrow : width * t -> t

  (* converts values in range -2^(WID-1)^..2^(WID-1)^-1 to 0..2^(WID)^-1 *)
    val toUnsigned : width * t -> t

    val uAdd  : width * t * t -> t
    val uSub  : width * t * t -> t
    val uMul  : width * t * t -> t
    val uDiv  : width * t * t -> t
    val uMod  : width * t * t -> t
    val uShL  : width * t * t -> t      (* shift left *)
    val uShR  : width * t * t -> t      (* shift right (zero-extend) *)

  (* 2's complement of argument as unsigned value *)
    val uNeg  : width * t -> t

  (* unsigned comparisons, which correctly handle negative arguments *)
    val uLess   : width * t * t -> bool
    val uLessEq : width * t * t -> bool

  end
