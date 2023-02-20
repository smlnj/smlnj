(* signed-const-arith-sig.sml
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

signature SIGNED_CONST_ARITH =
  sig

  (* we use arbitrary-precision integers to represent constant values *)
    type t = IntInf.int

  (* bit-widths are represented as integers *)
    type width = int

  (* narrow a signed-constant to fit within the range -2^(WID-1)^..2^(WID-1)^-1.
   * Depending on the semantics of the implementation, this operation may raise
   * Overflow on values that are outside the range -2^(WID-1)^..2^(WID-1)^.
   *)
    val sNarrow : width * t -> t

  (* converts values in range 0..pow2(width)-1 to -pow2(width-1)..pow2(width-1)-1 *)
    val toSigned : width * t -> t

    val sAdd  : width * t * t -> t
    val sSub  : width * t * t -> t
    val sMul  : width * t * t -> t
    val sDiv  : width * t * t -> t      (* division (round toward -∞) *)
    val sMod  : width * t * t -> t      (* sMod(n, m) = n - m*sDiv(n, m) *)
    val sQuot : width * t * t -> t      (* division (round toward 0) *)
    val sRem  : width * t * t -> t      (* sRem(n, m) = n - m*sQuot(n, m) *)
    val sShL  : width * t * t -> t      (* shift left *)
    val sShR  : width * t * t -> t      (* shift right (sign-extend) *)
    val sNeg  : width * t -> t          (* unary negation *)
    val sAbs  : width * t -> t          (* absolute value *)

  end
