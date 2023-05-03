(* const-arith-sig.sml
 *
 * A generic interface for constant-folding fixed-precision integer arithmetic.
 * Implementations with different semantics for overflow are provided.
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

signature CONST_ARITH =
  sig

  (* we use arbitrary-precision integers to represent constant values *)
    type t = IntInf.int

  (* bit-widths are represented as integers *)
    type width = int

  (* narrow a signed-constant to fit within the given number of bits.  Depending on the
   * semantics of the structure implementing this signature, this operation may raise
   * Overflow.
   *)
    val sNarrow : width * t -> t

  (* signed arithmetic *)
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

  (* narrow an unsigned-constant to fit within the given number of bits.  Depending on
   * the semantics of the structure implementing this signature, this operation may
   * raise Overflow.
   *)
    val uNarrow : int * IntInf.int -> IntInf.int

  (* unsigned arithmetic.  We assume that the arguments are non-negative *)
    val uAdd  : width * t * t -> t
    val uSub  : width * t * t -> t
    val uMul  : width * t * t -> t
    val uDiv  : width * t * t -> t      (* division (round toward 0) *)
    val uMod  : width * t * t -> t      (* uMod(n, m) = n - m*uDiv(n, m) *)
    val uShL  : width * t * t -> t      (* shift left *)
    val uShR  : width * t * t -> t      (* shift right (zero-extend) *)

  (* 2's complement of argument as unsigned value *)
    val uNeg  : width * t -> t

  (* unsigned comparisons, which correctly handle negative arguments *)
    val uLess   : width * t * t -> bool
    val uLessEq : width * t * t -> bool

  (* bitwise operations (these never trap) *)
    val bAnd : width * t * t -> t
    val bOr  : width * t * t -> t
    val bXor : width * t * t -> t
    val bNot : width * t -> t

  (* `toSigned (w, n)` converts an unsigned value `n` of width `w` to
   * a signed value of width `w`.  We assume that `0 <= n < 2^w-1`.
   *)
    val toSigned : width * t -> t       (* unsigned -> signed *)

  (* `toUnsigned (w, n)` converts a signed value `n` in the range
   * `~2^(w-1) <= n < 2^(w-1)` to an unsigned value in the range
   * `0 <= n < 2^w-1`.
   *)
    val toUnsigned : width * t -> t     (* signed -> unsigned *)

  end

