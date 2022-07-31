(* fn.sig
 *
 * COPYRIGHT (c) 2015 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This file is based on the SML Basis Library reference implementation.
 *)

(*
 * ============================================================================
 * Copyright (c) 2015 John Reppy (http://cs.uchicago.edu/~jhr)
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 * ============================================================================
 *
 * Reference code for SML Basis Library Proposal 2015-005.
 *)

signature FN =
  sig

    val id       : 'a -> 'a
    val const    : 'a -> 'b -> 'a
    val apply    : ('a -> 'b) * 'a -> 'b
    val o        : ('b -> 'c) * ('a -> 'b) -> ('a -> 'c)
    val curry    : ('a * 'b -> 'c) -> ('a -> 'b -> 'c)
    val uncurry  : ('a -> 'b -> 'c) -> ('a * 'b -> 'c)
    val flip     : ('a * 'b -> 'c) -> ('b * 'a -> 'c)
    val repeat   : int -> ('a -> 'a) -> ('a -> 'a)
    val equal    : ''a -> ''a -> bool
    val notEqual : ''a -> ''a -> bool

  end
