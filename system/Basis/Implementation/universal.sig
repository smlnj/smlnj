(* universal.sig
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This is an interface to a universal type mechanism based on the Poly/ML
 * module of the same name (https://www.polyml.org/documentation/Reference/Universal.html).
 *
 * See https://github.com/SMLFamily/BasisLibrary/wiki/2020-001-Addition-of-Universal-module
 * for discussion.
 *)

(* ============================================================================
 * Copyright (c) 2020 John Reppy (http://cs.uchicago.edu/~jhr)
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
 * Reference code for SML Basis Library Proposal 2020-001.
 *
 * This interface is copied from signature for the Poly/ML Universal
 * structure.
 *)

signature UNIVERSAL =
  sig

  (* The universal tagged-union type. *)
    type universal

 (* The type of a tag for values with type 'a. Note that it is possible to define
  * multiple tags with the same argument type; these will be distinct.
  *)
    type 'a tag

  (* `tag ()` returns a new tag value. *)
    val tag : unit -> 'a tag

  (* `tagInject tag x` returns a universal value that is `x` tagged by `tag`. *)
    val tagInject : 'a tag -> 'a -> universal

  (* `tagIs tag un` returns `true` if the universal value `univ` was formed
   * by the expression `tagInject tag x` for some `x` and `false` otherwise.
   *)
    val tagIs : 'a tag -> universal -> bool

  (* `tagProject tag un` returns `x` if the universal value `univ` was formed
   * by the expression `tagInject tag x` and raises the `Match` exception
   * otherwise.
   *)
    val tagProject : 'a tag -> universal -> 'a

  end