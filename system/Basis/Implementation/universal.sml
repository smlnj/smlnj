(* universal.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This is an interface to a universal type mechanism based on the Poly/ML
 * module of the same name (https://www.polyml.org/documentation/Reference/Universal.html).
 *
 * See https://github.com/SMLFamily/BasisLibrary/wiki/2020-001-Addition-of-Universal-module
 * for discussion.
 *
 * The implementation is based on the SML Basis Library reference implementation.
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
 *)

structure Universal :> UNIVERSAL =
  struct

    type universal = exn

    type 'a tag = { inj : 'a -> universal, prj : universal -> 'a option }

    fun 'a tag () = let
	  exception TAG of 'a
	  in {
	    inj = TAG,
	    prj = fn (TAG x) => SOME x | _ => NONE
	  } end

    fun tagInject (tag : 'a tag) = #inj tag
    fun tagIs (tag : 'a tag) univ = Option.isSome(#prj tag univ)
    fun tagProject (tag : 'a tag) univ = (case #prj tag univ
	   of SOME x => x
	    | NONE => raise Match
	  (* end case *))

  end