(* string-token.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * A trivial implementation of tokens as strings w/o style information.
 *)

structure StringToken : PP_TOKEN =
  struct
    type style = unit
    type token = string
    fun string s = s
    fun style _ = ()
    fun size s = String.size s
  end
