(* pre-perv.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure PrePervasive =
  struct

    exception Span

    open Order

    datatype option = datatype Assembly.option

    exception Option

    fun getOpt (SOME x, y) = x
      | getOpt (NONE, y) = y

    fun isSome (SOME _) = PrimTypes.true
      | isSome NONE = PrimTypes.false

    fun valOf (SOME x) = x
      | valOf NONE = raise Option

    val op = : ''a * ''a -> PrimTypes.bool = InlineT.=
    val op <> : ''a * ''a -> PrimTypes.bool = InlineT.<>

  end
