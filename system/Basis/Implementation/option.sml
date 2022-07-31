(* option.sml
 *
 * COPYRIGHT (c) 2015 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Option : OPTION =
  struct

    datatype option = datatype Assembly.option

    exception Option = Option

    fun getOpt (SOME x, y) = x
      | getOpt (NONE, y) = y
    fun isSome (SOME _) = true
      | isSome NONE = false
    fun valOf (SOME x) = x
      | valOf _ = raise Option

    fun filter pred x = if (pred x) then SOME x else NONE
    fun join (SOME opt) = opt
      | join NONE = NONE
    fun app f (SOME x) = f x
      | app f NONE = ()
    fun map f (SOME x) = SOME(f x)
      | map f NONE = NONE
    fun mapPartial f (SOME x) = f x
      | mapPartial f NONE = NONE
    fun compose (f, g) x = map f (g x)
    fun composePartial (f, g) x = mapPartial f (g x)

  (* added for Basis Library proposal 2015-003 *)
    fun isNone NONE = true
      | isNone _ = false

    fun fold f init NONE = init
      | fold f init (SOME x) = f(x, init);

  end;


