(* primop-bind.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Representation of primop bindings that define the Inline structure. These are
 * represented as triples with names, types, and the actual primop (datatype primop
 * defined in ElabData/prim/primop.sml).  The type primop_bind is abstract.
 *
 * See compiler/Semant/prim/primop-bindings.sml for the list of bindings.
 *)

structure PrimopBind :> sig

    type primop_bind

    val mk : string * Types.ty * Primop.primop -> primop_bind

    val nameOf : primop_bind -> string
    val typeOf : primop_bind -> Types.ty
    val defnOf : primop_bind -> Primop.primop

  end = struct

    structure T = Types
    structure P = Primop

    type primop_bind = string * T.ty * P.primop

    fun mk arg = arg

    fun nameOf (n, _, _) = n
    fun typeOf (_, ty, _) = ty
    fun defnOf (_, _, p) = p

  end


