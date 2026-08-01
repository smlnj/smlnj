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

    val mk : string * Types.ty * PrimOps.t -> primop_bind

    val nameOf : primop_bind -> string
    val typeOf : primop_bind -> Types.ty
    val defnOf : primop_bind -> PrimOps.t

  end = struct

    type primop_bind = string * Types.ty * PrimOps.t

    fun mk arg = arg

    fun nameOf (n, _, _) = n
    fun typeOf (_, ty, _) = ty
    fun defnOf (_, _, p) = p

  end
