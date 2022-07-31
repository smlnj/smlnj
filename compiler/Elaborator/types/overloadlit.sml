(* overloadlit.sml
 *
 * Lists of overloading candidates for literals.
 *
 * eventually, these may be defined elsewhere, perhaps via some
 * compiler configuration mechanism
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(* This structure now redundant.  Function taken over by OverloadClasses
 * (Elaborator/types/overloadclasses.sml).
 *)

signature OVERLOADLIT =
  sig

  (* Default for kind is is first element of litTypes(kind) (or filtered
   * version resulting from unificaton).
   *)
    val intTypes : Types.ty list
    val wordTypes : Types.ty list

(* eventually
    val realTypes : Types.ty list
    val charTypes : Types.ty list
    val stringTypes : Types.ty list
*)

  end  (* signature OVERLOADLIT *)

structure OverloadLit : OVERLOADLIT =
  struct

    structure BT = BasicTypes

    val intTypes = [BT.intTy, BT.int32Ty, BT.int64Ty, BT.intinfTy]

    val wordTypes = [BT.wordTy, BT.word8Ty, BT.word32Ty, BT.word64Ty]

  end (* structure OverloadLit *)
