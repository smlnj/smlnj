(* primop-id.sig
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * PRIMOPID: front-end representation of information identifying
 * primops (either in variables, or in structures). Replaces
 * INL_INFO
 *)

signature PRIMOP_ID =
sig

  datatype prim_id = Prim of PrimopBind.primop_bind | NonPrim

  datatype str_prim_elem
    = PrimE of prim_id
    | StrE of str_prim_info

  withtype str_prim_info = str_prim_elem list

  val isPrimop : prim_id -> bool

  val isPrimCallcc : prim_id -> bool
  val isPrimCast : prim_id -> bool

  val selStrPrimId : str_prim_elem list * int -> str_prim_elem list
  val selValPrimFromStrPrim : str_prim_elem list * int -> prim_id

  val ppPrim : prim_id -> string
  val ppStrInfo : str_prim_info -> unit

end (* signature PRIMOPID *)
