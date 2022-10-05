(* ElabData/prim/ppprim-new.sml
 *
 * COPYRIGHT (c) 2017, 2022 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure PPPrim =
struct

local
  structure PP = NewPP
in

(* fmtPrim : PrimopId.prim_id -> PP.format *)
fun fmtPrim prim =
    (case prim
      of PrimopId.NonPrim => PP.text "<NonPrim>"
       | PrimopId.Prim prim => PP.text ("<Prim: " ^  PrimopBind.nameOf prim ^ ">"))
    (* function fmtPrim *)

(* fmtStrPrimElem : PrimopId.str_prim_elem -> PP.format *)
fun fmtStrPrimElem (PrimopId.PrimE p) = fmtPrim p
  | fmtStrPrimElem (PrimopId.StrE ps) = PP.listFormats (map fmtStrPrimInfo ps)

(* fmtStrPrimInfo : PrimopId.str_prim_info -> PP.format *)
fun fmtStrPrimInfo strPrimInfo = PP.tupleFormats (map fmtStrPrimElem strPrimInfo)

end (* top local *)
end (* structure PPPrim *)

(* [DBM, 09.17.2022]: converted to use NewPP instead of PrettyPrint *)

