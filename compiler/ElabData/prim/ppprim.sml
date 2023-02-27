(* ElabData/prim/ppprim-new.sml
 *
 * COPYRIGHT (c) 2017, 2022 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure PPPrim =
struct

local
  structure PI = PrimopId
  structure PB = PrimopBind
  structure PP = PrettyPrint
in

(* fmtPrim : PrimopId.prim_id -> PP.format *)
fun fmtPrimId (prim: PI.prim_id) =
    (case prim
      of PI.NonPrim => PP.text "<NonPrim>"
       | PI.Prim prim => PP.text ("<Prim: " ^  PB.nameOf prim ^ ">"))
    (* function fmtPrim *)

(* fmtStrPrimElem : PI.str_prim_elem -> PP.format *)
fun fmtStrPrimElem (PI.PrimE p) = fmtPrimId p
  | fmtStrPrimElem (PI.StrE ps) = fmtStrPrimInfo ps

(* fmtStrPrimInfo : PrimopId.str_prim_info -> PP.format *)
and fmtStrPrimInfo (strPrimInfo: PI.str_prim_info) = PP.tuple (map fmtStrPrimElem strPrimInfo)

end (* top local *)
end (* structure PPPrim *)

(* [DBM, 09.17.2022]: converted to use new PrettyPrint *)

