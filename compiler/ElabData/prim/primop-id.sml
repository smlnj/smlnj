(* primopid.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(* [dbm, 6/19/06]
     Folded ii.sml into this structure, eliminating exn hack.
     Changed name of pureInfo to isPrimCast.
     Eliminated redundant INL_PRIM, INL_STR, INL_NO. *)

structure PrimopId : PRIMOP_ID = 
struct

  (* primops are represented by values of the abstract type primop_bind,
     which contains the name, type, and primop value of the primop.
     The file DEVNOTES/Flint/primop-list provides a catalog of primop names
     with their types and primop values, which corresponds to the list
     PrimopBindings/prims.  See dev-notes/prim/primops.txt. *)

  datatype prim_id = Prim of PrimopBind.primop_bind | NonPrim

  datatype str_prim_elem
    = PrimE of prim_id
    | StrE of str_prim_info

  withtype str_prim_info = str_prim_elem list

  fun bug s = ErrorMsg.impossible ("PrimopId: " ^ s)

  (* isPrimop : prim_id -> bool *)
  fun isPrimop (Prim _) = true
    | isPrimop NonPrim  = false

  (* Used in TopLevel/main/compile.sml to identify callcc/capture primops *)
  fun isPrimCallcc (Prim p) = (case PrimopBind.defnOf p
	 of (Primop.CALLCC | Primop.CAPTURE) => true
	  |  _ => false
	(* end case *))
    | isPrimCallcc _ = false

  (* Used in ElabData/modules/moduleutil.sml to identify cast primop *)
  fun isPrimCast (Prim p) = (case PrimopBind.defnOf p
	 of Primop.CAST => true
	  | _ => false
	(* end case *))
    | isPrimCast _ = false

  (* selStrPrimId : str_prim_info * int -> str_prim_info *)
  (* Select the prim ids for a substructure *)
  fun selStrPrimId([], slot) = []  (* not a bug? DBM *)
    | selStrPrimId(elems, slot) = 
      (case List.nth(elems, slot) 
	of StrE elems' => elems'
	 | PrimE _ => bug "PrimopId.selStrPrimId: unexpected PrimE")
      handle Subscript => (bug "PrimopId.selStrPrimId Subscript")
	(* This bug happens if we got a primid for a value 
	   component when we expected a str_prim_elem for a 
	   structure *)

  (* Select the prim id for a value component *)
  fun selValPrimFromStrPrim([], slot) = NonPrim (* not a bug? DBM *)
    | selValPrimFromStrPrim(elems, slot) =
      (case List.nth(elems, slot)
	of PrimE(id) => id
	 | StrE _ => 
	   bug "PrimopId.selValPrimFromStrPrim: unexpected StrE")
      handle Subscript => bug "PrimopId.selValPrimFromStrPrim Subscript"
        (* This bug occurs if we got a substructure's
           str_prim_elem instead of an expected value component's
           prim_id *)

  fun ppPrim NonPrim = "<NonPrim>"
    | ppPrim (Prim p) = ("<PrimE " ^ PrimopBind.nameOf p ^">")

  fun ppStrInfo strelems = 
      let fun ppElem [] = ()
	    | ppElem ((PrimE p)::xs) = (print (ppPrim p); ppElem xs)
	    | ppElem ((StrE s)::xs) = (ppStrInfo s; ppElem xs)
      in (print "[ "; ppElem strelems; print " ]\n")
      end

end (* structure PrimopId *)
