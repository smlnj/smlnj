(* bind-structs.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Standard bindings of Basis structure aliases; target-specific bindings
 * are in Target{32,64}Bit/bind-structs.sml.
 *)

(* integer structures *)
structure FixedIntImp = Int64Imp
structure LargeIntImp : INTEGER = IntInfImp
structure PositionImp = Int64Imp

(* word structures *)
structure LargeWordImp = Word64Imp

(* real structures *)
structure RealImp = Real64Imp
structure LargeRealImp = Real64Imp
structure Math = Math64
