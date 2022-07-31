(* ltyextern.sig
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This interface hides the implementation details of FLINT tkind, tyc, and
 * lty defined inside Lty. For each entity, we provide a series of
 * constructor funtions, deconstructor functions, predicate functions,
 * and other utility functions. We divide these functions into three files:
 * LtyDef contains the set of abstract constructor, deconstructor, and
 * predicate functions for tkind, tyc, and lty; LtyBasic includes all
 * functions in LtyDef plus all commonly used primitive tycs and ltys, and
 * utility functions; finally, the current LtyExtern structure includes all
 * functions in LtyBasic plus a set of rather specialized utility functions.
 *
 * We design this hierarchy in a way so that LtyDef as a stable interface,
 * so one can refer to types such as tkind, tyc, lty as LtyDef.tkind, etc;
 * LtyBasic is a medium-stable interface, only commonly used functions
 * should be left here; LtyExtern is a least-stable interface, any new
 * utility function that manipulates types should go here.
 *
 * The conventions are (1) types should be referenced as "LtyDef.foo"
 * (2) functions should all be accessed as "LtyExtern.foo". The client
 * in general should never need to access LtyKernel (?).
 *
 * This interface only refers to structures DebIndex, Lty,
 * FunRecMeta, PrimOp.  No types are exported by LTYEXTERN.
 *)

signature LTYEXTERN =
sig

(** instantiating a polymorphic type or an higher-order constructor *)
val lt_inst     : Lty.lty * Lty.tyc list -> Lty.lty list
val lt_pinst    : Lty.lty * Lty.tyc list -> Lty.lty

exception LtyAppChk

(* perform polytype instantiation with kind checking *)
val lt_inst_chk_gen : unit -> Lty.lty * Lty.tyc list * Lty.tkindEnv -> Lty.lty list

(* substitution of named type variables *)
(*** CLEAN THIS UP ***)
val tc_nvar_elim_gen : unit -> (Lty.tvar * DebIndex.depth -> Lty.tyc option)
                            -> DebIndex.depth -> Lty.tyc -> Lty.tyc
val lt_nvar_elim_gen : unit -> (Lty.tvar * DebIndex.depth -> Lty.tyc option)
                            -> DebIndex.depth -> Lty.lty -> Lty.lty

(* !! BEWARE !!
 * The `subst' argument is assumed to be sorted with increasing tvars *)
val tc_nvar_subst_gen : unit -> (Lty.tvar * Lty.tyc) list -> Lty.tyc -> Lty.tyc
val lt_nvar_subst_gen : unit -> (Lty.tvar * Lty.tyc) list -> Lty.lty -> Lty.lty

val tc_nvar_cvt_gen : unit -> (Lty.tvar * int) list
                           -> DebIndex.depth -> Lty.tyc -> Lty.tyc
val lt_nvar_cvt_gen : unit -> (Lty.tvar * int) list
                           -> DebIndex.depth -> Lty.lty -> Lty.lty

(* The equivalent to ltc_poly for the nvar case *)
val lt_nvpoly : (Lty.tvar * Lty.tkind) list * Lty.lty list -> Lty.lty

(* special adjustment functions used during type specializations *)
val lt_sp_adj : Lty.tkind list * Lty.lty * Lty.tyc list * int * int -> Lty.lty
val tc_sp_adj : Lty.tkind list * Lty.tyc * Lty.tyc list * int * int -> Lty.tyc
val lt_sp_sink: Lty.tkind list * Lty.lty * DebIndex.depth * DebIndex.depth -> Lty.lty
val tc_sp_sink: Lty.tkind list * Lty.tyc * DebIndex.depth * DebIndex.depth -> Lty.tyc

(** utility functions used in CPS only, should go away soon ! *)
val lt_iscont   : Lty.lty -> bool
val ltw_iscont  : Lty.lty * (Lty.lty list -> 'a) * (Lty.tyc list -> 'a) * (Lty.lty -> 'a)
		  -> 'a

(** other utility functions --- requires clean up!*)
val lt_select : Lty.lty * int * string -> Lty.lty
val lt_swap : Lty.lty -> Lty.lty

(** functions that manipulate the FLINT function and record types *)
val ltc_fkfun   : FunRecMeta.fkind * Lty.lty list * Lty.lty list -> Lty.lty
val ltd_fkfun   : Lty.lty -> Lty.lty list * Lty.lty list (* fkind omitted *)

val ltc_rkind   : FunRecMeta.rkind * Lty.lty list -> Lty.lty
val ltd_rkind   : Lty.lty * int -> Lty.lty

(** given a tyc, select the appropriate update primop *)
val tc_upd_prim : Lty.tyc -> Primop.primop

(** translating the tkind into the corresponding type *)
val tk_lty      : Lty.tkind -> Lty.lty

(** type wrapping translation generator, used once in Wrapping.wrapping *)
val typeWrapGen : unit -> (Lty.tyc -> Lty.tyc) * (Lty.lty -> Lty.lty)
			  * (Lty.tyc -> Lty.tyc) * (Lty.lty -> Lty.lty)

(** type narrowing translation generator, used once in Reify.reify *)
val typeNarrowGen : unit -> (Lty.tyc -> Lty.tyc) * (Lty.lty -> Lty.lty)

end (* signature LTYEXTERN *)
