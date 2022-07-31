(* ltybasic.sig
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(*
 * This file contains all the standard operations defined in LtyDef plus
 * the set of common functions used to manipulate kinds, tycs, and ltys.
 * The rule of thumb about what should be included in this file rather
 * than the ltyextern.sml: well, all primitive lambda tkinds, tycs and
 * ltys should be here, all common utility functions on tkinds, tycs,
 * and ltys should be here. Functions that are of specific use should
 * go to the ltyextern.sml. Still, the module LtyExtern will include
 * all functions defined here, so all clients should use functions via
 * the LtyExtern structure.
 *)

signature LTYBASIC =
sig

(*
 * The internal implementation of tkind,
 * tyc, and lty are in the ltykernel.sig and ltykernel.sml files. In general,
 * the clients of the lambda types should never need to understand what is
 * going on inside the LtyKernel.
 *)

(** the definitions of tkind, tyc, and lty *)
(* include LTYDEF        (* see ltydef.sig for details *)  -- not including *)

(** primitives and utility functions for fflags *)
val ffc_plambda: Lty.fflag
val ffc_rrflint: Lty.fflag
val ffc_fspec  : Lty.fflag * (bool * bool) -> Lty.fflag
val ffd_fspec  : Lty.fflag -> bool * bool

(** primitive lambda tycs *)
val tcc_int    : Lty.tyc		(* default tagged int type *)
val tcc_num    : int -> Lty.tyc	(* int type of given size *)
val tcc_real   : Lty.tyc
val tcc_string : Lty.tyc
val tcc_exn    : Lty.tyc
val tcc_void   : Lty.tyc
val tcc_unit   : Lty.tyc
val tcc_bool   : Lty.tyc

val tcc_tv     : int -> Lty.tyc
val tcc_ref    : Lty.tyc -> Lty.tyc
val tcc_array  : Lty.tyc -> Lty.tyc
val tcc_vector : Lty.tyc -> Lty.tyc
val tcc_etag   : Lty.tyc -> Lty.tyc

(** primitive lambda ltys *)
val ltc_num    : int -> Lty.lty
val ltc_int    : Lty.lty	(* = ltc_num Target.defaultIntSz *)
val ltc_real   : Lty.lty	(* REAL32: need ltc_real32/ltc_real64 *)
val ltc_string : Lty.lty
val ltc_exn    : Lty.lty
val ltc_void   : Lty.lty
val ltc_unit   : Lty.lty
val ltc_bool   : Lty.lty

val ltc_tv     : int -> Lty.lty
val ltc_ref    : Lty.lty -> Lty.lty
val ltc_array  : Lty.lty -> Lty.lty
val ltc_vector : Lty.lty -> Lty.lty
val ltc_etag   : Lty.lty -> Lty.lty

val ltc_top    : Lty.lty    (* used in a dirty hack in prim.sml *)

(** pretty printing of tkinds, tycs, and ltys *)
val tk_print   : Lty.tkind -> string
val tc_print   : Lty.tyc -> string
val lt_print   : Lty.lty -> string

(** adjusting an lty or tyc from one DebIndex.depth to another *)
val lt_adj     : Lty.lty * DebIndex.depth * DebIndex.depth -> Lty.lty
val tc_adj     : Lty.tyc * DebIndex.depth * DebIndex.depth -> Lty.tyc

val lt_adj_k   : Lty.lty * DebIndex.depth * DebIndex.depth * int -> Lty.lty
val tc_adj_k   : Lty.tyc * DebIndex.depth * DebIndex.depth * int -> Lty.tyc

(** an ltyEnv maps from lvars to their ltys; notice that the ltys are depth-dependent *)
type ltyEnv
val initLtyEnv : ltyEnv
val ltLookup : ltyEnv * LambdaVar.lvar * DebIndex.depth -> Lty.lty option
val ltInsert : ltyEnv * LambdaVar.lvar * Lty.lty * DebIndex.depth -> ltyEnv

end (* signature LTYBASIC *)


