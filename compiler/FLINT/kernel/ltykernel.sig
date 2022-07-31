(* COPYRIGHT (c) 1997 YALE FLINT PROJECT *)
(* ltykernel.sig *)

signature LTYKERNEL =
sig

exception TCENV
exception TeUnbound

(** testing equivalence of tkinds, tycs, ltys, and fflags *)
val tk_eqv   : Lty.tkind * Lty.tkind -> bool
val tc_eqv   : Lty.tyc * Lty.tyc -> bool
val lt_eqv   : Lty.lty * Lty.lty -> bool
val ff_eqv   : Lty.fflag * Lty.fflag -> bool

(** finding out the depth for a tyc's innermost-bound free variables *)
val tc_depth : Lty.tyc * DebIndex.depth -> DebIndex.depth
val tcs_depth: Lty.tyc list * DebIndex.depth -> DebIndex.depth
val tc_nvars : Lty.tyc -> Lty.tvar list
val lt_nvars : Lty.lty -> Lty.tvar list

(** utility functions for TC_ENV and LT_ENV types *)
val tcc_env  : Lty.tyc * int * int * Lty.tycEnv -> Lty.tyc
val ltc_env  : Lty.lty * int * int * Lty.tycEnv -> Lty.lty

(** reducing a tyc or lty to the weak-head normal form *)
val tc_whnm : Lty.tyc -> Lty.tyc
val lt_whnm : Lty.lty -> Lty.lty

(* "wh-normalizing" projections of tyc and lty *)
val tc_whnm_out : Lty.tyc -> Lty.tycI
val lt_whnm_out : Lty.lty -> Lty.ltyI

(** reducing a tyc or lty to the true normal form *)
val tc_norm : Lty.tyc -> Lty.tyc
val lt_norm : Lty.lty -> Lty.lty

(** automatically flattening the argument or the result type *)
val lt_autoflat : Lty.lty -> bool * Lty.lty list * bool

(** automatically tupling up the multiple argument/result into a single one *)
val tc_autotuple : Lty.tyc list -> Lty.tyc

(** tcc_arrow does automatic argument and result flattening.
 *   perhaps belongs with other tcc_ functions in LtyDef, but depends on
 *   tc_autoflat, which is not currently exported from LtyKernel *)
val tcc_arrow : Lty.fflag * Lty.tyc list * Lty.tyc list -> Lty.tyc

end (* signature LTYKERNEL *)
