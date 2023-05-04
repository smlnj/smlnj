(* ltybasic.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure LtyBasic : LTYBASIC =
struct

local
  structure LV = LambdaVar
  structure PT = PrimTyc
  structure LT = Lty
  structure LK = LtyKernel
  structure LD = LtyDef

      (** common utility functions *)
		   
  val say = Control_Print.say

  fun bug msg = ErrorMsg.impossible("LtyExtern: "^msg)

in

(* deBruijn indices *)
type depth = int  (* bindings in context *)
type index = int  (* deBruijn index *)
val innermost: index = 1 (* refers to directly enclosing type abstraction *)

(** primitive fflags *)
val ffc_plambda = LD.ffc_var (false, false)
val ffc_rrflint = LD.ffc_var (true, true)

fun ffc_fspec (x as LT.FF_FIXED, (true,true)) = x
  | ffc_fspec (x as LT.FF_VAR _, nx) = LD.ffc_var nx
  | ffc_fspec _ = bug "unexpected case in ffc_fspec"

fun ffd_fspec (LT.FF_FIXED) = (true,true)
  | ffd_fspec (LT.FF_VAR x) = x

(** utility functions for constructing tycs *)
val tcc_int    = LD.tcc_prim PT.ptc_int
val tcc_num    = LD.tcc_prim o PT.ptc_num
val tcc_real   = LD.tcc_prim PT.ptc_real	(* REAL32: FIXME *)
val tcc_string = LD.tcc_prim PT.ptc_string
val tcc_exn    = LD.tcc_prim PT.ptc_exn
val tcc_void   = LD.tcc_prim PT.ptc_void
val tcc_unit   = LD.tcc_tuple []
val tcc_bool   =
  let val tbool = LD.tcc_sum [tcc_unit, tcc_unit]
      val tsig_bool = LD.tcc_fn ([LD.tkc_mono], tbool)
   in LD.tcc_fix((1, #["bool"], tsig_bool, []), 0)
  end

val tcc_list   =  (* not exported, used for printing *)
  let val alpha = LD.tcc_dvar (1, 0)  (* innermost type abstraction *)
      val tlist = LD.tcc_dvar (2, 0)  (* next to innermost type abstraction *)
      val alist = LD.tcc_app (tlist, [alpha])
      val tcc_cons = LD.tcc_tuple [alpha, alist]
      val tlist = LD.tcc_fn ([LD.tkc_mono], LD.tcc_sum [tcc_cons, tcc_unit])
                            (** the order here should be consistent with
                                that in basics/basictypes.sml **)
      val tsig_list = LD.tcc_fn ([LD.tkc_int 1], tlist)
   in LD.tcc_fix((1, #["list"], tsig_list, []), 0)
  end

fun tcc_tv i     = LD.tcc_dvar (innermost, i)
fun tcc_ref x    = LD.tcc_app (LD.tcc_prim PT.ptc_ref, [x])
fun tcc_array x  = LD.tcc_app (LD.tcc_prim PT.ptc_array, [x])
fun tcc_vector x = LD.tcc_app (LD.tcc_prim PT.ptc_vector, [x])
fun tcc_etag x   = LD.tcc_app (LD.tcc_prim PT.ptc_etag, [x])

(** primitive lambda ltys *)
val ltc_int    = LD.ltc_tyc tcc_int
val ltc_num    = LD.ltc_tyc o tcc_num
val ltc_real   = LD.ltc_tyc tcc_real
val ltc_string = LD.ltc_tyc tcc_string
val ltc_exn    = LD.ltc_tyc tcc_exn
val ltc_void   = LD.ltc_tyc tcc_void
val ltc_unit   = LD.ltc_tyc tcc_unit
val ltc_bool   = LD.ltc_tyc tcc_bool

val ltc_tv     = LD.ltc_tyc o tcc_tv

fun ltc_ref x = LD.ltc_tyc (tcc_ref (LD.ltd_tyc x))
		handle DeconExn => bug "ltc_ref on Poly"

fun ltc_array x = LD.ltc_tyc (tcc_array (LD.ltd_tyc x))
		  handle DeconExn => bug "ltc_array on Poly"

fun ltc_vector x = LD.ltc_tyc (tcc_vector (LD.ltd_tyc x))
		   handle DeconExn => bug "ltc_vector on Poly"

fun ltc_etag x = LD.ltc_tyc (tcc_etag (LD.ltd_tyc x))
		 handle DeconExn => bug "ltc_etag on Poly"

val ltc_top = LD.ltc_ppoly([LD.tkc_mono], ltc_tv 0)


(** adjusting an lty or tyc from one depth to another *)

fun lt_adj (lt, d, nd) =
  if d = nd then lt
  else LK.ltc_env(lt, 0, nd - d, LT.teEmpty)

fun tc_adj (tc, d, nd) =
  if d = nd then tc
  else LK.tcc_env(tc, 0, nd - d, LT.teEmpty)
       (* handle LK.TCENV => bug "tc_adj" *)

(** The following functions are similiar to lt_adj and tc_adj;
    they adjust an lty (or tyc) from depth d+k to depth nd+k,
    assuming the last k levels are type abstractions. So lt_adj
    is really lt_adj_k with k set to 0. Both functions are currently
    called only in lcontract.sml. *)
local
fun mkTycEnv (i, k, dd, te) =
  if i >= k then te
  else mkTycEnv(i+1, k, dd, LT.teCons(LT.Lamb(dd+i,[]),te))
  (* dbm: no ks available *)

in
fun lt_adj_k (lt, d, nd, k) =
  if d = nd then lt
  else LK.ltc_env(lt, k, nd-d+k, mkTycEnv(0, k, nd-d, LT.teEmpty))

fun tc_adj_k (tc, d, nd, k) =
  if d = nd then tc
  else LK.tcc_env(tc, k, nd-d+k, mkTycEnv(0, k, nd-d, LT.teEmpty))
       handle LK.TCENV => bug "tc_adj_k"

end (* lt_adj_k and tc_adj_k *)


(***************************************************************************
 *            UTILITY FUNCTIONS ON LTY ENVIRONMENT                         *
 ***************************************************************************)

(* ltyEnv: environment mapping lvars to lty and depth
 *  plus utility values and functions on ltyEnv,
 *  used in: plambda/chkplexp.sml, flint/chkflint-named.sml, flint/chkflint.sml *)

type ltyEnv = (LT.lty * depth) LambdaVar.Map.map

(* initLtyEnv : ltyEnv *)
val initLtyEnv : ltyEnv = LV.Map.empty

(* ltLookup : ltyEnv * LambdaVar.lvar * depth -> SOME lty *)
fun ltLookup (venv, lv, nd) =
    (case LambdaVar.Map.find (venv, lv)
       of NONE  =>
	    (say ("ltLookup: unbound lvar: " ^ LambdaVar.prLvar lv ^ "\n");
	     NONE)
	| SOME (lty, d) =>
	    if d=nd then SOME lty
	    else if d > nd then bug "ltLookup: unexpected depth"
	    else SOME (LK.ltc_env (lty, 0, nd - d, LT.teEmpty))
    (*end case*))

(* ltInsert : ltyEnv * LV.lvar * LT.lty * depth -> ltyEnv *)
fun ltInsert (venv, lvar, lty, depth) =
    LambdaVar.Map.insert(venv, lvar, (lty, depth))

end (* top-level local *)
end (* structure LtyBasic *)
