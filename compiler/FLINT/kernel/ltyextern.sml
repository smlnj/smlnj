(* ltyextern.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure LtyExtern : LTYEXTERN =
struct

local
  structure DI = DebIndex
  structure PT = PrimTyc
  structure LT = Lty
  structure LK = LtyKernel
  structure LD = LtyDef
  structure FR = FunRecMeta  (* fkind, rkind, CC_*, RK_*, LT_*, etc., formally defined here *)
  structure LK = LtyKernel
  structure LB = LtyBasic
  structure LKC = LtyKindChk
  structure PO = Primop     (* really should not refer to this *)
  structure PP = PrettyPrint
  structure PU = PPUtil

  val with_pp = PP.with_default_pp
  val say = Control.Print.say

  fun bug msg = ErrorMsg.impossible("LtyExtern: "^msg)
  val wrdebugging = FLINT_Control.wrdebugging
  fun dbsay msg = if !wrdebugging then say msg else ()

in

(* open LtyBasic  (* FIXME -- don't include LtyBasic *) *)

(* FIXME -- don't reexport these Lty values *)

fun ppTyc tyc =
    with_pp (fn ppstm => (PPLty.ppTyc 20 ppstm tyc))

val ltKindChk = LtyKindChk.ltKindCheckGen ()
val (tcKindChk,tcKindVer,teKindChk) = LtyKindChk.tcteKindCheckGen ()

(** instantiating a polymorphic type or an higher-order constructor *)
fun lt_inst (lt : LT.lty, ts : LT.tyc list) =
  let val nt = LK.lt_whnm lt
   in case (LK.lt_whnm_out nt, ts)
        of (LT.LT_POLY(ks, b), ts) =>
             if length ks <> length ts
             then (with_pp (fn ppstm =>
                     (PU.pps ppstm "### arity error in lt_inst:\n|ks| = ";
                      PU.ppi ppstm (length ks);
                      PU.pps ppstm ", |ts| = "; PU.ppi ppstm (length ts);
                      PP.newline ppstm;
                      PU.pps ppstm "lt: ";
                      PP.openHOVBox ppstm (PP.Rel 0);
                      PPLty.ppLty 20 ppstm lt;
                      PP.closeBox ppstm;
                      PP.newline ppstm;
                      PU.pps ppstm "nt: ";
                      PP.openHOVBox ppstm (PP.Rel 0);
                      PPLty.ppLty 20 ppstm nt;
                      PP.closeBox ppstm;
                      PP.newline ppstm;
                      PU.pps ppstm "ts: ";
                      PPLty.ppList ppstm
                        {sep = ",",pp=PPLty.ppTyc 20}
                        ts;
                      PP.newline ppstm));
                   bug "lt_inst - arity mismatch")
             else
             let val nenv = LT.teCons(LT.Beta(0,ts,ks), LT.teEmpty)
(* (no kind env)                val _ = teKindChk(nenv,0,Lty.initTkEnv) *)
              in map (fn x => LK.ltc_env(x, 1, 0, nenv)) b
             end
         | (_, []) => [nt]   (* this requires further clarifications !!! *)
         | (lt,ts) =>
           (with_pp (fn ppstm =>
              (PU.pps ppstm "lt_inst arg:"; PP.newline ppstm;
               PPLty.ppLty 20 ppstm (LT.lt_inj lt); PP.newline ppstm;
               PU.pps ppstm "ts length: ";
               PU.ppi ppstm (length ts); PP.newline ppstm;
               PU.pps ppstm "ts head: ";
               PPLty.ppTyc 20 ppstm (hd ts); PP.newline ppstm));
            bug "incorrect lty instantiation in lt_inst")
  end

fun lt_pinst (lt : LT.lty, ts : LT.tyc list) =
  (case lt_inst (lt, ts) of [y] => y | _ => bug "unexpected lt_pinst")

exception LtyAppChk

(* lty application with kind-checking (exported) *)
fun lt_inst_chk_gen() = let
    val tkChk = LKC.tcKindVerifyGen()
    fun lt_inst_chk (lt : LT.lty, ts : LT.tyc list, kenv : LT.tkindEnv) =
        let val nt = LK.lt_whnm lt
        in (case (LK.lt_whnm_out nt, ts)
              of (LT.LT_POLY(ks, b), ts) =>
                 let val _ = ListPair.app (tkChk kenv) (ks, ts)
                     fun h x = LK.ltc_env(x, 1, 0, LT.teCons(LT.Beta(0,ts,ks),LT.teEmpty))
                 in map h b
                 end
               | (_, []) => [nt]    (* ? problematic *)
               | _ => raise LtyAppChk)
        end
in
    lt_inst_chk
end

(** a special lty application --- used inside the translate/specialize.sml *)
fun lt_sp_adj(ks, lt, ts, dist, bnl) =
    let fun h(abslevel, ol, nl, tenv) =
          if abslevel = 0 then LK.ltc_env(lt, ol, nl, tenv)
          else if abslevel > 0 then
                 h(abslevel-1, ol+1, nl+1,
                   LT.teCons (LT.Lamb(nl,ks (* dbm ??? *)), tenv))
               else bug "unexpected cases in ltAdjSt"
        val btenv = LT.teCons (LT.Beta(0,ts,ks (* dbm ??? *)),LT.teEmpty)
        val nt = h(dist, 1, bnl, btenv)
     in nt
    end

(** a special tyc application --- used in translate/specialize.sml *)
fun tc_sp_adj(ks, tc, ts, dist, bnl) =
    let fun h(abslevel, ol, nl, tenv) =
          if abslevel = 0 then LK.tcc_env(tc, ol, nl, tenv)
          else if abslevel > 0 then
                 h(abslevel-1, ol+1, nl+1,
                   LT.teCons(LT.Lamb(nl,ks (* dbm ??? *)), tenv))
               else bug "unexpected cases in tcAdjSt"
        val btenv = LT.teCons(LT.Beta(0,ts,ks (* dbm ??? *)), LT.teEmpty)
        val nt = h(dist, 1, bnl, btenv)
     in nt (* was LK.tc_norm nt *)
    end

(** sinking the lty one-level down --- used in specialize.sml *)
fun lt_sp_sink (ks, lt, d, nd) =
    let fun h(abslevel, ol, nl, tenv) =
          if abslevel = 0 then LK.ltc_env(lt, ol, nl, tenv)
          else if abslevel > 0 then
                 h(abslevel-1, ol+1, nl+1,
                   LT.teCons(LT.Lamb(nl,ks (* dbm ??? *)), tenv))
               else bug "unexpected cases in ltSinkSt"
        val nt = h(nd-d, 0, 1, LT.teEmpty)
     in nt (* was LK.lt_norm nt *)
    end

(** sinking the tyc one-level down --- used in specialize.sml *)
fun tc_sp_sink (ks, tc, d, nd) =
    let fun h(abslevel, ol, nl, tenv) =
          if abslevel = 0 then LK.tcc_env(tc, ol, nl, tenv)
          else if abslevel > 0 then
                 h(abslevel-1, ol+1, nl+1, LT.teCons(LT.Lamb(nl,ks), tenv))
               else bug "unexpected cases in ltSinkSt"
        val nt = h(nd-d, 0, 1, LT.teEmpty)
     in nt (* was LK.tc_norm nt *)
    end

(** utility functions used in CPS *)
fun lt_iscont lt =
      (case LK.lt_whnm_out lt
        of LT.LT_CONT _ => true
         | LT.LT_TYC tc =>
             (case LK.tc_whnm_out tc of LT.TC_CONT _ => true | _ => false)
         | _ => false)

fun ltw_iscont (lt, f, g, h) =
      (case LK.lt_whnm_out lt
        of LT.LT_CONT t => f t
         | LT.LT_TYC tc =>
             (case LK.tc_whnm_out tc of LT.TC_CONT x => g x | _ => h lt)
         | _ => h lt)

(** other misc utility functions *)

fun lt_select(lty: LT.lty, i: int, whereCalled) =
    let fun tc_select(tyc, i) =
	    (case LK.tc_whnm_out tyc
	      of LT.TC_TUPLE tycs =>
		 (List.nth (tycs, i)
		  handle Subscript =>
			 bug (concat ["tc_select: bad index into TC_TUPLE: i=",
				      Int.toString i, ", |tycs| = ",
				      Int.toString(length tycs), " [", whereCalled, "]"]))
	       | _ => (with_pp
			 (fn ppstm =>
			     (PU.pps ppstm "LtyExtern.tc_select: expected TC_TUPLE; tyc = ";
			      PPLty.ppTyc 20 ppstm tyc; PP.newline ppstm));
		       bug ("tc_select: bad tyc [" ^ whereCalled ^ "]")))
    in
      (case LK.lt_whnm_out lty
	of LT.LT_STR ltys =>
	     (List.nth (ltys, i)
	      handle Subscript => bug "lt_select: LT_STR: bad index")
	 | LT.LT_TYC tyc => LD.ltc_tyc(tc_select(tyc, i))
	 | _ => bug ("lt_select: bad lty [" ^ whereCalled ^ "}"))
    end

fun tc_swap t =
  (case (LK.tc_whnm_out t)
    of LT.TC_ARROW (LT.FF_VAR (r1,r2), [s1], [s2]) =>
         LK.tcc_arrow(LT.FF_VAR (r2,r1), [s2], [s1])
     | LT.TC_ARROW (LT.FF_FIXED, [s1], [s2]) =>
         LK.tcc_arrow(LT.FF_FIXED, [s2], [s1])
     | _ => bug "unexpected tycs in tc_swap")

fun lt_swap t =
  (case (LK.lt_whnm_out t)
    of (LT.LT_POLY (ks, [x])) => LD.ltc_poly(ks, [lt_swap x])
     | (LT.LT_TYC x) => LD.ltc_tyc(tc_swap x)
     | _ => bug "unexpected type in lt_swap")

(** functions that manipulate the FLINT function and record types *)
fun ltc_fkfun ({cconv=FR.CC_FCT, ...}: FR.fkind, atys, rtys) =
      LD.ltc_fct (atys, rtys)
  | ltc_fkfun ({cconv=FR.CC_FUN fixed, ...}, atys, rtys) =
      LD.ltc_arrow(fixed, atys, rtys)

fun ltd_fkfun (lty: LT.lty) : LT.lty list * LT.lty list =
  if LD.ltp_fct lty then LD.ltd_fct lty
  else let val (_, atys, rtys) = LD.ltd_arrow lty
        in (atys, rtys)
       end

fun ltc_rkind (FR.RK_TUPLE, lts) = LD.ltc_tuple lts
  | ltc_rkind (FR.RK_STRUCT, lts) = LD.ltc_str lts
  | ltc_rkind (FR.RK_VECTOR t, _) = LB.ltc_vector (LD.ltc_tyc t)

fun ltd_rkind (lt, i) = lt_select (lt, i, "ltyextern.sml#279")

(****************************************************************************
 *             UTILITY FUNCTIONS USED BY POST-REPRESENTATION ANALYSIS       *
 ****************************************************************************)
(* tc_upd_prim : LT.tyc -> PO.primop *)
(** find out what is the appropriate primop given a tyc *)
fun tc_upd_prim tc =
  let fun h(LT.TC_PRIM pt) = if PT.ubxupd pt then PO.UNBOXEDUPDATE else PO.UPDATE
        | h(LT.TC_TUPLE _ | LT.TC_ARROW _) = PO.UPDATE
        | h(LT.TC_FIX{family={size=1,gen=tc,params=ts,...},index=0}) =
            let val ntc = case ts of [] => tc
                                   | _ => LD.tcc_app(tc, ts)
             in (case (LK.tc_whnm_out ntc)
                  of LT.TC_FN([k],b) => h (LK.tc_whnm_out b)
                   | _ => PO.UPDATE)
            end
        | h(LT.TC_SUM tcs) =
            let fun g (a::r) = if LK.tc_eqv(a, LB.tcc_unit) then g r else false
                  | g [] = true
             in if (g tcs) then PO.UNBOXEDUPDATE else PO.UPDATE
            end
        | h _ = PO.UPDATE
   in h(LK.tc_whnm_out tc)
  end

(** tk_lty : tkind -> lty --- finds out the corresponding type for a tkind *)
fun tk_lty tk =
  (case LT.tk_out tk
    of LT.TK_MONO => LB.ltc_int
     | LT.TK_BOX => LB.ltc_int
     | LT.TK_SEQ ks => LD.ltc_tuple (map tk_lty ks)
     | LT.TK_FUN (ks, k) =>
         LD.ltc_arrow(LD.ffc_fixed, [LD.ltc_tuple(map tk_lty ks)], [tk_lty k]))


(* memo-ized type translation functions for Reify (typeNarrowGen) and Wrapping (typeWrapGen) *)

structure TcDict = RedBlackMapFn(struct
                                   type ord_key = LT.tyc
				   val compare = Lty.tc_cmp
			         end)

structure LtDict = RedBlackMapFn(struct
                                   type ord_key = LT.lty
				   val compare = Lty.lt_cmp
			         end)

(* typeNarrowGen : unit -> (tyc -> tyc) * (lty -> lty) *)
(* Called once in FLINT/reps/reify.sml *)
fun typeNarrowGen () =
  let val tycDictR = ref (TcDict.empty)
      val ltyDictR = ref (LtDict.empty)

      fun tycNarrow tyc =
          (case TcDict.find(!tycDictR, tyc)
             of SOME tyc' => tyc'
              | NONE =>
                let val newtyc =
		    (case (LK.tc_whnm_out tyc)
		       of LT.TC_PRIM pt =>
			    if PT.isvoid pt then LB.tcc_void else tyc
			| LT.TC_TUPLE tcs => LD.tcc_tuple (map tycNarrow tcs)
			| LT.TC_ARROW (r, ts1, ts2) =>
			    LK.tcc_arrow(LD.ffc_fixed, map tycNarrow ts1, map tycNarrow ts2)
			| _ => LB.tcc_void)
                 in tycDictR := TcDict.insert(!tycDictR, tyc, newtyc);
		    newtyc
                end)

      fun ltyNarrow lty =
          (case LtDict.find(!ltyDictR, lty)
             of SOME lty' => lty'
              | NONE =>
                let val newlty =
		    (case LK.lt_whnm_out lty
		       of LT.LT_TYC tc => LD.ltc_tyc (tycNarrow tc)
			| LT.LT_STR ts => LD.ltc_str (map ltyNarrow ts)
			| LT.LT_FCT (ts1, ts2) => LD.ltc_fct(map ltyNarrow ts1, map ltyNarrow ts2)
			| LT.LT_POLY (ks, xs) =>
			    LD.ltc_fct([LD.ltc_str (map tk_lty ks)], map ltyNarrow xs)
			| LT.LT_CONT _ => bug "ltyNarrow: LT_CONT"
			| LT.LT_IND _ => bug "ltyNarrow: LT_IND"
			| LT.LT_ENV _ => bug "ltyNarrow LT_ENV")
		 in ltyDictR := LtDict.insert(!ltyDictR, lty, newlty);
		    newlty
                end)

   in (tycNarrow o LK.tc_norm, ltyNarrow o LK.lt_norm)
  end (* function typeNarrowGen *)

(* typeWrapGen   : unit -> (tyc -> tyc) * (lty -> lty) * (tyc -> tyc) * (lty -> lty)
 * Called once in FLINT/reps/wrapping.sml.
 * result is two wrap functions for tyc and lty, and two unwrap functions for tyc and lty *)
fun typeWrapGen () =
  let val tycWrapMap = ref (TcDict.empty)
      val tycUnwrapMap = ref (TcDict.empty)
      val ltyUnwrapMap  = ref (LtDict.empty)

      (* tycWrap : tyc -> tyc *)
      fun tycWrap (tyc: LT.tyc) =
	  (case TcDict.find(!tycWrapMap, tyc)
	     of SOME tyc' => tyc'
	      | NONE =>
		let val newtyc =
		    (case (LK.tc_whnm_out tyc)
		       of (LT.TC_VAR _ | LT.TC_NVAR _) => tyc
			| LT.TC_PRIM pt => if PT.unboxed pt then LD.tcc_wrap tyc else tyc
			| LT.TC_FN (ks, tyc') => LD.tcc_fn (ks, tycWrap tyc') (* impossible case *)
			| LT.TC_APP (tyc', tycs) => LD.tcc_app (tycWrap tyc', map tycWrap tycs)
			| LT.TC_SEQ tycs => LD.tcc_seq (map tycWrap tycs)
			| LT.TC_PROJ (tc, i) => LD.tcc_proj (tycWrap tc, i)
			| LT.TC_SUM tcs => LD.tcc_sum (map tycWrap tcs)
			| LT.TC_FIX{family={size,names,gen,params},index=i} =>
			    LD.tcc_fix ((size, names, LK.tc_norm (tycUnwrap gen), map tycWrap params), i)
			| LT.TC_TUPLE tycs => LD.tcc_wrap (LD.tcc_tuple (map tycWrap tycs)) (* ? *)
			| LT.TC_ARROW (LT.FF_VAR(b1,b2), domain, range) =>
			    let val wdomain =    (* too specific ! *)
				    (case domain
				       of [tyc1,tyc2] => [tycWrap tyc1, tycWrap tyc2]
					| _ => [tycWrap (LK.tc_autotuple domain)])
				val wrange = [tycWrap (LK.tc_autotuple range)]
				val newtyc = LK.tcc_arrow(LD.ffc_fixed, wdomain, wrange)
			     in if b1 then newtyc else LD.tcc_wrap newtyc
			    end
			| LT.TC_ARROW (LT.FF_FIXED, _, _) => bug "tycWrap: TC_ARROW(FF_FIXED)"
			| LT.TC_WRAP _ => bug "tycWrap: TC_WRAP"
			| LT.TC_BOX _ => bug "tycWrap: TC_BOX"
			| _ => bug "tycWrap: bad tyc")
		in tycWrapMap := TcDict.insert(!tycWrapMap, tyc, newtyc);
		   newtyc
		end)

      (* tycUnwrap : tyc -> tyc *)
      and tycUnwrap (tyc: LT.tyc) =
	  (case TcDict.find(!tycUnwrapMap, tyc)
	    of SOME tyc' => tyc'
	     | NONE =>
	       let val newtyc =
		   (case (LK.tc_whnm_out tyc)
		      of (LT.TC_VAR _ | LT.TC_NVAR _ | LT.TC_PRIM _) => tyc
		       | LT.TC_FN (ks, tyc') => LD.tcc_fn(ks, tycUnwrap tyc') (* impossible case *)
		       | LT.TC_APP (tyc', tycs) => LD.tcc_app(tycUnwrap tyc', map tycWrap tycs)
		       | LT.TC_SEQ tycs => LD.tcc_seq(map tycUnwrap tycs)
		       | LT.TC_PROJ (tyc', i) => LD.tcc_proj(tycUnwrap tyc', i)
		       | LT.TC_SUM tycs => LD.tcc_sum (map tycUnwrap tycs)
		       | LT.TC_FIX{family={size,names,gen,params},index=i} =>
			   LD.tcc_fix((size, names, LK.tc_norm (tycUnwrap gen), map tycWrap params), i)
		       | LT.TC_TUPLE tycs => LD.tcc_tuple (map tycUnwrap tycs)
		       | LT.TC_ARROW (LT.FF_VAR(b1,b2), ts1, ts2) =>
			   LK.tcc_arrow(LD.ffc_fixed, map tycUnwrap ts1, map tycUnwrap ts2)
		       | LT.TC_WRAP t => bug "unexpected TC_WRAP in tycUnwrap"
		       | LT.TC_ARROW (LT.FF_FIXED, _, _) =>
			   bug "tycUnwrap: TC_ARROW(FF_FIXED"
		       | LT.TC_PARROW _ => bug "unexpected TC_PARROW in tycUnwrap"
		       | LT.TC_BOX _ => bug "unexpected TC_BOX in tycUnwrap"
		       | _ => bug "unexpected other tycs in tycUnwrap")
	        in tycUnwrapMap := TcDict.insert(!tycUnwrapMap, tyc, newtyc);
		   newtyc
	       end)

      (* ltyUnwrap : lty -> lty *)
      fun ltyUnwrap (lty: LT.lty) =
	  (case LtDict.find(!ltyUnwrapMap, lty)
	    of SOME lty' => lty'
	     | NONE =>
	       let val newlty =
		   (case (LK.lt_whnm_out lty)
		      of LT.LT_TYC tyc => LD.ltc_tyc (tycUnwrap tyc)
		       | LT.LT_STR ltys => LD.ltc_str (map ltyUnwrap ltys)
		       | LT.LT_FCT (ltys1, ltys2) =>
			   LD.ltc_fct(map ltyUnwrap ltys1, map ltyUnwrap ltys2)
		       | LT.LT_POLY (ks, ltys) => LD.ltc_poly(ks, map ltyUnwrap ltys)
		       | LT.LT_CONT _ => bug "ltyUnwrap: LT_CONT"
		       | LT.LT_IND _ => bug "ltyUnwrap: LT_IND"
		       | LT.LT_ENV _ => bug "ltyUnwrap: LT_ENV")
	        in ltyUnwrapMap := LtDict.insert(!ltyUnwrapMap, lty, newlty);
		   newlty
	       end)

      fun ltyWrap x =
          LD.ltw_tyc (x, (fn tc => LD.ltc_tyc (tycWrap tc)),
                      (fn _ => bug "unexpected case in ltWrap"))

   in (tycWrap o LK.tc_norm, ltyWrap o LK.lt_norm, tycUnwrap o LK.tc_norm, ltyUnwrap o LK.lt_norm)
  end


(************************************************************************
 *            SUBSTITION OF NAMED VARS IN A TYC/LTY                     *
 ************************************************************************)

fun tc_nvar_elim_gen () =
    let val dict = ref (TcDict.empty)

    fun tc_nvar_elim find depth tyc =
        case LK.tc_nvars tyc
          of [] => tyc                   (* nothing to elim *)
           | _ =>
    let val tycdepth = LD.tcc_proj (tyc, depth)
	(* encode the tyc and the depth for memoization using LD.tcc_proj *)
     in case TcDict.find(!dict, tycdepth)
          of SOME newtyc => newtyc     (* hit! *)
           | NONE =>                   (* must recompute *)
             let val elim = tc_nvar_elim find depth (* default recursive invoc. *)
                 val elimList = map elim          (* recursive invocation on list *)
                 val newtyc =
                    case LK.tc_whnm_out tyc
                      of LT.TC_NVAR tvar =>
                         let val _ = dbsay ("tc_nvar_elim: TC_NVAR: "^LambdaVar.lvarName  tvar^"\n")
			     val newtyc =
			     (case find (tvar, depth)
                               of SOME tyc' => (dbsay "tc_nvar_elim: TC_NVAR: found!\n"; tyc')
				| NONE => (dbsay "tc_nvar_elim: TC_NVAR: not found!\n"; tyc))
			 in if !wrdebugging
			    then (say "tc_nvar_elim(TC_NVAR):\n  old = "; ppTyc tyc;
				  say "  new = "; ppTyc newtyc)
			    else ();
			    newtyc
			 end
                      | LT.TC_VAR _ => tyc   (* deBruijn type variable *)
                      | LT.TC_PRIM _ => tyc
                      | LT.TC_FN (tks, tyc') =>
                            LD.tcc_fn (tks, tc_nvar_elim find (depth+1) tyc')
                      | LT.TC_APP (t, ts) =>
                            LD.tcc_app (elim t, elimList ts)
                      | LT.TC_SEQ ts =>
                            LD.tcc_seq (elimList ts)
                      | LT.TC_PROJ (t, i) =>
                            LD.tcc_proj (elim t, i)
                      | LT.TC_SUM ts =>
                            LD.tcc_sum (elimList ts)
                      | LT.TC_FIX {family={size,names,gen,params},index} =>
                            LD.tcc_fix ((size,names,elim gen,elimList params),index)
                      | LT.TC_TUPLE ts =>
                            LD.tcc_tuple (elimList ts)
                      | LT.TC_ARROW (ff, domainTycs, rangeTycs) =>
                            LK.tcc_arrow (ff, elimList domainTycs, elimList rangeTycs)
                      | LT.TC_PARROW (t, t') =>
                            LD.tcc_parrow (elim t, elim t')
                      | LT.TC_BOX t =>
                            LD.tcc_box (elim t)
                      | LT.TC_WRAP t =>
                            LT.tc_inj (LT.TC_WRAP (elim t))
                      | LT.TC_CONT ts =>
                            LD.tcc_cont (elimList ts)
                      | LT.TC_IND _ =>
                            bug "unexpected TC_IND in tc_nvar_elim"
                      | LT.TC_ENV _ =>
                            bug "unexpected TC_ENV in tc_nvar_elim"

             in dict := TcDict.insert(!dict, tycdepth, newtyc);
                newtyc
            end
    end (* tc_nvar_elim *)
in
    tc_nvar_elim
end

(* lt_nvar_elim_gen : unit
                      -> (tvar * DebIndex.depth -> tyc option)   (* find *)
                      -> DebIndex.depth                          (* depth *)
		      -> lty -> lty *)                           (* conversion *)
fun lt_nvar_elim_gen () =
let val dict = ref (LtDict.empty)
    val tc_nvar_elim = tc_nvar_elim_gen()

    fun lt_nvar_elim find depth lty =
        case LK.lt_nvars lty
          of [] => lty         (* nothing to elim *)
           | _ =>
	     let val ltydepth = LT.lt_inj (LT.LT_ENV (lty, depth, 0, LT.teEmpty))
		 (* encode the lty and depth info using LT_ENV
		  * (only first 2 args are useful) *)
	      in case LtDict.find(!dict, ltydepth)
		   of SOME newLty => newLty       (* hit! *)
		    | NONE =>                     (* must recompute *)
		      let val elim = lt_nvar_elim find depth (* default recursive invoc. *)
			  val elimList = map elim            (* recursive invocation on list *)
			  val newLty =
			     case LK.lt_whnm_out lty
			       of LT.LT_TYC tyc =>
				     LD.ltc_tyc (tc_nvar_elim find depth tyc)
			       | LT.LT_STR ts =>
				     LD.ltc_str (elimList ts)
			       | LT.LT_FCT (ltys, ltys') =>
				     LD.ltc_fct (elimList ltys, elimList ltys')
			       | LT.LT_POLY (tks, ltys) =>
				     LD.ltc_poly (tks,
					       map (lt_nvar_elim find (depth+1)) ltys)
			       | LT.LT_CONT ltys =>
				     LD.ltc_cont (elimList ltys)
			       | LT.LT_IND _ =>
				     bug "unexpected LT_IND in lt_nvar_elim"
			       | LT.LT_ENV _ =>
				     bug "unexpected LT_ENV in lt_nvar_elim"
		       in dict := LtDict.insert(!dict, ltydepth, newLty);
			  newLty
		      end
	     end (* lt_nvar_elim *)

 in lt_nvar_elim
end (* lt_nvar_elim_gen *)

(************************************************************)

type smap = (LT.tvar * LT.tyc) list

(* is the intersection of two sorted lists non-nil? *)
fun intersectionNonEmpty(nil, _: LT.tvar list) = false
  | intersectionNonEmpty(_,nil) = false
  | intersectionNonEmpty(s1 as (h1:LT.tvar,_)::t1, s2 as h2::t2) =
        case LambdaVar.compare (h1, h2) of
            LESS => intersectionNonEmpty(t1, s2)
          | GREATER => intersectionNonEmpty(s1, t2)
          | EQUAL => true

fun searchSubst (tv: LT.tvar, s) =
    let fun h [] = NONE
          | h ((tv': LT.tvar, tyc)::s) =
                case LambdaVar.compare (tv, tv') of
                    LESS => NONE
                  | GREATER => h s
                  | EQUAL => SOME tyc
    in h s
    end

fun tc_nvar_subst_gen () = let
    val dict = ref (TcDict.empty)

    fun tc_nvar_subst subst = let
        fun loop tyc =
        (* check if substitution overlaps with free vars list *)
        (case intersectionNonEmpty(subst, LK.tc_nvars tyc) of
             false => tyc               (* nothing to subst *)
           | true =>
             (* next check the memoization table *)
             (case TcDict.find(!dict, tyc) of
                  SOME t => t           (* hit! *)
                | NONE =>
              let                       (* must recompute *)
                  val t =
                    case LK.tc_whnm_out tyc of
                        LT.TC_NVAR tv =>
                            (case searchSubst(tv,subst) of
                                 SOME t => t
                               | NONE => tyc
                                 )
                      | LT.TC_VAR _ => tyc
                      | LT.TC_PRIM _ => tyc
                      | LT.TC_FN (tks, t) =>
                            LD.tcc_fn (tks, loop t)
                      | LT.TC_APP (t, ts) =>
                            LD.tcc_app (loop t, map loop ts)
                      | LT.TC_SEQ ts =>
                            LD.tcc_seq (map loop ts)
                      | LT.TC_PROJ (t, i) =>
                            LD.tcc_proj (loop t, i)
                      | LT.TC_SUM ts =>
                            LD.tcc_sum (map loop ts)
                      | LT.TC_FIX{family={size,names,gen,params},index} =>
                            LD.tcc_fix ((size, names, loop gen, map loop params),index)
                      | LT.TC_TUPLE ts =>
                            LD.tcc_tuple (map loop ts)
                      | LT.TC_ARROW (ff, ts, ts') =>
                            LK.tcc_arrow (ff, map loop ts, map loop ts')
                      | LT.TC_PARROW (t, t') =>
                            LD.tcc_parrow (loop t, loop t')
                      | LT.TC_BOX t =>
                            LD.tcc_box (loop t)
                      | LT.TC_WRAP t =>
                            LT.tc_inj (LT.TC_WRAP (loop t))
                      | LT.TC_CONT ts =>
                            LD.tcc_cont (map loop ts)
                      | LT.TC_IND _ =>
                            bug "unexpected TC_IND in substTyc"
                      | LT.TC_ENV _ =>
                            bug "unexpected TC_ENV in substTyc"
              in
                  (* update memoization table *)
                  dict := TcDict.insert(!dict, tyc, t);
                  t
              end
                  )) (* end cases *)
    in loop
    end (* tc_nvar_subst *)
in tc_nvar_subst
end (* tc_nvar_subst_gen *)

fun lt_nvar_subst_gen () = let
    val dict = ref (LtDict.empty)
    val tc_nvar_subst' = tc_nvar_subst_gen()

    fun lt_nvar_subst subst = let
        val tc_nvar_subst = tc_nvar_subst' subst

        fun loop lty =
        (* check if there are any free type variables first *)
        (case intersectionNonEmpty(subst, LK.lt_nvars lty) of
             false => lty                  (* nothing to subst *)
           | true =>
             (* next check the memoization table *)
             (case LtDict.find(!dict, lty) of
                  SOME t => t           (* hit! *)
                | NONE =>
              let                       (* must recompute *)
                  val t =
                    case LK.lt_whnm_out lty of
                        LT.LT_TYC t =>
                            LD.ltc_tyc (tc_nvar_subst t)
                      | LT.LT_STR ts =>
                            LD.ltc_str (map loop ts)
                      | LT.LT_FCT (ts, ts') =>
                            LD.ltc_fct (map loop ts, map loop ts')
                      | LT.LT_POLY (tks, ts) =>
                            LD.ltc_poly (tks, map loop ts)
                      | LT.LT_CONT ts =>
                            LD.ltc_cont (map loop ts)
                      | LT.LT_IND _ =>
                            bug "unexpected LT_IND in lt_nvar_elim"
                      | LT.LT_ENV _ =>
                            bug "unexpected LT_ENV in lt_nvar_elim"
              in
                  (* update memoization table *)
                  dict := LtDict.insert(!dict, lty, t);
                  t
              end
                  )) (* end cases *)
    in loop
    end (* lt_nvar_subst *)
in lt_nvar_subst
end (* lt_nvar_subst_gen *)

(************************************************************)

(** building a polymorphic type by abstracting over a list of named vars **)

type tvoffs = (LT.tvar * int) list

fun intersect(nil, _:LT.tvar list) = nil
  | intersect(_, nil) = nil
  | intersect(s1 as (h1:LT.tvar,n)::t1, s2 as h2::t2) =
        case LambdaVar.compare (h1, h2) of
            LESS => intersect(t1, s2)
          | GREATER => intersect(s1, t2)
          | EQUAL => (h1,n) :: intersect(t1, t2)

fun tc_nvar_cvt_gen () =
let val dict = ref (TcDict.empty)

    fun tc_nvar_cvt (tvoffs:tvoffs) d tyc =
        ((* Stats.addStat s_iter 1; *)
         (* Stats.addStat s_tvoffs (length tvoffs); *)
         (* Stats.addStat s_nvars (length (LK.tc_nvars tyc)); *)

         (* check if substitution overlaps with free vars list *)
         case intersect(tvoffs, LK.tc_nvars tyc)
           of [] => tyc   (* (Stats.addStat s_cuts 1; tyc) *)
            | tvoffs =>
		let val tycdepth = LD.tcc_proj (tyc, d)
		    (* encode the tyc and the depth for memoization using LD.tcc_proj *)
		 in case TcDict.find(!dict, tycdepth)
		      of SOME t => t  (* hit! *)  (* (Stats.addStat s_hits 1; t) *)
		       | NONE =>      (* must recompute *)
			   let val r = tc_nvar_cvt tvoffs d (* default recursive invoc. *)
			       val rs = map r          (* recursive invocation on list *)
			       val t =
				case LK.tc_whnm_out tyc
				  of LT.TC_NVAR tvar =>
					(case searchSubst(tvar,tvoffs) of
					     SOME i => LD.tcc_var (d, i)
					   | NONE => tyc)
				   | LT.TC_VAR _ => tyc
				   | LT.TC_PRIM _ => tyc
				   | LT.TC_FN (tks, t) =>
				       LD.tcc_fn (tks, tc_nvar_cvt tvoffs (DI.next d) t)
				   | LT.TC_APP (t, ts) =>
				       LD.tcc_app (r t, rs ts)
				   | LT.TC_SEQ ts =>
				       LD.tcc_seq (rs ts)
				   | LT.TC_PROJ (t, i) =>
				       LD.tcc_proj (r t, i)
				   | LT.TC_SUM ts =>
				       LD.tcc_sum (rs ts)
				   | LT.TC_FIX{family={size,names,gen,params},index} =>
				       LD.tcc_fix ((size, names, r gen, rs params), index)
				   | LT.TC_TUPLE ts =>
				       LD.tcc_tuple (rs ts)
				   | LT.TC_ARROW (ff, ts, ts') =>
				       LK.tcc_arrow (ff, rs ts, rs ts')
				   | LT.TC_PARROW (t, t') =>
				       LD.tcc_parrow (r t, r t')
				   | LT.TC_BOX t =>
				       LD.tcc_box (r t)
				   | LT.TC_WRAP t =>
				       LT.tc_inj (LT.TC_WRAP (r t))
				   | LT.TC_CONT ts =>
				       LD.tcc_cont (rs ts)
				   | LT.TC_IND _ =>
				       bug "unexpected TC_IND in tc_nvar_cvt"
				   | LT.TC_ENV _ =>
				       bug "unexpected TC_ENV in tc_nvar_cvt"
			    in dict := TcDict.insert(!dict, tycdepth, t);
			       t
			   end
		end (* tc_nvar_cvt *))
in
    tc_nvar_cvt
end (* tc_nvar_cvt_gen *)


fun lt_nvar_cvt_gen() = let
    val dict = ref (LtDict.empty)
    val tc_nvar_cvt = tc_nvar_cvt_gen()

    fun lt_nvar_cvt tvoffs d lty =
        (* check if substitution overlaps with free vars list *)
        case intersect(tvoffs, LK.lt_nvars lty) of
            [] => lty                (* nothing to cvt *)
          | tvoffs =>
    let
        (* encode the lty and depth info using LT_ENV
         * (only first 2 args are useful) *)
        val ltydepth = LT.lt_inj (LT.LT_ENV (lty, d, 0, LT.teEmpty))
    in
        case LtDict.find(!dict, ltydepth) of
            SOME t => t                 (* hit! *)
          | NONE => let                 (* must recompute *)
                val r = lt_nvar_cvt tvoffs d (* default recursive invoc. *)
                val rs = map r          (* recursive invocation on list *)
                val t =
                    case LK.lt_whnm_out lty of
                        LT.LT_TYC t =>
                            LD.ltc_tyc (tc_nvar_cvt tvoffs d t)
                      | LT.LT_STR ts =>
                            LD.ltc_str (rs ts)
                      | LT.LT_FCT (ts, ts') =>
                            LD.ltc_fct (rs ts, rs ts')
                      | LT.LT_POLY (tks, ts) =>
                            LD.ltc_poly (tks,
                                      map (lt_nvar_cvt tvoffs (DI.next d)) ts)
                      | LT.LT_CONT ts =>
                            LD.ltc_cont (rs ts)
                      | LT.LT_IND _ =>
                            bug "unexpected LT_IND in lt_nvar_cvt"
                      | LT.LT_ENV _ =>
                            bug "unexpected LT_ENV in lt_nvar_cvt"
            in
                dict := LtDict.insert(!dict, ltydepth, t);
                t
            end
    end (* lt_nvar_cvt *)
in
    lt_nvar_cvt
end (* lt_nvar_cvt_gen *)

(* make a type abstraction from nvar to lty *)
fun lt_nvpoly(tvks, lt) =
    let
	fun frob ((tv,k)::tvks, n, ks, tvoffs) =
	    frob (tvks, n+1, k::ks, (tv,n)::tvoffs)
	  | frob ([], _, ks, tvoffs) =
	    (rev ks, rev tvoffs)

	val (ks, tvoffs) = frob (tvks, 0, [], [])
	fun gt ((tvar1,_), (tvar2,_)) = LambdaVar.>(tvar1, tvar2)
	val tvoffs = ListMergeSort.sort gt tvoffs

	(* temporarily gen() *)
	val ltSubst = lt_nvar_cvt_gen() tvoffs (DI.next DI.top) (* = 1 *)
    in LD.ltc_poly(ks, map ltSubst lt)
    end

end (* top-level local *)
end (* structure LtyExtern *)
