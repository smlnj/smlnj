(* rttype.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature RTTYPE =
sig
  type tcode

  type rtype
  val tcode_void   : tcode
  val tcode_record : tcode
  val tcode_pair   : tcode
  val tcode_fpair  : tcode
  val tcode_real   : tcode
  val tcode_realN  : int -> tcode
  val tovalue      : tcode -> FLINT.value

end (* signature RTTYPE *)

structure RuntimeType (* :> RTTYPE *) =
struct

local
  structure DI = DebIndex
  structure LT = Lty
  structure LK = LtyKernel
  structure LD = LtyDef
  structure LB = LtyBasic
  structure LE = LtyExtern
  structure PO = Primop
  structure PT = PrimTyc
  structure LV = LambdaVar
  structure FR = FunRecMeta
  open FLINT (* Lty LtyKernel *)
in

  type tcode = int
  type rtype = FLINT.lexp

val debugging = FLINT_Control.rtdebugging

fun bug s = ErrorMsg.impossible ("RuntimeType: " ^ s)
fun say (s : string) = Control.Print.say s

fun debugmsg(m) = if !debugging then (say m; say "\n") else ()
fun ppTyc tc =
    PrettyPrint.with_default_pp (fn ppstrm => PPLty.ppTycEnv 20 ppstrm tc)

fun mkv _ = LV.mkLvar()
val ident = fn le => le
val fkfun = {isrec=NONE, known=false, inline=FR.IH_ALWAYS, cconv=FR.CC_FUN LD.ffc_fixed}
val fkfct = {isrec=NONE, known=false, inline=FR.IH_SAFE, cconv=FR.CC_FCT}

fun mkarw(ts1, ts2) = LK.tcc_arrow(LD.ffc_fixed, ts1, ts2)

val lt_arw = LD.ltc_tyc o LK.tcc_arrow
fun wty tc = (NONE, PO.WRAP, lt_arw(LD.ffc_fixed, [tc], [LB.tcc_void]), [])
fun uwty tc = (NONE, PO.UNWRAP, lt_arw(LD.ffc_fixed, [LB.tcc_void], [tc]), [])

fun FU_WRAP(tc, vs, v, e) = PRIMOP(wty tc, vs, v, e)
fun FU_UNWRAP(tc, vs, v, e) = PRIMOP(uwty tc, vs, v, e)
val FU_rk_tuple = FlintUtil.rk_tuple

fun WRAP(t, u) =
  let val v = mkv()
   in FU_WRAP(t, [u], v, RET[VAR v])
  end

fun UNWRAP(t, u) =
  let val v = mkv()
   in FU_UNWRAP(t, [u], v, RET[VAR v])
  end

(****************************************************************************
 *                  UTILITY FUNCTIONS AND CONSTANTS                         *
 ****************************************************************************)
fun split(RET [v]) = (v, ident)
  | split x = let val v = mkv()
               in (VAR v, fn z => LET([v], x, z))
              end

fun SELECTg(i, e) =
  let val _ = debugmsg ">>SELECTg"
      val _ = if !debugging then PrintFlint.printLexp e else ()
      val (v, hdr) = split e
      val x = mkv()
      val res = hdr(SELECT(v, i, x, RET [VAR x]))
      val _ = if !debugging then PrintFlint.printLexp res else ()
      val _ = debugmsg "<<SELECTg"
   in res
  end

fun FNg(vts, e) =
  let val f = mkv()
   in FIX([(fkfun, f, vts, e)], RET[VAR f])
  end

fun SELECTv(i, u) =
  let val x = mkv()
   in SELECT(u, i, x, RET [VAR x])
  end

fun APPg(e1, e2) =
  let val (v1, h1) = split e1
      val (v2, h2) = split e2
   in h1(h2(APP(v1, [v2])))
  end

fun RECORDg es =
  let fun f ([], vs, hdr) =
               let val x = mkv()
                in hdr(RECORD(FU_rk_tuple, rev vs, x, RET[VAR x]))
               end
        | f (e::r, vs, hdr) =
              let val (v, h) = split e
               in f(r, v::vs, hdr o h)
              end
   in f(es, [], ident)
  end

fun SRECORDg es =
  let fun f ([], vs, hdr) =
               let val _ = debugmsg "<<SRECORDg base case"
		   val x = mkv()
                in hdr(RECORD(FR.RK_STRUCT, rev vs, x, RET[VAR x]))
               end
        | f (e::r, vs, hdr) =
              let val _ = debugmsg "--SRECORD g"
		  val _ = if !debugging then PrintFlint.printLexp e else ()
		  val (v, h) = split e
               in f(r, v::vs, hdr o h)
              end
      val _ = debugmsg ">>SRECORDg"
   in f(es, [], ident)
  end

fun WRAPg (z, b, e) =
  let val (v, h) = split e
   in h(WRAP(z, v))
  end

fun UNWRAPg (z, b, e) =
  let val (v, h) = split e
   in h(UNWRAP(z, v))
  end

fun WRAPcast (z, b, e) =
  let val (v, h) = split e
      val pt = LD.ltc_arrow(LD.ffc_fixed, [LD.ltc_tyc z], [LB.ltc_void])
      val pv = (NONE,PO.CAST,pt,[])
      val x = mkv()
   in h(PRIMOP(pv, [v], x, RET[VAR x]))
  end

fun UNWRAPcast (z, b, e) =
  let val (v, h) = split e
      val pt = LD.ltc_arrow(LD.ffc_fixed, [LB.ltc_void], [LD.ltc_tyc z])
      val pv = (NONE,PO.CAST,pt,[])
      val x = mkv()
   in h(PRIMOP(pv, [v], x, RET[VAR x]))
  end

fun SWITCHg (e, s, ce, d) =
  let val (v, h) = split e
   in h(SWITCH(v, s, ce, d))
  end

fun COND(u,e1,e2) = u(e1,e2)

fun WRAP(t, u) =
  let val v = mkv()
   in FU_WRAP(t, [u], v, RET[VAR v])
  end

fun UNWRAP(t, u) =
  let val v = mkv()
   in FU_UNWRAP(t, [u], v, RET[VAR v])
  end


  val intty = LB.ltc_int
  val boolty = (* LE.ltc_bool *) LB.ltc_void
  val inteqty = LD.ltc_arrow(LD.ffc_fixed, [intty, intty], [boolty])
  val intopty = LD.ltc_arrow(LD.ffc_fixed, [intty, intty], [intty])
  val ieqprim = (NONE, PrimopUtil.IEQL, inteqty, [])
  val iaddprim = (NONE, PrimopUtil.IADD, intopty, [])
  fun ieqLexp (e1, e2) =
      let val (v1, h1) = split e1
	  val (v2, h2) = split e2
      in fn (te, fe) => h1(h2(BRANCH(ieqprim, [v1,v2], te, fe)))
      end
  fun iaddLexp (e1, e2) =
      let val (v1, h1) = split e1
	  val (v2, h2) = split e2
	  val x = mkv ()
      in h1(h2(PRIMOP(iaddprim, [v1,v2], x, RET[VAR x])))
      end


  val tcode_void = 0
  val tcode_record = 1
  val tcode_pair = 2
  val tcode_fpair = 3
  val tcode_real = 4
  fun tcode_realN n = n * 4


  fun tovalue i = FLINT.INT{ival = IntInf.fromInt i, ty = Target.defaultIntSz}
  val tolexp = fn tcode => RET[tovalue tcode]
  val tcode_void   : lexp = tolexp tcode_void
  val tcode_record : lexp = tolexp tcode_record
  val tcode_pair   : lexp = tolexp tcode_pair
  val tcode_fpair  : lexp = tolexp tcode_fpair
  val tcode_real   : lexp = tolexp tcode_real
  val tcode_realN  : int -> lexp = fn i => tolexp (tcode_realN i)

  datatype outcome
    = YES
    | NO
    | MAYBE of lexp

(****************************************************************************
 *                           KIND ENVIRONMENTS                              *
 ****************************************************************************)

type kenv = (LV.lvar list * LT.tkind list) list

val initKE = []
fun addKE(kenv, vs, ks) = (vs,ks)::kenv
fun vlookKE(kenv, i, j) =
  let val (vs,_) = (List.nth(kenv, i-1)
		     handle _ => bug "unexpected case1 in vlookKE")
   in ((List.nth(vs, j) handle _ => bug "unexpected case2 in vlookKE"))
  end

fun klookKE(kenv, i, j) =
  let val (_,ks) = (List.nth(kenv, i-1)
		     handle _ => bug "unexpected case1 in klookKE")
   in ((List.nth(ks, j) handle _ => bug "unexpected case2 in klookKE"))
  end


(* val tkAbsGen : kenv * lvar list * LT.tkind list * lvar * fkind
                  -> kenv * ((lexp *lexp) -> lexp) *)
fun tkAbsGen (kenv, vs, ks, f, fk) =
  let val mkArgTy = case fk of {cconv=FR.CC_FUN _,...} => LD.ltc_tuple
                             | {cconv=FR.CC_FCT,...} => LD.ltc_str
      val argt = mkArgTy (map LE.tk_lty ks)

      val w = mkv()
      fun h([], i, base) = base
	| h(v::r, i, base) = h(r, i+1, SELECT(VAR w, i, v, base))

      fun hdr (e1, e2) = FIX([(fk, f, [(w, argt)], h(vs,0,e1))], e2)
   in (addKE(kenv, vs, ks), hdr)
  end

(* val tkAbs: kenv * (tvar * LT.tkind) list -> kenv * (lexp * lexp -> lexp) *)
fun tkAbs (kenv, tvks, f) =
  let val (vs, ks) = ListPair.unzip tvks
   in tkAbsGen(kenv, vs, ks, f, fkfct)
  end

(* val tkTfn: kenv * LT.tkind list -> kenv * (lexp -> lexp) *)
fun tkTfn (kenv, ks) =
  let val vs = map (fn _ => mkv ()) ks
      val f = mkv()
      val (nkenv, hdr) = tkAbsGen(kenv, vs, ks, f, fkfun)
   in (nkenv, fn e => hdr(e, RET[VAR f]))
  end


(* rtLexp maps LT.TC_VAR to proper lvars, LT.TC_PRIM to proper constants *)
(* val rtLexp : kenv -> tyc -> rtype *)

fun rtLexp (kenv : kenv) (tc : LT.tyc) =
  let val _ = (debugmsg ">>rtLexp";
	       if !debugging then debugmsg(LB.tc_print tc)
				  else ())
      fun loop (x : LT.tyc) =
	(case (LK.tc_whnm_out x)
	  of (LT.TC_FN(ks, tx)) =>
		let val (nenv, hdr) = tkTfn(kenv, ks)
		 in hdr(rtLexp nenv tx)
		end
	   | (LT.TC_APP(tx, ts)) =>
	       (debugmsg ">>rtLexp TC_APP";
		if !debugging
		then (debugmsg(LB.tc_print tx); debugmsg "\n";
		     app (fn tx => (debugmsg(LB.tc_print tx); debugmsg ", ")) ts)
		else ();
		(case LK.tc_whnm_out tx
		  of (LT.TC_APP _ | LT.TC_PROJ _ | LT.TC_VAR _ | LT.TC_NVAR _) =>
			APPg(loop tx, tcsLexp(kenv, ts))
		   | _ => (debugmsg "--rtLexp LT.TC_APP void!!"; tcode_void))
		(* [GK 5/2/07] This looks very, very wrong. If we have any
		   malformed LT.TC_APP, we get void; should it be a bug? *)
		before debugmsg "<<rtLexp LT.TC_APP")
	   | (LT.TC_SEQ ts) => tcsLexp(kenv, ts)
	   | (LT.TC_PROJ(tx, i)) => (debugmsg ">>rtLexp LT.TC_PROJ: ";
				  if !debugging then debugmsg(LB.tc_print tx)
				  else ();
				  SELECTg(i, loop tx)
				  before debugmsg "<<rtLexp LT.TC_PROJ")
	   | (LT.TC_PRIM pt) =>
		if (pt = PT.ptc_real) then tcode_real
		else tcode_void
	   | (LT.TC_VAR(i, j)) => RET[(VAR(vlookKE(kenv, i, j)))]
	   | (LT.TC_TUPLE [t1,t2]) =>
	       (debugmsg ">>rtLexp LT.TC_TUPLE";
		(case (isFloat(kenv,t1), isFloat(kenv,t2))
		  of (YES, YES) => tcode_fpair
		   | ((NO, _) | (_, NO)) => tcode_pair
		   | ((MAYBE e, YES) | (YES, MAYBE e)) =>
			let val test = ieqLexp(e, tcode_real)
			 in COND(test, tcode_fpair, tcode_pair)
			end
		   | (MAYBE e1, MAYBE e2) =>
			let val e = iaddLexp(e1, e2)
			    val test = ieqLexp(e, tcode_realN 2)
			 in COND(test, tcode_fpair, tcode_pair)
			end) before
		debugmsg "<<rtLexp LT.TC_TUPLE")
	   | (LT.TC_TUPLE []) => tcode_void
	   | (LT.TC_TUPLE ts) => tcode_record
	   | (LT.TC_ARROW (_,tc1,tc2)) => tcode_void
	   | (LT.TC_WRAP tx) => loop tx
	   | (LT.TC_FIX{family={size=n,gen=tx,params=ts,...}, index=i}) =>
		let val ntx =
                      (case ts
                        of [] => tx
                         | _ =>
                            (case LK.tc_whnm_out tx
                              of LT.TC_FN(_, x) => x
                               | _ => bug "unexpected FIX 333 in rtLexp-loop"))
                    val tk =
		     (case LK.tc_whnm_out ntx
		       of LT.TC_FN (ks, _) => List.nth(ks, i)
			| _ => bug "unexpected FIX tycs in rtLexp-loop")
		 in case LT.tk_out tk
		     of LT.TK_FUN(ks, _) =>
			  (let val (_, hdr) = tkTfn(kenv, ks)
			    in hdr(tcode_void)
			   end)
		      | _ => tcode_void
		end
	   | (LT.TC_SUM _) => bug "unexpected LT.TC_SUM tyc in rtLexp-loop"
	   | (LT.TC_ENV _) => bug "unexpected LT.TC_ENV tyc in rtLexp-loop"
	   | (LT.TC_CONT _) => bug "unexpected LT.TC_CONT tyc in rtLexp-loop"
	   | (LT.TC_IND _) => bug "unexpected LT.TC_IND tyc in rtLexp-loop"
	   | (LT.TC_NVAR v) => RET[VAR v]
	   |  _ => bug "unexpected tyc in rtLexp-loop")
   in loop tc
  end (* function rtLexp *)

and tcsLexp (kenv, ts) =
  let fun h tc = rtLexp kenv tc
   in RECORDg(map h ts)
  end (* function tcsLexp *)

and tsLexp (kenv, ts) =
  let val _ = (debugmsg ">>tsLexp";
	       if !debugging then app (fn tc => (debugmsg(LB.tc_print tc); debugmsg "\n")) ts
	       else ())
      fun h tc = rtLexp kenv tc
   in SRECORDg(map h ts)
  end (* function tsLexp *)

and isFloat (kenv, tc) =
  let fun loop x =
	(case (LK.tc_whnm_out x)
	  of (LT.TC_PRIM pt) =>
		if (pt = PT.ptc_real) then YES else NO
	   | (LT.TC_TUPLE ts) => NO
	   | (LT.TC_ARROW (_,tc1,tc2)) => NO
	   | (LT.TC_WRAP tx) => loop tx
	   | (LT.TC_FIX _) => NO
	   | (LT.TC_APP(tx, _)) =>
		(case LK.tc_whnm_out tx
		  of (LT.TC_APP _ | LT.TC_PROJ _ | LT.TC_VAR _) =>
		       MAYBE(rtLexp kenv x)
		   | _ => NO)
	   | (LT.TC_VAR(i,j)) =>
		let val k = klookKE(kenv, i, j)
		 in case (LT.tk_out k)
		     of LT.TK_BOX => NO
		      | _ => MAYBE(rtLexp kenv x)
		end
	   | _ => MAYBE(rtLexp kenv x))

   in loop tc
  end

fun isPair (kenv, tc) =
  let fun loop x =
	(case (LK.tc_whnm_out x)
	  of (LT.TC_PRIM pt) => NO
	   | (LT.TC_TUPLE [_,_]) => YES
	   | (LT.TC_TUPLE _) => NO
	   | (LT.TC_ARROW _) => NO
	   | (LT.TC_WRAP tx) => loop tx
	   | (LT.TC_FIX _) => NO
	   | (LT.TC_APP(tx, _)) =>
		(case LK.tc_whnm_out tx
		  of (LT.TC_APP _ | LT.TC_PROJ _ | LT.TC_VAR _ | LT.TC_NVAR _) =>
		       MAYBE(rtLexp kenv x)
		   | _ => NO)
	   | _ => MAYBE(rtLexp kenv x))

   in loop tc
  end

end (* local *)
end (* structure RuntimeType *)

