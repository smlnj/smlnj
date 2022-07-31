(* typeoper.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature TYPEOPER =
sig
  type kenv

  val initKE : kenv

  val tkAbs  : kenv * (Lty.tvar * Lty.tkind) list * LambdaVar.lvar ->
                  (kenv * (FLINT.lexp * FLINT.lexp -> FLINT.lexp))
  val tcLexp : kenv -> Lty.tyc -> FLINT.lexp
  val tsLexp : kenv * Lty.tyc list -> FLINT.lexp

  val utgc   : Lty.tyc * kenv * Lty.tyc -> FLINT.value -> FLINT.lexp
  val utgd   : Lty.tyc * kenv * Lty.tyc -> FLINT.value -> FLINT.lexp
  val tgdc   : int * Lty.tyc * kenv * Lty.tyc -> FLINT.value -> FLINT.lexp
  val tgdd   : int * Lty.tyc * kenv * Lty.tyc -> FLINT.value -> FLINT.lexp

  val mkwrp  : Lty.tyc * kenv * bool * Lty.tyc -> FLINT.lexp -> FLINT.lexp
  val mkuwp  : Lty.tyc * kenv * bool * Lty.tyc -> FLINT.lexp -> FLINT.lexp

end (* signature TYPEOPER *)

structure TypeOper : TYPEOPER =
struct

local
  structure DI = DebIndex
  structure LT = Lty
  structure FR = FunRecMeta
  structure LK = LtyKernel
  structure LD = LtyDef
  structure LB = LtyBasic
  structure LE = LtyExtern
  structure LV = LambdaVar
  structure PO = Primop
  structure PT = PrimTyc
  structure BT = BasicTypes
  structure TP = Types
  structure F  = FLINT
  structure RT = RuntimeType
  open FLINT
in

fun bug s = ErrorMsg.impossible ("TypeOper: " ^ s)
fun say (s : string) = Control.Print.say s
fun mkv _ = LV.mkLvar()
val ident = fn le => le

type kenv = RT.kenv

datatype outcome = datatype RuntimeType.outcome

val fkfun = {isrec=NONE, known=false, inline=FR.IH_ALWAYS, cconv=FR.CC_FUN LD.ffc_fixed}

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
  let val (v, hdr) = split e
      val x = mkv()
   in hdr(SELECT(v, i, x, RET [VAR x]))
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
               let val x = mkv()
                in hdr(RECORD(FR.RK_STRUCT, rev vs, x, RET[VAR x]))
               end
        | f (e::r, vs, hdr) =
              let val (v, h) = split e
               in f(r, v::vs, hdr o h)
              end
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


(****************************************************************************
 *                           KIND ENVIRONMENTS                              *
 ****************************************************************************)

fun addKE(kenv, vs, ks) = RT.addKE


(****************************************************************************
 *                            MAIN FUNCTIONS                                *
 ****************************************************************************)

(* val tkAbsGen : kenv * LV.lvar list * LT.tkind list * LV.lvar * FR.fkind
                  -> kenv * ((F.lexp * F.lexp) -> F.lexp) *)
(* val tkAbsGen = RT.tkAbsGen *)


(* val tkAbs: kenv * (LT.tvar * LT.tkind) list -> kenv * (F.lexp * F.lexp -> F.lexp) *)
val tkAbs = RT.tkAbs

(* val tkTfn: kenv * LT.tkind list -> kenv * (F.lexp -> F.lexp) *)
val tkTfn = RT.tkTfn

val ieqLexp = RT.ieqLexp

val iaddLexp = RT.iaddLexp


val tovalue = RT.tovalue
val tcode_void = RT.tcode_void
val tcode_record = RT.tcode_record
val tcode_pair = RT.tcode_pair
val tcode_fpair = RT.tcode_fpair
val tcode_real = RT.tcode_real
val tcode_realN = RT.tcode_realN


(* tcLexp maps LT.TC_VAR to proper lvars, LT.TC_PRIM to proper constants *)
(* val tcLexp : kenv -> tyc -> lexp *)

val initKE = RT.initKE

val tcLexp = RT.rtLexp
val tsLexp = RT.tsLexp

val isFloat  = RT.isFloat

val isPair = RT.isPair

fun tagInt i = INT{ival = IntInf.fromInt i, ty = Target.defaultIntSz}

(****************************************************************************
 *                      TYPED INTERPRETATION OF UNTAGGED                    *
 ****************************************************************************)

(** tc is of kind Omega; this function tests whether tc can be a tagged int ? *)
fun tcTag (kenv, tc) =
  let fun loop x =     (* a lot of approximations in this function *)
	(case (LK.tc_whnm_out x)
	  of (LT.TC_PRIM pt) => if PT.unboxed pt then NO else YES
                (* if PT.ubxupd pt then YES else NO *)
		    (* this is just an approximation *)
	   | (LT.TC_TUPLE []) => YES
	   | (LT.TC_TUPLE ts) => NO
	   | (LT.TC_ARROW (_,tc1,tc2)) => YES (* NO *)
	   | (LT.TC_WRAP tx) => loop tx
	   | (LT.TC_FIX _) => YES
	   | (LT.TC_APP(tx, _)) =>
		(case LK.tc_whnm_out tx
		  of (LT.TC_APP _ | LT.TC_PROJ _ | LT.TC_VAR _) =>
		       MAYBE (tcLexp kenv x)
		   | _ => YES)
	   | _ => (MAYBE (tcLexp kenv x)))
   in loop tc
  end (* function tcTag *)

(* val utgc : tyc * kenv * tyc -> value -> lexp *)
fun utgc (tc, kenv, rt) =
  (case tcTag(kenv, tc)
    of YES => (fn u => let val v = mkv()
                        in RECORD(FU_rk_tuple, [u], v,
                             WRAP(LD.tcc_tuple[rt], VAR v))
                       end)
     | NO => (fn u => WRAP(rt, u))
     | MAYBE ne =>
	 (fn u => let val v = mkv()
                      val hh = ieqLexp(ne, tcode_void)
                   in COND(hh, RECORD(FU_rk_tuple, [u], v,
                                      WRAP(LD.tcc_tuple[rt], VAR v)),
                               WRAP(rt, u))
           	  end))

(* val utgd : tyc * kenv * tyc -> value -> lexp *)
fun utgd (tc, kenv, rt) =
  (case tcTag(kenv, tc)
    of YES => (fn u => let val v = mkv() and z = mkv()
                        in FU_UNWRAP(LD.tcc_tuple [rt], [u], v,
                               SELECT(VAR v, 0, z, RET[VAR z]))
                       end)
     | NO => (fn u => UNWRAP(rt, u))
     | MAYBE ne =>
          (fn u => let val v = mkv() and z = mkv()
                       val hh = ieqLexp(ne, tcode_void)
                    in COND(hh, FU_UNWRAP(LD.tcc_tuple [rt], [u], v,
                               SELECT(VAR v, 0, z, RET[VAR z])),
                            UNWRAP(rt, u))
                   end))

(* val tgdc : int * tyc * kenv * tyc -> value -> lexp *)
fun tgdc (i, tc, kenv, rt) =
  let val nt = LD.tcc_tuple [LB.tcc_int, rt]
   in fn u => let val x = mkv()
               in RECORD(FU_rk_tuple, [tagInt i, u], x, WRAP(nt, VAR x))
              end
  end

(* val tgdd : int * tyc * kenv * tyc -> value -> lexp *)
fun tgdd (i, tc, kenv, rt) =
  let val nt = LD.tcc_tuple [LB.tcc_int, rt]
   in fn u => let val x = mkv() and v = mkv()
               in FU_UNWRAP(nt, [u], x, SELECT(VAR x, 1, v, RET[VAR v]))
              end
  end

(****************************************************************************
 *                      TYPED INTERPRETATION OF FP RECORD                   *
 ****************************************************************************)
(** tc is a ground tyc of kind Omega, only record types and arrow types are
    interesting for the time being. *)
(** all of these wrappers probably should be lifted to the top of the
    program, otherwise we may run into space blow-up ! *)
(* val tcCoerce : kenv * tyc * bool * bool -> (lexp -> lexp) option *)
fun tcCoerce (kenv, tc, nt, wflag, b) =
  (case (LK.tc_whnm_out tc, LK.tc_whnm_out nt)
    of (LT.TC_TUPLE ts, _) =>
	 let fun h([], i, e, el, 0) = NONE
	       | h([], i, e, el, res) =
		   let val w = mkv()
		       val wx = VAR w
		       fun g(i, NONE) =  SELECTv(i, wx)
			 | g(i, SOME _) =
			     if wflag then
			       UNWRAPg(LB.tcc_real, b, SELECTv(i, wx))
			     else WRAPg(LB.tcc_real, b, SELECTv(i, wx))

		       val ntc = LD.tcc_tuple(map (fn _ => LB.tcc_real) ts)

		       val ne = RECORDg (map g (rev el))
		       val test = ieqLexp(e, tcode_realN res)

		       fun hdr0 xe =
			 if wflag then
			   COND(test, LET([w], xe, WRAPcast(ntc, b, ne)),
				      WRAPcast(nt, b, xe))
			 else COND(test, LET([w], UNWRAPcast(ntc, b, xe), ne),
					 UNWRAPcast(nt, b, xe))

		       fun hdr (xe as (RET[(VAR _)])) = hdr0 xe
			 | hdr xe = let val z = mkv()
				     in LET([z], xe, hdr0 (RET[VAR z]))
				    end
		    in SOME hdr
		   end
	       | h(a::r, i, e, el, res) =
		   (case isFloat(kenv, a)
		     of NO => NONE
		      | YES => h(r, i+1, e, (i,NONE)::el, res)
		      | MAYBE z => h(r, i+1, iaddLexp(e, z),
				     (i, SOME a)::el, res+1))

	  in h(ts, 0, RET[tagInt 0], [], 0)
	 end
     | (LT.TC_ARROW _, _) => (* (tc1, tc2) => *)
        let val (tc1, _) = LD.tcd_parrow tc
            val (_, tc2) = LD.tcd_parrow nt
         in (case isPair(kenv, tc1)
              of (YES | NO) => NONE
               | (MAYBE e) =>
                 let val w = mkv()
                     val test1 = ieqLexp(RET[(VAR w)], tcode_pair)
                     val test2 = ieqLexp(RET[(VAR w)], tcode_fpair)
                     val m = mkv() and m2 = mkv()
                     val n = mkv() and n2 = mkv()

                     val tc_real = LB.tcc_real
                     val tc_breal = LB.tcc_void (* LT.tcc_wrap tc_real *)
                     val lt_breal = LD.ltc_tyc tc_breal
                     val tc_void = LB.tcc_void
                     val lt_void = LB.ltc_void
                     val tc_pair = LD.tcc_tuple [tc_void, tc_void]
                     val tc_fpair = LD.tcc_tuple [tc_real, tc_real]
                     val tc_bfpair = LD.tcc_tuple [tc_breal, tc_breal]
                     val lt_pair = LD.ltc_tyc tc_pair
                     val lt_fpair = LD.ltc_tyc tc_fpair
                     val lt_bfpair = LD.ltc_tyc tc_bfpair
                     val ident = fn le => le

                     val (argt1, body1, hh1) =
                       if wflag then (* wrapping *)
                         ([(m,lt_void),(m2,lt_void)],
                          fn sv =>
                            let val xx = mkv() and yy = mkv()
                             in RECORD(FU_rk_tuple, [VAR m, VAR m2], xx,
                                  FU_WRAP(tc_pair, [VAR xx], yy,
                                    APP(sv, [VAR yy])))
                            end,
                          fn le =>
                            WRAPcast(mkarw([tc_void,tc_void],[tc2]),
                                     true, le))
                       else (* unwrapping *)
                         let val x = mkv() and y = mkv() and z = mkv()
                          in ([(m, lt_void)],
                              fn sv =>
                                let val xx = mkv()
                                 in LET([xx],
                                      UNWRAPcast(
                                         mkarw([tc_void, tc_void], [tc2]),
                                              true, RET[sv]),
                                        FU_UNWRAP(tc_pair, [VAR m], x,
                                         SELECT(VAR x, 0, y,
                                         SELECT(VAR x, 1, z,
                                          APP(VAR xx, [VAR y, VAR z])))))
                                end,
                             ident)
                         end

                     val (argt2, body2, hh2) =
                       if wflag then  (* wrapping *)
                         ([(n,lt_breal),(n2,lt_breal)],
                          fn sv =>
                            let val xx = mkv() and yy = mkv()
                             in LET ([xx],
                                   RECORDg [UNWRAP(tc_real, VAR n),
                                            UNWRAP(tc_real, VAR n2)],
                                FU_WRAP(tc_fpair, [VAR xx], yy,
                                   APP(sv, [VAR yy])))
                            end,
                          fn le => WRAPcast(mkarw([tc_breal,tc_breal],[tc2]),
                                            true, le))
                       else  (* unwrapping *)
                         let val x = mkv() and y = mkv() and z = mkv()
                             val q0 = mkv() and q1 = mkv()
                          in ([(n, lt_void)],
                              fn sv =>
                                let val xx = mkv()
                                 in LET([xx],
                                      UNWRAPcast(
                                         mkarw([tc_breal, tc_breal], [tc2]),
                                            true, RET[sv]),
                                      FU_UNWRAP(tc_fpair, [VAR n], x,
                                        SELECT(VAR x, 0, y,
                                          FU_WRAP(tc_real, [VAR y], q0,
                                        SELECT(VAR x, 1, z,
                                          FU_WRAP(tc_real, [VAR z], q1,
                                         APP(VAR xx, [VAR q0, VAR q1])))))))
                                end,
                            ident)
                         end

                     val hh3 = if wflag then fn le => WRAPcast(nt, true, le)
                               else fn le => UNWRAPcast(nt, true, le)

                     (*** NEEDS MORE WORK TO DO THE RIGHT COERCIONS ***)
                     fun hdr0(sv) =
                       LET([w], e,
                         COND(test1, hh1(FNg(argt1, body1 sv)),
                           COND(test2, hh2(FNg(argt2, body2 sv)),
                                hh3(RET[sv]))))

                     fun hdr (xe as RET [sv]) = hdr0 sv
                       | hdr xe = let val z = mkv()
                                   in LET([z], xe, hdr0(VAR z))
                                  end
                  in SOME hdr
                 end)
        end
     | _ => NONE)

(* val mkwrp : tyc * kenv * bool * tyc -> lexp -> lexp *)
fun mkwrp (tc, kenv, b, nt) =
  (case tcCoerce(kenv, tc, nt, true, b)
    of NONE => (fn le => WRAPg(nt, b, le))
     | SOME hdr => hdr)

(* val mkuwp  : tyc * kenv * bool * tyc -> lexp -> lexp *)
fun mkuwp (tc, kenv, b, nt) =
  (case tcCoerce(kenv, tc, nt, false, b)
    of NONE => (fn le => UNWRAPg(nt, b, le))
     | SOME hdr => hdr)

end (* toplevel local *)
end (* structure TypeOper *)

