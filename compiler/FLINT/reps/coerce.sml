(* Copyright 1998 YALE FLINT PROJECT *)
(* coerce.sml *)

signature COERCE = sig

  type wpEnv
  val initWpEnv: unit -> wpEnv
  val wpNew    : wpEnv * DebIndex.depth -> wpEnv
  val wpBuild  : wpEnv * FLINT.lexp -> FLINT.lexp

  val unwrapOp : wpEnv * Lty.lty list * Lty.lty list * DebIndex.depth
                   -> (FLINT.value list -> FLINT.lexp) option

  val wrapOp   : wpEnv * Lty.lty list * Lty.lty list * DebIndex.depth
                   -> (FLINT.value list -> FLINT.lexp) option

end (* signature COERCE *)

structure Coerce : COERCE =
struct

local
  structure DI = DebIndex
  structure LV = LambdaVar
  structure PF = PFlatten
  structure FU = FlintUtil
  structure LT = Lty
  structure FR = FunRecMeta
  structure LK = LtyKernel
  structure LD = LtyDef
  structure LB = LtyBasic
  structure LE = LtyExtern
  open Lty LtyKernel FLINT
in

(****************************************************************************
 *                  UTILITY FUNCTIONS AND CONSTANTS                         *
 ****************************************************************************)

fun bug s = ErrorMsg.impossible ("Coerce: " ^ s)
fun say (s : string) = Control.Print.say s

fun mkv _ = LV.mkLvar ()
val ident = fn le => le
val fkfun = {isrec=NONE, known=false, inline=FR.IH_ALWAYS, cconv=FR.CC_FUN LD.ffc_fixed}
val fkfct = {isrec=NONE, known=false, inline=FR.IH_SAFE, cconv=FR.CC_FCT}
val tfk = {inline=FR.IH_ALWAYS}
fun fromto(i,j) = if i < j then (i::fromto(i+1,j)) else []

fun opList (NONE :: r) = opList r
  | opList ((SOME _) :: r) = true
  | opList [] = false

fun WRAP(t, u, kont) =
  let val v = mkv()
   in FU.WRAP(t, [u], v, kont(VAR v))
  end

fun UNWRAP(t, u, kont) =
  let val v = mkv()
   in FU.UNWRAP(t, [u], v, kont (VAR v))
  end

fun RETv (v) = RET [v]

(****************************************************************************
 *              WRAPPER CACHES AND WRAPPER ENVIRONMENTS                     *
 ****************************************************************************)
type hdr = value -> lexp
type hdrOp = hdr option

type wpCache = (lty * hdrOp) list IntRedBlackMap.map
type wpEnv = (fundec list ref * wpCache ref) list

val initWpCache : wpCache = IntRedBlackMap.empty
fun initWpEnv () = [(ref [], ref initWpCache)]

fun wcEnter([], t, x) = bug "unexpected wenv in wcEnter"
  | wcEnter((_, z as ref m)::_, t, x) =
      let val h = lt_key t
       in z :=
	    IntRedBlackMap.insert
	      (m, h, (t,x)::(Option.getOpt(IntRedBlackMap.find(m,h), nil)))
      end

fun wcLook([], t) = bug "unexpected wenv in wcLook"
  | wcLook((_, z as ref m)::_, t) =
       let fun loop((t',x)::rest) = if lt_eqv(t,t') then SOME x else loop rest
             | loop [] = NONE
	in
	  case IntRedBlackMap.find(m, lt_key t)
	    of SOME x => loop x
             | NONE => NONE
        end

fun wpNew(wpEnv, d) =
  let val od = length wpEnv
      val _ = (* sanity check *)
        if (d+1 = od) then ()
        else bug "inconsistent state in wpNew"
   in (ref [], ref initWpCache)::wpEnv
  end

fun wpBuild ([], base) = base
  | wpBuild ((wref,_)::_, base) =
      foldl (fn (fd, b) => FIX([fd], b)) base (!wref)

fun addWrappers(wenv, p, d) =
  let (** the d value is ignored now but we may use it in the future *)
      val (wref, _) = (hd wenv (* (List.nth(wenv, d)) *)
                       handle _ => bug "unexpected cases in addWrappers")
   in (wref := (p::(!wref)))
  end

(* appWraps : hdrOp list * value list * (value list -> lexp) -> lexp *)
fun appWraps (wps, vs, cont) =
  let fun g (NONE::ws, v::vs, hdr, nvs) = g(ws, vs, hdr, v::nvs)
        | g ((SOME f)::ws, v::vs, hdr, nvs) =
              let val nv = mkv()
               in g(ws, vs, fn le => hdr(LET([nv], f v, le)), (VAR nv)::nvs)
              end
        | g ([], [], hdr, nvs) = hdr(cont(rev nvs))
        | g _ = bug "unexpected cases in appWraps"
   in g(wps, vs, ident, [])
  end (* function appWraps *)

(* appWrapsWithFiller does the same thing as appWraps, except that
 * it also fills in proper coercions when there are mismatches between
 * the original values. Occurs strictly only for TC_ARROW case. The
 * filler is generated by the PFlatten.v_coerce function.
 *
 * The boolean flag indicates if the filler should be applied before
 * wrapping or after wrapping.
 *
 * appWrapsWithFiller:
 *   bool -> {filler: (value list -> (value list * (lexp -> lexp))) option,
 *            wps: hdrOp list, args: value list, cont: (value list -> lex)}
 *        -> lexp
 *)
fun appWrapsWithFiller before_wrap {filler=NONE, wps, args, cont} =
      appWraps(wps, args, cont)

  | appWrapsWithFiller before_wrap {filler=SOME ff, wps, args, cont} =
      let val ((nargs, nhdr), ncont) =
            if before_wrap then (ff args, cont)
            else ((args, ident),
                  fn vs => let val (nvs, h) = ff vs
                            in h(cont(nvs))
                           end)
       in nhdr(appWraps(wps, nargs, ncont))
      end (* function appWrapsWithFiller *)

(****************************************************************************
 *                            MAIN FUNCTIONS                                *
 ****************************************************************************)
fun wrapperGen (wflag, sflag) (wenv, nts, ots, d) =
let

val doWrap =
  if sflag then
    (fn (w, fdec) => (addWrappers(wenv, fdec, d);
                      (fn u => APP(VAR w, [u]))))
  else
    (fn (w, fdec) => (fn u => FIX([fdec], APP(VAR w, [u]))))

fun getWTC(wflag, nx, ox, doit) =
  if LK.tc_eqv(nx, ox) then NONE
  else (if sflag then
          (let val mark = if wflag then LB.ltc_int else LB.ltc_real (* hack *)
               val key = LD.ltc_str [LD.ltc_tyc nx, LD.ltc_tyc ox, mark]
            in case wcLook(wenv, key)
                of SOME x => x
                 | NONE => (let val res = doit (tc_out nx, tc_out ox)
                             in wcEnter(wenv, key, res); res
                            end)
           end)
        else doit (tc_out nx, tc_out ox))

fun getWLT(wflag, nx, ox, doit) =
  if lt_eqv(nx, ox) then NONE
  else (if sflag then  (*** we could always force the sharing here ***)
          (let val mark = if wflag then LB.ltc_int else LB.ltc_real (* hack *)
               val key = LD.ltc_str [nx, ox, mark]
            in case wcLook(wenv, key)
                of SOME x => x
                 | NONE => (let val res = doit (lt_out nx, lt_out ox)
                             in wcEnter(wenv, key, res); res
                            end)
           end)
        else doit (lt_out nx, lt_out ox))

fun tcLoop wflag (nx, ox) =
  getWTC(wflag, nx, ox,
   (fn (TC_WRAP nz, _) => (* sanity check: tcc_wrap(ox) = nx *)
          if LD.tcp_wrap nx then
              let val (ax, act) = if wflag then (ox, WRAP) else (nx, UNWRAP)
               in if LD.tcp_prim ox then SOME (fn u => act(ox, u, RETv))
                  else let val wp = tcLoop wflag (nz, ox)
                           val f = mkv() and v = mkv()
                           val (tx, kont, u, hdr) =
                             (case wp
                               of NONE => (ox, RETv, VAR v, ident)
                                | SOME hh =>
                                    if wflag then
                                      let val z = mkv()
                                       in (nz, RETv, VAR z,
                                           fn e => LET([z], hh(VAR v), e))
                                      end
                                    else (nz, hh, VAR v, ident))
                           val fdec = (fkfun, f, [(v, LD.ltc_tyc ax)],
                                       hdr(act(tx, u, kont)))
                        in SOME(doWrap(f, fdec))
                       end
              end
          else bug "unexpected TC_WRAP in tcLoop"
     | (TC_TUPLE nxs, TC_TUPLE oxs) =>
          let val wps = ListPair.map (tcLoop wflag) (nxs, oxs)
           in if opList wps then
                let val f = mkv() and v = mkv()
                    val nl = fromto(0, length nxs)
                    val u = VAR v
                    val (nvs, hdr) =  (* take out all the fields *)
                      foldr (fn (i, (z,h)) =>
                              let val x = mkv()
                               in ((VAR x)::z,
                                   fn le => SELECT(u, i, x, h le))
                              end) ([], ident) nl

                    val ax =
                      if wflag then LD.ltc_tyc ox
                      else LD.ltc_tyc nx
                    fun cont nvs =
                      let val z = mkv()
                       in RECORD(FR.RK_TUPLE, nvs, z, RET[VAR z])
                      end
                    val body = hdr(appWraps(wps, nvs, cont))
                    val fdec = (fkfun, f, [(v, ax)], body)
                 in SOME(doWrap(f, fdec))
                end
              else NONE
          end
     | (TC_ARROW (_, nxs1, nxs2), TC_ARROW (_, oxs1, oxs2)) =>
          let val (awflag, rwflag) = (not wflag, wflag) (* polarity *)
              val (oxs1', filler1) = PF.v_coerce (awflag, nxs1, oxs1)
              val wps1 = ListPair.map (tcLoop awflag) (nxs1, oxs1')
              val (oxs2', filler2) = PF.v_coerce (rwflag, nxs2, oxs2)
              val wps2 = ListPair.map (tcLoop rwflag) (nxs2, oxs2')
           in (case (opList wps1, opList wps2, filler1, filler2)
                of (false, false, NONE, NONE) => NONE
                 | _ =>
                    let val wf = mkv() and f = mkv() and rf = mkv()
                        val (ax, rxs1, rxs2) =
                          if wflag then (LD.ltc_tyc ox, nxs1, oxs2)
                          else (LD.ltc_tyc nx, oxs1, nxs2)

                        val params = map (fn t => (mkv(), LD.ltc_tyc t)) rxs1
                        val avs = map (fn (x, _) => VAR x) params
                        val rvs = map mkv rxs2
                        val rbody =
                          LET(rvs,
                              appWrapsWithFiller awflag
                                {filler=filler1, wps=wps1, args=avs,
                                 cont=(fn wvs => APP(VAR f, wvs))},
                              appWrapsWithFiller rwflag
                                {filler=filler2, wps=wps2,
                                 args=map VAR rvs, cont=RET})

                        val rfdec = (fkfun, rf, params, rbody)
                        val body = FIX([rfdec], RET[VAR rf])
                        val fdec = (fkfun, wf, [(f, ax)], body)
                     in SOME (doWrap(wf, fdec))
                    end)
          end
     | (_, _) =>
          if LK.tc_eqv(nx, ox) then NONE
          else (say " Type nx is : \n"; say (LB.tc_print nx);
                say "\n Type ox is : \n"; say (LB.tc_print ox); say "\n";
                bug "unexpected other tycs in tcLoop")))

fun ltLoop wflag (nx, ox) =
  getWLT(wflag, nx, ox,
   (fn (LT_TYC nz, LT_TYC oz) => tcLoop wflag (nz, oz)
     | (LT_STR nxs, LT_STR oxs) =>
          let val wps = ListPair.map (ltLoop wflag) (nxs, oxs)
           in if opList wps then
                let val f = mkv() and v = mkv()
                    val nl = fromto(0, length nxs)
                    val u = VAR v
                    val (nvs, hdr) =  (* take out all the fields *)
                      foldr (fn (i, (z,h)) =>
                              let val x = mkv()
                               in ((VAR x)::z,
                                   fn le => SELECT(u, i, x, h le))
                              end) ([], ident) nl
                    fun cont nvs =
                      let val z = mkv()
                       in RECORD(FR.RK_STRUCT, nvs, z, RET[VAR z])
                      end
                    val body = hdr(appWraps(wps, nvs, cont))
                    val ax = if wflag then ox else nx
                    val fdec = (fkfct, f, [(v, ax)], body)
                 in SOME(doWrap(f, fdec))
                end
              else NONE
          end
     | (LT_FCT (nxs1, nxs2), LT_FCT (oxs1, oxs2)) =>
          let val wps1 = ListPair.map (ltLoop (not wflag)) (nxs1, oxs1)
              val wps2 = ListPair.map (ltLoop wflag) (nxs2, oxs2)
           in (case (opList wps1, opList wps2)
                of (false, false) => NONE
                 | _ =>
                    let val wf = mkv() and f = mkv() and rf = mkv()
                        val (ax, rxs1, rxs2) =
                          if wflag then (ox, nxs1, oxs2) else (nx, oxs1, nxs2)

                        val params = map (fn t => (mkv(), t)) rxs1
                        val avs = map (fn (x, _) => VAR x) params
                        val rvs = map mkv rxs2
                        val rbody =
                          LET(rvs,
                              appWraps(wps1, avs, fn wvs => APP(VAR f, wvs)),
                              appWraps(wps2, map VAR rvs, fn wvs => RET wvs))

                        val rfdec = (fkfct, rf, params, rbody)
                        val body = FIX([rfdec], RET[VAR rf])
                        val fdec = (fkfct, wf, [(f, ax)], body)
                     in SOME (doWrap(wf, fdec))
                    end)
          end
     | (LT_POLY(nks, nzs), LT_POLY(oks, ozs)) =>
          let val nwenv = wpNew(wenv, d)
              val wp = wrapperGen (wflag, sflag) (nwenv, nzs, ozs, DI.next d)
           in (case wp
                of NONE => NONE
                 | SOME (hdr : value list -> lexp) =>
                    let val wf = mkv() and f = mkv() and rf = mkv()
                        val (ax, aks, rxs)  =
                          if wflag then (ox, nks, ozs) else (nx, oks, nzs)
                        val nl = fromto(0, length nks)
                        val ts = map (fn i => LD.tcc_var(DI.innermost, i)) nl
                        val avs = map mkv rxs
                        val rbody =
                          LET(avs, TAPP(VAR f, ts), hdr (map VAR avs))
                        val nrbody = wpBuild(nwenv, rbody)
                        val atvks = map (fn k => (LT.mkTvar(),k)) aks
                        val body = TFN((tfk, rf, atvks, nrbody), RET[VAR rf])
                        val fdec = (fkfct, wf, [(f, ax)], body)
                     in SOME(doWrap(wf, fdec))
                    end)
          end
     | _ =>
          (say " Type nx is : \n"; say (LB.lt_print nx);
           say "\n Type ox is : \n"; say (LB.lt_print ox); say "\n";
           bug "unexpected other ltys in ltLoop")))

val wps = ListPair.map (ltLoop wflag) (nts, ots)

in if opList wps
   then SOME (fn vs => appWraps(wps, vs, RET))
   else NONE
end (* function wrapperGen *)

fun unwrapOp (wenv, nts, ots, d) =
  let val nts' = map lt_norm nts
      val ots' = map lt_norm ots
      val sflag = !Control.FLINT.sharewrap
   in wrapperGen (false, sflag) (wenv, nts', ots', d)
  end (* function unwrapOp *)

fun wrapOp (wenv, nts, ots, d) =
  let val nts' = map lt_norm nts
      val ots' = map lt_norm ots
      val sflag = !Control.FLINT.sharewrap
   in wrapperGen (true, sflag) (wenv, nts', ots', d)
  end (* function wrapOp *)

end (* toplevel local *)
end (* structure Coerce *)
