(* reify.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature REIFY =
sig
  val reify : FLINT.prog -> FLINT.prog
end (* signature REIFY *)

structure Reify : REIFY =
struct

local
  structure LP = TypeOper
  structure LT = Lty
  structure FR = FunRecMeta
  structure LK = LtyKernel
  structure LD = LtyDef
  structure LB = LtyBasic
  structure LE = LtyExtern
  structure LV = LambdaVar
  structure DA = Access
  structure DI = DebIndex
  structure PO = Primop
  structure PL = PLambda
  structure FU = FlintUtil
  open FLINT
in

val debugging = FLINT_Control.redebugging


fun bug s = ErrorMsg.impossible ("Reify: " ^ s)
val say = Control_Print.say
fun debugmsg(m) = if !debugging then say (m^"\n") else ()

val mkv = LambdaVar.mkLvar

fun tagInt i = INT{ival = IntInf.fromInt i, ty = Target.defaultIntSz}

(** a special version of WRAP and UNWRAP for post-reify typechecking *)
val lt_arw = LD.ltc_tyc o LK.tcc_arrow
val lt_vfn = lt_arw(LD.ffc_fixed, [LB.tcc_void], [LB.tcc_void])

fun wty tc = (NONE, PO.WRAP, lt_arw(LD.ffc_fixed, [tc], [LB.tcc_void]), [])
fun uwty tc = (NONE, PO.UNWRAP, lt_arw(LD.ffc_fixed, [LB.tcc_void], [tc]), [])

fun WRAP(tc, vs, v, e) = PRIMOP(wty tc, vs, v, e)
fun UNWRAP(tc, vs, v, e) = PRIMOP(uwty tc, vs, v, e)

(** a major gross hack: use of fct_lty in WCAST primops **)
fun mkWCAST (u, oldt, newt) =
  let val v = mkv()
   in (fn e => PRIMOP((NONE, PO.WCAST, LD.ltc_fct([oldt],[newt]), []),
                      [u], v, e), v)
  end

fun mcastSingle (oldt, newt) =
  if LK.lt_eqv(oldt, newt) then NONE
  else SOME (fn u => mkWCAST(u, oldt, newt))

fun mcast (oldts, newts) =
  let fun f (a::r, b::s, z, flag) =
              (case mcastSingle(a,b)
                of NONE => f(r, s, NONE::z, flag)
                 | x => f(r, s, x::z, false))
        | f ([], [], z, flag) =
              if flag then fn le => le
              else (let val vs = map (fn _ => mkv()) oldts
                        val (hdr, nvs) =
                          let fun g(NONE::xx, v::yy, h, q) =
                                     g(xx, yy, h, (VAR v)::q)
                                | g((SOME vh)::xx, v::yy, h, q) =
                                     let val (h', k) = vh (VAR v)
                                      in g(xx, yy, h o h', (VAR k)::q)
                                     end
                                | g([], [], h, q) = (h, rev q)
                                | g _ = bug "unexpected case in mcast"
                           in g(rev z, vs, Fn.id, [])
                          end
                     in fn e => LET(vs, e, hdr(RET nvs))
                    end)
        | f _ = bug "unexpected case in mcast"
   in f(oldts, newts, [], true)
  end

(****************************************************************************
 * Reify does the following several things:                                 *
 *                                                                          *
 *   (1) Conreps in CON and DECON are given type-specific meanings.         *
 *   (2) Type abstractions TFN are converted into function abstractions;    *
 *   (3) Type applications TAPP are converted into function applications;   *
 *   (4) Type-dependent primops such as WRAP/UNWRAP are given               *
 *       type-specific meanings;                                            *
 *   (5) FLINT is now transformed into a monomorphically typed lambda       *
 *       calculus. Type mismatches are fixed via the use of type cast       *
 ****************************************************************************)
(* reify : fundec -> fundec *)
fun reify fdec =
let val getlty =  Recover.recover (fdec, false)
    val (tycNarrow, ltyNarrow) = LE.typeNarrowGen ()

    fun dcf ((name,rep,lt),ts) = (name,rep,lt_vfn)
    fun dargtyc ((name,rep,lt), ts) =
      let val skt = LE.lt_pinst(lt, map (fn _ => LB.tcc_void) ts)
          val (tc, _) = LD.tcd_parrow (LD.ltd_tyc skt) handle LD.DeconExn => bug "reify in dargtyc"
          val nt = ltyNarrow (LE.lt_pinst(lt, ts))
          val (rt, _) = LD.tcd_parrow (LD.ltd_tyc nt) handle LD.DeconExn => bug "reify in dargtyc 2"
       in (tc, rt, (name,rep,lt_vfn))
      end

    (* transform: kenv * DI.depth -> lexp -> lexp *)
    fun transform (kenv) =
     let (* lpfd: fundec -> fundec *)
         fun lpfd (fk, f, vts, e) =
           let val nfk =
                 case fk
                  of {isrec=SOME (lts,lk), cconv, known, inline} =>
                       {isrec=SOME(map ltyNarrow lts, lk), cconv=cconv,
			known=known, inline=inline}
                   | _ => fk
               val nvts = map (fn (v,t) => (v, ltyNarrow t)) vts
            in (nfk, f, nvts, loop e)
           end

         (* lpcon: con -> con * (lexp -> lexp) *)
         and lpcon (PL.DATAcon(dc as (_, DA.EXN _, nt), [], v)) =
               let val ndc = dcf(dc, []) and z = mkv() and w = mkv()
                   (* WARNING: the 3rd field should (string list) *)
                   val (ax,_) = LD.tcd_parrow (LD.ltd_tyc nt)
		       handle LD.DeconExn => bug "transform"
                   val lt_exr =
                     LD.tcc_tuple [LB.tcc_void, tycNarrow ax, LB.tcc_int]
                in (PL.DATAcon(ndc, [], z),
                    fn le => UNWRAP(lt_exr, [VAR z], w,
                               SELECT(VAR w, 1, v, le)))
               end
           | lpcon (PL.DATAcon(dc as (name, DA.CONSTANT _, lt), ts, v)) =
               let val ndc = dcf(dc, ts) and z = mkv()
                in (PL.DATAcon(ndc, [], z),
                    fn le => RECORD(FU.rk_tuple, [], v, le))
               end
           | lpcon (PL.DATAcon(dc as (_, DA.UNTAGGED, _), ts, v)) =
               let val (tc, rt, ndc) = dargtyc(dc, ts)
                   val hdr = LP.utgd(tc, kenv, rt)
                   val z = mkv()
                in (PL.DATAcon(ndc, [], z),
                    fn le => LET([v], hdr(VAR z), le))
               end
           | lpcon (PL.DATAcon(dc as (_, DA.TAGGED i, _), ts, v)) =
               let val (tc, rt, ndc) = dargtyc(dc, ts)
                   val hdr = LP.tgdd(i, tc, kenv, rt)
                   val z = mkv()
                in (PL.DATAcon(ndc, [], z),
                    fn le => LET([v], hdr(VAR z), le))
               end
           | lpcon (PL.DATAcon _) = bug "unexpected case in lpcon"
           | lpcon c = (c, Fn.id)

         (* lpev : lexp -> (value * (lexp -> lexp)) *)
         and lpev (RET [v]) = (v, Fn.id)
           | lpev e = (* bug "lpev not implemented yet" *)
               let val x= mkv()
                in (VAR x, fn y => LET([x], e, y))
               end

         (* loop: lexp -> lexp *)
         and loop le =
           (case le
             of RET _ => le
              | LET(vs, e1, e2) => LET(vs, loop e1, loop e2)

              | FIX(fdecs, e) => FIX(map lpfd fdecs, loop e)
              | APP _  => le

              | TFN((tfk, v, tvks, e1), e2) =>
                  let val (nkenv, hdr) = LP.tkAbs(kenv, tvks, v)
                      val ne1 = transform (nkenv) e1
                   in hdr(ne1, loop e2)
                  end
              | TAPP(v, ts) =>
                  let val _ = debugmsg ">>loop TAPP"
		      val _ = if !debugging then PrintFlint.printLexp le
			      else ()
		      val args = LP.tsLexp(kenv, ts)
		      val _ = debugmsg "--loop TAPP tsLexp args:"
		      val _ = if !debugging then PrintFlint.printLexp args
			      else ()
		      val (u, hdr) = lpev(args)
		      val _ = debugmsg "--loop TAPP lpev: "
		      val _ = if !debugging then PrintFlint.printLexp (hdr(RET [v]))
			      else ()
                      (* a temporary hack that fixes type mismatches *)
                      val lt = getlty v
                      val oldts = map ltyNarrow (#2 (LD.ltd_poly lt))
                      val newts = map ltyNarrow (LE.lt_inst(lt, ts))
                      val nhdr = mcast(oldts, newts)

		      val _ = debugmsg "<<loop TAPP"
                   in nhdr (hdr (APP(v, [u])))
                  end

              | RECORD(FR.RK_VECTOR tc, vs, v, e) =>
                  RECORD(FR.RK_VECTOR (tycNarrow tc), vs, v, loop e)
              | RECORD(rk, vs, v, e) => RECORD(rk, vs, v, loop e)
              | SELECT(u, i, v, e) => SELECT(u, i, v, loop e)

              | CON ((_, DA.CONSTANT i, _), _, _, v, e) =>
                  WRAP(LB.tcc_int, [tagInt i], v, loop e)

              | CON ((_, DA.EXN (DA.LVAR x), nt), [], u, v, e) =>
                  let val z = mkv()
                      val (ax,_) = LD.tcd_parrow (LD.ltd_tyc nt)
			  handle LD.DeconExn => bug "transform loop"
                      val lt_exr =
                        LD.tcc_tuple [LB.tcc_void, tycNarrow ax, LB.tcc_int]
                   in RECORD(FU.rk_tuple, [VAR x, u, tagInt 0], z,
                             WRAP(lt_exr, [VAR z], v, loop e))
                  end

              | CON (dc as (_, DA.UNTAGGED, _), ts, u, v, e) =>
                  let val (tc, rt, _) = dargtyc(dc, ts)
                      val hdr = LP.utgc(tc, kenv, rt)
                   in LET([v], hdr(u), loop e)
                  end
              | CON (dc as (_, DA.TAGGED i, _), ts, u, v, e) =>
                  let val (tc, rt, _) = dargtyc(dc, ts)
                      val hdr = LP.tgdc(i, tc, kenv, rt)
                   in LET([v], hdr(u), loop e)
                  end
              | CON (_, ts, u, v, e) => bug "unexpected case CON in loop"

              | SWITCH (v, csig, cases, opp) =>
                  let fun g (c, x) =
                        let val (nc, hdr) = lpcon c
                         in (nc, hdr(loop x))
                        end
                   in SWITCH(v, csig, map g cases, Option.map loop opp)
                  end

              | RAISE (u, ts) => RAISE(u, map ltyNarrow ts)
              | HANDLE(e, v) => HANDLE(loop e, v)

              | BRANCH(xp as (NONE, po, lt, []), vs, e1, e2) =>
                  BRANCH((NONE, po, ltyNarrow lt, []), vs, loop e1, loop e2)
              | BRANCH(_, vs, e1, e2) =>
                  bug "type-directed branch primops are not supported"

              | PRIMOP(xp as (_, PO.WRAP, _, _), u, v, e) =>
                  let val tc = FU.getWrapTyc xp
                      val hdr = LP.mkwrp(tc, kenv, true, tycNarrow tc)
                   in LET([v], hdr(RET u), loop e)
                  end
              | PRIMOP(xp as (_, PO.UNWRAP, _, _), u, v, e) =>
                  let val tc = FU.getUnWrapTyc xp
                      val hdr = LP.mkuwp(tc, kenv, true, tycNarrow tc)
                   in LET([v], hdr(RET u), loop e)
                  end
	      | PRIMOP((_, PO.INLMKARRAY, _, _), _, _, _) => bug "unexpected INLMKARRAY"
              | PRIMOP(xp as (NONE, po, lt, ts), vs, v, e) =>
                  PRIMOP((NONE, po, ltyNarrow lt, ts), vs, v, loop e)
              | PRIMOP((_,po,_,_), vs, v, e) =>
                  (say(concat["\n####", PrimopUtil.toString po, "####\n"]);
                   bug "unexpected PRIMOP in loop")
             (* end case *))
      in loop
     end (* function transform *)

     val (fk, f, vts, e) = fdec

  in (fk, f, map (fn (v,t) => (v, ltyNarrow t)) vts, transform (LP.initKE) e)
 end (* function reify *)

end (* toplevel local *)
end (* structure Reify *)
