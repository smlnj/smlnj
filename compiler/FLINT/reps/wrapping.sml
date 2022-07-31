(* wrapping.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature WRAPPING =
sig
  val wrapping : FLINT.prog -> FLINT.prog
end (* signature WRAPPING *)

structure Wrapping : WRAPPING =
struct

local
  structure CO = Coerce
  structure DI = DebIndex
  structure PO = Primop
  structure LV = LambdaVar 
  structure DA = Access
  structure LT = Lty
  structure FR = FunRecMeta
  structure LK = LtyKernel
  structure LD = LtyDef
  structure LB = LtyBasic
  structure LE = LtyExtern
  structure PL = PLambda
  structure F = FLINT
  structure PF = PrintFlint
  open FLINT
in

val debugging = FLINT_Control.wrdebugging
val say = Control_Print.say
fun dbsay msg = if !debugging then say msg else ()
fun bug msg = ErrorMsg.impossible ("Wrapping: " ^ msg)

(* identity *)
val ident = fn x => x

(* mkLvars : int -> LV.lvar list) *)
fun mkLvars n = List.tabulate (n, fn i => LV.mkLvar ())
			      
(****************************************************************************
 *                   MISC UTILITY FUNCTIONS                                 *
 ****************************************************************************)
local (* utility functions *)
    val lt_upd =
        let val x = LB.ltc_array (LB.ltc_tv 0)
         in LD.ltc_poly([LD.tkc_mono],
              [LD.ltc_arrow(LB.ffc_rrflint, [x, LB.ltc_int, LB.ltc_tv 0],
                                            [LB.ltc_unit])])
        end

    val lt_sub =
        let val x = LB.ltc_array (LB.ltc_tv 0)
         in LD.ltc_poly([LD.tkc_mono],
              [LD.ltc_arrow(LB.ffc_rrflint, [x, LB.ltc_int], [LB.ltc_tv 0])])
        end
in

fun isArraySub lty = LK.lt_eqv(lty, lt_sub)
fun isArrayUpd lty = LK.lt_eqv(lty, lt_upd)

val f64sub = PO.NUMSUBSCRIPT(PO.FLOAT 64)
val f64upd = PO.NUMUPDATE(PO.FLOAT 64)

(* classPrim : primop -> primop * bool
 * takes a primop and classifies its kind. It returns a new primop,
 * and a flag that indicates if this primop has been specialized; this
 * flag will be true only for NUMSUBSCRIPT and NUMUPDATE primops.
 * [2019-06-25] removed run-time type specialization
 *)
fun classPrim (px as (d, p, lt, ts)) =
    (case (p, ts)
       of ((PO.NUMSUBSCRIPT _ | PO.NUMUPDATE _), _) =>   (* overloaded primops, instantiate types *)
	    ((d, p, LE.lt_pinst(lt, ts), []), true)
	| (PO.ASSIGN, [tc]) =>			         (* special *)
	   if (LE.tc_upd_prim tc = PO.UNBOXEDUPDATE)
	     then ((d, PO.UNBOXEDASSIGN, lt, ts), false) (* avoid store-list allocation *)
	     else ((d, p, lt, ts), false)
	| (PO.UPDATE, [tc]) => ((d, LE.tc_upd_prim tc, lt, ts), false)
	| _ => (px, false))

val argbase = fn vs => (vs, ident)
val resbase = fn v => (v, ident)

end (* local -- utility functions *)

(****************************************************************************
 * The "wrapping" function does the following several things:               *
 *                                                                          *
 *   (1) representation coercions are inserted at TAPP, BRANCH, PRIMOP,     *
 *       CON, SWITCH, and RECORD(FR.RK_VECTOR _, _). For CON and SWITCH        *
 *       these wrap/unwrap the arguments of a datatype constuctor while     *
 *       for FR.RK_VECTOR it wraps the vector elements only.                   *
 *   (2) all primops in PRIM are given type-specific meanings;              *
 *   (3) all conreps in CON and SWITCH are given type-specific meanings ??  *
 *                                                                          *
 ****************************************************************************)
(* wrapping : F.prog -> F.prog *)
fun wrapping (fdec: F.prog) =
let (* In pass1, we calculate the "old"(?) type of each variable in the FLINT
     * expression. We do this for the sake of having simpler wrapping code. *)

    val getlty = Recover.recover (fdec, false)

    (** generate a set of new wrappers using LE.typeWrapGen *)
      (* tycWrap : tyc -> tyc;
       * ltyWrap : lty -> lty;
       * tycUnwrap : tyc -> tyc;
       * ltyUnwrap : lty -> lty *)
    val (tycWrap, ltyWrap, tycUnwrap, ltyUnwrap) = LE.typeWrapGen ()

    fun fixDconTy lt =
        if LD.ltp_ppoly lt
        then let val (ks, t) = LD.ltd_ppoly lt
              in LD.ltc_ppoly(ks, ltyWrap t)
             end
	else ltyWrap lt

    (* transform : CO.wpEnv * DI.depth -> (lexp -> lexp) *)
    fun transform (wenv, d) =
      let (* lpfd : fkind * v? * vts? * F.lexp -> fkind * v? * ? * lexp *)
	  fun lpfd ({isrec,known,inline,cconv}, v, vts, e) =
	      let val nisrec = case isrec
				 of SOME(ts,l) => SOME(map ltyUnwrap ts, l)
				  | NONE => NONE
		  val ncconv = case cconv
				 of FR.CC_FUN fixed => FR.CC_FUN LD.ffc_fixed
				  | FR.CC_FCT => cconv
	      in ({isrec=nisrec, known=known,
		   cconv=ncconv, inline=inline},
		  v,
		  map (fn (x,t) => (x, ltyUnwrap t)) vts,
		  loop e)
	      end

          (* lpdc : dcon * tyc list * value * bool
                    -> dcon * tyc list * (lexp -> lexp) * value  *)
          and lpdc (dc as (name,rep,lt), ts, u, wflag) =
            let (*** fixing the potential mismatch in the type *)
                val ndc = (name, rep, fixDconTy lt)

                val aty = case LD.ltd_arrow (LE.lt_pinst(lt, ts))
                           of (_, [x], _) => x
                            | _ => bug "unexpected case in lpdc"
                val (naty, oaty) = (ltyWrap aty, ltyUnwrap aty)

                val hdr = if wflag then CO.wrapOp(wenv,[naty],[oaty],d)
                          else CO.unwrapOp(wenv,[naty],[oaty],d)


                val nts = map tycWrap ts
             in case hdr
                 of NONE => (ndc, nts, ident, u)
                  | SOME hhh =>
                      let val z = LV.mkLvar()
                          val nu = VAR z
                       in if wflag then  (* CON *)
                            (ndc, nts, fn xe => LET([z], hhh([u]), xe), nu)
                          else           (* DECON *)
                            let val x = case u
					    of VAR q => q
                                             | _ => bug "unexpected case in lpdc"
                             in (ndc, nts,
                                 fn xe => LET([x], hhh([nu]), xe), nu)
                            end
                      end
            end (* function lpdc *)

          (* lpsw : con * lexp -> con * lexp *)
          and lpsw (PL.DATAcon(dc, ts, v), e) =
                let val (ndc, nts, hdr, u) = lpdc(dc, ts, VAR v, false)
                 in (case u
                      of VAR nv => (PL.DATAcon(ndc, nts, nv), hdr(loop e))
                       | _ => bug "unexpected case in lpsw")
                end
            | lpsw (c, e) = (c, loop e)


          (* lprim : primop -> (primop *
           *                    (value list -> value list * (lexp -> lexp))
           *                    (lvar -> lvar * (lexp -> lexp)))
           *)
          and lprim (dict, p, lt, []) =
                ((dict, p, ltyUnwrap lt, []), argbase, resbase)
            | lprim px =
                let val ((dict, np, lt, ts), issp) = classPrim px
                    val nlt = ltyUnwrap lt
                    val wts = map tycWrap ts
                 in if issp (* primop has been specialized *)
                    then ((dict, np, nlt, wts), argbase, resbase)
                    else (* still a polymorphic primop *)
                     (let val nt = LE.lt_pinst(nlt, wts)
                          val (_, nta, ntr) = LD.ltd_arrow nt
                          val ot = ltyUnwrap(LE.lt_pinst(lt, ts))
                          val (_, ota, otr) = LD.ltd_arrow ot
                          val arghdr =
                            (case CO.wrapOp(wenv, nta, ota, d)
                              of NONE => argbase
                               | SOME hhh =>
                                   (fn vs =>
                                     let val nvs = mkLvars (length vs)
                                      in (map VAR nvs,
                                          fn le => LET(nvs, hhh(vs), le))
                                     end))
                          val reshdr =
                            (case CO.unwrapOp(wenv, ntr, otr, d)
                              of NONE => resbase
                               | SOME hhh =>
                                   (fn v =>
                                     let val nv = LV.mkLvar ()
                                      in (nv,
                                          fn le => LET([v], hhh([VAR nv]), le))
                                     end))
                          val npx' = (dict, np, nt, [])
                       in (npx', arghdr, reshdr)
                      end)
                end (* function lprim *)

          and loop le =
            (case le
              of RET _ => le
               | LET (vs, e1, e2) => LET (vs, loop e1, loop e2)
               | FIX (fdecs, e) => FIX(map lpfd fdecs, loop e)
               | APP _ => le
               | TFN ((tfk, v, tvks, e1), e2) =>  (* put down all wrappers *)
                   let val nwenv = CO.wpNew(wenv, d)
                       val ne1 = transform (nwenv, DI.next d) e1
                    in TFN((tfk, v, tvks, CO.wpBuild(nwenv, ne1)), loop e2)
                   end

               | TAPP (v, ts) =>
                   let val _ = if !debugging
			     then (say ">>> Wrapping.transform.loop#TAPP: v = ";
				   PF.printValue v; say "\n")
			     else ()
		       val _ = if !debugging
				then (say "ty: "; PF.printTycList ts; say "\n")
				else ()
		       val olt: LT.lty = getlty v
		       val _ = if !debugging
			       then (say "olt: "; PF.printLty olt; say "\n")
			       else ()
                       val nts : LT.tyc list = map tycWrap ts
		       val _ = if !debugging
			       then (say "nts: "; PF.printTycList nts; say "\n")
			       else ()
                       val nlts: LT.lty list = LE.lt_inst(ltyUnwrap olt, nts)
		       val _ = if !debugging
			       then (say "nlts: "; PF.printLtyList nlts; say "\n")
			       else ()
                       val olts: LT.lty list = map ltyUnwrap (LE.lt_inst(olt, ts))
		       val _ = if !debugging
			       then (say "olts: "; PF.printLtyList olts; say "\n")
			       else ()
                       val hdr = CO.unwrapOp (wenv, nlts, olts, d)
                    in case hdr
                        of NONE =>
			     let val _ = dbsay "loop#TAPP: hdr = NONE\n";
				 val result = TAPP(v, nts)
			     in if !debugging
				then (say "result:\n"; PF.printLexp result;
				      say "<<<Wrapping.transform.loop#TAPP\n")
				else ();
				result
			     end
                         | SOME hhh =>
                             let val _ = dbsay "loop#tapp: hdr = SOME\n"
			         val nvs = mkLvars (length nlts)
				 val result = LET(nvs, TAPP(v, nts), hhh(map VAR nvs))
                              in if !debugging
				 then (say "result:\n"; PF.printLexp result;
				       say "<<<Wrapping.transform.loop#TAPP\n")
				 else ();
				 result
                             end
                   end

               | CON (dc, ts, u, v, e) =>
                   let val (ndc, nts, hdr, nu) = lpdc(dc, ts, u, true)
                    in hdr (CON(ndc, nts, nu, v, loop e))
                   end

               | SWITCH (v, csig, cases, opp) =>
                   SWITCH(v, csig, map lpsw cases, Option.map loop opp)

               | RECORD(FR.RK_VECTOR t, vs, v, e) =>
                   let val (otc, ntc) = (tycUnwrap t, tycWrap t)
                       val ot = LD.ltc_tyc otc
                       val nt = LD.ltc_tyc ntc
                    in (case CO.wrapOp(wenv, [nt], [ot], d)
                         of NONE => RECORD(FR.RK_VECTOR ntc, vs, v, loop e)
                          | SOME hhh =>
                              let val f = LV.mkLvar () and x = LV.mkLvar ()
				  val fkfun = {isrec = NONE, known = false,
					       inline = FR.IH_ALWAYS,
					       cconv = FR.CC_FUN LD.ffc_fixed}
                                  fun mh xe =
                                    FIX([(fkfun,f,[(x,ot)],hhh([VAR x]))], xe)

                                  fun pass([], nvs, h) =
                                        h(RECORD(FR.RK_VECTOR ntc,
                                                rev nvs, v, loop e))
                                    | pass(u::r, nvs, h) =
                                        let val z = LV.mkLvar ()
                                            fun h0 xe =
                                              LET([z], APP(VAR f, [u]), xe)
                                         in pass(r, (VAR z)::nvs, h o h0)
                                        end
                               in pass(vs, [], mh)
                              end)
                   end
               | RECORD (rk, vs, v, e) => RECORD(rk, vs, v, loop e)
               | SELECT (u, i, v, e) => SELECT(u, i, v, loop e)

               | RAISE (u, lts) => RAISE(u, map ltyUnwrap lts)
               | HANDLE (e, v) => HANDLE (loop e, v)

               (* resolving the polymorphic equality in a special way *)
               | BRANCH (p as (_, PO.POLYEQL, _, _), vs, e1, e2) =>
                   loop(Equal.equal_branch (p, vs, e1, e2))
               | PRIMOP (p as (_, PO.POLYEQL, _, _), vs, v, e) =>
                   bug "unexpected POLYEQL in wrapping"
               | PRIMOP ((_, PO.INLMKARRAY, _, _), vs, v, e) =>
                   bug "unexpected INLMKARRAY in wrapping"

               (* resolving the usual primops *)
               | BRANCH (p, vs, e1, e2) =>
                   let val (np, hg, _) = lprim p
                       val (nvs, nh) = hg vs
                    in nh(BRANCH(np, nvs, loop e1, loop e2))
                   end
               | PRIMOP (p, vs, v, e) =>
                   let val (np, hg1, hg2) = lprim p
                       val (nvs, nh1) = hg1 vs
                       val (nv, nh2) = hg2 v
                    in nh1(PRIMOP(np, nvs, nv, nh2(loop e)))
                   end)
       in loop
      end (* function transform *)

    val (fk, f, vts, e) = fdec
    val nvts = map (fn (v, t) => (v, ltyUnwrap t)) vts
    val wenv = CO.initWpEnv()
    val ne = transform (wenv, DI.top) e
 in (fk, f, nvts, CO.wpBuild(wenv, ne))
end (* function wrapping *)

end (* toplevel local *)
end (* structure Wrapping *)

