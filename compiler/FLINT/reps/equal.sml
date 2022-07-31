(* equal.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature EQUAL =
sig

  (*
   * Constructing generic equality functions; the current version will
   * use runtime polyequal function to deal with abstract types. (ZHONG)
   * But abstract types are not equality types!? (DBM)
   *)
  val equal_branch : FLINT.primop * FLINT.value list * FLINT.lexp * FLINT.lexp
                     -> FLINT.lexp
  val debugging : bool ref

end (* signature EQUAL *)


structure Equal : EQUAL =
struct

local
  structure BT = BasicTypes
  structure LT = Lty
  structure FR = FunRecMeta
  structure LK = LtyKernel
  structure LD = LtyDef
  structure LB = LtyBasic
  structure LE = LtyExtern
  structure PT = PrimTyc
  structure PO = Primop
  structure PL = PLambda
  structure PP = PrettyPrint
  structure FU = FlintUtil
  open FLINT
in

val debugging = ref false
fun bug msg = ErrorMsg.impossible("Equal: "^msg)
val say = Control.Print.say
val mkv = LambdaVar.mkLvar
val ident = fn x => x

val (trueDcon', falseDcon') =
  let val lt = LD.ltc_arrow(LB.ffc_rrflint, [LB.ltc_unit], [LB.ltc_bool])
      fun h (Types.DATACON{name, rep, ...}) = (name, rep, lt)
   in (h BT.trueDcon, h BT.falseDcon)
  end

exception Poly

(****************************************************************************
 *                   Commonly-used FLINT Types                              *
 ****************************************************************************)

(** assumptions: typed created here will be reprocessed in wrapping.sml *)
fun eqLty lt  = LD.ltc_arrow(LB.ffc_rrflint, [lt, lt], [LB.ltc_bool])
fun eqTy tc   = eqLty(LD.ltc_tyc tc)
val booleqty  = eqLty (LB.ltc_bool)
fun numeqty sz = eqLty (LB.ltc_num sz)

(****************************************************************************
 *              equal --- the equality function generator                   *
 ****************************************************************************)
exception Notfound

val fkfun = {isrec=NONE, known=false, cconv=FR.CC_FUN LB.ffc_rrflint, inline=FR.IH_SAFE}

fun branch (e, te, fe) =
    let val x = mkv()
    in LET([x], e,
	   SWITCH(VAR x, BT.boolsign,
		  [(PL.DATAcon(trueDcon', [], mkv()), te),
		   (PL.DATAcon(falseDcon', [], mkv()), fe)], NONE))
    end

(* equal : lvar * lvar -> lexp
 *  peqv is an lvar bound to the polyequal function, while seqv is an lvar bound to
 *  a string equality function. Gives up and invokes polyequal on tuples of length
 *  greater than 10.  This is used only in the module Wrapping (FLINT/reps/wrapping.sml)
 *  to replace a branch on POLYEQUAL with a branch on a more type-specific equality in
 *  certain cases, such as numeric, string, boolean, and ref equality, and tuples of such. *)
fun equal (peqv, seqv) =
    let fun eq (tc: LT.tyc, x: value, y: value, te: lexp, fe: lexp) =
	    let fun eq_tuple (_: int, []: LT.tyc list, te, fe) = te
		  | eq_tuple (n, ty::tys, te, fe) =
		      let val a = mkv()
			  val b = mkv()
		      in SELECT(x, n, a,
				SELECT(y, n, b,
				       eq(ty, VAR a, VAR b,
					  eq_tuple(n + 1, tys, te, fe),
					  fe)))
		      end

	     in if LD.tcp_tuple tc then
		   if length(LD.tcd_tuple tc) > 10 then raise Poly
		   else (case fe
		           of (APP _ | RET _) =>
			       eq_tuple(0, LD.tcd_tuple tc, te, fe)
			    | _ => let val f = mkv()
				   in FIX([(fkfun, f, [], fe)],
					  eq_tuple(0, LD.tcd_tuple tc,
						   te, APP(VAR f, [])))
				   end)
		else if LD.tcp_prim tc then
		    let val prim = LD.tcd_prim tc
		    in case PT.numSize prim  (* is it a PT_NUM? *)
			of SOME sz =>
			   BRANCH((NONE, PrimopUtil.mkIEQL sz, numeqty sz, []), [x,y], te, fe)
			 | NONE =>
			   if PT.pt_eq(prim, PT.ptc_string) then
			       branch(APP(VAR seqv, [x,y]), te, fe)
			   else raise Poly
		    end
		else if LK.tc_eqv(tc,LB.tcc_bool) then
		    BRANCH((NONE, PrimopUtil.IEQL, booleqty, []), [x,y], te, fe)
	        else if (LD.tcp_app tc) then
	            let val (t, _) = LD.tcd_app tc
		     in if LD.tcp_prim t then
			    let val prim = LD.tcd_prim t
			    in if PT.pt_eq(prim, PT.ptc_ref) then
				   BRANCH((NONE, PO.PTREQL, eqTy tc, []), [x,y], te, fe)
			       else raise Poly
			    end
			else raise Poly
		    end
		else raise Poly
	    end

    in (fn (args as (tc, x, y, te, fe))
	         => eq args
		    handle Poly =>
		      let val f = mkv()
		       in LET([f], TAPP(VAR peqv, [tc]),
			      branch(APP(VAR f, [x,y]), te, fe))
		      end)
    end (* equal *)

fun equal_branch ((d, p, lt, ts): FLINT.primop,
		  vs: FLINT.value list,
		  e1: FLINT.lexp, e2: FLINT.lexp) : FLINT.lexp =
  (case (d, p, ts, vs)
    of (SOME{default=pv, table=[(_,sv)]}, PO.POLYEQL, [tc], [x, y]) =>
          equal (pv, sv) (tc, x, y, e1, e2)
     | _ => bug "unexpected case in equal_branch")

end (* toplevel local *)
end (* structure Equal *)
