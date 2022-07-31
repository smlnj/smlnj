(* flintnm.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Converting the Standard PLambda.lexp into the FLINT IL
 *)

signature FLINTNM =
sig
  val norm : PLambda.lexp -> FLINT.fundec
end (* signature FLINTNM *)

structure FlintNM : FLINTNM =
struct

local
  structure DI = DebIndex
  structure DA = Access
  structure BT = BasicTypes
  structure LT = Lty
  structure LK = LtyKernel
  structure LD = LtyDef
  structure LB = LtyBasic
  structure LE = LtyExtern
  structure FR = FunRecMeta
  structure PT = PrimTyc
  structure PO = Primop
  structure PL = PLambda
  structure F  = FLINT
  structure FU = FlintUtil
  structure FL = PFlatten		(* argument flattening *)
  structure PP = PrettyPrint
  structure PU = PPUtil
in

(* debugging *)
val say = Control_Print.say

val debugging = FLINT_Control.nmdebugging

fun debugmsg (msg : string) =
    if !debugging then (say msg; say "\n") else ()

val pd = 20  (* debugging print depth *)

fun ppTycEnv (tenv: Lty.tycEnv) =
    PP.with_default_pp
      (fn ppstrm => (PPLty.ppTycEnv pd ppstrm tenv; PU.pps ppstrm "\n"))

fun ppLty (lty: Lty.lty) =
    PP.with_default_pp
      (fn ppstrm => (PPLty.ppLty pd ppstrm lty; PU.pps ppstrm "\n"))

fun debugLty (lty: Lty.lty) =
    if !debugging then ppLty lty else ()

fun debugLexp (lexp) =
    if !debugging then
       PP.with_default_pp(fn ppstrm => PPLexp.ppLexp pd ppstrm lexp)
    else ()

val mkv = LambdaVar.mkLvar
val cplv = LambdaVar.dupLvar
val ident = fn le : PL.lexp => le

val (iadd_prim, uadd_prim) = let
      val lt_int = LB.ltc_int
      val intOpTy = LD.ltc_parrow(LD.ltc_tuple[lt_int,lt_int],lt_int)
      in
        (PL.PRIM(PrimopUtil.IADD, intOpTy, []), PL.PRIM(PrimopUtil.UADD, intOpTy, []))
      end

fun bug msg = ErrorMsg.impossible("FlintNM: "^msg)


local val (trueDcon', falseDcon') =
        let val lt = LD.ltc_arrow(LB.ffc_rrflint, [LB.ltc_unit], [LB.ltc_bool])
            fun h (Types.DATACON{name, rep, ...}) = (name, rep, lt)
         in (h BT.trueDcon, h BT.falseDcon)
        end

      fun boolLexp b =
        let val v = mkv() and w = mkv()
            val dc = if b then trueDcon' else falseDcon'
         in F.RECORD(FU.rk_tuple, [], v,
             F.CON(dc, [], F.VAR v, w, F.RET[F.VAR w]))
        end
in

fun flint_prim (po as (d, p, lt, ts), vs, v, e) =
  (case p
    of (PO.BOXED  | PO.UNBOXED | PO.CMP _ | PO.FSGN _ | PO.PTREQL |
        PO.PTRNEQ | PO.POLYEQL | PO.POLYNEQ) =>
          (*** branch primops get translated into F.BRANCH ***)
          F.LET([v], F.BRANCH(po, vs, boolLexp true, boolLexp false), e)
     | (PO.GETHDLR | PO.GETVAR) =>
          (*** primops that take zero arguments; argument types
               must be unit ***)
          let fun fix t =
                LD.ltw_arrow(t,
		(fn (ff,[t1],ts2) =>
		    (if LK.tc_eqv(t1, LB.tcc_unit)
		     then LD.ltc_tyc(LK.tcc_arrow(ff, [], ts2))
		     else bug "unexpected zero-args prims 1 in flint_prim")
		  | _ => bug "flint_prim:t1"),
                fn _ => bug "unexpected zero-args prims 2 in flint_prim")
              val nlt =
                LD.ltw_ppoly(lt,
                   fn (ks, t) => LD.ltc_ppoly(ks, fix t),
                   fn _ => fix lt)
           in F.PRIMOP((d,p,nlt,ts), [], v, e)
          end
     | _ =>
          F.PRIMOP(po, vs, v, e))

end (* local flint_prim *)

(* force_raw freezes the calling conventions of a data constructor;
   strictly used by the CON and DATAcon only
 *)
fun force_raw (pty) =
      if LD.ltp_ppoly pty
      then
	let val (ks, body) = LD.ltd_ppoly pty
	    val (aty, rty) = LD.ltd_parrow body
	 in LD.ltc_ppoly(ks,
	       LD.ltc_arrow(LB.ffc_rrflint, [aty], [rty]))
	end
      else
	let val (aty, rty) = LD.ltd_parrow pty
	 in LD.ltc_arrow(LB.ffc_rrflint, [aty], [rty])
	end (* function force_raw *)

fun tocon con = (case con
       of PL.INTcon{ty=0, ...} => bug "IntInf"
	| PL.DATAcon x => bug "unexpected case in tocon"
	| _ => con
      (* end case *))

fun tofundec (venv,d,f_lv,arg_lv,arg_lty,body,isrec) =
    let val _ = (debugmsg "tofundec normalize argument:\n";
		 debugLty arg_lty;
		 debugmsg "\ntofundec normalize body: \n";
		 debugLexp body)
	val (body',body_lty) =
        (* first, we translate the body (in the extended env) *)
        tolexp (LB.ltInsert(venv, arg_lv, arg_lty, d), d) body
	val _ =  debugmsg ">>tofundec detuple arg type"
        (* detuple the arg type *)
	val ((arg_raw, arg_ltys, _), unflatten) = FL.v_punflatten arg_lty
        val _ = debugmsg ">>unflatten body"
        (* now, we add tupling code at the beginning of the body *)
        val (arg_lvs, body'') = unflatten(arg_lv, body')
	val _ = debugmsg ">>construct return type"
	(* construct the return type if necessary *)
	val (body_raw, body_ltys, _) = FL.t_pflatten body_lty
	val rettype = if not isrec then NONE
		      else SOME(body_ltys, FR.LK_UNKNOWN)
	val _ = debugmsg ">>Handle fcn or fct"
	val (f_lty, fkind) =
	    if (LD.ltp_tyc arg_lty andalso LD.ltp_tyc body_lty) then
		(* a function *)
		(LD.ltc_parrow(arg_lty, body_lty),
		 {isrec=rettype, known=false, inline=FR.IH_SAFE,
		  cconv=FR.CC_FUN(LD.ffc_var(arg_raw, body_raw))})
	    else
		(* a functor *)
		(LD.ltc_pfct(arg_lty, body_lty),
		 {isrec=rettype, known=false, inline=FR.IH_SAFE,
		  cconv=FR.CC_FCT})
	val arg_ltys' =  arg_ltys
	val _ = debugmsg "<<tofundec"
    in ((fkind, f_lv, ListPair.zip(arg_lvs, arg_ltys'), body''),
	f_lty)
    end


(* used to translate expressions whose structure is the same
 * in Flint as in PLambda (either both binding or both non-binding)
 * a continuation is unnecessary *)
and tolexp (venv, d) lexp =
    let val _ = debugmsg ">>tolexp"
	fun default_tovalues () =
            tovalues(venv, d, lexp, fn (vals, lty) => (F.RET vals, lty))
        val v = case lexp
                  of PL.APP (PL.PRIM _, arg) => default_tovalues()
      | PL.APP (PL.GENOP _,arg) => default_tovalues()
      | PL.APP (PL.FN (arg_lv,arg_lty,body), arg_le) =>
	    tolexp (venv,d) (PL.LET(arg_lv, arg_le, body))
      | PL.APP (f,arg) =>
            (* first, evaluate f to a mere value *)
            tovalue(venv, d, f,
                    fn (f_val, f_lty) =>
                    (* then eval the argument *)
                    tovalues(venv, d, arg,
			     fn (arg_vals, arg_lty) =>
			     (* now find the return type *)
			     let val (_, r_lty) =
                                   if LD.ltp_pfct f_lty then LD.ltd_pfct f_lty
                                   else LD.ltd_parrow f_lty
			     (* and finally do the call *)
			     in (F.APP(f_val,arg_vals), r_lty)
			     end))

      | PL.FIX (lvs,ltys,lexps,lexp) =>
            (* first, let's setup the enriched environment with those funs *)
            let val venv' = ListPair.foldl
			      (fn (lv,lty,ve) => LB.ltInsert(ve, lv, lty, d))
                              venv (lvs, ltys)

		fun map3 _ ([], _, _) = []
		  | map3 _ (_, [], _) = []
		  | map3 _ (_, _, []) = []
		  | map3 f (x :: xs, y :: ys, z :: zs) =
		      f (x, y, z) :: map3 f (xs, ys, zs)

                (* then translate each function in turn *)
                val funs = map3 (fn (f_lv,f_lty,PL.FN(arg_lv,arg_lty,body)) =>
                                #1(tofundec(venv', d,
					    f_lv, arg_lv, arg_lty, body, true))
				 | _ => bug "non-function in PL.FIX")
				(lvs, ltys, lexps)

                (* finally, translate the lexp *)
                val (lexp',lty) = tolexp (venv',d) lexp
            in (F.FIX(funs,lexp'), lty)
            end

      | PL.LET (lvar,lexp1,lexp2) =>
            tolvar(venv, d, lvar, lexp1,
                   fn lty1 =>
                   tolexp (LB.ltInsert(venv,lvar,lty1,d), d) lexp2)

      | PL.RAISE (le, r_lty) =>
            tovalue(venv, d, le,
                    fn (le_val,le_lty) =>
                    let val (_, r_ltys, _) = FL.t_pflatten r_lty
                    in (F.RAISE(le_val, r_ltys), r_lty)
                    end)

      | PL.HANDLE (body, handler) =>
            tovalue(venv, d, handler,
                    fn (h_val,h_lty) =>
                    let val (body', body_lty) = tolexp (venv, d) body
                    in (F.HANDLE(body', h_val), body_lty)
                    end)

      | PL.SWITCH (le,acs,[],NONE) => bug "unexpected case in L.SWITCH"
	    (* tovalue(venv, d, le, fn _ => (F.RET[], [])) *)
      | PL.SWITCH (le,acs,[],SOME lexp) =>
	    tovalue(venv, d, le, fn (v,lty) => tolexp (venv,d) lexp)
      | PL.SWITCH (le,acs,conlexps,default) =>
	    let fun f (PL.DATAcon((s,cr,lty),tycs,lvar),le) =
		    let val (lv_lty,_) = LD.ltd_parrow(LE.lt_pinst(lty,tycs))
			val newvenv = LB.ltInsert(venv,lvar,lv_lty,d)
			val (le, le_lty) = tolexp (newvenv,d) le
		    in
			((PL.DATAcon((s, cr, force_raw lty), tycs, lvar),
			  le),
			 le_lty)
		    end
		  | f (con,le) =
		    let val (lexp,lty) = tolexp (venv,d) le
		    in ((tocon con, lexp), lty)
		    end
	    in tovalue(venv, d, le,
		       fn (v, lty) =>
		       let val default = Option.map (#1 o tolexp(venv,d)) default
			   val conlexps = map f conlexps
			   val lty = #2 (List.hd conlexps)
		       in (F.SWITCH(v, acs, map #1 conlexps, default), lty)
		       end)
	    end

      (* for mere values, use tovalues *)
      | _ => default_tovalues ()
    val _ = debugmsg "<<tolexp"
    in v
    end

(*
 * tovalue: turns a PLambda lexp into a value+type and then calls
 * the continuation that will turn it into an Flint lexp+type
 * (ltyenv * DebIndex * PL.lexp * ((value * lty) -> (F.lexp * lty list))) -> (F.lexp * lty)
 *
 * - venv is the type environment for values
 * - conts is the continuation
 *)
and tovalue (venv,d,lexp,cont) = let
      val _ = debugmsg ">>tovalue"
      val _ = debugLexp lexp
      val v = (case lexp
                 (* for simple values, it's trivial *)
	         of PL.VAR v => (case LB.ltLookup(venv, v, d)
				  of SOME lty => cont(F.VAR v, lty)
				   | NONE => bug ("tovalue: unbound lvar: " ^
						  LambdaVar.lvarName v))
		  | PL.INT i => cont(F.INT i, LB.ltc_num(#ty i))
		  | PL.WORD w => cont(F.WORD w, LB.ltc_num(#ty w))
(* REAL32: *)
		  | PL.REAL x => cont(F.REAL x, LB.ltc_real)
		  | PL.STRING s => cont(F.STRING s, LB.ltc_string)
		  | _ => (* for cases where tolvar is more convenient *)
                    let val lv = mkv()
                     in tolvar(venv, d, lv, lexp,
			       fn lty => (debugmsg ">>tovalue tolvar cont";
					  debugLexp lexp;
					  cont(F.VAR lv, lty)))
                    end
	    (* end case *))
      val _ = debugmsg "<<tovalue"
      in v
      end (* tovalue *)

(*
 * tovalues: turns a PLambda lexp into a list of values and a list of types
 * and then calls the continuation that will turn it into an Flint lexp+type
 *
 * (ltyenv * DebIndex * PL.lexp * ((value list * lty list) -> (F.lexp * lty list))) -> (F.lexp * lty)
 *
 * - venv is the type environment for values
 * - cont is the continuation
 *)
and tovalues (venv,d,lexp,cont) =
    let val _ = debugmsg ">>tovalues"
	val _ = debugLexp lexp
	val _ = 1
    val v = case lexp of
	PL.RECORD (lexps) =>
	    lexps2values(venv,d,lexps,
			 fn (vals,ltys) =>
			 let val _ = debugmsg ">>tovalues continuation"
			     val _ =
			 if length ltys = 0 then
			     debugmsg ("tovalues cont ltys null")
			 else if length ltys = 1
			 then (debugmsg ("tovalues cont ltys singleton:\n");
			       debugLty (hd ltys))
			 else (debugmsg ("tovalues cont ltys > 1\n \
					\length is: "
					^ Int.toString (length ltys) ^
					"\nstarting with:\n");
			       debugLty (hd ltys))
			      fun scan [] = debugmsg "tovalues end of ltys"
				| scan (lts) =
				    let fun scan' ([], [], n) = ()
					  | scan' (x::xs, l::ls, n) =
					    (debugmsg ("tovalues cont ltys ["^
						      Int.toString n ^"]:");
					     debugLexp l;
					     debugLty x;
					     scan' (xs, ls, n + 1))
					  | scan' _ =
					    raise Fail "flintnm.sml:\
							\ tovalues::scan'"

				    in scan'(lts, lexps, 0)
				    end
                             val _ = scan ltys
			     val lty = LD.ltc_tuple ltys
			     val _ = (debugmsg ("<<tovalues cont tupled");
				      debugLty lty)
			     val (_, ltys, _) = FL.t_pflatten lty
			     val _ = debugmsg "<<tovalues cont flatten"
			     val _ = (debugmsg (">>tovalues cont LK.lt_eqv");
				      debugLty lty)
			     val _ = debugmsg ">>tovalues cont LD.ltc_tuple"
			     val ltyst = (LD.ltc_tuple ltys)
			     val _ = (debugmsg ("<<tovalues cont LD.ltc_tuple");
				      debugLty ltyst)
			     val eqvLty = LK.lt_eqv(lty, LD.ltc_tuple ltys)
			     val _ = debugmsg "<<tovalues cont lt_eqv"
			 in
			     (* detect the case where flattening is trivial *)
			     if eqvLty then
				 cont(vals,lty)
			     else
				 let val lv = mkv()
                                     val (_, pflatten) = FL.v_pflatten lty
				     val (vs,wrap) = pflatten (F.VAR lv)
				     val (c_lexp,c_lty) = cont(vs, lty)
				     val _ = debugmsg "<<tovalues continuation"
				 in
				     (F.RECORD(FU.rk_tuple,
					       vals, lv, wrap c_lexp),
				      c_lty)
				 end
			 end)

      | _ => tovalue(venv,d,lexp,
		     fn (v, lty) =>
		     let val (vs,wrap) = (#2(FL.v_pflatten lty)) v
			 val (c_lexp, c_lty) = cont(vs, lty)
		     in (wrap c_lexp, c_lty)
		     end)
    val _ = debugmsg "<<tovalues"
    in v
    end

(* eval each lexp to a value *)
and lexps2values (venv,d,lexps,cont) =
    let val _ = debugmsg ">>lexps2values"
	val _ = map debugLexp lexps
	val _ = 1

fun ppTycEnv tenv =
    PrettyPrint.with_default_pp (fn ppstrm => PPLty.ppTycEnv 20 ppstrm tenv)
fun ppTyc tyc =
    PrettyPrint.with_default_pp (fn ppstrm => PPLty.ppTyc 20 ppstrm tyc)

	fun f [] (vals,ltys) = cont (rev vals, rev ltys)
	  | f (lexp::lexps) (vals,ltys) =
	    ((* This debugging printing is quadratic
		debugmsg ("lexps2values ltys:");
                map debugLty ltys; *)
	     tovalue(venv,d,lexp,
		     fn (v, lty) =>
                       ((* debugmsg ">>lexps2values tovalue";
			 debugLexp lexp;
			 debugmsg "lty:"; debugLty lty *)
		        f lexps (v::vals, lty::ltys))))
	    (* handle LtyKernel.tcUnbound (tenv,tyc) =>
		   (with_pp(fn s =>
                      (PU.pps s "*** lexps2values ***; PP.newline s;
                       lexp: \n";
		       PPLexp.printLexp lexp;
		       print "\ntype: \n";
		       ppTyc 20 s tyc; PP.newline s;
		       PU.pps s "tenv:"; PP.newline s;
		       ppTycEnv 20 s tenv;
                       raise LtyKernel.tcUnbound (tenv,tyc)))) *)
	val v = f lexps ([], [])
	val _ = debugmsg "<<lexp2values"
    in
	v
    end

(*
 * tolvar: same as tovalue except that it binds the value of the PLambda
 * to the indicated lvar and passes just the type to the continuation
 *)
and tolvar (venv,d,lvar,lexp,cont) =
    let val _ = debugmsg ">>tolvar"
	val _ = debugLexp lexp
	fun eta_expand (f, f_lty) =
            let val lv = mkv()
                val (arg_lty, ret_lty) = (LD.ltd_parrow f_lty)
            in tolvar(venv, d, lvar,
                      PL.FN(lv, arg_lty, PL.APP(f, PL.VAR lv)),
                      cont)
            end

        (* inbetween tolvar and tovalue: it binds the lexp to a variable but
         * is free to choose the lvar and passes it to the continutation *)
        fun tolvarvalue (venv,d,lexp,cont) =
            tovalue(venv, d, lexp,
                    fn (v,lty) =>
                    case v of
                        F.VAR lv => cont(lv, lty)
                      | _ => let val lv = mkv()
                                 val (lexp',lty) = cont(lv, lty)
                             in (F.LET ([lv], F.RET [v], lexp'), lty)
                             end)

        fun PO_helper (arg,f_lty,tycs,filler) =
            (* invariants: primop's types are always fully closed *)
            let (* pty is the resulting FLINT type of the underlying primop,
                   r_lty is the result PLambda type of this primop expression,
                   and flat indicates whether we should flatten the arguments
                   or not. The results of primops are never flattened.
                 *)
		val _ = debugmsg ">>tolvar PO_helper"
                val (pty, r_lty, flat) =
                  (case (LD.ltp_ppoly f_lty, tycs)
                    of (true, _) =>
                         let val (ks, lt) = LD.ltd_ppoly f_lty
                             val (aty, rty) = LD.ltd_parrow lt
                             val r_lty =
                               LE.lt_pinst(LD.ltc_ppoly(ks, rty), tycs)

                             val (_, atys, flat) = FL.t_pflatten aty
                             (*** you really want to have a simpler
                                  flattening heuristics here; in fact,
                                  primop can have its own flattening
                                  strategy. The key is that primop's
                                  type never escape outside.
                              ***)

                             val nrty = rty
                             val pty = LD.ltc_arrow(LB.ffc_rrflint,atys,[nrty])
                          in ( LD.ltc_ppoly(ks, pty), r_lty, flat)
                         end
                     | (false, []) => (* monomorphic case *)
                         let val (aty, rty) = LD.ltd_parrow f_lty
                             val (_, atys, flat) = FL.t_pflatten aty
                             val nrty = rty
                             val pty = LD.ltc_arrow(LB.ffc_rrflint,atys,[nrty])
                          in (pty, rty, flat)
                         end
                     | _ => bug "unexpected case in PO_helper")
                val r = if flat then
                 (* ZHONG asks: is the following definitely safe ?
                    what would happen if ltc_raw is not an identity function ?
                  *)
                  tovalues(venv, d, arg,
		     	   fn (arg_vals, arg_lty) =>
		     	   let val _ = debugmsg ">>tolvar PO_helper cont"
			       val (c_lexp, c_lty) = cont(r_lty)
		     	   (* put the filling inbetween *)
		     	   val r' = (filler(arg_vals, pty, c_lexp), c_lty)
			   val _ = debugmsg "<<tolvar PO_helper cont"
			   in r'
		     	   end)
                else
                   tovalue(venv, d, arg,
		     	   fn (arg_val, arg_lty) =>
		     	   let val (c_lexp, c_lty) = cont(r_lty)
		     	   (* put the filling inbetween *)
		     	   in (filler([arg_val], pty, c_lexp), c_lty)
		     	   end)
	     val _ = debugmsg "<<tolvar PO_helper"
	    in r
            end (* function PO_helper *)

        fun default_tolexp () =
            let val (lexp', lty) = tolexp (venv, d) lexp
                val (c_lexp, c_lty) = cont(lty)
                val (_, punflatten) = FL.v_punflatten lty
                val (lvs,c_lexp') = punflatten (lvar, c_lexp)
            in (F.LET(lvs, lexp', c_lexp'), c_lty)
            end

(*         fun default_tovalue () = *)
(*             tovalue(venv, d, lexp, *)
(*                     fn (v,lty) => *)
(*                     let val (lexp', ltys) = cont(lty) *)
(*                     in (F.LET([lvar], F.RET[v], lexp'), ltys) *)
(*                     end) *)

    val r = case lexp of
      (* primops have to be eta-expanded since they're not valid
       * function values anymore in Flint *)
        PL.PRIM (po,lty,tycs) => eta_expand(lexp, LE.lt_pinst(lty, tycs))
      | PL.GENOP (dict,po,lty,tycs) => eta_expand(lexp, LE.lt_pinst(lty, tycs))

      | PL.FN (arg_lv,arg_lty,body) =>
            (* translate the body with the extended env into a fundec *)
            let val (fundec as (fk,f_lv,args,body'), f_lty) =
		    tofundec(venv, d, lvar, arg_lv, arg_lty, body, false)
                val (lexp, lty) = cont(f_lty)
            in (F.FIX([fundec], lexp), lty)
            end

      (* this is were we really deal with primops *)
      | PL.APP (PL.PRIM ((po,f_lty,tycs)),arg) =>
	let val _ = debugmsg ">>tolvar PL.APP"
	    val (lexp', lty') =
            PO_helper(arg, f_lty, tycs,
                       fn (arg_vals,pty, c_lexp) =>
                       flint_prim((NONE, po, pty, tycs),
				  arg_vals, lvar, c_lexp))
	    val _ = debugmsg "<<tolvar PL.APP"
	    val _ = debugLty lty'
	in (lexp', lty')
	end

      | PL.APP (PL.GENOP({default,table},po,f_lty,tycs),arg) =>
            let fun f ([],table,cont) = cont (table)
                  | f ((tycs,le)::t1,t2,cont) =
                tolvarvalue(venv,d,le,
                            fn (le_lv,le_lty) =>
                            f(t1, (tycs,le_lv)::t2, cont))
            (* first, eval default *)
            in tolvarvalue(venv,d,default,
                           fn (dflt_lv,dflt_lty) =>
                           (* then eval the table *)
                           f(table, [],
                             fn table' =>
                             PO_helper(arg, f_lty, tycs,
                                        fn (arg_vals,pty,c_lexp) =>
                                        flint_prim((SOME {default=dflt_lv,
                                                          table=table'},
                                                    po, pty,
                                                    tycs),
						   arg_vals, lvar, c_lexp))))
            end

      (*  | PL.TFN ([], body) => bug "TFN[]" *)
      | PL.TFN (tks, body) =>
            let val (body', body_lty) =
                  tovalue(venv, DI.next d, body,
                          fn (le_val, le_lty) => (F.RET [le_val], le_lty))
                val lty = LD.ltc_ppoly(tks, body_lty)
                val (lexp', lty) = cont(lty)
		val args = map (fn tk => (mkv(), tk)) tks
            in  (F.TFN(({inline=FR.IH_SAFE}, lvar, args, body'), lexp'),
                 lty)
            end

      (*  | PL.TAPP (f,[]) => bug "TAPP[]" *)
      | PL.TAPP (f,tycs) =>
            (* similar to APP *)
            tovalue(venv, d, f,
                    fn (f_val,f_lty) =>
                    let val f_lty = LE.lt_pinst(f_lty, tycs)
			val (c_lexp, c_lty) = cont(f_lty)
                    in  (F.LET([lvar], F.TAPP(f_val, tycs),
                               c_lexp), c_lty)
                    end)

      | PL.ETAG (le,lty) =>
            tovalue(venv, d, le,
                    fn (le_lv, le_lty) =>
                    let val (c_lexp, c_lty) = cont(LB.ltc_etag lty)
                        val mketag = FU.mketag (LD.ltd_tyc lty)
				     handle LD.DeconExn => bug "etag in flintnm"
                    in (flint_prim(mketag, [le_lv], lvar, c_lexp), c_lty)
                    end)
      | PL.CON ((s,cr,lty),tycs,le) =>
	    tovalue(venv, d, le,
		     fn (v,_) =>
		     let val r_lty = LE.lt_pinst(lty, tycs)
                         val (_,v_lty) = LD.ltd_parrow r_lty
			 val (c_lexp, c_lty) = cont(v_lty)
		     in (F.CON((s, cr, force_raw lty), tycs, v, lvar, c_lexp),
			 c_lty)
		     end)

      | PL.VECTOR (lexps,tyc) =>
	   lexps2values(venv,d,lexps,
			fn (vals, ltys) =>
			let val _ = debugmsg ">>tolvar VECTOR cont"
			    val lty = LD.ltc_tyc(LB.tcc_vector tyc)
			    val (c_lexp, c_lty) = cont(lty)
			in (F.RECORD(FR.RK_VECTOR tyc, vals, lvar, c_lexp),
			    c_lty)
			end)
      | PL.RECORD lexps =>
	   lexps2values(venv,d,lexps,
			fn (vals, ltys) =>
			let val _ = debugmsg ">>tolvar RECORD cont"
			    val lty = LD.ltc_tuple ltys
			    val (c_lexp, c_lty) = cont(lty)
			in (F.RECORD(FU.rk_tuple,
                                     vals, lvar, c_lexp), c_lty)
			end)
      | PL.SRECORD lexps =>
	   lexps2values(venv,d,lexps,
			fn (vals, ltys) =>
			let val _ = debugmsg ">>tolvar SRECORD cont"
			    val lty = LD.ltc_str(ltys)
			    val (c_lexp, c_lty) = cont(lty)
			in (F.RECORD(FR.RK_STRUCT, vals, lvar, c_lexp), c_lty)
			end)

      | PL.SELECT (n,lexp) =>
	    tovalue(venv, d, lexp,
		    fn (v, lty) =>
		       let val lty = LE.lt_select (lty, n, "flintnm.sml#661")
			   val (c_lexp, c_lty) = cont(lty)
		        in (F.SELECT(v, n, lvar, c_lexp), c_lty)
		       end)

      (* these ones shouldn't matter because they shouldn't appear *)
(*       | L.WRAP _ => bug "unexpected WRAP in plambda" *)
(*       | PL.UNWRAP _ => bug "unexpected UNWRAP in plambda" *)

      | _ => default_tolexp ()
    in (debugmsg "<<tolvar"; r)
    end (* function tolvar *)

fun norm (lexp as PL.FN(arg_lv,arg_lty,e)) =
    let val r =
	    (#1(tofundec(LB.initLtyEnv, DI.top, mkv(), arg_lv, arg_lty, e, false))
	     handle x => raise x)
    in (debugmsg "<<norm" (*; PrintFlint.printFundec r *); r)
    end
  | norm _ = bug "unexpected toplevel lexp"

end (* toplevel local *)
end (* structure FlintNM *)
