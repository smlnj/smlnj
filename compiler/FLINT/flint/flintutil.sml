(* flintutil.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature FLINTUTIL =
sig
  val rk_tuple : FunRecMeta.rkind

  val mketag : Lty.tyc -> FLINT.primop
  val wrap   : Lty.tyc -> FLINT.primop
  val unwrap : Lty.tyc -> FLINT.primop

  val WRAP   : Lty.tyc * FLINT.value list
                         * LambdaVar.lvar * FLINT.lexp -> FLINT.lexp
  val UNWRAP : Lty.tyc * FLINT.value list
                         * LambdaVar.lvar * FLINT.lexp -> FLINT.lexp

  val getEtagTyc   : FLINT.primop -> Lty.tyc
  val getWrapTyc   : FLINT.primop -> Lty.tyc
  val getUnWrapTyc : FLINT.primop -> Lty.tyc

  (* copy a lexp with alpha renaming.
   * free variables remain unchanged except for the renaming specified
   * in the first (types) and second (values) argument *)
  val copy : (Lty.tvar * Lty.tyc) list ->
             LambdaVar.lvar LambdaVar.Map.map ->
             FLINT.lexp -> FLINT.lexp
  val copyfdec : FLINT.fundec -> FLINT.fundec

  val freevars : FLINT.lexp -> LambdaVar.Set.set

  val dcon_eq : PLambda.dataconstr * PLambda.dataconstr -> bool

(* are two FLINT values equal? *)
  val sameValue : FLINT.value * FLINT.value -> bool

(* is a value a specific variable? *)
  val valueIsVar : LambdaVar.lvar -> FLINT.value -> bool

end (* signature FLINTUTIL *)


structure FlintUtil : FLINTUTIL =
struct

local
  structure EM = ErrorMsg
  structure LV = LambdaVar
  structure LT = Lty
  structure FR = FunRecMeta
  structure LK = LtyKernel
  structure LD = LtyDef
  structure LB = LtyBasic
  structure LE = LtyExtern
  structure PO = Primop
  structure DA = Access
  structure M  = LambdaVar.Map
  structure A  = Access
  structure O  = Option
  structure S  = LambdaVar.Set
  structure PL = PLambda
  structure F  = FLINT
  open FLINT
in

fun bug msg = EM.impossible("FlintUtil: "^msg)

val rk_tuple : FR.rkind = FR.RK_TUPLE

(* a set of useful primops used by FLINT *)
val tv0 = LB.ltc_tv 0
val btv0 = LD.ltc_tyc (LD.tcc_box (LB.tcc_tv 0))
val etag_lty =
  LD.ltc_ppoly ([LD.tkc_mono],
                 LD.ltc_arrow(LB.ffc_rrflint, [LB.ltc_string],
                                              [LB.ltc_etag tv0]))
fun wrap_lty tc =
  LD.ltc_tyc(LK.tcc_arrow(LD.ffc_fixed, [tc], [LD.tcc_wrap tc]))
fun unwrap_lty tc =
  LD.ltc_tyc(LK.tcc_arrow(LD.ffc_fixed, [LD.tcc_wrap tc], [tc]))

fun mketag tc = (NONE, PO.MKETAG, etag_lty, [tc])
fun wrap tc = (NONE, PO.WRAP, wrap_lty tc, [])
fun unwrap tc = (NONE, PO.UNWRAP, unwrap_lty tc, [])

fun WRAP(tc, vs, v, e) = PRIMOP(wrap tc, vs, v, e)
fun UNWRAP(tc, vs, v, e) = PRIMOP(unwrap tc, vs, v, e)

(* the corresponding utility functions to recover the tyc *)
fun getEtagTyc (_, _, lt, [tc]) = tc
  | getEtagTyc (_, _, lt, []) =
      let val nt = LD.ltd_tyc (#2(LD.ltd_parrow lt))
		   handle LD.DeconExn => bug "getEtagTyc"
       in if LD.tcp_app nt then
            (case #2 (LD.tcd_app nt)
              of [x] => x
               | _ => bug "unexpected case 1 in getEtagTyc")
          else LB.tcc_void
      end
  | getEtagTyc _ = bug "unexpected case 2 in getEtagTyc"

fun getWrapTyc (_, _, lt, []) =
    (LD.ltd_tyc(#1(LD.ltd_parrow lt))
     handle LD.DeconExn => bug "getWrapTyc: bad lt")
  | getWrapTyc _ = bug "getWrapTyc: non-null tycs"

fun getUnWrapTyc (_, _, lt, []) =
    (LD.ltd_tyc(#2(LD.ltd_parrow lt))
     handle LD.DeconExn => bug "getUnWrapTyc: bad lt")
  | getUnWrapTyc _ = bug "getUnWrapTyc: non-null tycs component"

fun dcon_eq ((s1,c1,t1):PLambda.dataconstr, (s2,c2,t2)) =
    Symbol.eq (s1,s2) andalso (c1 = c2) andalso LK.lt_eqv(t1, t2)

val cplv = LambdaVar.dupLvar
(*
 * general alpha-conversion on lexp free variables remain unchanged
 * except for the renaming specified in the first argument.
 *   val copy: lvar M.intmap -> fundec -> fundec
 *)
fun copy ta alpha le = let

    val tc_subst = LE.tc_nvar_subst_gen()
    val lt_subst = LE.lt_nvar_subst_gen()

    val tmap_sort = ListMergeSort.sort (fn ((v1,_),(v2,_)) => LambdaVar.>(v1, v2))
    fun substvar alpha lv = case M.find(alpha,lv) of SOME(lv) => lv | NOE => lv
    fun substval alpha (VAR lv) = VAR(substvar alpha lv)
      | substval alpha v = v
    fun newv (lv,alpha) =
	let val nlv = cplv lv in (nlv, M.insert(alpha,lv,nlv)) end
    fun newvs (lvs,alpha) =
	foldr (fn (lv,(lvs,alpha)) =>
	       let val (nlv,nalpha) = newv(lv,alpha) in (nlv::lvs,nalpha) end)
	      ([],alpha) lvs
    fun cdcon ta alpha (s,ac,lty) =
	(s,
	 case ac
	  of A.EXN(A.LVAR lv) => A.EXN(A.LVAR(substvar alpha lv))
	   | _ => ac,
	 lt_subst ta lty)
    fun cpo ta alpha (dict,po,lty,tycs) =
	(O.map (fn {default,table} =>
		{default=substvar alpha default,
		 table=map (fn (tycs,lv) =>
			    (map (tc_subst ta) tycs, substvar alpha lv))
			   table}) dict,
	 po, lt_subst ta lty, map (tc_subst ta) tycs)
    fun cfk ta {isrec=SOME(ltys,lk),known,inline,cconv} =
	{isrec=SOME(map (lt_subst ta) ltys,lk),
	 known=known, inline=inline, cconv=cconv}
      | cfk _ fk = fk

    fun crk ta (FR.RK_VECTOR tyc) = FR.RK_VECTOR(tc_subst ta tyc)
      | crk _ rk = rk

    fun copy' ta alpha le = let
	val cpo = cpo ta alpha
	val cdcon = cdcon ta alpha
	val substvar = substvar alpha
	val substval = substval alpha
	val copy = copy' ta
    in case le
    of RET vs => RET(map substval vs)
     | LET (lvs,le,body) =>
       let val nle = copy alpha le
	   val (nlvs,nalpha) = newvs(lvs,alpha)
       in LET(nlvs, nle, copy nalpha body)
       end
     | FIX (fdecs,le) =>
       let fun cfun alpha ((fk,f,args,body):fundec,nf) =
	       let val (nargs,nalpha) = newvs(map #1 args, alpha)
	       in (cfk ta fk, nf,
		   ListPair.zip(nargs, (map (lt_subst ta o #2) args)),
		   copy nalpha body)
	       end
	   val (nfs, nalpha) = newvs(map #2 fdecs, alpha)
	   val nfdecs = ListPair.map (cfun nalpha) (fdecs, nfs)
       in
	   FIX(nfdecs, copy nalpha le)
       end
     | APP (f,args) => APP(substval f, map substval args)
     | TFN ((tfk,lv,args,body),le) =>
       (* don't forget to rename the tvar also *)
       let val (nlv,nalpha) = newv(lv,alpha)
	   val (nargs,ialpha) = newvs(map #1 args, nalpha)
	   val ita = tmap_sort ((ListPair.map
				     (fn ((t,k),nt) => (t, LD.tcc_nvar nt))
				     (args, nargs)) @ ta)
       in TFN((tfk,nlv,
	       ListPair.zip(nargs, map #2 args),
	       copy' ita ialpha body),
	      copy nalpha le)
       end
     | TAPP (f,tycs) => TAPP(substval f, map (tc_subst ta) tycs)
     | SWITCH (v,ac,arms,def) =>
       let fun carm (PL.DATAcon(dc,tycs,lv),le) =
	       let val (nlv,nalpha) = newv(lv, alpha)
	       in (PL.DATAcon(cdcon dc, map (tc_subst ta) tycs, nlv),
		   copy nalpha le)
	       end
	     | carm (con,le) = (con, copy alpha le)
       in SWITCH(substval v, ac, map carm arms, Option.map (copy alpha) def)
       end
     | CON (dc,tycs,v,lv,le) =>
       let val (nlv,nalpha) = newv(lv, alpha)
       in CON(cdcon dc, map (tc_subst ta) tycs, substval v, nlv, copy nalpha le)
       end
     | RECORD (rk,vs,lv,le) =>
       let val (nlv,nalpha) = newv(lv, alpha)
       in RECORD(crk ta rk, map substval vs, nlv, copy nalpha le)
       end
     | SELECT (v,i,lv,le) =>
       let val (nlv,nalpha) = newv(lv, alpha)
       in SELECT(substval v, i, nlv, copy nalpha le)
       end
     | RAISE (v,ltys) => RAISE(substval v, map (lt_subst ta) ltys)
     | HANDLE (le,v) => HANDLE(copy alpha le, substval v)
     | BRANCH (po,vs,le1,le2) =>
       BRANCH(cpo po, map substval vs, copy alpha le1, copy alpha le2)
     | PRIMOP (po,vs,lv,le) =>
       let val (nlv,nalpha) = newv(lv, alpha)
       in PRIMOP(cpo po, map substval vs, nlv, copy nalpha le)
       end
    end
in copy' (tmap_sort ta) alpha le
end
fun copyfdec fdec =
    case copy [] M.empty (F.FIX([fdec], F.RET[]))
     of F.FIX([nfdec], F.RET[]) => nfdec
      | _ => bug "copyfdec"

fun freevars lexp =
let val loop = freevars

    fun S_rmv(x, s) = S.delete(s, x) handle NotFound => s

    fun addv (s,F.VAR lv) = S.add(s, lv)
      | addv (s,_) = s
    fun addvs (s,vs) = foldl (fn (v,s) => addv(s, v)) s vs
    fun rmvs (s,lvs) = foldl (fn (l,s) => S_rmv (l, s)) s lvs
    fun singleton (F.VAR v) = S.singleton v
      | singleton _ = S.empty

    fun fpo (fv,(NONE:F.dict option,po,lty,tycs)) = fv
      | fpo (fv,(SOME{default,table},po,lty,tycs)) =
	addvs(addv(fv, F.VAR default), map (F.VAR o #2) table)

    fun fdcon (fv,(s,Access.EXN(Access.LVAR lv),lty)) = addv(fv, F.VAR lv)
      | fdcon (fv,_) = fv

in case lexp
    of F.RET vs => addvs(S.empty, vs)
     | F.LET (lvs,body,le) => S.union(rmvs(loop le, lvs), loop body)
     | F.FIX (fdecs,le) =>
       rmvs((foldl (fn ((_,_,args,body),fv) =>
		    S.union(rmvs(loop body, map #1 args), fv))
		   (loop le) fdecs),
	    map #2 fdecs)
     | F.APP (f,args) => addvs(S.empty, f::args)
     | F.TFN ((tfk,f,args,body),le) => S.union(S_rmv(f, loop le), loop body)
     | F.TAPP (f,args) => singleton f
     | F.SWITCH (v,ac,arms,def) =>
       let fun farm ((dc,le),fv) =
	       let val fvle = loop le
	       in S.union(fv,
			  case dc
			   of PL.DATAcon(dc,_,lv) => fdcon(S_rmv(lv, fvle),dc)
			    | _ => fvle)
	       end
	   val fvs = case def of NONE => singleton v
			       | SOME le => addv(loop le, v)
       in foldl farm fvs arms
       end
     | F.CON (dc,tycs,v,lv,le) => fdcon(addv(S_rmv(lv, loop le), v),dc)
     | F.RECORD (rk,vs,lv,le) => addvs(S_rmv(lv, loop le), vs)
     | F.SELECT (v,i,lv,le) => addv(S_rmv(lv, loop le), v)
     | F.RAISE (v,ltys) => singleton v
     | F.HANDLE (le,v) => addv(loop le, v)
     | F.BRANCH (po,vs,le1,le2) => fpo(addvs(S.union(loop le1, loop le2), vs), po)
     | F.PRIMOP (po,vs,lv,le) => fpo(addvs(S_rmv(lv, loop le), vs),po)
end

(* sameValue : F.value * F.value -> bool
 *  are two FLINT values equal? *)
fun sameValue (v1, v2) =
    (case (v1, v2)
       of (VAR x, VAR y) => LV.same(x, y)
	| (INT n1, INT n2) => (#ty n1 = #ty n2) andalso (#ival n1 = #ival n2)
	| (WORD w1, WORD w2) => (#ty w1 = #ty w2) andalso (#ival w1 = #ival w2)
	| (REAL r1, REAL r2) => RealLit.same(#rval r1, #rval r2)
	| (STRING s1, STRING s2) => (s1 = s2)
	| _ => false
      (* end case *))

(* valueIsVar : LV.lvar -> F.value -> bool
 *  is a value a specific variable? *)
  fun valueIsVar (x: LV.lvar) (VAR y: F.value) = LV.same(x, y)
    | valueIsVar _ _ = false

end (* top-level local *)
end (* structure FlintUtil *)
