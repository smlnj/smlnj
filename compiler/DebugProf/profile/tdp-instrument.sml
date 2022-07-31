(* tdp-instrument.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Perform Absyn annotations for tracing- debugging- and profiling support.
 *   This adds a tdp_enter at the entry point of each FNexp,
 *   a push-restore sequence (tdp_push) at each non-tail call site of
 *   a non-primitive function, and a save-restore sequence to each HANDLEexp.
 *
 * Author: Matthias Blume (blume@tti-c.org)
 *)
local
    structure A = Absyn
    structure T = Types
    structure SE = StaticEnv
    structure SP = SymPath
    structure EM = ErrorMsg
    structure V = Variable
    structure BT = BasicTypes
    structure AU = AbsynUtil
in

signature TDP_INSTRUMENT = sig
    val enabled : bool ref
    val instrument :
	(Symbol.symbol -> bool) ->	(* isSpecial *)
	SE.staticEnv * A.dec CompInfo.compInfo -> A.dec -> A.dec
end

structure TDPInstrument :> TDP_INSTRUMENT = struct

    val priority = [10, 1]
    val obscurity = 1
    val prefix = "tdp"

    val registry = ControlRegistry.new { help = "tracing/debugging/profiling" }

    val _ = BasicControl.nest (prefix, registry, priority)

    val bool_cvt = ControlUtil.Cvt.bool

    val enabled = SMLofNJ.Internals.TDP.mode

    val p = 0
    val ctl = Controls.control { name = "instrument",
				 pri = [p],
				 obscurity = obscurity,
				 help = "trace-, debug-, and profiling \
					\instrumentation mode",
				 ctl = enabled }
    val _ = ControlRegistry.register
		registry
		{ ctl = Controls.stringControl bool_cvt ctl,
		  envName = SOME "TDP_INSTRUMENT" }

    fun impossible s = EM.impossible ("TDPInstrument: " ^ s)

    infix -->
    val op --> = BT.-->

    val i_i_Ty = BT.intTy --> BT.intTy
    val ii_u_Ty = BT.tupleTy [BT.intTy, BT.intTy] --> BT.unitTy
    val ii_u_u_Ty = ii_u_Ty --> BT.unitTy
    val u_u_Ty = BT.unitTy --> BT.unitTy
    val u_u_u_Ty = BT.unitTy --> u_u_Ty
    val iiis_u_Ty =
	BT.tupleTy [BT.intTy, BT.intTy, BT.intTy, BT.unitTy] --> BT.unitTy

    fun instrument0 isSpecial (senv, cinfo: A.dec CompInfo.compInfo) d = let

	val matchstring = #errorMatch cinfo

	val mkv = #mkLvar cinfo

	fun tmpvar (n, t) = let
	    val sy = Symbol.varSymbol n
	in
	    V.VALvar { access = Access.namedAcc (sy, mkv),
	    		prim = PrimopId.NonPrim,
			path = SP.SPATH [sy], typ = ref t, btvs = ref []}
	end

(*
	val isSpecial = let
	    val l = [SpecialSymbols.paramId,
		     SpecialSymbols.functorId,
		     SpecialSymbols.hiddenId,
		     SpecialSymbols.tempStrId,
		     SpecialSymbols.tempFctId,
		     SpecialSymbols.fctbodyId,
		     SpecialSymbols.anonfsigId,
		     SpecialSymbols.resultId,
		     SpecialSymbols.returnId,
		     SpecialSymbols.internalVarId]
	in
	    fn s => List.exists (fn s' => Symbol.eq (s, s')) l
	end
*)

	fun cons (s, []) = if isSpecial s then [] else [(s, 0)]
	  | cons (s, l as ((s', m) :: t)) =
	      if isSpecial s then l
	      else if Symbol.eq (s, s') then (s, m+1) :: t
	      else (s, 0) :: l

	fun getCoreVal s = CoreAccess.getVar senv [s]
	fun getCoreCon s = CoreAccess.getCon senv [s]

	val tdp_reserve = getCoreVal "tdp_reserve"
	val tdp_register = getCoreVal "tdp_register"
	val tdp_save = getCoreVal "tdp_save"
	val tdp_push = getCoreVal "tdp_push"
	val tdp_nopush = getCoreVal "tdp_nopush"
	val tdp_enter = getCoreVal "tdp_enter"
	val matchcon = getCoreCon "Match"

	val tdp_register_var = tmpvar ("<tdp_register>", iiis_u_Ty)
	val tdp_save_var = tmpvar ("<tdp_save>", u_u_u_Ty)
	val tdp_push_var = tmpvar ("<tdp_push>", ii_u_u_Ty)
	val tdp_nopush_var = tmpvar ("<tdp_nopush>", ii_u_Ty)
	val tdp_enter_var = tmpvar ("<tdp_enter>", ii_u_Ty)
	val tdp_reserve_var = tmpvar ("<tdp_reserve>", i_i_Ty)
	val tdp_module_var = tmpvar ("<tdp_module>", BT.intTy)

	fun VARexp v = A.VARexp (ref v, [])
	fun INTexp i = A.NUMexp("<lit>", {ival = IntInf.fromInt i, ty = BT.intTy})

	val uExp = AU.unitExp
	val pushexp = A.APPexp (VARexp tdp_push_var, uExp)
	val saveexp = A.APPexp (VARexp tdp_save_var, uExp)

	fun mkmodidexp fctvar id =
	    A.APPexp (VARexp fctvar,
		      AU.TUPLEexp [VARexp tdp_module_var, INTexp id])

	val mkenterexp = mkmodidexp tdp_enter_var
	val mkpushexp = mkmodidexp tdp_push_var
	val mknopushexp = mkmodidexp tdp_nopush_var

	fun mkregexp (k, id, s) =
	    A.APPexp (VARexp tdp_register_var,
		      AU.TUPLEexp [VARexp tdp_module_var,
				   INTexp k, INTexp id, A.STRINGexp s])

	val regexps = ref []
	val next = ref 0

	fun newid k s =
	    let val id = !next
	    in
		next := id + 1;
		regexps := mkregexp (k, id, s) :: !regexps;
		id
	    end

	val mkenter = mkenterexp o newid Core.tdp_idk_entry_point
	val mkpush = mkpushexp o newid Core.tdp_idk_non_tail_call
	val mknopush = mknopushexp o newid Core.tdp_idk_tail_call

	fun VALdec (v, e) =
	    A.VALdec [A.VB { pat = A.VARpat v, exp = e,
			     typ = T.UNDEFty, tyvars = ref [], boundtvs = [] }]
	fun LETexp (v, e, b) = A.LETexp (VALdec (v, e), b)
	fun AUexp v = A.APPexp (VARexp v, uExp)	(* apply to unit *)

	fun is_prim_exp (A.VARexp (ref (V.VALvar v), _)) =
              (case #prim v
                 of PrimopId.Prim _ => true
                  | PrimopId.NonPrim => false)
	  | is_prim_exp (A.CONexp _) = true
	  | is_prim_exp (A.CONSTRAINTexp (e, _)) = is_prim_exp e
	  | is_prim_exp (A.MARKexp (e, _)) = is_prim_exp e
	  | is_prim_exp _ = false

	fun is_raise_exp (A.RAISEexp (e, _)) =
	    let fun is_simple_exn (A.VARexp _) = true
		  | is_simple_exn (A.CONexp _) = true
		  | is_simple_exn (A.CONSTRAINTexp (e, _)) = is_simple_exn e
		  | is_simple_exn (A.MARKexp (e, _)) = is_simple_exn e
		  | is_simple_exn (A.RAISEexp (e, _)) =
		    is_simple_exn e	(* !! *)
		  | is_simple_exn _ = false
	    in
		is_simple_exn e
	    end
	  | is_raise_exp (A.MARKexp (e, _) |
			  A.CONSTRAINTexp (e, _) |
			  A.SEQexp [e]) = is_raise_exp e
	  | is_raise_exp _ = false

	fun mkDescr ((n, r), what) =
	    let fun name ((s, 0), a) = Symbol.name s :: a
		  | name ((s, m), a) = Symbol.name s :: "[" ::
				       Int.toString (m + 1) :: "]" :: a
		fun dot ([z], a) = name (z, a)
		  | dot (h :: t, a) = dot (t, "." :: name (h, a))
		  | dot ([], a) = impossible (what ^ ": no path")
		val ms = matchstring r
	    in
		concat (ms :: ": " :: dot (n, []))
	    end

	fun i_exp _ loc (A.RECORDexp l) =
	      A.RECORDexp (map (fn (l, e) => (l, i_exp false loc e)) l)
	  | i_exp _ loc (exp as (A.RSELECTexp _)) = exp
	  | i_exp _ loc (exp as (A.VSELECTexp _)) = exp
	  | i_exp _ loc (A.VECTORexp (l, t)) =
	      A.VECTORexp (map (i_exp false loc) l, t)
	  | i_exp tail loc (e as A.APPexp (f, a)) =
	    let val mainexp =  A.APPexp (i_exp false loc f, i_exp false loc a)
	    in
		if is_prim_exp f then mainexp
		else if tail then A.SEQexp [mknopush (mkDescr (loc, "GOTO")),
					    mainexp]
		else let val ty = Reconstruct.expType e
			 val result = tmpvar ("tmpresult", ty)
			 val restore = tmpvar ("tmprestore", u_u_Ty)
			 val pushexp = mkpush (mkDescr (loc, "CALL"))
		     in
			 LETexp (restore, pushexp,
				 LETexp (result, mainexp,
					 A.SEQexp [AUexp restore,
						   VARexp result]))
		     end
	    end
	  | i_exp tail loc (A.HANDLEexp (e, (rl, t1, t2))) =
	    let val restore = tmpvar ("tmprestore", u_u_Ty)
		fun rule (r as A.RULE (p, e)) =
		    if is_raise_exp e then r
		    else A.RULE (p, A.SEQexp [AUexp restore, i_exp tail loc e])
	    in
		LETexp (restore, saveexp,
			A.HANDLEexp (i_exp false loc e, (map rule rl, t1, t2)))
	    end
	  | i_exp _ loc (A.RAISEexp (e, t)) =
	      A.RAISEexp (i_exp false loc e, t)
	  | i_exp tail loc (A.CASEexp (e, (rules, lhsTy, rhsTy))) =
	      A.CASEexp (i_exp false loc e, (map (i_rule tail loc) rules, lhsTy, rhsTy))
	  | i_exp tail loc (A.IFexp { test, thenCase, elseCase }) =
	      A.IFexp { test = i_exp false loc test,
			thenCase = i_exp tail loc thenCase,
			elseCase = i_exp tail loc elseCase }
	  | i_exp tail loc (A.ANDALSOexp (e1, e2)) =
	      A.ANDALSOexp (i_exp false loc e1, i_exp tail loc e2)
	  | i_exp tail loc (A.ORELSEexp (e1, e2)) =
	      A.ORELSEexp (i_exp false loc e1, i_exp tail loc e2)
	  | i_exp _ loc (A.WHILEexp { test, expr }) =
	      A.WHILEexp { test = i_exp false loc test,
			   expr = i_exp false loc expr }
	  | i_exp tail loc (A.FNexp (rl, lhsTy, rhsTy)) =
	    let val enterexp = mkenter (mkDescr (loc, "FN"))
		val arg = tmpvar ("fnvar", lhsTy)
		val rl' = map (i_rule true loc) rl
		val re = let val A.RULE (_, lst) = List.last rl
			     val t = Reconstruct.expType lst
			 in
			     A.RAISEexp (A.CONexp (matchcon, []), rhsTy)
			 end
	    in
		A.FNexp ([A.RULE (A.VARpat arg,
				  A.SEQexp [enterexp,
					    A.CASEexp (A.VARexp (ref arg, []), (rl',lhsTy,rhsTy))]),
			  A.RULE (A.WILDpat, re)],
			 lhsTy, rhsTy)
	    end
	  | i_exp tail loc (A.LETexp (d, e)) =
	      A.LETexp (i_dec loc d, i_exp tail loc e)
	  | i_exp tail loc (A.SEQexp l) =
	      A.SEQexp (#1 (foldr (fn (e, (l, t)) =>
				      (i_exp t loc e :: l, false))
				  ([], tail) l))
	  | i_exp tail loc (A.CONSTRAINTexp (e, t)) =
	      A.CONSTRAINTexp (i_exp tail loc e, t)
	  | i_exp tail (n, _) (A.MARKexp (e, r)) =
	      A.MARKexp (i_exp tail (n, r) e, r)
	  | i_exp _ _ (e as (A.VARexp _ | A.CONexp _ | A.NUMexp _ |
			     A.REALexp _ | A.STRINGexp _ | A.CHARexp _)) = e

	and i_dec loc (A.VALdec l) = A.VALdec (map (i_vb loc) l)
	  | i_dec loc (A.VALRECdec l) = A.VALRECdec (map (i_rvb loc) l)
	  | i_dec loc (A.ABSTYPEdec { abstycs, withtycs, body }) =
	      A.ABSTYPEdec { abstycs = abstycs, withtycs = withtycs,
			     body = i_dec loc body }
	  | i_dec loc (A.EXCEPTIONdec l) = A.EXCEPTIONdec (map (i_eb loc) l)
	  | i_dec loc (A.STRdec l) = A.STRdec (map (i_strb loc) l)
	  | i_dec loc (A.FCTdec l) = A.FCTdec (map (i_fctb loc) l)
	  | i_dec loc (A.LOCALdec (d, d')) =
	      A.LOCALdec (i_dec loc d, i_dec loc d')
	  | i_dec loc (A.SEQdec l) = A.SEQdec (map (i_dec loc) l)
	  | i_dec (n, _) (A.MARKdec (d, r)) = A.MARKdec (i_dec (n, r) d, r)
	  | i_dec _ (d as (A.TYPEdec _ | A.DATATYPEdec _ |
			   A.SIGdec _ | A.FSIGdec _ | A.OPENdec _ |
			   A.OVLDdec _ | A.FIXdec _)) = d

	and i_rule tail loc (A.RULE (p, e)) = A.RULE (p, i_exp tail loc e)

	and i_vb (n, r) (vb as A.VB { pat, exp, typ, boundtvs, tyvars }) =
	    let fun gv (A.VARpat v) = SOME v
		  | gv (A.CONSTRAINTpat (p, _)) = gv p
		  | gv (A.LAYEREDpat (p, p')) =
		      (case gv p of
			   SOME v => SOME v
			 | NONE => gv p')
		  | gv _ = NONE
		fun recur n = A.VB { pat = pat, exp = i_exp false (n, r) exp,
				     typ = typ, boundtvs = boundtvs, tyvars = tyvars }
	    in
		case gv pat of
		    SOME (V.VALvar { path = SP.SPATH [x], prim, ... }) =>
                      (case prim
                        of PrimopId.Prim _ => vb
                         | PrimopId.NonPrim => recur (cons (x, n)))
		  | SOME (V.VALvar { prim, ... }) =>
                      (case prim
                        of PrimopId.Prim _ => vb
                         | PrimopId.NonPrim => recur n)
		  | _ => recur n
	    end

	and i_rvb (n, r) (A.RVB { var, exp, resultty, tyvars }) =
	    let val x =
		    case var of
			V.VALvar { path = SymPath.SPATH [x], ... } => x
		      | _ => impossible "VALRECdec"
	    in
		A.RVB { var = var, exp = i_exp false (cons (x, n), r) exp,
			resultty = resultty, tyvars = tyvars }
	    end

	and i_eb loc (A.EBgen { exn, etype }) =
	      A.EBgen { exn = exn, etype = etype }
	  | i_eb _ eb = eb

	and i_strb (n, r) (A.STRB { name, str, def }) =
	    A.STRB { name = name, str = str,
		     def = i_strexp (cons (name, n), r) def }

	and i_fctb (n, r) (A.FCTB { name, fct, def }) =
	    A.FCTB { name = name, fct = fct,
		     def = i_fctexp (cons (name, n), r) def }

	and i_strexp loc (A.LETstr (d, s)) =
	      A.LETstr (i_dec loc d, i_strexp loc s)
	  | i_strexp (n, _) (A.MARKstr (s, r)) =
	      A.MARKstr (i_strexp (n, r) s, r)
	  | i_strexp _ s = s

	and i_fctexp loc (A.FCTfct { param, argtycs, def }) =
	      A.FCTfct { param = param, argtycs = argtycs,
			 def = i_strexp loc def }
	  | i_fctexp loc (A.LETfct (d, f)) =
	      A.LETfct (i_dec loc d, i_fctexp loc f)
	  | i_fctexp (n, _) (A.MARKfct (f, r)) =
	      A.MARKfct (i_fctexp (n, r) f, r)
	  | i_fctexp _ f = f

	val d' = i_dec ([], (0, 0)) d
    in
	A.LOCALdec (A.SEQdec [VALdec (tdp_reserve_var, VARexp tdp_reserve),
			      VALdec (tdp_module_var,
				      A.APPexp (VARexp tdp_reserve_var,
						INTexp (!next))),
			      VALdec (tdp_save_var, AUexp tdp_save),
			      VALdec (tdp_push_var, AUexp tdp_push),
			      VALdec (tdp_nopush_var, AUexp tdp_nopush),
			      VALdec (tdp_register_var, AUexp tdp_register),
			      VALdec (tdp_enter_var,
				      A.SEQexp (!regexps @
						[AUexp tdp_enter]))],
		    d')
    end

    fun instrument isSpecial params d =
	if !enabled then
	    instrument0 isSpecial params d
	    handle NoCore => d		(* this takes care of core.sml *)
	else d
end

end (* local *)
