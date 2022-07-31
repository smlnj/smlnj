(* check-unused.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Check for unused variables.
 *
 * This module is based on an implementation by Jacob Van Buren
 *
 *	https://gist.github.com/jvanburen/2ef1a722fba13fc169b670ba6392da52
 *)

structure CheckUnused : sig

    val check : ErrorMsg.errorFn -> Absyn.dec -> unit

  end = struct

    structure V = Variable
    structure A = Absyn

  (* debugging *)
    val say = Control_Print.say
    val debugging = ElabControl.etopdebugging (* default false *)
    fun debugmsg (msg: string) =
          if !debugging then (say msg; say "\n") else ()

  (* sets of used variables.  We assume that all variables in the set
   * are `VALvar` variables with access=LVAR lv, which use that as the key.
   *)
    structure VSet : sig
	type set
	val empty : set
	val add : set * V.var -> set
	val member : set * V.var -> bool
	val delete : set * V.var -> set
	val toList : set -> LambdaVar.lvar list
      end = struct
	type set = LambdaVar.Set.set
	val empty = LambdaVar.Set.empty
	fun add (set, V.VALvar{access=Access.LVAR lv, ...}) =
	      LambdaVar.Set.add (set, lv)
	  | add (set, _) = set
	fun member (set, V.VALvar{access=Access.LVAR lv, ...}) =
	      LambdaVar.Set.member (set, lv)
	  | member (set, _) = false (* QUESTION: should this case be a compiler error? *)
	fun delete (set, V.VALvar{access=Access.LVAR lv, ...}) =
	      LambdaVar.Set.delete (set, lv)
	  | delete (set, _) = set
	val toList = LambdaVar.Set.toList
      end

    fun debugPrSet (msg, used) = debugmsg (concat [
	    msg, "{", String.concatWithMap "," LambdaVar.lvarName (VSet.toList used),
	    "}\n"
	  ])

    fun check err = let
	  fun warning (region, V.VALvar{path, access, ...}) =
		err region ErrorMsg.WARN (concat [
		    "variable ", Symbol.name (SymPath.first path),
		    " is defined but not used"
		  ]) ErrorMsg.nullErrorBody
	    | warning _ = () (* should never happen *)
	(* compute the local variables used by an expression and also check any
	 * nested declarations.
	 *)
	  fun chkExp (region, e, used) = (case e
		 of A.VARexp(ref x, _) => VSet.add(used, x)
		  | A.CONexp _ => used
		  | A.NUMexp _ => used
		  | A.REALexp _ => used
		  | A.STRINGexp _ => used
		  | A.CHARexp _ => used
		  | A.RECORDexp flds => List.foldl
		      (fn ((_, e), used) => chkExp(region, e, used))
			used flds
		  | A.RSELECTexp(var, index) => raise Fail "unexpected RSELECTexp"
		  | A.VSELECTexp(exp, _, index) => raise Fail "unexpected VSELECTexp"
		  | A.VECTORexp(es, _) => List.foldl
		      (fn (e, used) => chkExp(region, e, used))
			used es
		  | A.APPexp(e1, e2) =>
		      chkExp(region, e2, chkExp(region, e1, used))
		  | A.HANDLEexp(e, (rules, _, _)) =>
		      List.foldl (chkRule region) (chkExp(region, e, used)) rules
		  | A.RAISEexp(e, _) => chkExp(region, e, used)
		  | A.CASEexp(e, (rules,_,_)) =>
		      List.foldl (chkRule region) (chkExp(region, e, used)) rules
		  | A.IFexp{test, thenCase, elseCase} =>
		      chkExp(region, elseCase,
			chkExp(region, thenCase,
			  chkExp (region, test, used)))
		  | A.ANDALSOexp(e1, e2) =>
		      chkExp(region, e2, chkExp(region, e1, used))
		  | A.ORELSEexp(e1, e2) =>
		      chkExp(region, e2, chkExp(region, e1, used))
		  | A.WHILEexp{test, expr} =>
		      chkExp(region, expr, chkExp(region, test, used))
		  | A.FNexp(rules, _, _) => List.foldl (chkRule region) used rules
		  | A.LETexp(d, e) =>
		      chkDec (region, false, d, chkExp(region, e, used))
		  | A.SEQexp es => List.foldl
		      (fn (e, used) => chkExp(region, e, used))
			used es
		  | A.CONSTRAINTexp(e, _) => chkExp(region, e, used)
		  | A.MARKexp(e, region) => chkExp(region, e, used)
		  | A.SWITCHexp _ => raise Fail "unexpected SWITCHexp"
		  | A.VSWITCHexp _ => raise Fail "unexpected VSWITCHexp"
		(* end case *))
	  and chkRule region (A.RULE(p, e), used) =
		chkPat false (region, p, chkExp (region, e, used))
	(* check if any of the variables bound by a pattern are unused *)
	  and chkPat top = let
		fun chk (region, p, used) = (case p
		       of A.VARpat x => if VSet.member(used, x)
			    then VSet.delete(used, x)
			    else (
			    (* we do not warn about unused top-level vars *)
			      if top then () else warning (region, x);
			      used)
			| A.RECORDpat{fields, ...} => List.foldl
			    (fn ((_, p), used) => chk (region, p, used))
			      used fields
			| A.APPpat(_, _, p) => chk (region, p, used)
			| A.CONSTRAINTpat(p, _) => chk (region, p, used)
			| A.LAYEREDpat(p1, p2) =>
			    chk (region, p2, chk (region, p1, used))
			| A.ORpat(p, _) =>
			  (* Since the same variables are bound on both sides
			   * of the "|", we only need to check one side!
			   *)
			    chk (region, p, used)
			| A.VECTORpat(ps, _) =>
			    List.foldl (fn (p, used) => chk (region, p, used)) used ps
			| A.MARKpat(p, region) => chk (region, p, used)
			| _ => used
		      (* end case *))
		in
		  chk
		end
	(* check a declaration and return the set of used local variables.
	 * For top-level decls we do not warn about unused variables,
	 * since they may be exported.
	 * TODO: check unused top-level variables against the signature and
	 * report unused variables that are not exported.
	 *)
	  and chkDec (region, top, d, used) = (case d
		 of A.VALdec vbs => let
		      fun chkVB (A.VB{pat, exp, ...}, used) =
			    chkPat top (region, pat, chkExp (region, exp, used))
		      in
			List.foldr chkVB used vbs
		      end
		  | A.VALRECdec rvbs => let
		      fun chk1 (A.RVB{exp, ...}, used) = chkExp(region, exp, used)
		      fun chk2 (A.RVB{var, ...}, used) = if VSet.member(used, var)
			    then VSet.delete(used, var)
			    else (
			      if top then () else warning (region, var);
			      used)
		      in
			List.foldl chk2 (List.foldl chk1 used rvbs) rvbs
		      end
		  | A.DOdec e => chkExp (region, e, used)
		  | A.TYPEdec _ => used
		  | A.DATATYPEdec _ => used
		  | A.ABSTYPEdec{body, ...} => chkDec (region, top, body, used)
		  | A.EXCEPTIONdec _ => used
		  | A.STRdec strbs => let
		      fun chk (A.STRB{def, ...}, used) = chkStrExp (region, def, used)
		      in
			List.foldl chk used strbs
		      end
		  | A.FCTdec fctbs => let
		      fun chk (A.FCTB{def, ...}, used) = chkFctExp (region, def, used)
		      in
			List.foldl chk used fctbs
		      end
		  | A.SIGdec _ => used
		  | A.FSIGdec _ => used
		  | A.OPENdec _ => used
		  | A.LOCALdec(d1, d2) =>
		      chkDec(region, false, d1, chkDec(region, top, d2, used))
		  | A.SEQdec ds =>
		    (* process right-to-left *)
		      List.foldr (fn (d, used) => chkDec (region, top, d, used)) used ds
		  | A.OVLDdec _ => used
		  | A.FIXdec _ => used
		  | A.MARKdec(d, region) => chkDec (region, top, d, used)
		(* end case *))
	  and chkStrExp (region, sexp, used) = (case sexp
		 of A.VARstr _ => used
		  | A.STRstr _ => used
		  | A.APPstr _ => used
		  | A.LETstr(dec, sexp) =>
		      chkStrExp (region, sexp, chkDec(region, true, dec, used))
		  | A.MARKstr(sexp, region) => chkStrExp (region, sexp, used)
		(* end case *))
	  and chkFctExp (region, fexp, used) = (case fexp
		 of A.VARfct _ => used
		  | A.FCTfct{def, ...} => chkStrExp (region, def, used)
		  | A.LETfct(dec, fexp) =>
		      chkFctExp (region, fexp, chkDec(region, true, dec, used))
		  | A.MARKfct(fexp, region) => chkFctExp (region, fexp, used)
		(* end case *))
	  in
	    fn dec => if !ElabControl.unusedWarn
		then (
		  debugmsg ">>checkUnused";
		  chkDec (SourceMap.nullRegion, true, dec, VSet.empty);
		  debugmsg "<<checkUnused")
		else ()
	  end

  end
