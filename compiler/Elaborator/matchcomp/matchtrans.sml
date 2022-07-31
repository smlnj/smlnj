(* Elaborator/matchcomp/matchtrans.sml *)

(* translation functions: translate AS.exp and AS.dec while compiling matches
 *   transExp : region -> AS.exp -> AS.exp
 *   transDec : region -> AS.dec -> AS.dec
 * 
 * top function (only export) is
 *   transMatchDec : AS.dec * StaticEnv.staticEnv * ErrorMsg.errorFn * SourceMap.region -> AS.dec
 *)

structure MatchTrans =
struct

local
  structure T = Types
  structure TU = TypesUtil
  structure BT = BasicTypes
  structure S = Symbol
  structure SP = SymPath
  structure A = Access
  structure V = Variable
  structure AS = Absyn
  structure AU = AbsynUtil
  structure EU = ElabUtil
  structure PP = PrettyPrint
  structure MU = MCUtil
  structure MC = MatchComp
  open Absyn

  fun bug msg = ErrorMsg.impossible ("MatchTrans: " ^ msg)

  val printAndor = MCControl.printAndor
  val printDecisionTree = MCControl.printDecisionTree
  val printMatchAbsyn = MCControl.printMatchAbsyn
  val debugging = MCControl.mcdebugging
  val stats = MCControl.mcstats

  val say = Control_Print.say
  fun newline () = say "\n"
  fun saynl msg = (say msg; newline())
  fun says strings = saynl (concat strings)

  fun dbsay msg =
      if !debugging
      then (say msg; newline())
      else ()
  fun dbsays msgs = dbsay (concat msgs)

  fun ppExp (exp, msg) =
      PP.with_default_pp
          (fn ppstrm =>
	      (PP.string ppstrm msg;
	       PPAbsyn.ppExp (StaticEnv.empty, NONE) ppstrm (exp, 20);
	       PP.newline ppstrm))

  fun ppDec (dec, msg) =
      PP.with_default_pp
          (fn ppstrm =>
	      (PP.string ppstrm msg;
	       PPAbsyn.ppDec (StaticEnv.empty, NONE) ppstrm (dec, 20);
	       PP.newline ppstrm))

  fun ppPat pat =
      PP.with_default_pp(fn ppstrm => PPAbsyn.ppPat StaticEnv.empty ppstrm (pat, 20))

in

(* transMatchDec : AS.dec * StaticEnv.staticEnv * ErrorMsg.errorFn * SourceMap.region -> AS.dec *)
fun transMatchDec (dec, env, errorFn, region) =
let

(* simpleVALdec : V.var * AS.exp * T.tyvar list -> AS.dec *)
fun simpleVALdec (var, exp, boundtvs) =
    VALdec [VB{pat = VARpat var, exp = exp,
	       typ = V.varType var, boundtvs = boundtvs, tyvars = ref nil}]

(* transRules : region -> AS.rule list -> (AS.pat * AS.exp) list *)
fun transRules region rules =  (* apply fillPat to rule patterns, translate rhss *)
    map (fn RULE(pat,exp) => (EU.fillPat pat, transExp region exp)) rules

(* transExp : region -> AS.exp -> AS.exp *)
and transExp region exp =
    let val _ = dbsay ">> transExp"
        fun trans region exp =
            (case exp
	       of RECORDexp fields =>
		    RECORDexp (map (fn (numberedLabel, exp) => (numberedLabel, trans region exp))
				   fields)
		| VECTORexp (exps,ty) => VECTORexp(map (trans region) exps, TU.prune ty)
		| APPexp (rator, rand) => APPexp (trans region rator, trans region rand)
		| FNexp (rules, argTy, resTy) =>
		    let val RULE(pat1,exp1)::_ = rules (* debugging *)
			val _ = (if !debugging
				 then (say "transExp:FNexp:pat1 = "; ppPat pat1; newline())
				 else ())
			val (bodyExp, matchVar) =
			    MC.matchCompile (transRules region rules, argTy, resTy, errorFn, region, env)
		     in FNexp ([RULE(VARpat matchVar, bodyExp)], TU.prune argTy, TU.prune resTy)
		    end
		| HANDLEexp (baseExp, (rules, argTy, resTy)) =>
		    let val (handlerBody, matchVar) =
			    MC.handlerCompile (transRules region rules, resTy, errorFn, region, env)
			val matchRule = RULE(VARpat matchVar, handlerBody)
		     in HANDLEexp(trans region baseExp, ([matchRule], TU.prune argTy, TU.prune resTy))
		    end
		| CASEexp (scrutinee, (rules, scrutTy, resTy)) =>
		    let val RULE(pat1,exp1)::_ = rules (* debugging *)
			val _ = (if !debugging
				 then (say "transExp:CASEexp:pat1 = "; ppPat pat1; newline())
				 else ())
			val (caseBody, matchVar) =
			    MC.matchCompile (transRules region rules, scrutTy, resTy, errorFn, region, env)
		    in LETexp(VALdec[VB{pat = VARpat matchVar,
					exp = trans region scrutinee,
					typ = scrutTy,
					boundtvs = nil,
					tyvars = ref nil}],
			      caseBody)
		    end
		| RAISEexp (exp, ty) => RAISEexp (trans region exp, ty) (* original source raise *)
		| IFexp {test, thenCase, elseCase} =>
		  IFexp {test = trans region test,
			 thenCase = trans region thenCase,
			 elseCase = trans region elseCase}
		| ANDALSOexp (exp1, exp2) => ANDALSOexp (trans region exp1, trans region exp2)
		| ORELSEexp (exp1, exp2) => ORELSEexp (trans region exp1, trans region exp2)
		| WHILEexp { test, expr } => WHILEexp{ test = trans region test,
						       expr = trans region expr}
		| LETexp (dec, exp) => LETexp(transDec region dec, trans region exp)
		| SEQexp exps => SEQexp (map (trans region) exps)
		| CONSTRAINTexp (exp, ty) => CONSTRAINTexp (trans region exp, ty)
		| MARKexp (exp, region) => MARKexp (trans region exp, region)
		| _ => exp)
		  (* (VARexp _ | CONexp _ | NUMexp _ | REALexp _ | STRINGexp _  | CHARexp _ |
		   *  RSELECTexp _ | VSELECTexp _ | SWITCHexp _ | VSWITCHexp) => exp *)
     in trans region exp
    end (* transExp *)

(* transDec : AS.dec -> AS.dec *)
and transDec (region: SourceMap.region) (dec: AS.dec): AS.dec =
    let fun transDec0 (region: SourceMap.region) (dec: AS.dec) : AS.dec =
            (case dec
	      of VALdec vbs => transVBs region vbs
	       | VALRECdec rvbs => VALRECdec (map (transRVB region) rvbs)
	       | DOdec exp => DOdec (transExp region exp)
	       | LOCALdec (innerDec, outerDec) =>
		   LOCALdec (transDec0 region innerDec, transDec0 region outerDec)
	       | SEQdec decs => SEQdec (map (transDec0 region) decs)
	       | ABSTYPEdec {abstycs, withtycs, body} =>
		   ABSTYPEdec {abstycs = abstycs, withtycs = withtycs, body = transDec0 region body}
	       | MARKdec (dec, region) => MARKdec (transDec0 region dec, region)
	       | _ => dec) (* transDec called locally for LETSTR, LETFCT, STRdec, FCTdec *)

	(* transvb : AS.vb -> AS.dec *)
	(* translation of vb to dec
	 * -- We can get away without a d (DB depth) parameter, leaving it to Translate.
	 * -- Looks like we can get away with never dealing with an internal mvar in the match.
	 * -- We need the type of the pat, which is available as the typ field of VB.
	 * -- Do we need an absyn equivalent to mkPE, say transPolyExp? We don't have an equivalent
	 *      to TFN in absyn -- yet! [... deal with this in type system rewrite].
         * -- following revmc translate.sml for treatment of VB: alpha-convert pattern, bind match
	 *    to produce value of the tuple of pattern variables *)
	and transVB region (VB{pat, exp = defExp, typ, boundtvs, tyvars}) =
	    (* match compile [(pat, defExp)] if pat is nontrivial (not a var);
	     * -- check what the match compiler does with (single) irrefutable patterns
	     *    DONE -- claim it does the right thing (explain?). *)
	    (if !debugging
	     then (say "transVB:pat = "; ppPat pat; ppExp (defExp, "transVB:exp = "))
	     else ();
	     case EU.fillPat(AU.stripPatMarks pat)   (* does fillPat also strip MARKpats? *)
	       of (WILDpat | CONSTRAINTpat(WILDpat, _)) =>  (* WILDpat pattern -- replace with DOdec *)
		    DOdec (transExp region defExp)
		| (VARpat var | CONSTRAINTpat(VARpat var, _)) =>  (* var pattern -- only surviving VB case *)
		    VALdec([VB{pat = VARpat var, exp = transExp region defExp,
			       typ = typ, boundtvs = boundtvs, tyvars = tyvars}])
		| pat =>  (* compound, possibly refutable, pattern. Works for simple constants. *)
		  let val (newpat,oldpvars,newpvars) = EU.aconvertPat pat
		      (* this is the only call of aconvertPat; it replaces pattern variables with
		       * new versions with fresh lvar access values. This ensures that
		       * original pvars are only bound once in the generated absyn (and hence in the
		       * translated PLambda.lexp [example showing this is necessary?] *)
		  in case newpvars
		       of nil =>   (* "constant" pattern, no pattern variables
				    *  need to generate and execute matchExp in case of match failure
				    *  -- not necessary if pat is irrefutable *)
			  if TU.refutable pat  (* true if pat ontains any OR pats *)
			  then let val bindRules = [(pat, AU.unitExp)]
				   val (matchExp, rootVar) =
				       MC.bindCompile (bindRules, typ, BT.unitTy,  (* typ = pattern type *)
						       errorFn, region, env)
				      (* bindCompile could tell for sure if pat is refutable, even with OR pats,
				       * -- or just check whether matchExp could raise Bind! *)
				   val resultDec =
				       LOCALdec(simpleVALdec(rootVar, transExp region defExp, nil), (* for effect *)
						DOdec matchExp) (* also for effect *)
			       in if !debugging then ppDec (resultDec, "transVB (no var): \n") else ();
				  resultDec
			       end
			  else DOdec (transExp region defExp)
			| [newpvar] =>     (* single pattern variable *)
			  let val pvarTy = TU.dePolyVar newpvar (* was V.varType newpvar *)
			      val (matchExp, rootVar) =
				  MC.bindCompile ([(newpat, EU.varToExp newpvar)],
						  typ, pvarTy, errorFn, region, env)
			   in LOCALdec(simpleVALdec(rootVar, transExp region defExp, nil),
				       simpleVALdec(hd oldpvars, matchExp, boundtvs))
			  end
			| _ =>  (* "multiple" pattern variables (1 or more) *)
			  let val pvarsTy = BT.tupleTy (map TU.dePolyVar oldpvars)  (* was map V.varType oldpvars *)
			      val newPvarTuple = EU.TUPLEexp(map EU.varToExp newpvars)
			      val bindRules = [(newpat, newPvarTuple)]  (* single rule match, with new pvars *)
			      val (matchExp, rootVar) =
				  MC.bindCompile (bindRules, typ, pvarsTy, errorFn, region, env)
				  (* rootVar will be bound to MC-translation of exp *)
			      val ptupleVar = V.VALvar{path = SP.SPATH [S.varSymbol "<ptupleVar>"],
						       typ = ref(pvarsTy),
						       btvs = ref(boundtvs),  (* possibly polymorphic *)
						       access = A.LVAR(LambdaVar.mkLvar()),
						       prim = PrimopId.NonPrim}
			      fun varSelDec (n, pvar) = VARSELdec (pvar, ptupleVar, n)
				    (* defining a pattern var by (record) selection from a
				     * var (ptupleVar) bound to the tuple of all the pattern
				     * var values; the btvs of each pvar is a subset
				     * of the btvs of ptupleVar, which is equal to boundtvs of the VB. *)
			      val resultDec =
				  LOCALdec(SEQdec [simpleVALdec(rootVar, transExp region defExp, nil),
						   simpleVALdec(ptupleVar, matchExp, boundtvs)],
					   SEQdec (List.mapi varSelDec oldpvars))
					  (* rebinding orig pattern variables *)
			   in if !debugging
			      then ppDec (resultDec, "transVB (multiple vars): \n")
			      else ();
			      resultDec
			  end
		  end (* pat case *))

	    (* NOTE: Given the way Translate.transDec deals with LOCALdec (i.e. not
             * hiding the local decls), we use a single SEQ encompassing all the
             * declarations. *)

	(* transVBs : region -> AS.vb list -> AS.dec *)
	and transVBs region vbs = 
            (case vbs
	      of nil => bug "transVBs: nil"  (* expect at least one *)
	       | [vb] => transVB region vb
	       | _ => SEQdec (map (transVB region) vbs))

	and transRVB region
		     (RVB{var: V.var, exp: exp, resultty: T.ty option, tyvars: T.tyvar list ref}) =
	    (dbsays [">> transRVB:var = ", Symbol.name (V.varName var)];
	     RVB {var = var, exp = transExp region exp, resultty = resultty, tyvars = tyvars})

     in transDec0 region dec
    end (* transDec *)

 in transDec region dec
end (* transMatchDec *)

end (* local *)
end (* structure MatchTrans *)
