(* Elaborator/matchcomp/preprocessing.sml *)

(* preprocessing patterns to expand OR patterns into additional rules sharing original RHS *)

structure Preprocessing =
struct

local

  structure T = Types
  structure TU = TypesUtil
  structure BT = BasicTypes
  structure V  = Variable
  structure AS = Absyn
  structure AU = AbsynUtil
  structure P  = Paths
  structure MC = MCCommon
  structure MU = MCUtil
  open Absyn MCCommon

  fun bug msg = ErrorMsg.impossible ("Preprocessing: " ^ msg)

in

(* info relating post-expansion rule numbers with the shared pre-expansion
 * rule *)
type ruleInfo =
     (P.path list  (* the andor paths of the pattern-bound variables (pvars) *)
    * V.var        (* the var naming the abstracted rhs function *)
    * ruleno)      (* the original, pre-expansion, ruleno *)

type ruleMap = int -> ruleInfo

fun allConses (hds, tls) =
      List.concat (map (fn hd => (map (fn tl => hd::tl) tls)) hds)

(* orExpand : pat -> pat list *)
(* does not deal with MARKpat *)
fun orExpand (ORpat(pat1,pat2)) =
      (orExpand pat1)@(orExpand pat2)
  | orExpand (pat as RECORDpat{fields,...}) =
     map (MU.mkRECORDpat pat) (foldr allConses [nil] (map (orExpand o #2) fields))
  | orExpand (VECTORpat(pats,t)) =
      map (fn p => VECTORpat(p,t)) (foldr allConses [nil] (map orExpand pats))
  | orExpand (APPpat(k,t,pat)) =
      map (fn pat => APPpat(k,t,pat)) (orExpand pat)
  | orExpand (CONSTRAINTpat(pat,_)) =
      orExpand pat
  | orExpand (LAYEREDpat(CONSTRAINTpat(lpat, _), bpat)) =
      orExpand (LAYEREDpat(lpat, bpat))
  | orExpand (LAYEREDpat(lpat, bpat)) =
      map (fn pat => LAYEREDpat(lpat,pat)) (orExpand bpat)
  | orExpand pat = [pat]

(* expandPats : (pat * lexp) list  (* pre-expansion rules *) * T.ty * T.ty
                -> int * pat list * (exp -> exp) list * ruleMap
 *  preProcessPat is applied to each hybrid rule in the match. All the patterns
 *  in the ramifiedLHS are derived from the original single pattern by OR expansion.
 * QUESTION: can argTy and/or resTy be polymorphic (i.e. POLYty)?
 *)
fun expandPats (rules, argTy, resTy) =
let
    val argTy = TU.dePoly argTy
    val resTy = TU.dePoly resTy

    fun processRule ((pat, rhs), (expandedRuleno, originalRuleno, ruleTable, pats, paths, rhsBinders)) =
	 let val pat = AU.stripPatMarks pat
	     val patVariables = AU.patternVars pat (* works even with OR patterns *)

	     (* abstractRHS : V.var list * AS.exp -> AS.exp *)
	     fun abstractRHS (pvars, rhs) =
		 (case pvars
		   of nil =>
		        let val argVar = V.newVALvar (Symbol.varSymbol "marg", BT.unitTy)
			 in AS.FNexp ([RULE(AS.VARpat(argVar), rhs)], BT.unitTy, resTy)
			end
		    | [var] =>
		        let val argVarTy = TU.dePolyVar var  (* not the same as argTy! *)
			 in AS.FNexp ([AS.RULE(AS.VARpat var, rhs)], argVarTy, resTy)
			end
		    | vars =>
			let val argVarsTy = BT.tupleTy (map TU.dePolyVar vars) (* not the same as argTy! *)
			    val argVar = V.newVALvar (Symbol.varSymbol "rulevars", argVarsTy)
			    val argVarExp = VARexp (ref argVar, nil)
			    fun wrapLet (nil, n) = rhs
			      | wrapLet (v::rest, n) =
				  AS.LETVexp (v, AS.RSELECTexp (argVarExp, n), wrapLet (rest, n+1))
			    val body = wrapLet (vars, 0)
			 in AS.FNexp ([AS.RULE (AS.VARpat(argVar), body)], argVarsTy, resTy)
			end)

	     val rhsFun = abstractRHS (patVariables, rhs)

             (* var naming the abstracted rhs function *)
	     val fvar: V.var = V.newVALvar (Symbol.varSymbol "fvar", BT.--> (argTy, resTy))

             (* rhsFunBinder : AS.exp -> AS.exp *)
	     val rhsFunBinder = fn (body: AS.exp) => AS.LETVexp (fvar, rhsFun, body)

	     (* list of pats produced by or-expansion of pat (ramification of pat)
              *  all pats in this ramified family share the same bound pvars fvar *)
	     val ramifiedPats = orExpand pat

	     (* number of rules produced by or-expansion of pat *)
	     val expandSize = length ramifiedPats

	     val nextExpandedRuleno = expandedRuleno + expandSize
	     val nextOriginalRuleno = originalRuleno + 1

	     val blockVarPaths: P.path list list =
		 map (MU.bindingPaths patVariables) ramifiedPats

	 in (nextExpandedRuleno, nextOriginalRuleno,
	     (expandedRuleno, originalRuleno, expandSize, patVariables, fvar)::ruleTable,
	     ramifiedPats::pats, blockVarPaths::paths, rhsFunBinder::rhsBinders)
	 end

    val (numRules, _, ruleTable, patss, pathss, rhsBinders) =
	  foldl processRule (0, 0, nil, nil, nil, nil) rules

    val expandedPats : pat list = List.concat (rev patss)

    val varPaths : P.path list list = List.concat (rev pathss)

    val ruleTable = rev ruleTable

    (* ruleMap : ruleMap
     *  r: ruleno is index in OR-expanded rules *)
    fun ruleMap (r: ruleno) : ruleInfo =
        let fun loop nil = bug "lookTable"
	      | loop ((exr, orr, sz, pvars, fvar) :: rest) =
		if r >= exr andalso r < exr + sz
		then (fvar, orr)
		else loop rest
	    val (fvar, orr) = loop ruleTable
	    val paths = List.nth (varPaths, r)
	 in (paths, fvar, orr)
        end

    (* ASSERT: length expandedPats = numRules *)
 in (numRules, expandedPats, rhsBinders, ruleMap)
end

end (* top local *)
end (* structure Preprocessing *)
