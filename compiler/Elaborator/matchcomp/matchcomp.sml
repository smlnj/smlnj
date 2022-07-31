(* FLINT/trans/matchcomp.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature MATCH_COMP =
sig

  val bindCompile :
	(Absyn.pat * Absyn.exp) list     (* stripped (RULE) rules *)
	* Types.ty                       (* lhs (pattern) type *)
	* Types.ty                       (* rhs type *)
	* ErrorMsg.errorFn               (* error function *)
	* SourceMap.region               (* match region *)
        * StaticEnv.staticEnv            (* static environment (for printing in error messages) *)
	-> Absyn.exp * Variable.var      (* var option or var ??? *)

  val matchCompile :
	(Absyn.pat * Absyn.exp) list     (* stripped (RULE) rules *)
	* Types.ty                       (* lhs (pattern) type *)
	* Types.ty                       (* rhs type *)
	* ErrorMsg.errorFn               (* error function *)
	* SourceMap.region               (* match region *)
        * StaticEnv.staticEnv            (* static environment (for printing) *)
	-> Absyn.exp * Variable.var        (* var option or var ??? *)

  val handlerCompile :
	(Absyn.pat * Absyn.exp) list
	* Types.ty                       (* rhsTy -- needed for RAISEexp; lhsTy = exnTy *)
	* ErrorMsg.errorFn               (* error function *)
	* SourceMap.region               (* match region *)
        * StaticEnv.staticEnv
	-> Absyn.exp * Variable.var

end (* signature MATCH_COMP *)


structure MatchComp : MATCH_COMP =
struct

local
  structure T  = Types
  structure BT = BasicTypes
  structure V  = Variable
  structure AS = Absyn
  structure AU = AbsynUtil
  structure EU = ElabUtil
  structure LV = LambdaVar
  structure EM = ErrorMsg
  structure PP = PrettyPrint
  structure PU = PPUtil
  structure DT = DecisionTree
  structure MCP = MCPrint
  structure ST = MCStats
  structure MCC = MCControl (* match compiler control flags *)
 
  open MCCommon 
		     
  val mcdebugging = MCC.mcdebugging
  val stats = MCC.mcstats
  val timings = MCC.mcstats
  val printProtoAndor = MCC.printProtoAndor
  val printAndor = MCC.printAndor
  val printDectree = MCC.printDecisionTree
		       
  fun bug s = EM.impossible ("MatchComp: " ^ s)
  fun say msg = (Control_Print.say msg; Control_Print.flush ())
  fun says msgs = say (concat msgs)
  fun saynl msg = (say (msg^"\n"))
  fun saysnl msgs = (saynl (concat msgs))
  fun newline () = say "\n"

  fun dbsay msg = if !mcdebugging then say msg else ()
  fun dbsays msgs = if !mcdebugging then says msgs else ()
  fun dbsaynl msg = if !mcdebugging then saynl msg else ()


  val db_printDepth = 100

  fun ppDectree dectree =
      PP.with_default_pp (fn ppstrm => MCPrint.ppDectree ppstrm dectree)

  fun ppExp (exp, msg) =
      PP.with_default_pp
          (fn ppstrm =>
	      (PP.string ppstrm msg;
	       PPAbsyn.ppExp (StaticEnv.empty, NONE) ppstrm (exp, db_printDepth);
	       PP.newline ppstrm))

in

val choiceTotalThreshold = 10

fun reportStats (nodeCount: int, {rulesUsed, failures, choiceTotal, choiceDist}: DT.decTreeStats) =
    if !MCC.mcstats andalso choiceTotal > choiceTotalThreshold
    then (say "decTree Stats: \n";
	  says ["  nodeCount =   ", Int.toString nodeCount];
	  says ["  choiceTotal = ", Int.toString choiceTotal];
	  newline())
    else ()


(* --------------------------------------------------------------------------- *)
(* matchComp: Main match compiler driver function *)

(* Both the lhsTy (the patterns type) and rhsTy are needed to construct types for the fvar variables
 *  to which the abstracted rhs functions are bound. They are passed as args to Preprocessing.expandPats.
 *  Generate.generate also needs (just) the rhsTy when generating a raise expression (RAISEexp) *)

(* matchComp : AS.rule list * T.ty * T.ty * T.datacon option
                -> AS.exp * V.var * ruleno list * bool * bool *)
fun matchComp (rules, lhsTy: T.ty, rhsTy: T.ty, failExnOp: T.datacon option, region) =
let fun timeIt x = TimeIt.timeIt (!MCC.mcstats) x
    val location = "nolocation"
        (* might be derived from region argument, but need current Source.inputSource or
         * errorMatch function from the ErrorMsg.errors record (found in compInfo now)  *)
    val _ = MCPrint.debugPrint mcdebugging
              ("matchComp: match = \n", MCPrint.ppMatch, rules)

    val (numExpandedRules, expandedPats, rhsFunBinders, ruleMap) =
	Preprocessing.expandPats (rules, lhsTy, rhsTy)

    (* RS.set of rulenos after or-expansion. If there are or-patterns
     * in the match, numRulesExpanded > length hybridMatch. *)
    val allRules: RS.set = RS.fromList (List.tabulate(numExpandedRules, fn x => x));

    val _ = ST.initialLvar := LV.nextLvar ()  (* for counting how many lvars are generated *)

    val protoAndor: protoAndor = (* ProtoAndor.makeProtoAndor expandedPats *)
        timeIt ("makeProtoAndor", location, ProtoAndor.makeProtoAndor, expandedPats)

    val _ = MCPrint.debugPrint printProtoAndor
	      ("** matchComp: protoAndor = ", MCPrint.ppProtoAndor, protoAndor)

    val andor: andor =
	(* Andor.makeAndor (protoAndor, allRules) *)
	timeIt ("Andor.makeAndor", location, Andor.makeAndor, (protoAndor, allRules))
    val _ = if !stats then ST.collectAndorStats andor else ()

    val _ = MCPrint.debugPrint printAndor ("** matchComp: andor", MCPrint.ppAndor, andor)

    val dectree = (* DecisionTree.makeDectree (andor, allRules) *)
	timeIt ("makeDectree", location, DT.makeDectree, (andor, allRules))
    val _ = ST.collectDectreeStats dectree  (* must collect dectree stats for rulesUsed and numFAIL *)

    val _ = MCPrint.debugPrint printDectree ("** matchComp: dectree = ", MCPrint.ppDectree, dectree)

    (* checking exhaustiveness and redundancy of rules *)
    (* It may be that there are unused _ramified_ rules, but all original rules are used!? Example? *)
    val SOME{rulesUsed, numFAIL, ...} = !ST.dectreeStats
    val unusedRules : ruleset = RS.difference (allRules, rulesUsed)  (* expanded rules *)
    val unusedOriginalRules : ruleset = RS.map (#3 o ruleMap) unusedRules
        (* unusedRules translated back to corresponding original rule numbers. WRONG??? *)
    val redundant = not (RS.isEmpty unusedRules)
    val nonexhaustive = numFAIL > 0  (* any FAIL nodes => nonexhaustive rules *)

    (* generating the "raw" lexp for the match *)

    val (coreExp, rootVar) = (* Generate.generate (dectree, ruleMap, allRules, failExnOp, rhsTy) *)
        timeIt ("Generate.generate", location, Generate.generate,
		(dectree, ruleMap, allRules, failExnOp, rhsTy))

    (* wrapping let-bindings of abstracted right-hand-sides around match code,
     * (corresponds to "genprelude" in newmc) *)

    val fullExp: AS.exp = foldl (fn (fbinder, body) => fbinder body) coreExp
			      rhsFunBinders

    val _ = if !MCC.printMatchAbsyn then ppExp (fullExp, "** matchComp: match absyn = ")
	    else ()

    val _ = ST.finalLvar := LV.nextLvar ()
				    
    val _ = if !stats then ST.reportStats () else ()

    (* rudundant <=> not (null unusedOriginalRules) <=> not (null unusedExpandedRules) *)
 in (fullExp, rootVar, RS.toList unusedOriginalRules, redundant, nonexhaustive)
end (* fun matchComp *)

(* --------------------------------------------------------------------------- *)
(* Match Compiler entry points *)

(*
 * The three entry points for the match compiler are bindCompile, handleCompile,
 * and matchCompile.
 *
 * They take as arguments an environment (env); a match represented
 * as a list of pattern--lambda expression pairs (hybrid rules); a
 * function err to use in printing warning messages (err), etc.
 *
 * env and err are only used in the printing of diagnostic information.
 *
 * If the control flag MCC.printArgs is set, they print the match.
 *
 * They call matchComp to actually compile the match.
 * This returns a 4-tuple (code, unused, redundant, exhaustive).
 * code is a PLambda.lexp expression implementing the match.
 *   - unused is a list of the indices of the unused rules.
 *   - redundant and exhaustive are boolean flags which are set if
 *     match is redundant or exhaustive respectively.
 *
 * They print warning messages as appropriate, as described below.
 * If the control flag MCC.printCode is set, they print the match code.
 *)

(* bindCompile: Entry point for compiling matches induced by val declarations
 *  (e.g., val x::xs = list).
 *  The match consists of a _single_ rule that corresponds to the let binding itself.
 *  If the control flag MCC.bindNonExhaustiveWarn
 *  is set then a nonexhaustive binding warning is printed. If the control
 *  flag MCC.bindNoVariableWarn is set, and pattern contains no variables or
 *  wildcards, a warning is printed. Arguably, a pattern containing no
 *  variables, but one or more wildcards, should also trigger a warning,
 *  but this would cause warnings on constructions like
 *  val _ = <exp>  and  val _:<ty> = <exp>.
 *)
fun bindCompile (rules: (AS.pat * AS.exp) list, lhsTy: T.ty, rhsTy: T.ty, errorFn: ErrorMsg.errorFn,
		 region: SourceMap.region, env: StaticEnv.staticEnv): (AS.exp * V.var) =
    let val bindExn = EU.getBindExn ()
	val (code, rootVar, _, _, nonexhaustive) =
	      matchComp (rules, lhsTy, rhsTy, SOME bindExn, region)

	val nonexhaustiveF =
	    nonexhaustive andalso (!MCC.bindNonExhaustiveWarn orelse !MCC.bindNonExhaustiveError)
	val noVarsF = !MCC.bindNoVariableWarn andalso AU.noVarsInPat (#1 (hd rules))

     in if nonexhaustiveF
        then errorFn region
	       (if !MCC.bindNonExhaustiveError then EM.COMPLAIN else EM.WARN)
	       ("binding not exhaustive" ^ (if noVarsF then " and contains no variables" else ""))
	       (MatchPrint.bindPrint (env,rules))
        else if noVarsF
        then errorFn region
	       EM.WARN
	       "binding contains no variables"
               (MatchPrint.bindPrint(env,rules))
        else ();
	(code, rootVar)
    end

(* handlerCompile: Entry point for compiling matches induced by exception handlers.
 *  (e.g., handle Bind => Foo).  If the control flag
 *  MCC.matchRedundantWarn is set, and match is redundant,
 *  a warning is printed.  If MCC.matchRedundantError is also
 *  set, the warning is promoted to an error message.
 *)
fun handlerCompile (rules, rhsTy, errorFn, region, env): (AS.exp * V.var) =
    let val (code, rootVar, unused, redundant, _) =
	      matchComp (rules, BT.exnTy, rhsTy, NONE, region)
	val redundantF= !MCC.matchRedundantWarn andalso redundant

     in if redundantF
	then errorFn region
	     (if !MCC.matchRedundantError then EM.COMPLAIN else EM.WARN)
	     "redundant patterns in match"
             (MatchPrint.matchPrint(env,rules,unused))
	else ();
	(code, rootVar)
    end

(*
 * matchCompile: Entry point for compiling matches induced by function expressions
 *  (and thus case expression, if-then-else expressions, while expressions
 *  and fun declarations) (e.g., fn (x::y) => ([x],y)). If the control flag
 *  MCC.matchRedundantWarn is set, and match is redundant, a warning
 *  is printed; if MCC.matchRedundantError is also set, the warning
 *  is promoted to an error. If the control flag MCC.matchExhaustive
 *  is set, and match is nonexhaustive, a warning is printed.
 *)
fun matchCompile (rules, lhsTy, rhsTy, errorFn, region, env): (AS.exp * V.var) =
    let val matchExn = EU.getMatchExn ()
	val (matchExp, rootVar, unused, redundant, nonexhaustive) =
	      matchComp (rules, lhsTy, rhsTy, SOME matchExn, region)

	val nonexhaustiveF =
	    nonexhaustive andalso (!MCC.matchNonExhaustiveError orelse !MCC.matchNonExhaustiveWarn)
	val redundantF =
	    redundant andalso (!MCC.matchRedundantError orelse !MCC.matchRedundantWarn)
     in case (nonexhaustiveF,redundantF)
	  of (true, true) =>
             errorFn region
	       (if !MCC.matchRedundantError orelse !MCC.matchNonExhaustiveError
		then EM.COMPLAIN else EM.WARN)
	       "match redundant and nonexhaustive"
	       (MatchPrint.matchPrint(env, rules, unused))
           | (true, false) =>
             errorFn region
	       (if !MCC.matchNonExhaustiveError then EM.COMPLAIN else EM.WARN)
               "match nonexhaustive"
	       (MatchPrint.matchPrint(env, rules, unused))
           | (false, true) =>
             errorFn region
	       (if !MCC.matchRedundantError then EM.COMPLAIN else EM.WARN)
	       "match redundant"
	       (MatchPrint.matchPrint(env, rules, unused))
           | _ => ();
        (matchExp, rootVar)
    end

val matchCompile : (AS.pat * AS.exp) list * T.ty * T.ty * ErrorMsg.errorFn * SourceMap.region * StaticEnv.staticEnv
                   -> AS.exp * V.var
    = Stats.doPhase(Stats.makePhase "Compiler 045 matchcomp") matchCompile

end (* topleve local *)
end (* structure MatchComp *)
