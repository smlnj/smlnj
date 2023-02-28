(* Elaborator/matchcomp/mcmatchcomp.sml
 *
 * COPYRIGHT (c) 2022 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(* pretty printing for (revised old) match compiler (MC) internal structures *)

structure PPMatchComp =
struct

local (* top local *)

  structure PP = PrettyPrint
  structure LV = LambdaVar
  structure A = Access
  structure V = Variable
  structure SE = StaticEnv
  structure AS = Absyn
  structure AU = AbsynUtil
  structure PPA = PPAbsyn
  structure P = Paths
  structure MU = MCUtil

  open MCCommon  (* includes RS = RuleSet *)

  val say = Control_Print.say
  fun newline () = say "\n"
  fun saynl msg = (say msg; newline())

  fun bug msg = ErrorMsg.impossible ("MCPrint: " ^ msg)

  val printDepth = Control_Print.printDepth

  (* ivcat : PP.format list -> PP.format *)
  fun ivcat formats = PP.indent 3 (PP.vcat formats)

in

(* debugMsg : bool ref -> string -> unit *)
fun debugMsg flag (msg: string) =
    if !flag then saynl msg else ()

(* debugPrint : bool ref -> string * PP.format -> unit *)
fun debugPrint flag (msg: string, format: PP.format) =
    if !flag
    then PP.printFormatNL (PP.vcat [PP.text msg, PP.indent 2 format])
    else ()

(* fmtCon : AS.con -> PP.format *)
fun fmtCon (con : AS.con) : PP.format = PP.text (AU.conToString con)

(* fmtPath : P.path -> PP.format *)
fun fmtPath (path: P.path) = PP.text (P.pathToString path)

(* fmtOption : ('a -> PP.format) -> 'a option -> PP.format *)
(* local variant, superceding PP.option *)
fun fmtOption formatter elemOp =
    case elemOp
      of NONE => PP.text "<<>>"
       | SOME e => PP.enclose {front = PP.text "<< ", back = PP.text " >>"} (formatter e)

(* fmtConsig : A.consig -> PP.format *)
fun fmtConsig (A.CSIG(n,m)) = 
      PP.ccat [PP.text "CSIG", PP.parens (PP.ccat [PP.integer n, PP.comma, PP.integer m])]
  | fmtConsig A.CNIL = PP.text "NIL"

(* fmtRuleset : ruleset -> PP.format *)
fun fmtRuleset (ruleset: ruleset) =
    PP.braces (PP.psequence PP.comma (map PP.integer (RS.listItems ruleset)))

(* fmtSubcase : ('a -> PP.format) -> subcase -> PP.format *)
fun fmtSubcase caseFormatter subcase =
    (case subcase
      of CONST => PP.text "CONST"
       | DCARG thing => caseFormatter thing
       | VELEMS elems => 
	   PP.vcat [PP.text "VELEMS:",
		    ivcat (map caseFormatter elems)])

(* fmtProtoAndor : protoAndor -> PP.format
 * pretty printer for protoAndor nodes *)
val fmtProtoAndor =
    let fun fmtNode (ANDp {varRules, children}) =
	      PP.vcat [PP.hcat [PP.text "ANDp", fmtRuleset varRules],
		       PP.indent 3 (fmtAndChildren children)]
	  | fmtNode (ORp {varRules, sign, cases}) =
	      PP.vcat
	        [PP.hcat [PP.text "OR", fmtRuleset varRules, fmtConsig sign],
		 PP.indent 3 (fmtVariants cases)]
	  | fmtNode (VARp {varRules}) = PP.hcat [PP.text "VAR", fmtRuleset varRules]
	  | fmtNode WCp = PP.string "WCp"

	and fmtAndChildren nodes = PP.vcat (map fmtNode nodes)

	and fmtVariants variants = PP.vcat (map fmtVariant variants)

	and fmtVariant (con, rules, subcase) =
	      PP.hcat
	        [PP.hcat [PP.text (AU.conToString con), fmtRuleset rules],
		 fmtSubcase fmtNode subcase]

    in fmtNode 
    end  (* fmtProtoAndor *)

(* fmtAndor : andor -> unit
 *  pretty print formatter for AND-OR nodes *)
val fmtAndor =
    let fun fmtNode (AND {id, children}) =
	      PP.vcat [PP.hcat [PP.string "AND", PP.integer id],
		       fmtAndChildren children]
	  | fmtNode (OR {id, path, sign, defaults, cases}) =
	      PP.vcat
	        [PP.hcat [PP.text "OR", PP.integer id, fmtPath path, fmtRuleset defaults, fmtConsig sign],
		 fmtVariants cases]
	  | fmtNode (VAR {id}) =
	      PP.hcat [PP.text "VAR", PP.integer id]
	  | fmtNode WC = PP.string "WC"

	and fmtAndChildren nodes = ivcat (map fmtNode nodes)

	and fmtVariants variants = ivcat (map fmtVariant variants)

	and fmtVariant (con, rules, subcase) =
	      PP.hcat [PP.text (AU.conToString con), fmtSubcase fmtNode subcase]

     in fmtNode
    end (* fmtAndor *)

(* fmtDectree : decTree -> PP.format *)
val fmtDectree =
    let fun fmtDec (SWITCH {id, path, sign, cases, defaultOp, live}) =
              PP.vcat
                [PP.hcat [PP.text "SWITCH", PP.integer id, fmtPath path, fmtConsig sign],
	         fmtSwitch (cases, defaultOp)]
	  | fmtDec (RHS ruleno) =
	      PP.hcat [PP.text "RHS", PP.integer ruleno]
	  | fmtDec (FAIL) =
	      PP.text "FAIL"

	and fmtSwitch (cases,defaultOp) =
              ivcat
	        (map fmtCase cases @
		 (case defaultOp
	            of SOME dectree =>
          	         [PP.pcat [PP.text "*", fmtDec dectree]]
		     | NONE => nil))

	and fmtCase (con, decTree) =
	      PP.pcat
	        [PP.text (AU.conToString con),
		 PP.indent 3 (fmtDec decTree)]
     in fmtDec
    end (* fmtDectree *)

(* fmtRule : ppstream -> Absyn.pat * Absyn.exp -> unit *)
(* format absyn rule *)
fun fmtRule (pat, exp) =
      PP.pcat
        [PPA.fmtPat StaticEnv.empty (pat, 100),
	 PP.text "=>",
         PP.indent 3 (PPA.fmtExp (StaticEnv.empty, NONE) (exp, 100))]

fun fmtMatch match = PP.vcat (map fmtRule match)

(* formatMatch and formatBind were formerly MatchPrint.matchPrint and MatchPrint.bindPrint.
 * These are used in error messages in the *Compile functions. *)

(* formatMatch: SE.staticEnv * AS.pat list * int list (ruleset) -> PP.format
 * Prints abbreviated rules, indicating unused rules with a preceeding "-->".
 * Assumes unused is a "ruleset", an ordered list of rule numbers, rule numbers start with 0 *)
fun formatMatch (env: SE.staticEnv, rules: AS.pat list, unused: int list) =
    let val postfix = PP.text "=> ..."
	val usedPrefix =  PP.text "       "   (* 8 spaces *)
        val unusedPrefix = PP.text "  -->  "
        fun ruleFmts (nil, _, _, fmts) = rev fmts
	  | ruleFmts (pat::rest, n, u::us, fmts) = 
	      let val (prefix, remaining_unused) =
		      case Int.compare (n, u)
			of EQUAL => (unusedPrefix, us)   (* u matches n, unused rule *)
			 | _ => (usedPrefix, u::us)      (* n < u *)
		  val fmt = PP.hcat [prefix, PPA.fmtPat env (pat, !printDepth), postfix]
	      in ruleFmts (rest, n+1, remaining_unused, fmt::fmts)
	      end
	  | ruleFmts (pat::rest, n, nil, fmts) =  (* beyond the last unused ruleno *)
	      let val fmt = PP.hcat [usedPrefix, PPA.fmtPat env (pat, !printDepth), postfix]
	       in ruleFmts (rest, n, nil, fmt::fmts)
	      end
     in PP.vcat (ruleFmts (rules, 0, unused, nil))
    end

(* bindPrint : SE.staticEnv * (AS.pat * AS.exp) list -> PP.format
 * prints only the first rule pattern, which should be the only one for a binding *)
fun formatBind (env: SE.staticEnv, (pat, _) :: _) =
      PP.hcat [PP.text "        ", PPAbsyn.fmtPat env (pat, !printDepth), PP.text "= ..."]
  | formatBind _ = bug "bindPrint -- unexpected args"

end (* top local *)
end (* structure PPMatchComp *)
