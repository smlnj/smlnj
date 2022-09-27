(* mcprint.sml
 *
 * COPYRIGHT (c) 2021 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(* pretty printing for (revised old) match compiler (MC) internal structures *)

structure PPMatchComp =
struct

local
   structure PP = NewPP
   structure LV = LambdaVar
   structure A = Access
   structure V = Variable
   structure AS = Absyn
   structure AU = AbsynUtil
   structure PPA = PPAbsyn
   structure P = Paths
   structure MU = MCUtil

   open MCCommon  (* includes RS = RuleSet *)

in

val say = Control.Print.say
fun newline () = say "\n"
fun saynl msg = (say msg, newline())

fun bug msg = ErrorMsg.impossible ("MCPrint: " ^ msg)

(* debugMsg : bool ref -> string -> unit *)
fun debugMsg flag (msg: string) =
    if !flag then saynl msg else ()

(* debugPrint : bool ref -> string * PP.format -> unit *)
fun debugPrint flag (msg: string, format: PP.format) =
    if !flag
    then PP.printFormatNL (PP.vcat (PP.text msg, PP.hardIndent 2 format))
    else ()

(* fmtCon : AS.con -> PP.format *)
fun fmtCon (con : AS.con) : PP.format = PP.text (AU.conToString con)

(* fmtPath : P.path -> PP.format *)
fun fmtPath (path: P.path) = PP.text (P.pathToString path)

(* fmtOption : ('a -> PP.format) -> 'a option -> PP.format *)
fun fmtOption formatter elemOp =
    case elemOp
      of NONE => PP.text "<<>>"
       | SOME e => PP.enclose {front = PP.text "<< ", back = PP.text " >>"} (formatter e)

(* fmtConsig : A.consig -> PP.format *)
fun fmtConsig (A.CSIG(n,m)) = 
      PP.ccat (PP.text "CSIG", PP.parens (PP.concat [PP.integer n, PP.comma, PP.integer m]))
  | fmtConsig A.CNIL => PP.text "NIL"

(* fmtRuleset : RS.ruleset -> PP.format *)
fun fmtRuleset (ruleset: RS.ruleset) =
    PP.braces (PP.sequence {alignment = PP.P, sep = PP.comma} (map PP.integer (RS.listItems ruleset)))

(* fmtSubcase : -> ('a -> PP.format) -> subcase -> PP.format *)
fun fmtSubcase fmtcase subcase =
    (case subcase
      of CONST => PP.text "CONST"
       | DCARG thing => fmtcase thing
       | VELEMS elems => 
	   PP.vcat (PP.text "VELEMS",
		    PP.hardIndent 2 (PP.vblock (map fmtcase elems))))

(* fmtProtoAndor : protoAndor -> PP.format
 * pretty printer for protoAndor nodes *)
fun fmtProtoAndor =
    let fun fmtNode (ANDp {varRules, children}) =
	      PP.vcat (PP.hcat (PP.text "ANDp", fmtRuleset varRules),
		       PP.hardIndent 3 (fmtAndChildren children))
	  | fmtNode (ORp {varRules, sign, cases}) =
	      PP.vcat
	        (PP.hblock [PP.text "OR", fmtRuleset varRules, fmtConsig sign],
		 PP.hardIndent 3 (fmtVariants cases))
	  | fmtNode (VAR {varRules}) = PP.hcat (PP.text "VAR", fmtRuleset varRules)
	  | fmtNode WCp = PP.string "WCp"

	and fmtAndChildren nodes = PP.vblock (map fmtNode nodes)

	and fmtVariants variants = PP.vblock (map fmtVariant variants)

	and fmtVariant (con, rules, subcase) =
	      PP.hcat
	        (PP.hcat (PP.text (AU.conToString con), fmtRuleset rules),
		 fmtSubcase fmtAndor subcase)

    in fmtNode 
    end  (* fun fmtProtoAndor *)

(* fmtAndor : andor -> unit
 *  pretty printer for AND-OR nodes *)
fun ppAndor =
    let fun fmtNode (AND {id, children}) =
	      PP.vcat (PP.hcat (PP.string "AND", PP.integer id),
		       PP.hardIndent 3 (fmtAndChildren children))
	  | fmtNode (OR {id, path, sign, defaults, cases}) =
	      PP.vcat
	        (PP.hblock [PP.text "OR", PP.integer id, fmtPath path, fmtRuleset defaults, fmtConsig sign],
		 PP.hardIndent 3 (fmtVariants cases))
	  | fmtNode (VAR {id}) =
	      PP.hcat (PP.text "VAR", PP.integer id);
	  | fmtNode WC = PP.string "WC"

	and fmtAndChildren nodes =
	      PP.vblock (map fmtNode nodes)

	and fmtVariants variants =
	      PP.vblock (map fmtVariant variants)

	and fmtVariant (con, rules, subcase) =
	      PP.hcat
	        (PP.text (AU.conToString con),
		 fmtSubcase fmtAndor subcase)

     in fmtNode 
    end (* fun ppAndor *)

(* fmtDectree : decTree -> PP.format *)
val fmtDectree =
    let fun fmtDec (SWITCH {id, path, sign, cases, defaultOp, live}) =
              PP.vcat
                (PP.hblock [PP.text "SWITCH", PP.integer id, fmtPath path, fmtConsig sign],
	         hardIndent 3 (fmtSwitch (cases, defaultOp)))
	  | fmtDec (RHS ruleno) =
	      PP.hcat (PP.text "RHS", PP.integer ruleno))
	  | fmtDec (FAIL) =
	      PP.text "FAIL";

	and fmtSwitch (cases,defaultOp) =
              PP.vblock
	        (map fmtCase cases @
		 (case defaultOp
	            of SOME dectree =>
          	         [PP.pcat (PP.text "*", fmtDec dectree)]
		     | NONE => nil))

	and fmtCase (con, decTree) =
	      PP.pcat
	        (PP.text (AU.conToString con),
		 PP.softIndent 3 (fmtDec decTree)
     in fmtDec
    end (* fmtDectree *)

(* fmtRule : ppstream -> Absyn.pat * Absyn.exp -> unit *)
(* format absyn rule *)
fun fmtRule (pat, exp) =
      PP.pblock
        [PPA.fmtPat StaticEnv.empty (pat, 100),
	 PP.text "=>",
         PP.softIndent 3 (PPA.fmtExp (StaticEnv.empty, NONE) (exp, 100))]

fun fmtMatch match = PP.vblock (map fmtRule match)

(* formatMatch and formatBind were formerly MatchPrint.matchPrint and MatchPrint.bindPrint.
 * These are used in error messages in the *Compile functions. *)

(* formatMatch: StaticEnv.staticEnv * (AS.pat * AS.exp) list * int list -> PP.format
 * Prints abbreviated rules, indicating unused rules with a preceeding "-->".
 * Assumes unused is a "ruleset", an ordered list of rule numbers, rule numbers start with 0 *)
fun formatMatch (env, rules, unused) =
    let val postfix = PP.text "=> ..."
	val uprefix =  PP.text "       "   (* 8 spaces *)
        val unuprefix = PP.text "  -->  "
        fun ruleFmts (nil, _, _, fmts) = ref fmts
	  | ruleFmts ((pat,_)::rest, n, u::us, fmts) = 
	    let val n_vs_u = Int.compare (n, u)
		val (prefix, remaining_unused) =
		    case n_vs_u
		      of EQUAL => (unuprefix, us)   (* u matches n, unused rule *)
		       | _ => (uprefix, u::us)      (* n_vs_u will only be LESS *)
		val fmt = PP.hblock [prefix, PPA.fmtPat env (pat, !printDepth), postfix]
	    in ruleFmt (rest, n+1, remaining_unused, fmt::fmts)
	    end
     in PP.vblock (ruleFmts (rules, 0, unused))
    end

(* bindPrint : StaticEnv.staticEnv * (AS.pat * AS.exp) list -> PP.format
 * prints only the first rule pattern, which should be the only one for a binding *)
fun formatBind (env, (pat, _) :: _) =
      PP.hblock [(PP.text "        ", PPAbsyn.fmtPat env (pat, !printDepth), PP.text "= ..."]
  | bindPrint _ = bug "bindPrint -- unexpected args"

end (* top local *)
end (* structure PPMatchComp *)
