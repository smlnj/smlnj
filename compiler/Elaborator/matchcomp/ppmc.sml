(* mcprint.sml
 *
 * COPYRIGHT (c) 2021 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(* pretty printing for new match compiler (MC) internal structures defined in mccommon.sml *)

structure PPMc =
struct

local
   structure PU = PPUtil
   structure LV = LambdaVar
   structure A = Access
   structure V = Variable
   structure AS = Absyn
   structure AU = AbsynUtil
   structure P = Paths

   structure PP = NewPrettyPrint
   open MCCommon

in

fun bug msg = ErrorMsg.impossible ("MCPrint: " ^ msg)

val debugging = MCControl.mcdebugging

val say = Control.Print.say
fun newline () = say "\n"
fun saynl (msg: string) = (say msg; newline())
fun dbsay (msg: string) = if !debugging then saynl msg else ()

(* debugPrint : string * (PP.stream -> 'a -> unit) * 'a -> unit *)
fun debugPrint (msg: string, formatter: 'a -> unit, subject: 'a) =
    if !debugging
    then PP.printFormatNL
             (PP.vcat (PP.text msg,
		       formatter subject))
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

(* fmtSign : A.consig -> PP.format *) 
fun fmtSign sign =
    (case sign
      of A.CSIG(n,m) =>
	 (PP.hcat (PP.text "CSIG",
		   PP.tupleFormat [PP.integer n, PP.integer m]))
       | A.CNIL => PP.text "CNIL")

(* fmtRuleset : RS.ruleset -> PP.format *)
fun fmtRuleset (ruleset: RS.ruleset) =
      PP.braces (PU.sequence {alignment=PP.P, sep=PP.comma} (map PP.integer (RS.listItems ruleset)

(* fmtSubcase : ('a -> unit) -> 'a subcase -> PP.format *)
fun fmtSubcase fmtCase subcase =
    (case subcase
      of CONST => PP.text "CONST"
       | DCARG thing => fmtCase thing
       | VELEMS elems => 
	    PP.hcat (PP.text "VELEMS:",
		     PP.formatTuple fmtCase elems))

(* fmtProtoAndor : MC.protoAndor -> PP.format *)
(* formatter for protoAndor nodes *)
fun fmtProtoAndor protoAndor =
    let fun fmtProtoVariant ((con, rules, subcase): MC.protoVariant) =
	      PP.hblock
	       [PP.text (AU.conToString con), 
		fmtRuleset rules,
		fmtSubcase fmtProtoAndor subcase]

	fun fmtProtoVariants (variants: MC.protoVariant list) =
	      PP.sequence {alignment=PP.V, sep=PP.empty}
	         (map fmtProtoVariant variants)

	fun fmtAndChildren (nodes: MC.protoAndor list) =
	      PP.sequence {alignment=PP.V, sep=PP.empty}
	         (map fmtProtoAndor nodes)

    in (case protoAndor
	 of (ANDp {varRules, children}) =>
              PP.hblock [PP.text "ANDp", fmtRuleset varRules, fmtAndChildren children]
	  | (ORp {varRules, sign, cases}) =>
	      PP.pcat (PP.hblock [PP.text "ORp", fmtRuleset varRules, ppSign sign],
		       fmtProtoVariants cases)
	  | (VARp {varRules}) =>
	      PP.hcat (PP.text "VARp", fmtRuleset varRules)
	  | WCp => PP.text "WCp")
    end

(* fmtAndor : andor -> PP.format *)
(*  pretty printer for AND-OR nodes
 *  could develop a "path" while printing the andor tree *)
fun fmtAndor andor =
    let fun fmtVariant (con, rules, subcase) =
	      PP.hcat (PP.text (AU.conToString con), fmtSubcase fmtAndor subcase)
    in case andor
	of (AND {id, children}) =>
	      PP.vcat (PP.hcat (PP.text "AND", PP.integer id),
		       PP.indent 2
			 (PP.sequence {alignment=PP.V, sep=PP.empty} (map fmtNode children)))
	 | (OR {id, path, sign, defaults, cases}) =>
	      PP.vcat
		  (PP.hblock
		     [PP.text "OR", PP.integer id, fmtPath path, fmtRuleset defaults,
		      fmtSign sign],
		   PP.indent 2 
                     (PP.sequence {alignment=PP.V, sep=PP.empty} (map fmtVariant cases)))
	 | (VAR {id}) =>
	      PP.hcat (PP.text "VAR", PP.integer id)
	 | WC = PP.text "WC"
    end (* fun ppAndor *)

(* fmtDectree : decTree -> PP.format *)
val fmtDectree decTree =
    let fun fmtCase (con, decTree) =
	      PP.hcat (PP.text (AU.conToString con), fmtDectree decTree)
	fun fmtSwitch (cases, defaultOp) =
              PP.vcat
	       (PP.sequence {alignment=PP.V, sep=PP.empty} (map fmtCase cases),
	        (case defaultOp
	           of SOME dectree => PP.hcat (PP.text "*", fmtDectree dectree)
		    | NONE => PP.empty))

    in case decTree
	 of SWITCH {id, path, sign, cases, defaultOp, live} =>
              PP.vcat (PP.hblock [PP.text "SWITCH", PP.integer id, fmtPath path, fmtSign sign],
		       fmtSwitch(cases, defaultOp))
	  | RHS ruleno => PP.hcat (PP.text "RHS", PP.integer ruleno)
	  | FAIL => PP.text "FAIL"
    end (* ppDectree *)

(* fmtRule : Absyn.pat * Absyn.exp -> PP.format *)
(* format absyn rule *)
fun ppRule(pat, exp) =
      PP.pcat
        (PP.hcat (PPAbsyn.fmtPat StaticEnv.empty (pat, 100), PP.text "=>"),
         PPAbsyn.fmtExp (StaticEnv.empty, NONE) (exp, 100))

(* fmtMatch : (Absyn.pat * Absyn.exp) list -> PP.format *)
fun fmtMatch match = PP.sequence {alignment=PP.V, sep=PP.text " |"} (map ppRule match)

end (* top local *)
end (* structure PPMatchComp *)
