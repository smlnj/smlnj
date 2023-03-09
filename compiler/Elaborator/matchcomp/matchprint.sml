(* FLINT/trans/matchprint.sml *)
(* pattern printing (using PPAbsyn.ppPat) *)

structure MatchPrint =
struct

local
  structure PP = Formatting
  structure PPA = PPAbsyn
  val printDepth = Control_Print.printDepth

  fun bug msg = ErrorMsg.impossible ("MatchPrint: "^ msg)
in

(* Renamed:
   matchPrint -> formatMatch
   bindPrint -> formatBind
 *)

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
      PP.hblock [PP.text "        ", PPAbsyn.fmtPat env (pat, !printDepth), PP.text "= ..."]
  | bindPrint _ = bug "bindPrint -- unexpected args"


end (* top local *)
end (* structure MatchPrint *)
