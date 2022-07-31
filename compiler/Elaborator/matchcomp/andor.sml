(* FLINT/trans/andor.sml *)
(* revmc: revised "old" match compiler *)

(* Translate proto-andor trees (protoAndor) to full andor trees. This involves
 *  (1) adding an id field with an unique nodeId number that identifies the node.
 *  (2) processing the "varRules" field of protoAndors to create a relevance map
 *      (relmap) that maps nodeIds to sets of rules such that (1) the node is "relevant".
 *      to a rule (case-relevant or var-relevant) and (2) the rule is (structurally)
 *      live at that node.
 *  (3) computing the "defaults" rule set for OR nodes, by removing the caserules for
 *      the nodes cases from the structurally live rules at that node.
 *  Andor trees are related to the results of the "flatten" functions (e.g. "flatteningAndor)
 *  of oldmc.)
 *)

structure Andor =
struct

local
  structure AS = Absyn
  structure P = Paths
  structure PP = PrettyPrint
  open MCCommon

  val debugging = MCControl.mcdebugging

  fun bug msg = ErrorMsg.impossible ("Andor: " ^ msg)

  val say = Control_Print.say
  fun newline () = say "\n"
  fun saynl msg = (say msg; newline())
  fun says strings = saynl (concat strings)

  fun ppCon con =
      PP.with_default_pp (fn ppstrm => MCPrint.ppCon ppstrm con)

  fun ppRules rules =
      PP.with_default_pp (fn ppstrm => MCPrint.ppRuleset ppstrm rules)

in

(* NOTE: The pvars (absyn pattern variables) are not recorded in the protoAndor or andor
trees, only the rulenos in which variables occur at a node.
The relation between pvars and match administrative variables (mvars) is handled by
 positional correspondence between the pvars and mvars based on path positions of the
pvars (which translate to nodeIds and thence to mvars via an mvarenv).

We build a relevance mapping from nodeIds in the andor tree to rulesets containing
relevant rules for the nodeId.  A nodeId maps to the set of rules that either have
a variable at the node, or form the subject of a switch (derived from the node, which must
in this case be an OR node. These relevant rules are then propagated up the "trail" of
ancestor nodes [relevant(r, p) & p' < p ==> relevant(r, p')] by being added to their relmap
values.
END NOTE *)

(* trail of nodeIds of ancestors of a node, in reverse order, with current node first *)
type trail = nodeId list

(* relevance map *)
structure IM = IntRedBlackMap	 (* nodeId (int) maps *)
type relmap = ruleset IM.map    (* relevance (partial) map : nodeId -> ruleSet *)

(* makeAndor : protoAndor * ruleset -> andor
 *   allRules will be the complete set of post-OR-expansion rule numbers *)
fun makeAndor (protoAndor: protoAndor, allRules: ruleset): andor =
let
    (* generating new nodeIds *)
    val idCounter = ref 0;
    fun newId () = !idCounter before (idCounter := !idCounter + 1)

    (* ================================================================================ *)
    (* translateAndor : protoAndor * path * ruleset -> andor
       Traverses the protoAndor tree accumulating a bindenv binding all generated paths
       (starting with path as the root) to the associated "live" rules from
       the bindings fields in all the andor tree's nodes.

     * computing the "defaults" field of OR nodes:
       Taking a starting set of "live" rules passed as an argument, we successively subtract
       the caserules component of each variant in the cases arg. The result is a subset
       of the original "live" ruleset that is disjoint from the rules of each caseAndor.
       Defaults are rules that are "live" at the OR node, not because they
       match the (explicit) discriminant of any variant, but because of a variable or
       wildcard.
       NOTE: the paths to vector "element" nodes (accessed via a VELEMS subcase)
       end in [..., VI k], with no DC(VLENcon _) link.
       and k is the index of the element in the vector.
    *)

    (* translateAndor : ruleset * rpath -> protoandor -> andor *)
    fun translateAndor (live, rpath) pandor =
	case pandor
	  of ANDp {varRules, children} =>
	       let val id = newId ()
		   fun folder (n, pandor, andors) =
		         translateAndor (live, P.addLinkR(P.PI n, rpath)) pandor :: andors
		in AND {id = id,
			children = List.foldli folder nil children}
	       end
	   | ORp {varRules, cases, sign} =>
	       let val id = newId ()
		   fun removeCaseRules ((_,caseRules,_): protoVariant, defaults: ruleset) =
		       RS.difference (defaults, caseRules)
		   val defaults = foldl removeCaseRules live cases
		   (* ASSERT: defaults subset live *)
		   fun transCase pcase = translateCase (pcase, defaults, (live, rpath))
		   val cases' = map transCase cases
		   val _ = if !debugging
			   then (say ("translateAndor: id = " ^ Int.toString id);
				 say "\n  live = ";
				 ppRules live;
				 say "  defaults = ";
				 ppRules defaults)
			   else ()
	       in OR {id = id, path = P.rpathToPath rpath, sign = sign, cases = cases',
		      defaults = defaults}
	       end
	   | VARp {varRules} =>
	       (* ASSERT: varRules not empty *)
	       let val id = newId ()
		in VAR {id = id}
	       end
	   | WCp => WC

    (* translateCase : protoVariant * ruleset * (ruleset * rpath) -> variant *)
    and translateCase ((con, caserules, subcase), defaults, (live, rpath)) =
	let val caseLive = RS.intersection(caserules, live)  (* == caserules?; CLAIM caserule subset live *)
	    val _ = (* check conjecture that caserules subset live *)
		    if RS.numItems caseLive < RS.numItems caserules
		    then saynl "@@@@@@@@@ #caseLive < #caserules @@@@@@@@@@"
		    else ()
	    val stillLive = RS.union (caseLive, defaults)
	    val _ =  if !debugging
		     then (say "translateCase: con = ";
			   ppCon con;
			   say "  caseLive = "; ppRules caseLive;
			   say "  stillLive = "; ppRules stillLive)
		     else ()
	  in case (con, subcase)
	      of (AS.VLENcon (k, ty), VELEMS elements) => (* no DC link introduced for vector-length "decon" *)
		   let fun folder (n, pandor, andors) =
			   translateAndor (live, P.addLinkR(P.VI(n,ty), rpath)) pandor
			   :: andors
		       val elements' = List.foldli folder nil elements
		   in (con, caseLive, VELEMS elements')
		   end
	       | (AS.DATAcon _, DCARG pandor) => (* non-constant datacon *)
		   (con, caseLive,
		    DCARG (translateAndor (stillLive, P.addLinkR(P.DC con, rpath)) pandor))
	       | (_, CONST) => (* con should be constant, not checked *)
		   (con, caseLive, CONST)
	       | _ => bug "translateCase: inconsistent cases"
	  end

 in translateAndor (allRules, P.rootpath) protoAndor
end (* makeAndor *)

end (* local *)
end (* structure Andor *)
