(* FLINT/trans/decisiontree.sml *)
(* revised "old" match compiler *)

(* building decision trees *)

structure DecisionTree =
struct

local

  structure DA = Access
  structure IM = Andor.IM
  open MCCommon

  val debugging = MCControl.mcdebugging

  fun bug msg = ErrorMsg.impossible ("DecisionTree: " ^ msg)

  val say = Control_Print.say
  fun newline () = say "\n"
  fun saynl msg = (say msg; newline())
  fun says strings = saynl (concat strings)

  fun dbsay msg =
      if !debugging
      then (say msg; newline())
      else ()
  fun dbsays msgs = dbsay (concat msgs)

  (* signToWidth : DA.sign -> int
   *  calculate the number of datacons in a datatype ("width") from the
   *  DA.sign value (a component of a datacon).
   *  NOTE: This belongs in TypesUtil. *)
  fun signToWidth (DA.CSIG (i, j)) = i+j
    | signToWidth DA.CNIL = Types.infinity

in

(* relevant : andor * ruleno -> bool *)
(*   the andor node is "relevant" to the rule/pattern indexed by ruleno *)
fun relevant (OR{defaults,...}, ruleno) = not (RS.member(defaults, ruleno))
  | relevant _ = bug "relevant - ORd expected"

(* metric : andor -> int (>= 0)
 *   The "quality" metric for the OR node. Only defined for OR nodes. *)
fun metric (OR {cases, defaults,...}) = 10000 * (RS.numItems defaults) + length cases
  | metric _ = bug "metric - OR expected"

(* Node metric (int):
 *  Better: smaller number of defaults, or equal number of defaults and smaller number
 *  of cases; returns true if first arg is (strictly) better than second. Pair of
 *  metrics encoded as a single int.
 *    The intuition is that (1) more defaults makes an OR node less discriminating,
 *    and (2) favoring fewer number of cases makes the dectree less "bushy".
 *  *)

(* better: int * int -> bool *)
(* fun better((a:int,b:int),(c,d)) = a < c orelse (a = c andalso b < d) *)
fun better (m1, m2) = Int.< (m1, m2)

(* --------------------------------------------------------------------------- *)
(* ORnode queues ordered by metric -- simple version of a priority queue *)

(* nodeQueue: a list of (metric, node), where node: andor[OR] and metric is metric(node),
   ordered by "better" order on the metric, i.e. best is first element of queue. *)
type nodeQueue = (int * andor) list

(* emptyQ : nodeQueue *)
val emptyQ : nodeQueue = nil

(* insertQ : andor[OR] * nodeQueue -> nodeQueue *)
fun insertQ (andor, nodeQ) =
    let val m0 = metric andor
	fun ins (queue as ((m1, node)::queue')) =
	    if better (m0, m1)
	    then (m0, andor) :: queue
	    else (m1, node) :: ins queue'
	  | ins nil = [(m0, andor)]
     in ins nodeQ
    end

(* insertListQ : andor[OR] list * nodeQueue -> nodeQueue *)
fun insertListQ (nodes, nodeQ) = foldr insertQ nodeQ nodes

(* selectBest : (andor -> bool) * nodeQueue -> (andor * nodeQueue) option *)
(* returns the node with the best metric that satisfies the predicate pred *)
fun selectBest (pred, nodeQ) =
    let fun scan (nil, _) = NONE
	  | scan ((head as (_, node)) :: nodeQ, passed) = 
	    if pred node
	    then SOME (node, List.revAppend (passed, nodeQ))
	    else scan (nodeQ, head :: passed)
    in scan (nodeQ, nil)
    end

(* bestOrNode : nodeQueue * ruleset = (andor * nodeQueue) option *)
fun bestOrNode (nodeQ, live) = 
    let val minLive = RS.minItem live
	fun pred node = relevant (node, RS.minItem live)
     in selectBest (pred, nodeQ)
    end
 
(* accessible OR nodes *)
(* accessible : andor * nodeQueue -> nodeQueue *)
(* inserts the "accessible" OR nodes in this andor tree into the nodeQ *)
fun accessible (andor, nodeQ) =
    (case andor
       of (VAR _ | WC) => nodeQ  (* no OR nodes here *)
        | AND {children, ...} =>
	    accessibleList (children, nodeQ)  (* add OR nodes from children *)
        | OR _ => insertQ(andor, nodeQ))  (* insert this OR node into queue *)

(* accessibleList : andor[OR] list * nodeQueue -> nodeQueue *)
and accessibleList (andors, nodeQ) = foldl accessible nodeQ andors

(* makeDectree: andor * ruleset * relmap -> dectree * relmap
 *  On initial (and only) call, in matchcomp.sml: live = allRules *)
fun makeDectree (andor, live) =
let (* val relmapRef = ref relmap
   (* relmap is only used to record new relevant nodes, based on leading to an OR-SWITCH node, i.e.
    * it is only written, not read *)

    (* addRelevantRules : ruleset * trail -> unit *)
    fun addRelevantRules (relrules: ruleset, trail : nodeId list) : unit =
	let fun update (id, relmap) =
		(case IM.find (relmap, id)
		   of NONE => IM.insert (relmap, id, relrules)
		    | SOME rules' => IM.insert (relmap, id, RS.union(relrules, rules')))
	in if RS.empty relrules
	   then ()
	   else relmapRef := foldl update (!relmapRef) trail
	end
*)
    (* mkDecTree : (nodeQueue * ruleset -> dectree
     *  (dynamic) live rules = rules still "in play" = rules not in conflict with earlier choices
     *   It is possible that dtCases may be empty, in which case (if liveDefaults not empy) we 
     *   produce a degenerate switch with no cases but a default. Could produce just the default
     *   instead?  What about when dtCases is empty and liveDefaults is empty? Should produce FAIL
     *   in this case? *)
    fun mkDecTree (nodeQ: nodeQueue, live: ruleset) =
	  if RS.isEmpty live then FAIL else  (* no live rules, raise MATCH/BIND/unhandled exception *)
	  (case bestOrNode(nodeQ, live)
	     of (SOME(node as (OR{id, path, sign, cases, defaults}), remainder)) =>
		 let val casesRules = foldl RS.union RS.empty (map #2 cases) (* union of caserules *)
		     val casesLive = RS.intersection (casesRules, live)  (* subset of live *)
		     val dtCases = mkcases (cases, remainder, defaults, live)
		     val liveDefaults = RS.intersection (live, defaults)
		         (* subset of live, can be proper subset of defaults, can be empty *)
		     val stillLive = RS.union (casesLive, liveDefaults)
		     val defaultDtOp =
			 if length dtCases = signToWidth sign  (* remaining live OR-cases saturate the type? *)
			 then NONE (* dtCases saturated the type *)
			 else SOME (mkDecTree (remainder, liveDefaults))
			      (* default needed to cover con variants that were absent or killed *)
		 in SWITCH {id=id, path=path, sign=sign, cases=dtCases, defaultOp=defaultDtOp, live = stillLive}
		 end
	      | NONE => RHS (RS.minItem live))
		 (* no further choices => no more rules can be eliminated. In particular,
		  * the minimum ruleno in live can't be eliminated, so it is selected. *)

    (* mkcases : variant list * nodeQueue * (defaults: ruleset) * (live: ruleset) -> (con * dectree) list
     *  ASSERT: (1) length of result (decTree) cases <= length of input (OR-node) cases.
     *     decTree cases may be fewer if some cons cases are not "live" *)
    and mkcases (cases: variant list, nodeQ: nodeQueue, defaults: ruleset, live: ruleset) =
	let (* mkCase: variant -> (con * dectree) option; andor-case -> switch-case option *)
	    fun mkCase (con, caseRules, subcase) =
		let val caseLive = RS.intersection (caseRules, live) (* rules with con "here" *)
		    val defaultsLive = RS.intersection (defaults, live)
		    val casePlusDefaultsLive = RS.union (caseLive, defaultsLive)
		in if RS.isEmpty caseLive then NONE else
		     let val dectree =
			     (case subcase
			       of CONST => mkDecTree (nodeQ, casePlusDefaultsLive)
					(* no andor subtree to destruct; go to next OR choice *)
				| DCARG andor => mkDecTree (accessible (andor, nodeQ), casePlusDefaultsLive)
				| VELEMS elems => mkDecTree (accessibleList (elems, nodeQ), casePlusDefaultsLive))
		      in SOME (con, dectree)
		     end
		end
	 in List.mapPartial mkCase cases
    end

 in mkDecTree (accessible (andor, emptyQ), live)
end (* makeDectree *)

(* =========================================================================== *)
(* collecting decision tree stats *)
	      
structure NodeMap = IntRedBlackMap

type decTreeStats =
     {rulesUsed : RS.set,
      failures : int,
      choiceTotal : int,
      choiceDist : int NodeMap.map}

(* decTreeStats : dectree * int -> decTreeStats
 *  returns the set of all rules used in the dectree, maintaining ordering
 *  (because union operation does). The boolean value indicates presence of FAIL,
 *  signalling that the rules are non-exhaustive. *)
fun decTreeStats (dectree: dectree): decTreeStats =
    let val rules = ref RS.empty
	val failures = ref 0
	val choiceTotal = ref 0
	val choiceDist = ref NodeMap.empty
	fun scanTree (RHS n) = (rules := RS.add (!rules, n)) 
	  | scanTree FAIL = (failures := !failures + 1)
	  | scanTree (SWITCH {id, cases, defaultOp, ...}) =
	    (choiceTotal := !choiceTotal + 1;
	     choiceDist :=
		let val nmap = !choiceDist
		    val newcount =
			case NodeMap.find (nmap, id)
			 of NONE => 1
			  | SOME k => k+1
		in NodeMap.insert(nmap, id, newcount)
		end;
	     app (fn (_, dt) => scanTree dt) cases;
	     Option.app scanTree defaultOp)
    in scanTree dectree;
       {rulesUsed = !rules,
	failures = !failures,
	choiceTotal = !choiceTotal,
	choiceDist = !choiceDist}
    end

end (* top local *)
end (* structure DecisionTree *)

(* NOTES:
Example OR case where defaults is not a subset of live? (or defaults disjoint from live)
Example where caseLive is a proper subset of casePlusDefaultsLive?
*)
