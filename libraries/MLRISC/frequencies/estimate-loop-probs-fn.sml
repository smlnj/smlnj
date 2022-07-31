(* estimate-loop-probs-fn.sml
 *
 * COPYRIGHT (c) 2002 Bell Labs, Lucent Technologies.
 *
 * Given a CFG that may have some existing edge probabilities
 * (represented as BRANCHPROB annotations) add probabilities
 * based on the loop structure using the heuristics from Ball-Larus
 * and Wu-Larus.
 *
 * TODO:
 *	generalize to switch edges
 *	Loop Header Heuristic
 *)

functor EstimateLoopProbsFn (
    structure CFG : CONTROL_FLOW_GRAPH
  ) : sig

    structure CFG : CONTROL_FLOW_GRAPH

    val estimate : CFG.cfg -> unit

  end = struct

    structure CFG = CFG
    structure Dom = DominatorTree(DirectedGraph)
    structure LP = LoopStructure(
      structure GraphImpl = DirectedGraph
      structure Dom = Dom)
    structure An = Annotations
    structure Prob = Probability

  (* flags *)
    val disable = MLRiscControl.mkFlag (
	  "disable-loop-probabilities",
	  "when true, loop probability estimates are disabled")
    val dumpCFG = MLRiscControl.mkFlag (
	  "dump-cfg-after-loop-probabilities",
	  "when true, flow graph is shown after loop probability estimates")
    val dumpStrm = MLRiscControl.debug_stream

    local
      structure A = MLRiscAnnotations
      val {get, set, ...} = A.BRANCH_PROB
    in
    fun getEdgeProb (_, _, CFG.EDGE{a, ...}) = get(!a)
    fun setEdgeProb ((_, _, CFG.EDGE{a, ...}), p) = a := set(p, !a)
    end

  (* probabilities *)
    val probLBH = Prob.percent 88	(* Loob Branch Heuristic *)
    val probLEH = Prob.percent 80	(* Loop Exit Heuristic *)
    val prob50_50 = 50

  (* Compute loop structure information *)
    fun computeLoopStructure cfg = let
	  val domTree = Dom.makeDominator cfg
	  val dominates = Dom.dominates domTree
	  val Graph.GRAPH{has_node,  forall_nodes, ...} = LP.loop_structure domTree
	  in
	    { isLoopHeader = has_node,
	      forallLoops = forall_nodes
	    }
	  end

    fun sameEdge ((_, _, CFG.EDGE{a, ...}), (_, _, CFG.EDGE{a=a', ...})) = (a = a')

  (* add loop probabilities *)
    fun doEstimate (cfg as Graph.GRAPH{out_edges, ...}) = let
	  val {isLoopHeader, forallLoops} = computeLoopStructure cfg
	  fun computeProbs (trueE, falseE, takenProb) = let
		val {t, f} = (case (getEdgeProb trueE, getEdgeProb falseE)
		       of (NONE, NONE) =>
			    {t=takenProb, f=Prob.-(Prob.always, takenProb)}
			| (SOME p, _) =>
			    Prob.combineProb2 {trueProb=p, takenProb=takenProb}
			| (_, SOME p) => 
			    Prob.combineProb2 {
				trueProb=Prob.-(Prob.always, p),
				takenProb=takenProb
			      }
		      (* end case *))
		in
		  setEdgeProb (trueE, t);
		  setEdgeProb (falseE, f)
		end
	  fun doLoop (hdrId, LP.LOOP{backedges, exits, ...}) = let
	      (* apply the Loop Branch Heuristic to a back edge *)
		fun doBackEdge (e as (src, _, _)) = (case out_edges src
		       of [e1, e2] => if sameEdge(e, e1)
			    then computeProbs (e1, e2, probLBH)
			    else computeProbs (e2, e1, probLBH)
			| _ => ()
		      (* end case *))
	      (* apply the Loop Exit Heuristic to an exit edges; note that
	       * the probability is that the loop will NOT be exited.
	       *)
		fun doExitEdge (e as (src, _, _)) = (case out_edges src
		       of [e1, e2] =>
			    if sameEdge(e, e1)
			      then if isLoopHeader (#2 e2)
				then ()
			      (* e1 is exit edge, so e2 is taken branch *)
				else computeProbs (e2, e1, probLEH)
			      else if isLoopHeader (#2 e1)
				then ()
			      (* e2 is exit edge, so e1 is taken branch *)
				else computeProbs (e1, e2, probLEH)
			| _ => ()
		      (* end case *))
		in
		  List.app doBackEdge backedges;
		  List.app doExitEdge exits
		end
	  in
	    forallLoops doLoop
	  end

    fun estimate cfg = if !disable
	  then  ()
	  else (
	    doEstimate cfg;
	    if !dumpCFG
	      then CFG.dump (
		  !MLRiscControl.debug_stream,
		  "after loop probability estimates", cfg)
	      else ())

  end
