(* complete-probs-fn.sml
 *
 * COPYRIGHT (c) 2002 Bell Labs, Lucent Technologies
 *
 * Given a CFG that may have incomplete branch probability information,
 * fill in the information.
 *)


functor CompleteProbsFn (
    structure CFG : CONTROL_FLOW_GRAPH
  (* function to record edge probabilities *)
    val recordProb : (CFG.edge_info * real) -> unit
  ) : sig

    structure CFG : CONTROL_FLOW_GRAPH

    val completeProbs : CFG.cfg -> unit

  end = struct

    structure CFG = CFG
    structure Prob = Probability

    val dumpCFG = MLRiscControl.mkFlag
		      ("dump-cfg-after-complete-probs",
		       "when true, CFG is output after probability completion")

    val {get=getProb, ...} = MLRiscAnnotations.BRANCH_PROB

  (* Complete edge probabilities. *)
    fun completeProbs (cfg as Graph.GRAPH{forall_nodes, out_edges, ...}) = let
	  fun doBlock (blkId, _) = let
		fun computeProbs ((_, _, e as CFG.EDGE{a, ...})::r, remaining, n, es) = (
		      case getProb(!a)
		       of NONE => computeProbs (r, remaining, n+1, e::es)
			| SOME p => (
			    recordProb (e, Prob.toReal p);
			    computeProbs (r, Prob.-(remaining, p), n, es))
		      (* end case *))
		  | computeProbs ([], _, 0, _) = ()
		  | computeProbs ([], remaining, n, es) = let
		      val p = Prob.toReal(Prob./(remaining, n))
		      in
			List.app (fn e => recordProb (e, p)) es
		      end
		in
		  computeProbs (out_edges blkId, Prob.always, 0, [])
		end
	  in
	    forall_nodes doBlock
	    before (if !dumpCFG then CFG.dump (!MLRiscControl.debug_stream,
					      "after probability completion", cfg)
		   else ())
	  end

  end
