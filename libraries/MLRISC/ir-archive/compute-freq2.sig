(*
 * This module computes frequencies when given branch probabilities
 * It has been generalized from the old static branch predication 
 * so that it can be applied to other graph based reprensentations. 
 *
 * -- Allen
 *)

signature COMPUTE_FREQUENCIES2 =
sig

   structure Derived : DERIVED_GRAPH
   structure W       : FREQ

   val compute_frequencies : 
       { cfg            : ('n,'e,'g) Graph.graph,
         derived        : ('n,'e) Derived.derived_graph,
           (* multiplier for each loop nesting *) 
         loopMultiplier : int,
         nodeFreq       : 'n -> W.freq ref, (* frequency of a node *)
         edgeFreq       : 'e -> W.freq ref, (* frequency of an edge *)
         branchProb     : 'n -> int,        (* branch probability of a node *)
           (* is the edge a taken branch edge? *)
         isTakenBranch  : 'e Graph.edge -> bool 
       } -> unit

end
