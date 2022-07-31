(*
 * This module computes frequencies when given branch probabilities
 * It has been generalized from the old static branch predication 
 * so that it can be applied to other graph based reprensentations. 
 *
 * -- Allen
 *)

signature COMPUTE_FREQUENCIES =
sig

   structure Loop : LOOP_STRUCTURE
   structure W    : FREQ

   val compute_frequencies : 
       { cfg            : ('n,'e,'g) Graph.graph,
         loop           : ('n,'e,'g) Loop.loop_structure,
           (* multiplier for each loop nesting *) 
         loopMultiplier : int,
         nodeFreq       : 'n -> W.freq ref, (* frequency of a node *)
         edgeFreq       : 'e -> W.freq ref, (* frequency of an edge *)
         branchProb     : 'n -> int,        (* branch probability of a node *)
           (* is the edge a taken branch edge? *)
         isTakenBranch  : 'e Graph.edge -> bool 
       } -> unit

end
