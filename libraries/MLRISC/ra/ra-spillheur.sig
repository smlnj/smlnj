(* 
 *  Spill heuristics should match the following signature.
 *)
signature RA_SPILL_HEURISTICS =
sig
   structure G : RA_GRAPH = RAGraph

   exception NoCandidate

   val mode : G.mode

   val init : unit -> unit

   val chooseSpillNode : 
       { graph          : G.interferenceGraph,
         spillWkl       : G.node list,
         hasBeenSpilled : int -> bool
       } ->
       { spillWkl : G.node list,
         node     : G.node option,
         cost     : real
       }
end
