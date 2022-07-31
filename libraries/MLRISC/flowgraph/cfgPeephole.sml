(*
 * Run peephole optimization on a cluster
 *)
functor CFGPeephole
  (structure CFG      : CONTROL_FLOW_GRAPH
   structure PeepHole : PEEPHOLE
     sharing CFG.I = PeepHole.I
  ) : CFG_OPTIMIZATION =
struct
   structure CFG = CFG

   val name = "Peephole optimization"

   fun run (cfg as Graph.GRAPH graph) = let
         fun opt (_, CFG.BLOCK{insns, ...}) = insns := PeepHole.peephole(rev(!insns))
	 in
	   #forall_nodes graph opt;
	   cfg
	 end

end
