(*
 * This gives a cluster a graph view so that all graph based algorithms
 * can be applied on the cluster.  The view is readonly though. 
 *
 * -- Allen
 *)
signature CLUSTER_GRAPH =
sig

   structure F : FLOWGRAPH
   structure I : INSTRUCTIONS
   structure W : FREQ
     sharing F.W = W
     sharing F.I = I

   type info
   type block = F.block
   type edge_info = W.freq ref

   type cfg = (block,edge_info,info) Graph.graph

   val clusterGraph   : F.cluster -> cfg
   val cluster        : cfg -> F.cluster
   val table          : cfg -> block Array.array
   val isTakenBranch  : edge_info Graph.edge -> bool

   val annotations    : cfg -> Annotations.annotations ref

   val insns          : block -> I.instruction list ref
   val freq           : block -> W.freq ref
   val liveOut        : block -> I.C.cellset

end
