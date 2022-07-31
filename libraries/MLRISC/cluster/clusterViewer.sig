(*
 * This is a simple module for viewing a cluster graph graphically.
 * This is meant to be used only by those of you who don't want to 
 * migrate to the CFG data structure.
 *
 * -- Allen
 *)
signature CLUSTER_VIEWER =
sig

   structure ClusterGraph : CLUSTER_GRAPH

   val view : ClusterGraph.cfg -> unit
  
end
