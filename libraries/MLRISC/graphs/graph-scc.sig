(*
 * This module computes strongly connected components (SCC) of
 * a graph.  Each SCC is represented as a list of nodes.  All nodes
 * are folded together with a user supplied function.
 *
 * -- Allen
 *)

signature GRAPH_STRONGLY_CONNECTED_COMPONENTS = 
sig

      (* strongly connected components *)

   val scc : ('n,'e,'g) Graph.graph -> 
	       (Graph.node_id list * 'a -> 'a) -> 'a -> 'a

   val scc' : {N         : int,
               nodes     : Graph.node_id list,
               out_edges : Graph.node_id -> 'e Graph.edge list
              } -> (Graph.node_id list * 'a -> 'a) -> 'a -> 'a

end

