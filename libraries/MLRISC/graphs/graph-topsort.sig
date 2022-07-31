(*
 * This module returns a topological sort of an acyclic graph.
 * 
 * -- Allen
 *)

signature GRAPH_TOPOLOGICAL_SORT = 
sig

      (* topological sort *)

   val topsort : ('n,'e,'g) Graph.graph -> 
		    Graph.node_id list -> Graph.node_id list

end

