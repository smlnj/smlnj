(*
 * This module enumerates all simple cycles in a graph.
 * Each cycle is reprensented as a list of edges.  Adjacent edges
 * are adjacent in the list.  The function works like fold: all cycles
 * are ``folded'' together with a user supplied function.
 *
 * -- Allen
 *)

signature GRAPH_SIMPLE_CYCLES = 
sig

      (* enumerate all simple cycles *)

   val cycles : ('n,'e,'g) Graph.graph -> 
		    ('e Graph.edge list * 'a -> 'a) -> 'a -> 'a

end

