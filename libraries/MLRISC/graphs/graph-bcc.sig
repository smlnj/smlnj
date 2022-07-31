(*
 * This module computes biconnected components. 
 * The function works a fold: all biconnected edges are ``folded.''
 *
 * -- Allen
 *)

signature GRAPH_BICONNECTED_COMPONENTS = 
sig

      (* bi-connected components *)

   val biconnected_components : ('n,'e,'g) Graph.graph -> 
		    ('e Graph.edge list * 'a -> 'a) -> 'a -> 'a

end
