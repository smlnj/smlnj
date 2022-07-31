(*
 * Minimal cut of a graph.  The graph is treated as undirected.
 * Note: the graph must be simple!
 * 
 * -- Allen
 *)
signature MIN_CUT =
sig
   structure Num : ABELIAN_GROUP
   val min_cut : { graph  : ('n,'e,'g) Graph.graph,
                   weight : 'e Graph.edge -> Num.elem
                 } -> Graph.node_id list * Num.elem
end
