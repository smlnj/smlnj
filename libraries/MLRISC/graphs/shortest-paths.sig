(*
 * Signatures for shortest paths problems
 *
 * -- Allen
 *)

signature SINGLE_SOURCE_SHORTEST_PATHS =
sig

   structure Num : ABELIAN_GROUP_WITH_INF

   val single_source_shortest_paths :
                 { graph  : ('n,'e,'g') Graph.graph,
                   weight : 'e Graph.edge -> Num.elem,
                   s      : Graph.node_id
                 } -> 
                 { dist : Num.elem Array.array,
                   pred : Graph.node_id Array.array
                 }
end

signature ALL_PAIRS_SHORTEST_PATHS =
sig
   structure Num : ABELIAN_GROUP_WITH_INF
   val all_pairs_shortest_paths :  
                 { graph  : ('n,'e,'g') Graph.graph,
                   weight : 'e Graph.edge -> Num.elem
                 } -> 
                 { dist : Num.elem Array2.array,
                   pred : Graph.node_id Array2.array
                 }
end
