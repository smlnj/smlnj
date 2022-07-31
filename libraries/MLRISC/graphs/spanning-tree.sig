(*
 * Signature for the minimal cost spanning tree problem.
 * All spanning tree edges are folded together with a user supplied
 * function.
 * 
 * -- Allen
 *)

signature MIN_COST_SPANNING_TREE =
sig

   exception Unconnected

   val spanning_tree : { weight    : 'e Graph.edge -> 'w,
                         <         : 'w * 'w -> bool
                       } -> ('n, 'e, 'g) Graph.graph 
                         -> ('e Graph.edge * 'x -> 'x) -> 'x -> 'x
end

