(*
 * Signature of max flow computation
 *
 * -- Allen
 *)

signature MAX_FLOW =
sig
   structure Num : ABELIAN_GROUP
   val max_flow :          { graph    : ('n,'e,'g) Graph.graph,
                             s        : Graph.node_id,
                             t        : Graph.node_id,
                             capacity : 'e Graph.edge -> Num.elem,
                             flows    : 'e Graph.edge * Num.elem -> unit
                           } -> Num.elem

   val min_cost_max_flow : { graph    : ('n,'e,'g) Graph.graph,
                             s        : Graph.node_id,
                             t        : Graph.node_id,
                             capacity : 'e Graph.edge -> Num.elem,
                             cost     : 'e Graph.edge -> Num.elem,
                             flows    : 'e Graph.edge * Num.elem -> unit
                           } -> Num.elem
end
