(*
 * This module implements Kruskal's algorithm for minimal cost
 * spanning tree.
 *
 * -- Allen
 *)

structure Kruskal : MIN_COST_SPANNING_TREE =
struct
   structure P = NodePartition
   structure Q = PriorityQueue
   structure G = Graph

   exception Unconnected

   fun spanning_tree { weight, < } (G as G.GRAPH G') add_edge u =
   let fun less (e1,e2) = weight e1 < weight e2
       val Q            = Q.create less 
       val _            = #forall_edges G' (Q.insert Q) 
       val P            = P.node_partition G
       fun make_tree(1,u) = u
         | make_tree(M,u) =
            let val e as (i,j,_) = Q.deleteMin Q
            in  if P.== P (i,j) then 
                   make_tree(M,u)
                else
                   (P.union' P (i,j); make_tree(M-1,add_edge(e,u)))
            end
   in  
       make_tree(#order G' (),u)
   end handle Q.EmptyPriorityQueue => raise Unconnected
end

