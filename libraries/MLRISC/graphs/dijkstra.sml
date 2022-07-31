(*
 * This module implements the Dijkstra algorithm for computing
 * the single source shortest paths.
 *
 * -- Allen
 *)

functor Dijkstra(Num : ABELIAN_GROUP_WITH_INF) 
     : SINGLE_SOURCE_SHORTEST_PATHS =
struct

   structure Num = Num
   structure Q   = NodePriorityQueue(Array)
   structure G   = Graph
   structure A   = Array

   fun single_source_shortest_paths{ graph=G' as G.GRAPH G, weight, s } =
   let val N    = #capacity G ()
       val dist = A.array(N, Num.inf)
       val pred = A.array(N, ~1)
       val Q = Q.fromGraph (fn (i,j) => Num.<(A.sub(dist,i),A.sub(dist,j))) G'
       fun relax(e as (u,v,_)) =
       let val d_v = A.sub(dist,v)
           val d_x = Num.+(A.sub(dist,u),weight e)
       in  if Num.<(d_x,d_v) then 
             (A.update(dist,v,d_x); A.update(pred,v,u); Q.decreaseWeight(Q,v))
           else ()
       end
   in  A.update(dist,s,Num.zero);
       Q.decreaseWeight(Q,s);
       (while true do
          app relax (#out_edges G (Q.deleteMin Q))
       ) handle Q.EmptyPriorityQueue => ();
       { dist = dist,
         pred = pred
       }
   end
        
end
