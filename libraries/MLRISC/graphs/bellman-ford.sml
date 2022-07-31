(*
 * This module implements the Bellman Ford algorithm for single source
 * shortest paths.
 *
 * -- Allen
 *)

functor BellmanFord(Num : ABELIAN_GROUP_WITH_INF) : 
    sig include SINGLE_SOURCE_SHORTEST_PATHS 
        exception NegativeCycle
    end =
struct

   structure Num = Num
   structure G   = Graph
   structure A   = Array

   exception NegativeCycle

   fun single_source_shortest_paths {graph=G.GRAPH G,s,weight} =
   let val N      = #capacity G ()
       val dist   = A.array(N, Num.inf)
       val pred   = A.array(N, ~1)
       val count  = A.array(N, 0)
       fun driver([],[])  = ()
         | driver([],B)   = driver(rev B,[])
         | driver(u::A,B) = driver(iterate(u,A,B))
       and iterate(u,A,B) =
           let val n = Int.+(A.sub(count,u), 1)
               val _ = A.update(count,u,n)
               val _ = if n >= N then raise NegativeCycle else ()
               val du = A.sub(dist,u)
               fun relax([],A,B) = (A,B)
                 | relax((e as (_,v,_))::es,A,B) =
                   let val c = Num.+(du,weight e)
                   in  if Num.<(c,A.sub(dist,v)) then
                        (A.update(dist,v,c); A.update(pred,v,u);
                         relax(es,A,v::B))
                       else relax(es,A,B)
                   end
           in  relax(#out_edges G u,A,B) 
           end
   in  A.update(dist,s,Num.zero);
       driver([s],[]);
       { dist = dist,
         pred = pred
       }
   end
end

