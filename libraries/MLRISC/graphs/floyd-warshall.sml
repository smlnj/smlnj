(* 
 * This module implements the Floyd/Warshall algorithm for computing
 * all pairs shortest paths.
 *
 * -- Allen
 *)

functor FloydWarshall(Num : ABELIAN_GROUP_WITH_INF) 
   : ALL_PAIRS_SHORTEST_PATHS =
struct

   structure Num = Num
   structure G   = Graph
   structure A   = Array2

   fun all_pairs_shortest_paths{graph=G.GRAPH G,weight} =
   let val N = #capacity G ()
       val D = A.array(N,N,Num.inf)
       val P = A.array(N,N,~1)
       fun init() =
       let fun diag ~1 = ()
             | diag i  = (A.update(D,i,i,Num.zero); diag(i-1))
       in  diag(N-1);
           #forall_edges G (fn e as (i,j,_) =>
            let val w   = weight e
            in  if Num.<(w,A.sub(D,i,j)) then
                   (A.update(P,i,j,i); A.update(D,i,j,w))
                else ()
            end)
       end
       fun l1(k)   = if k < N then (l2(k,0); l1(k+1)) else ()
       and l2(k,i) = if i < N then (l3(k,i,0,A.sub(D,i,k)); l2(k,i+1)) else ()
       and l3(k,i,j,d_ik) = if j < N then 
              let val d_ij = A.sub(D,i,j)
                  val d_kj = A.sub(D,k,j)
                  val x = Num.+(d_ik,d_kj)
              in  if Num.<(x,d_ij) then
                     (A.update(P,i,j,A.sub(P,k,j)); A.update(D,i,j,x))
                  else ();
                  l3(k,i,j+1,d_ik)
              end else ()
   in  init();
       l1(0);
       {dist=D,pred=P}
   end
end
