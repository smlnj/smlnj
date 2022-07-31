(* 
 * This is Johnson's algorithm for computing all pairs shortest paths.
 * Good for sparse graphs.
 * -- Allen
 *)

functor Johnson(Num : ABELIAN_GROUP_WITH_INF) : 
    sig include ALL_PAIRS_SHORTEST_PATHS 
        exception NegativeCycle
    end =
struct

   structure Num = Num
   structure G   = Graph
   structure A2  = Array2
   structure A   = Array
   structure D   = Dijkstra(Num)
   structure BF  = BellmanFord(Num)
   structure GI  = DirectedGraph(HashArray)
   structure U   = UnionGraphView

   exception NegativeCycle = BF.NegativeCycle

   fun all_pairs_shortest_paths
         {graph=G as G.GRAPH g : ('n,'e,'g) G.graph,weight} =
   let val N    = #capacity g ()
       val dist = A2.array(N,N,Num.inf)
       val pred = A2.array(N,N,~1)
       exception EDGE of 'e
       exception NODE of 'n
       exception Empty
       fun arbEdge() = 
           (#forall_edges g (fn (_,_,e) => raise EDGE e); raise Empty)
                       handle EDGE e => e
       fun arbNode() = 
           (#forall_nodes g (fn (_,n) => raise NODE n); raise Empty)
                       handle NODE n => n
   in  let val e    = arbEdge()
           val n    = arbNode()
           val G' as G.GRAPH g' = GI.graph("dummy source",#graph_info g,1)
           val G''  = U.union_view (fn (a,b) => a) (G,G')
           val op+  = Num.+
           val op-  = Num.-
           val s    = N
           val _    = #forall_nodes g (fn (v,_) => #add_edge g' (s,v,e))
           val _    = #add_node g' (s,n)
           fun weight'(u,v,e) = if u = s then Num.zero else weight(u,v,e)
           val {dist=h,...} = D.single_source_shortest_paths
                                  {graph=G'',s=s,weight=weight'}
           fun weight''(u,v,e) = weight(u,v,e) + A.sub(h,u) - A.sub(h,v)
       in  #forall_nodes g
              (fn (u,_) =>
               let val {dist=d,pred=p} = BF.single_source_shortest_paths
                                           {graph=G,s=u,weight=weight''}
                   val h_u = A.sub(h,u)
               in  #forall_nodes g (fn (v,_) =>
                    (A2.update(dist,u,v,A.sub(d,v) + A.sub(h,v) - h_u);
                     A2.update(pred,u,v,A.sub(p,v))))
               end)
       end handle Empty => ();
       {dist=dist,pred=pred}
   end 
end
