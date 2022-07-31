(*
 * Acyclic subgraph adaptor.  This takes a linear order of node id
 * return a view in which only the edges (and nodes) consistent with
 * the linear order is visible.
 *
 * -- Allen
 *)

signature ACYCLIC_SUBGRAPH_VIEW = 
sig

     (* Acyclic node induced subgraph *)
   val acyclic_view : Graph.node_id list ->
                      ('n,'e,'g) Graph.graph -> 
                      ('n,'e,'g) Graph.graph 
end

structure AcyclicSubgraphView : ACYCLIC_SUBGRAPH_VIEW =
struct

   structure G = Graph
   structure A = HashArray
   structure S = Subgraph_P_View

   fun acyclic_view nodes (G as G.GRAPH g) =
   let val ord = A.array(#capacity g (),~1)
       fun order(i,[]) = ()
         | order(i,n::ns) = (A.update(ord,n,i); order(i+1,ns))
       val _ = order(0,nodes)
       fun node_p i = A.sub(ord,i) >= 0 
       fun edge_p (i,j) = 
           let val i = A.sub(ord,i)
           in  i >= 0 andalso i < A.sub(ord,j) end
   in  S.subgraph_p_view nodes node_p edge_p G
   end

end

