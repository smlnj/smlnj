(*
 * Trace subgraph adaptor.  This takes a linear list of node ids.
 * The view returned is the part of the graph that lies on this linear list.
 *)

signature TRACE_SUBGRAPH_VIEW = 
sig

   val trace_view : Graph.node_id list ->
                      ('n,'e,'g) Graph.graph -> 
                      ('n,'e,'g) Graph.graph 
end

structure TraceView : TRACE_SUBGRAPH_VIEW =
struct

   structure G = Graph
   structure A = HashArray
   structure S = Subgraph_P_View

   fun trace_view nodes (G as G.GRAPH g) =
   let val ord = A.array(#capacity g (),~100)
       fun order(i,[]) = ()
         | order(i,n::ns) = (A.update(ord,n,i); order(i+1,ns))
       val _ = order(0,nodes)
       fun node_p i = A.sub(ord,i) >= 0 
       fun edge_p (i,j) = A.sub(ord,i) + 1 = A.sub(ord,j) 
   in  S.subgraph_p_view nodes node_p edge_p G
   end

end

