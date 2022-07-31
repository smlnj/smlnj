(*
 *  Undirected graph view.  This makes a graph look undirected.
 *
 *  -- Allen
 *)

signature UNDIRECTED_GRAPH_VIEW =
sig

   val undirected_view : ('n,'e,'g) Graph.graph -> ('n,'e,'g) Graph.graph

end

structure UndirectedGraphView : UNDIRECTED_GRAPH_VIEW =
struct
   
   structure G = Graph
   structure Sort = ListMergeSort

   fun undirected_view (G.GRAPH G) =
   let fun adjacent_edges i =
       let val in_edges  = map (fn (i,j,e) => (j,i,e)) (#in_edges G i)
           val out_edges = #out_edges G i
       in
           Sort.uniqueSort (fn ((i,j,_),(i',j',_)) =>
                              if i < i' then LESS 
                              else if i = i' then 
                                   if j < j' then LESS
                                   else if j = j' then EQUAL
                                   else GREATER
                              else GREATER)
                          (in_edges @ out_edges)
       end
       fun adjacent_nodes i =
       let val succ = #succ G i
           val pred = #pred G i
       in
           Sort.uniqueSort Int.compare (succ @ pred)
       end

       fun has_edge (i,j) = #has_edge G (i,j) orelse #has_edge G (j,i)

   in
       G.GRAPH
       { name            = #name G,
         graph_info      = #graph_info G,
         new_id          = #new_id G,
         add_node        = #add_node G,
         add_edge        = #add_edge G,
         remove_node     = #remove_node G,
         set_in_edges    = #set_in_edges G,
         set_out_edges   = #set_out_edges G,
         set_entries     = #set_exits G,
         set_exits       = #set_entries G,
         garbage_collect = #garbage_collect G,
         nodes           = #nodes G,
         edges           = #edges G,
         order           = #order G,
         size            = #size G,
         capacity        = #capacity G,
         out_edges       = adjacent_edges,
         in_edges        = adjacent_edges,
         succ            = adjacent_nodes,
         pred            = adjacent_nodes,
         has_edge        = has_edge,
         has_node        = #has_node G,
         node_info       = #node_info G,
         entries         = #exits G,
         exits           = #entries G,
         entry_edges     = #entry_edges G,
         exit_edges      = #exit_edges G,
         forall_nodes    = #forall_nodes G,
         forall_edges    = #forall_edges G
       }
   end
end

