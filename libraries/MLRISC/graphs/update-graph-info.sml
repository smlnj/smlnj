(*
 *  Update graph info.
 *
 *  -- Allen
 *)

signature UPDATE_GRAPH_INFO =
sig

   val update : ('n,'e,'g) Graph.graph -> 'g -> ('n,'e,'g) Graph.graph

end

structure UpdateGraphInfo : UPDATE_GRAPH_INFO =
struct
   
   structure G = Graph

   fun update (G.GRAPH G) info =
       G.GRAPH
       { name            = #name G,
         graph_info      = info,
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
         out_edges       = #out_edges G,
         in_edges        = #in_edges G,
         succ            = #succ G,
         pred            = #pred G,
         has_edge        = #has_edge G,
         has_node        = #has_node G,
         node_info       = #node_info G,
         entries         = #entries G,
         exits           = #exits G,
         entry_edges     = #entry_edges G,
         exit_edges      = #exit_edges G,
         forall_nodes    = #forall_nodes G,
         forall_edges    = #forall_edges G
       }
end

