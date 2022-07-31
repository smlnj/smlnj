(*
 *  This view make a graph readonly.
 *
 *  -- Allen
 *)

signature READONLY_GRAPH_VIEW =
sig
  
   val readonly_view : ('n,'e,'g) Graph.graph -> ('n,'e,'g) Graph.graph

end

structure ReadOnlyGraphView : READONLY_GRAPH_VIEW =
struct

   structure G = Graph

   fun readonly_view (G.GRAPH G) =
   let fun unimplemented _ = raise G.Readonly
   in
       G.GRAPH
       { name            = #name G,
         graph_info      = #graph_info G,
         new_id          = unimplemented,
         add_node        = unimplemented,
         add_edge        = unimplemented,
         remove_node     = unimplemented,
         set_in_edges    = unimplemented,
         set_out_edges   = unimplemented,
         set_entries     = unimplemented,
         set_exits       = unimplemented,
         garbage_collect = unimplemented,
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
         (* fold_nodes      = #fold_nodes G,
         fold_edges      = #fold_edges G *)
       }
   end
end

