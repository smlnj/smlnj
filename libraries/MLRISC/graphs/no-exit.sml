(*
 * These modules provide views in which all entry or exit edges
 * are invisible.
 *
 * -- Allen
 *)

signature NO_ENTRY_VIEW = 
sig
    val no_entry_view : ('n,'e,'g) Graph.graph -> ('n,'e,'g) Graph.graph
end

signature NO_EXIT_VIEW = 
sig
    val no_exit_view : ('n,'e,'g) Graph.graph -> ('n,'e,'g) Graph.graph
end

structure NoEntryView : NO_ENTRY_VIEW =
struct

   structure G = Graph

   fun no_entry_view (G.GRAPH G) =
   let fun none _ = []
       fun unimplemented _ = raise G.Readonly
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
         set_entries     = unimplemented,
         set_exits       = #set_exits G,
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
         entries         = none,
         exits           = #exits G,
         entry_edges     = none,
         exit_edges      = #exit_edges G,
         forall_nodes    = #forall_nodes G,
         forall_edges    = #forall_edges G
       }
   end
end

structure NoExitView : NO_EXIT_VIEW =
struct

   structure G = Graph

   fun no_exit_view (G.GRAPH G) =
   let fun none _ = []
       fun unimplemented _ = raise G.Readonly
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
         set_entries     = #set_entries G,
         set_exits       = unimplemented,
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
         exits           = none,
         entry_edges     = #entry_edges G,
         exit_edges      = none,
         forall_nodes    = #forall_nodes G,
         forall_edges    = #forall_edges G
       }
   end
end
