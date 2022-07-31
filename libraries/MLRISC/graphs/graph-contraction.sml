(*
 *  Allows contraction of a set of nodes and replace it by a new node
 * 
 * -- Allen
 *)

signature GRAPH_MINOR =
sig

   val minor_view : 
      ('n,'e,'g) Graph.graph ->
      { minor    : ('n,'e,'g) Graph.graph,
        contract : Graph.node_id list * 'n Graph.node -> unit
      }
end

structure GraphMinor : GRAPH_MINOR =
struct

   structure G = Graph

   datatype node = 

   fun minor_view(G.GRAPH G) =
   let fun get n =
       val minor =
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
    in { minor = minor,
         contract = contract
       }
   end
end

