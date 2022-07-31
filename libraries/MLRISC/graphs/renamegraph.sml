(*
 * This view adds some number k to all node ids of the graph,
 * i.e. rename all node ids.
 *
 * -- Allen
 *)

signature RENAMED_GRAPH_VIEW =
sig

   val rename_view : int -> ('n,'e,'g) Graph.graph -> ('n,'e,'g) Graph.graph

end

structure RenamedGraphView : RENAMED_GRAPH_VIEW =
struct

   structure G = Graph

   fun rename_view k (G.GRAPH G) =
   let fun rename_nodes ns = map (fn (i,n) => (i+k,n)) ns
       fun rename_edges es = map (fn (i,j,e) => (i+k,j+k,e)) es
       fun rename_node_ids ns = map (fn i => i+k) ns
       fun rename_node_ids' ns = (map (fn i => i-k) ns)
   in
       G.GRAPH
       { name            = #name G,
         graph_info      = #graph_info G,
         new_id          = fn n => #new_id G () + k,
         add_node        = fn (i,n) => #add_node G (i-k,n),
         add_edge        = fn (i,j,e) => #add_edge G (i-k,j-k,e),
         remove_node     = fn i => #remove_node G (i-k),
         set_out_edges   = fn (i,es) => #set_out_edges G (i-k,
                               map (fn (i,j,e) => (i-k,j-k,e)) es),
         set_in_edges    = fn (i,es) => #set_in_edges G (i-k,
                               map (fn (i,j,e) => (i-k,j-k,e)) es),
         set_entries     = fn ns => #set_exits G (rename_node_ids ns),
         set_exits       = fn ns => #set_entries G (rename_node_ids ns),
         garbage_collect = #garbage_collect G,
         nodes           = fn () => rename_nodes (#nodes G ()),
         edges           = fn () => rename_edges (#edges G ()),
         order           = #order G,
         size            = #size G,
         capacity        = #capacity G,
         out_edges       = fn i => rename_edges (#out_edges G (i-k)),
         in_edges        = fn i => rename_edges (#in_edges G (i-k)),
         succ            = fn i => rename_node_ids (#succ G (i-k)),
         pred            = fn i => rename_node_ids (#pred G (i-k)),
         has_edge        = fn (i,j) => #has_edge G (i-k,j-k),
         has_node        = fn i => #has_node G (i-k),
         node_info       = fn i => #node_info G (i-k),
         entries         = fn () => rename_node_ids (#entries G ()),
         exits           = fn () => rename_node_ids (#exits G ()),
         entry_edges     = fn i => rename_edges (#entry_edges G (i-k)),
         exit_edges      = fn i => rename_edges (#exit_edges G (i-k)),
         forall_nodes    = fn f => #forall_nodes G (fn (i,n) => f(i+k,n)),
         forall_edges    = fn f => #forall_edges G (fn (i,j,e)=> f(i+k,j+k,e))
       }
   end
end

