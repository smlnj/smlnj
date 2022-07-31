(*
 *  The transpose of a graph
 *
 *  -- Allen
 *)

signature REVERSED_GRAPH_VIEW =
sig

   val rev_view : ('n,'e,'g) Graph.graph -> ('n,'e,'g) Graph.graph

end

structure ReversedGraphView : REVERSED_GRAPH_VIEW =
struct
   
   structure G = Graph

   fun rev_view (G.GRAPH G) =
   let fun swap  f (i,j,e) = f (j,i,e)
       fun swap' f (i,j) = f (j,i)
       fun swap'' (i,j,e) = (j,i,e)
       fun flip es = map (fn (i,j,e) => (j,i,e)) es
   in
       G.GRAPH
       { name            = #name G,
         graph_info      = #graph_info G,
         new_id          = #new_id G,
         add_node        = #add_node G,
         add_edge        = swap (#add_edge G),
         remove_node     = #remove_node G,
         set_in_edges    = fn (j,es) => #set_out_edges G (j,flip es),
         set_out_edges   = fn (i,es) => #set_in_edges G (i,flip es),
         set_entries     = #set_exits G,
         set_exits       = #set_entries G,
         garbage_collect = #garbage_collect G,
         nodes           = #nodes G,
         edges           = fn () => map swap'' (#edges G ()),
         order           = #order G,
         size            = #size G,
         capacity        = #capacity G,
         out_edges       = fn i => map swap'' (#in_edges G i),
         in_edges        = fn i => map swap'' (#out_edges G i),
         succ            = #pred G,
         pred            = #succ G,
         has_edge        = swap' (#has_edge G),
         has_node        = #has_node G,
         node_info       = #node_info G,
         entries         = #exits G,
         exits           = #entries G,
         entry_edges     = #exit_edges G,
         exit_edges      = #entry_edges G,
         forall_nodes    = #forall_nodes G,
         forall_edges    = fn f => #forall_edges G (swap f)
	 (*
         fold_nodes      = #fold_nodes G,
         fold_edges      = fn f => fn u => 
            #fold_edges G (fn ((i,j,e),l) => f((j,i,e),l)) u  *)
       }
   end
end

