(*
 * Graph isomorphism view.  This works like the map function on lists.
 *
 * -- Allen
 *)

signature ISOMORPHIC_GRAPH_VIEW =
sig
   
    val map : ('n Graph.node -> 'N) ->
              ('e Graph.edge -> 'E) ->
              ('g -> 'G) ->
              ('n,'e,'g) Graph.graph -> 
              ('N,'E,'G) Graph.graph
end

structure IsomorphicGraphView : ISOMORPHIC_GRAPH_VIEW =
struct

   structure G = Graph

   fun map P Q R (G.GRAPH G) =
   let fun rename_node f (i,n)   = f(i,P(i,n))
       fun rename_node' (i,n)    = (i,P(i,n))
       fun rename_edge f (i,j,e) = f(i,j,Q(i,j,e))
       fun rename_edge' (i,j,e)  = (i,j,Q(i,j,e))
       fun rename_edges es  = List.map rename_edge' es
       fun unimplemented _ = raise G.Unimplemented
   in
       G.GRAPH
       { name            = #name G,
         graph_info      = R(#graph_info G),
         new_id          = unimplemented,
         add_node        = unimplemented,
         add_edge        = unimplemented,
         remove_node     = unimplemented,
         set_in_edges    = unimplemented,
         set_out_edges   = unimplemented,
         set_entries     = unimplemented,
         set_exits       = unimplemented,
         garbage_collect = #garbage_collect G,
         nodes           = fn () => List.map rename_node' (#nodes G ()),
         edges           = fn () => rename_edges (#edges G ()),
         order           = #order G,
         size            = #size G,
         capacity        = #capacity G,
         out_edges       = fn i => rename_edges (#out_edges G i),
         in_edges        = fn i => rename_edges (#in_edges G i),
         succ            = #succ G,
         pred            = #pred G,
         has_edge        = #has_edge G,
         has_node        = #has_node G,
         node_info       = fn i => P(i,#node_info G i),
         entries         = #entries G,
         exits           = #exits G,
         entry_edges     = fn i => rename_edges (#entry_edges G i),
         exit_edges      = fn i => rename_edges (#exit_edges G i),
         forall_nodes    = fn f => #forall_nodes G (rename_node f),
         forall_edges    = fn f => #forall_edges G (rename_edge f)
	 (*
         fold_nodes      = fold_nodes,
         fold_edges      = fold_edges
	 *)
       }
   end
end

