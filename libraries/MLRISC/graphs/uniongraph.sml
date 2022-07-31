(*
 *  The union of two graphs.
 *
 *  -- Allen
 *)

signature UNION_GRAPH_VIEW =
sig

   val union_view : ('g1 * 'g2 -> 'g3) ->
               ('n,'e,'g1) Graph.graph * ('n,'e,'g2) Graph.graph -> 
               ('n,'e,'g3) Graph.graph

end

structure UnionGraphView : UNION_GRAPH_VIEW =
struct
   
   structure G = Graph
   structure Sort = ListMergeSort

   fun union_view f (G.GRAPH A, G.GRAPH B) =
   let
       fun merge_nodes ns =
           Sort.uniqueSort (fn ((i,_),(j,_)) => Int.compare(i,j)) ns
       fun merge_node_ids ns =
           Sort.uniqueSort (fn (i,j) => Int.compare(i,j)) ns
       fun merge_edges es =
           Sort.uniqueSort (fn ((i,j,_),(m,n,_)) => 
                              if i < m then LESS
                              else if i = m then 
                                 if j < n then LESS
                                 else if j = n then EQUAL
                                 else GREATER
                              else GREATER) es
       fun new_id ()  = Int.max(#capacity A (), #capacity B ())
       fun add_node n = (#add_node A n; #add_node B n)
       fun add_edge e = (#add_edge A e; #add_edge B e)
       fun remove_node i = (#remove_node A i; #remove_node B i)
       fun set_out_edges e = (#set_out_edges A e; #set_out_edges B e)
       fun set_in_edges e = (#set_in_edges A e; #set_in_edges B e)
       fun garbage_collect() = (#garbage_collect A (); #garbage_collect B ())
       fun nodes() = merge_nodes (#nodes A() @ #nodes B())
       fun edges() = merge_edges (#edges A() @ #edges B())
       fun order() = length(nodes())
       fun size()  = length(edges())
       fun capacity() = #capacity A() + #capacity B()
       fun out_edges i = merge_edges(#out_edges A i @ #out_edges B i)
       fun in_edges i = merge_edges(#in_edges A i @ #in_edges B i)
       fun succ i = merge_node_ids(#succ A i @ #succ B i)
       fun pred i = merge_node_ids(#pred A i @ #pred B i)
       fun has_edge e = #has_edge A e orelse #has_edge B e
       fun has_node n = #has_node A n orelse #has_node B n
       fun node_info n = #node_info A n handle _ => #node_info B n
       fun entries() = merge_node_ids(#entries A () @ #entries B ())
       fun exits() = merge_node_ids(#exits A () @ #exits B ())
       fun entry_edges i = merge_edges(#entry_edges A i @ #entry_edges B i)
       fun exit_edges i = merge_edges(#exit_edges A i @ #exit_edges B i)
       fun forall_nodes f = app f (nodes())
       fun forall_edges f = app f (edges())
       (*
       fun fold_nodes f u = List.foldr f u (nodes())
       fun fold_edges f u = List.foldr f u (edges())
       *)
   in
       G.GRAPH
       { name            = #name A ^ "+" ^ #name B,
         graph_info      = f(#graph_info A, #graph_info B),
         new_id          = new_id,
         add_node        = add_node,
         add_edge        = add_edge,
         remove_node     = remove_node,
         set_in_edges    = set_in_edges,
         set_out_edges   = set_out_edges,
         set_entries     = G.unimplemented,
         set_exits       = G.unimplemented,
         garbage_collect = garbage_collect,
         nodes           = nodes,
         edges           = edges,
         order           = order,
         size            = size,
         capacity        = capacity,
         out_edges       = out_edges,
         in_edges        = in_edges,
         succ            = pred,
         pred            = succ,
         has_edge        = has_edge,
         has_node        = has_node,
         node_info       = node_info,
         entries         = entries,
         exits           = exits,
         entry_edges     = entry_edges,
         exit_edges      = exit_edges,
         forall_nodes    = forall_nodes,
         forall_edges    = forall_edges
	 (*
         fold_nodes      = fold_nodes,
         fold_edges      = fold_edges
	 *)
       }
   end
end

