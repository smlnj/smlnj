(*
 * Subgraph adaptor. This restricts the view of a graph.
 *
 * -- Allen
 *)

signature SUBGRAPH_P_VIEW = 
sig

     (* Node and edge induced subgraph; readonly *)
   val subgraph_p_view 
                  : Graph.node_id list ->
                    (Graph.node_id -> bool) ->
                    (Graph.node_id * Graph.node_id -> bool) ->
                      ('n,'e,'g) Graph.graph -> 
                      ('n,'e,'g) Graph.graph 
end

structure Subgraph_P_View : SUBGRAPH_P_VIEW =
struct

   structure G = Graph

   fun subgraph_p_view nodes node_p edge_p (G.GRAPH G) =
   let 
       fun readonly _ = raise G.Readonly
       fun filter_nodes ns = List.filter (fn (i,_) => node_p i) ns
       fun filter_edges es = List.filter (fn (i,j,_) => edge_p(i,j)) es
       fun get_nodes () = map (fn i => (i,#node_info G i)) nodes
       fun get_edges () = List.foldr (fn (n,l) => 
                               List.foldr (fn (e as (i,j,_),l) =>
                                   if edge_p(i,j) then e::l else l) l 
                                       (#out_edges G n)) [] nodes
       fun order () = length nodes
       fun size()   = length (get_edges())
       fun out_edges i = filter_edges(#out_edges G i)
       fun in_edges i  = filter_edges(#in_edges G i)
       fun get_succ i = List.foldr (fn ((i,j,_),ns) =>
                                     if edge_p(i,j) then j::ns else ns)
                                   [] (#out_edges G i)
       fun get_pred i = List.foldr (fn ((i,j,_),ns) =>
                                     if edge_p(i,j) then i::ns else ns)
                                   [] (#in_edges G i)
       fun has_edge (i,j) = edge_p(i,j)
       fun has_node i  = node_p i 
       fun node_info i = #node_info G i
       fun entry_edges i = if node_p i then 
                              List.filter (fn (i,j,_) => not(edge_p(i,j))) 
                                 (#in_edges G i)
                           else []
       fun exit_edges i =  if node_p i then
                              List.filter (fn (i,j,_) => not(edge_p(i,j)))
                                 (#out_edges G i)
                           else []
       fun entries() = List.foldr (fn (i,ns) =>
                          if List.exists (fn (i,j,_) => not(edge_p(i,j)))
                                 (#in_edges G i) then i::ns else ns) [] 
                             (nodes)
       fun exits()   = List.foldr (fn (i,ns) =>
                          if List.exists (fn (i,j,_) => not(edge_p(i,j)))
                                 (#out_edges G i) then i::ns else ns) [] 
                             (nodes)
       fun forall_nodes f = app (fn i => f(i,#node_info G i)) nodes
       fun forall_edges f = app f (get_edges())
   in
       G.GRAPH
       { name            = #name G,
         graph_info      = #graph_info G,
         new_id          = #new_id G,
         add_node        = readonly,
         add_edge        = readonly,
         remove_node     = readonly,
         set_in_edges    = readonly,
         set_out_edges   = readonly,
         set_entries     = readonly,
         set_exits       = readonly,
         garbage_collect = #garbage_collect G,
         nodes           = get_nodes,
         edges           = get_edges,
         order           = order,
         size            = size,
         capacity        = #capacity G,
         out_edges       = out_edges,
         in_edges        = in_edges,
         succ            = get_succ,
         pred            = get_pred,
         has_edge        = has_edge,
         has_node        = has_node,
         node_info       = node_info,
         entries         = entries,
         exits           = exits,
         entry_edges     = entry_edges,
         exit_edges      = exit_edges,
         forall_nodes    = forall_nodes,
         forall_edges    = forall_edges
       }
   end


end

