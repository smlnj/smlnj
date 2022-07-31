(*
 * Subgraph adaptor.  This restricts the view of a graph.
 *
 * -- Allen
 *)

signature SUBGRAPH_VIEW = 
sig

     (* Node induced subgraph *)
   val subgraph_view : Graph.node_id list ->
                      ('e Graph.edge -> bool) ->
                      ('n,'e,'g) Graph.graph -> 
                      ('n,'e,'g) Graph.graph 
end

structure SubgraphView : SUBGRAPH_VIEW =
struct

   structure G   = Graph

   fun subgraph_view nodes edge_pred (G.GRAPH G) =
   let val set  = IntHashTable.mkTable(32,G.NotFound)
       val ins  = IntHashTable.insert set
       val ins  = fn i => ins (i,true)
       fun rmv r = (IntHashTable.remove set r; ()) handle _ => ()
       fun find r = getOpt(IntHashTable.find set r,false)

       val _    = app ins nodes
       fun edge_p (e as (i,j,_)) = find i andalso find j andalso edge_pred e
       fun check i = if find i then () else raise G.Subgraph
       fun check_edge e = if edge_p e then () else raise G.Subgraph
       fun add_node (n as (i,_)) = (ins i; #add_node G n)
       fun add_edge (e as (i,j,_)) = (check i; check j; #add_edge G e)
       fun remove_node i = (check i; rmv i; #remove_node G i)
       fun set_out_edges (i,es) = 
           (check i; app check_edge es; #set_out_edges G (i,es))
       fun set_in_edges (j,es) =
           (check j; app check_edge es; #set_in_edges G (j,es))
       fun get_nodes () = map (fn (i,_) => (i,#node_info G i)) 
                            (IntHashTable.listItemsi set)
       fun get_edges () = 
       let fun find_edges([],l) = l
             | find_edges(e::es,l) =
                 if edge_p e then find_edges(es,e::l) else find_edges(es,l)
       in  foldr (fn ((i,_),l) => find_edges(#out_edges G i,l)) [] 
               (IntHashTable.listItemsi set)
       end
       fun order () = IntHashTable.numItems set
       fun size  () =
       let fun find_edges([],n) = n
             | find_edges(e::es,n) =
                 if edge_p e then find_edges(es,n+1) else find_edges(es,n)
       in  foldr (fn ((i,_),n) => find_edges(#out_edges G i,n)) 0 
              (IntHashTable.listItemsi set)
       end
       fun out_edges i = (List.filter edge_p (#out_edges G i))
       fun in_edges i  = (List.filter edge_p (#in_edges G i))
       fun get_succ i = map #2 (out_edges i)
       fun get_pred i = map #1 (in_edges i)
       fun has_edge (i,j) = find i andalso find j
       fun has_node i = find i 
       fun node_info i = (check i; #node_info G i)

       fun entry_edges i = (List.filter(fn (j,_,_) => not(find j))
                                       (#in_edges G i))
       fun exit_edges i = (List.filter(fn (_,j,_) => not(find j))
                                       (#out_edges G i))
       fun entries() =  foldr (fn ((i,_),l) =>
                           if List.exists (fn (j,_,_) => not(find j)) 
                                (#in_edges G i) then i::l else l) [] 
                            (IntHashTable.listItemsi set)
       fun exits() =  foldr (fn ((i,_),l) =>
                           if List.exists (fn (_,j,_) => not(find j)) 
                                (#out_edges G i) then i::l else l) [] 
                              (IntHashTable.listItemsi set)
       fun forall_nodes f = IntHashTable.appi (fn (i,_) => f(i,#node_info G i)) set
       fun forall_edges f = IntHashTable.appi (fn (i,_) => app (fn e =>
                                                  if edge_p e then f e else ())
                                             (#out_edges G i)) set
   in
       G.GRAPH
       { name            = #name G,
         graph_info      = #graph_info G,
         new_id          = #new_id G,
         add_node        = add_node,
         add_edge        = add_edge,
         remove_node     = remove_node,
         set_in_edges    = set_in_edges,
         set_out_edges   = set_out_edges,
         set_entries     = fn _ => raise G.Readonly,
         set_exits       = fn _ => raise G.Readonly,
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
	 (*
         fold_nodes      = fold_nodes,
         fold_edges      = fold_edges
	 *)
       }
   end


end

