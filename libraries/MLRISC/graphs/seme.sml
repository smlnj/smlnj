(*
 * Single-entry-multiple exit view.  Add a new exit node to graph view.
 *
 * All exit edges are now directed into the exit node.
 * The unique node with entry edges becomes the new entry node.  
 *
 * -- Allen
 *)

signature SINGLE_ENTRY_MULTIPLE_EXIT_VIEW = 
sig

   exception NoEntry 
   exception MultipleEntries of Graph.node_id list

   val SEME : { exit : 'n Graph.node } -> 
              ('n,'e,'g) Graph.graph -> ('n,'e,'g) Graph.graph 
end

structure SingleEntryMultipleExit : SINGLE_ENTRY_MULTIPLE_EXIT_VIEW = 
struct

   structure G = Graph

   exception NoEntry 
   exception MultipleEntries of Graph.node_id list

   fun SEME {exit=exit as (EXIT,ex)} (G.GRAPH G) =
   let fun readonly _  = raise G.Readonly 
       fun get_nodes() = exit :: #nodes G ()
       fun order()     = #order G () + 1
       fun capacity()  = Int.max(EXIT+1,#capacity G ())
       fun findEntry() =  
           case #entries G () of
              [ENTRY] => ENTRY
           |  [] => raise NoEntry 
           |  nodes => raise MultipleEntries nodes
       val ENTRY = findEntry()
       fun exitEdges n = map (fn (i,j,e) => (i,EXIT,e)) (#exit_edges G n)
       fun out_edges n = exitEdges n @ #out_edges G n 
       fun in_edges n  = if n = EXIT then exitEdges n
                         else #in_edges G n
       fun get_edges() = List.concat(map (fn (n,_) => out_edges n)
                                         (get_nodes ()))
       fun get_succ n  = map #2 (out_edges n)
       fun get_pred n  = map #1 (in_edges n)
       fun has_edge(i,j) = List.exists (fn (_,k,_) => j = k) (out_edges i)
       fun has_node n    = n = EXIT orelse #has_node G n
       fun node_info n   = if n = EXIT then ex else #node_info G n 
       fun forall_nodes f = (#forall_nodes G f; f exit)
       fun forall_edges f = app f (get_edges())
       fun entries() = [ENTRY]
       fun exits() = [EXIT]
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
         size            = #size G,
         capacity        = capacity,
         out_edges       = out_edges,
         in_edges        = in_edges,
         succ            = get_succ,
         pred            = get_pred,
         has_edge        = has_edge,
         has_node        = has_node,
         node_info       = node_info,
         entries         = entries,
         exits           = exits,
         entry_edges     = #entry_edges G,
         exit_edges      = #exit_edges G,
         forall_nodes    = forall_nodes,
         forall_edges    = forall_edges
       }
   end

end

