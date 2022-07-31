(*
 *  A singleton graph view (i.e. graph with one node.)
 *
 * -- Allen
 *)

signature SINGLETON_GRAPH_VIEW =
sig
  
   val singleton_view : ('n,'e,'g) Graph.graph -> 
                          Graph.node_id -> ('n,'e,'g) Graph.graph

end

structure SingletonGraphView : SINGLETON_GRAPH_VIEW =
struct

   structure G = Graph

   fun singleton_view (G.GRAPH G) n =
   let fun unimplemented _ = raise G.Readonly
       fun none _ = []
       fun entries () = case #in_edges G n of [] => [] | _ => [n]
       fun exits ()   = case #out_edges G n of [] => [] | _ => [n]
   in
       G.GRAPH
       { name            = #name G,
         graph_info      = #graph_info G,
         new_id          = #new_id G,
         add_node        = unimplemented,
         add_edge        = unimplemented,
         remove_node     = unimplemented,
         set_in_edges    = unimplemented,
         set_out_edges   = unimplemented,
         set_entries     = unimplemented,
         set_exits       = unimplemented,
         garbage_collect = unimplemented,
         nodes           = fn _ => [(n,#node_info G n)],
         edges           = none,
         order           = fn _ => 1,
         size            = fn _ => 0,
         capacity        = #capacity G,
         out_edges       = none,
         in_edges        = none,
         succ            = none,
         pred            = none,
         has_edge        = fn _ => false,
         has_node        = fn i => i = n,
         node_info       = #node_info G,
         entries         = entries,
         exits           = exits,
         entry_edges     = fn i => if i = n then #in_edges G i else [],
         exit_edges      = fn i => if i = n then #out_edges G i else [],
         forall_nodes    = fn f => f(n,#node_info G n),
         forall_edges    = fn f => ()
       }
   end
end

