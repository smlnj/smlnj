(*
 * Start-stop adaptor.  Add a new start/stop node to a graph view.
 *
 * -- Allen
 *)

signature START_STOP_VIEW = 
sig

   val start_stop_view 
                  : {start : 'n Graph.node, 
                     stop  : 'n Graph.node,
                     edges : 'e Graph.edge list
                    } -> ('n,'e,'g) Graph.graph -> 
                         ('n,'e,'g) Graph.graph 
end

structure StartStopView : START_STOP_VIEW =
struct

   structure G = Graph

   fun start_stop_view {start=start as (START,x),
                        stop=stop as (STOP,y), edges} (G.GRAPH G) =
   let fun readonly _ = raise G.Readonly 
       fun get_nodes() = start :: stop :: #nodes G ()
       fun order()     = #order G () + 2
       fun size()      = #size G () + 1
       fun capacity()  = Int.max(START+1,Int.max(STOP+1,#capacity G ()))   
       fun exit_to_stop n = map (fn (i,_,e) => (i,STOP,e)) (#exit_edges G n)
       fun entry_to_start n = map (fn (_,j,e) => (START,j,e)) (#entry_edges G n)
       fun out_edges n = (if n = START then edges else [])
                         @ (exit_to_stop n) @ #out_edges G n
       fun in_edges n  = (if n = STOP then edges else []) 
                         @ (entry_to_start n) @ #in_edges G n
       fun get_edges() = List.concat(map (fn (n,_) => out_edges n)
                                         (get_nodes ()))
       fun get_succ n  = map #2 (out_edges n)
       fun get_pred n  = map #1 (in_edges n)
       fun has_edge(i,j) = List.exists (fn (_,k,_) => j = k) (out_edges i)
       fun has_node n    = n = START orelse n = STOP orelse #has_node G n
       fun node_info n   = if n = START then x 
                           else if n = STOP then y
                           else #node_info G n 
       fun entries()     = [START]
       fun exits()       = [STOP] 
       fun entry_edges n = []
       fun exit_edges n  = []
       fun forall_nodes f = app f (get_nodes())
       fun forall_edges f = app f (get_edges())
   in
       G.GRAPH
       { name            = #name G,
         graph_info      = #graph_info G,
         new_id          = readonly,
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
         entry_edges     = entry_edges,
         exit_edges      = exit_edges,
         forall_nodes    = forall_nodes,
         forall_edges    = forall_edges
       }
   end


end

