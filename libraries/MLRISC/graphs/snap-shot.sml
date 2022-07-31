(*
 * This combinator allows you to get a cached copy of a graph.
 *
 * -- Allen
 *)

signature GRAPH_SNAPSHOT =
sig
   val snapshot : ('n,'e,'g) Graph.graph -> 
        { picture : ('n,'e,'g) Graph.graph,
          button : unit -> unit
        }
end

(*
 * This is a naive implementation.
 *)
functor GraphSnapShot(GI : GRAPH_IMPLEMENTATION) : GRAPH_SNAPSHOT =
struct

   structure G = Graph
   fun snapshot (G.GRAPH G) =
   let val pict as G.GRAPH G' = GI.graph(#name G,#graph_info G,#capacity G ())
       fun clear() = #forall_nodes G' (fn (n,_) => #remove_node G' n)
       fun copy() =
          (#forall_nodes G (#add_node G');
           #forall_edges G (#add_edge G');
           #set_entries G' (#entries G ());
           #set_exits G' (#exits G ())
          )
       fun button() = (clear(); copy())
   in  copy();
       { picture = pict, button = button }
   end
end

