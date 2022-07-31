(*
 * This view combinator converts a multigraph into a simple graph.
 *
 * -- Allen
 *)

signature SIMPLE_GRAPH =
sig
    val simple_graph :
       (Graph.node_id * Graph.node_id * 'e list -> 'e) ->
           ('n,'e,'g) Graph.graph -> ('n,'e,'g) Graph.graph 
end

structure SimpleGraph =
struct

   structure G = Graph
   structure S = ListMergeSort
   fun simple_graph merge (G.GRAPH G) =
   let val sort = S.sort (fn ((i,j,_),(i',j',_)) => 
                      i > i' orelse i = i' andalso j > j')
       fun uniq([],_,_,[],es'') = es''
         | uniq([],i,j,[e],es'') = (i,j,e)::es''
         | uniq([],i,j,es,es'') = (i,j,merge(i,j,es))::es''
         | uniq((i,j,e)::es,_,_,[],es'') = uniq(es,i,j,[e],es'')
         | uniq((i,j,e)::es,i',j',es',es'') =
             if i = i andalso j = j' then
                 uniq(es,i',j',e::es',es'')
             else (case es' of
                     [e'] => uniq(es,i,j,[e],(i',j',e')::es'')
                    | _  => uniq(es,i,j,[e],(i',j',merge(i',j',es'))::es'')
                  ) 
       fun unique es = uniq(sort es,~1,~1,[],[])
       fun out_edges v = unique(#out_edges G v)
       fun in_edges v  = unique(#in_edges G v)
       fun succ v      = map #2 (out_edges v)
       fun pred v      = map #1 (in_edges v)
       fun edges()     = unique(#edges G ())

   in  G.GRAPH
       { name            = #name G,
         graph_info      = #graph_info G,
         new_id          = #new_id G,
         add_node        = #add_node G,
         add_edge        = #add_edge G,
         remove_node     = #remove_node G,
         set_in_edges    = #set_in_edges G,
         set_out_edges   = #set_out_edges G,
         set_entries     = #set_entries G,
         set_exits       = #set_exits G,
         garbage_collect = #garbage_collect G,
         nodes           = #nodes G,
         edges           = edges,
         order           = #order G,
         size            = #size G,
         capacity        = #capacity G,
         out_edges       = out_edges,
         in_edges        = in_edges,
         succ            = succ,
         pred            = pred,
         has_edge        = #has_edge G,
         has_node        = #has_node G,
         node_info       = #node_info G,
         entries         = #entries G,
         exits           = #exits G,
         entry_edges     = #entry_edges G,
         exit_edges      = #exit_edges G,
         forall_nodes    = #forall_nodes G,
         forall_edges    = fn f => app f (edges ())
       }
   end
end

