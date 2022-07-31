(*
 *  Graph minor.
 *  Allows contraction of nodes.  
 * 
 * -- Allen
 *) 
signature GRAPH_MINOR =
sig

   val minor : ('n,'e,'g) Graph.graph -> 
       { view      : ('n,'e,'g) Graph.graph,
         merge     : Graph.node_id list * 'n -> unit,
         ==        : Graph.node_id * Graph.node_id -> bool,
         partition : Graph.node_id -> Graph.node_id list 
       }

end

structure GraphMinor : GRAPH_MINOR =
struct

   structure G  = Graph
   structure H  = HashArray
   structure SL = SortedList

   fun minor (G.GRAPH G : ('n,'e,'g) Graph.graph) =
   let exception NotThere
       val uptree = H.array'(13,fn _ => raise NotThere)
       fun look n = #1(H.sub(uptree,n)) handle _ => n
       fun edge(i,j,e) = (look i,look j,e)
       fun set_in_edges(i,e) = #set_in_edges G (look i,e)
       fun set_out_edges(i,e) = #set_out_edges G (look i,e)
       fun all f n =
           let val (_,nodes,_,_) = H.sub(uptree,n)
           in  List.foldr (fn (i,l) => f i @ l) [] nodes
           end handle NotThere => f n
       fun in_edges i = map edge (all (#in_edges G) i)
       fun out_edges i = map edge (all (#out_edges G) i)
       fun pred i = map (look o #1) (all (#in_edges G) i)
       fun succ i = map (look o #2) (all (#out_edges G) i)
       fun entry_edges i = map edge (all (#entry_edges G) i)
       fun exit_edges i = map edge (all (#exit_edges G) i)
       fun has_node n = 
           let val (_,_,_,x) = H.sub(uptree,n)
           in  x end handle NotThere => #has_node G n
       fun node_info n =
           let val (_,_,n',x) = H.sub(uptree,n) 
           in  if x then n' else raise G.NotFound
           end handle NotThere => #node_info G n
       fun nodes() =
           List.foldr (fn (node as (n,_),ns) =>
               let val (n,_,n',x) = H.sub(uptree,n)
               in  if x then (n,n')::ns else ns 
               end handle NotThere => node::ns) [] (#nodes G ())
       fun edges() = 
           List.foldr (fn (node as (n,_),es) =>
               let val (n,_,n',x) = H.sub(uptree,n)
               in  if x then map edge (#out_edges G n)@es else es 
               end handle NotThere => map edge(#out_edges G n)@es) [] 
               (#nodes G ())
       fun order() = length(nodes())
       fun size() = length(edges())
       fun entries() = SL.uniq(map look (#entries G ()))
       fun exits() = SL.uniq(map look (#exits G ()))
       fun forall_nodes f = app f (nodes ())
       fun forall_edges f = app f (edges ())
       fun merge([],_) = ()
         | merge(nodes as n::ns,n') = 
           let val info  = (n,nodes,n',true)
               val info' = (n,nodes,n',false)
           in  H.update(uptree,n,info);
               app (fn i => H.update(uptree,i,info')) ns
           end
       fun ==(a,b) = look a = look b
       fun partition n = #2(H.sub(uptree,n)) handle NotThere => [n]
       val view =
          G.GRAPH
          { name            = #name G,
            graph_info      = #graph_info G,
            new_id          = #new_id G,
            add_node        = #add_node G,
            add_edge        = #add_edge G,
            remove_node     = #remove_node G,
            set_in_edges    = set_in_edges,
            set_out_edges   = set_out_edges,
            set_entries     = #set_entries G,
            set_exits       = #set_exits G,
            garbage_collect = #garbage_collect G,
            nodes           = nodes,
            edges           = edges,
            order           = order,
            size            = size,
            capacity        = #capacity G,
            in_edges        = in_edges,
            out_edges       = out_edges,
            pred            = pred,
            succ            = succ,
            has_edge        = #has_edge G,
            has_node        = has_node,
            node_info       = node_info,
            entries         = entries,
            exits           = exits,
            entry_edges     = entry_edges,
            exit_edges      = exit_edges,
            forall_nodes    = forall_nodes,
            forall_edges    = forall_edges
          }
   in  { view      = view,
         merge     = merge,
         ==        = ==,
         partition = partition
       }
   end
end

