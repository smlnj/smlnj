(*
 *  Undirected graph in adjacency list format.
 *
 *  -- Allen
 *)

functor UndirectedGraph(A : ARRAY) : GRAPH_IMPLEMENTATION =
struct

   structure G = Graph
   structure A = A

   fun graph(name,graph_info,n) =
   let val adj           = A.array(n,[])
       val nodes         = A.array(n,NONE)
       val node_count    = ref 0
       val edge_count    = ref 0
       val entries       = ref []
       val exits         = ref []
       val new_nodes     = ref []
       val garbage_nodes = ref []
       fun new_id() = case ! new_nodes of []  => A.length nodes
                                       | h::t => (new_nodes := t; h)
       fun garbage_collect () =
          (new_nodes := (!new_nodes) @ (!garbage_nodes); garbage_nodes := [])
       fun get_nodes() =
          A.foldri(fn(i,SOME n,l) =>(i,n)::l|(_,_,l) => l) [] nodes
       fun get_edges() = 
          A.foldri(fn (i,es,L) => foldr (fn ((j,e),L) => 
                if i <= j then (i,j,e)::L else L) L es)
              [] adj
       fun order() = !node_count
       fun size()  = !edge_count
       fun capacity() = A.length nodes
       fun add_node(i,n) =
         (case A.sub(nodes,i) 
             of NONE => node_count := 1 + !node_count
              | _    => (); 
          A.update(nodes,i,SOME n)
         )
       fun add_edge(i,j,e) = 
         (A.update(adj,i,(j,e)::A.sub(adj,i));
          if i <> j then A.update(adj,j,(i,e)::A.sub(adj,j)) else ();
          edge_count := 1 + !edge_count)

       fun set_edges(i,edges) =
       let fun rmv([],L) = L
             | rmv((e as (k,_))::es,L) = rmv(es,if k = i then L else e::L)
           fun add(i,j,e) =
               if i <> j then A.update(adj,j,(i,e)::A.sub(adj,j)) else ()
           val old_edges = A.sub(adj,i)
       in  app (fn (j,_) => A.update(adj,j,rmv(A.sub(adj,j),[]))) old_edges;
           app add edges;
           A.update(adj,i,map (fn (_,j,e) => (j,e)) edges);
           edge_count := !edge_count + length edges - length old_edges
       end

       fun remove_node i =
          case A.sub(nodes,i) of
             NONE => ()
          |  SOME _ => (set_edges(i,[]);
                        A.update(nodes,i,NONE);
                        node_count := !node_count - 1;
                        garbage_nodes := i :: !garbage_nodes)

       fun remove_nodes ns = app remove_node ns
       fun set_entries ns = entries := ns
       fun set_exits ns = exits := ns
       fun get_entries()  = !entries
       fun get_exits()  = !exits
       fun adj_edges i = map (fn (j,e) => (i,j,e)) (A.sub(adj,i))
       fun neighbors i = map #1 (A.sub(adj,i))
       fun has_edge(i,j) = List.exists (fn (k,_) => j = k) (A.sub(adj,i))
       fun has_node n = case A.sub(nodes,n) of
                           SOME _ => true | NONE => false
       fun node_info n = case A.sub(nodes,n) of
                            SOME x => x 
                          | NONE => raise G.NotFound
       fun forall_nodes f = 
           A.appi (fn (i,SOME x) => f(i,x) | _ => ()) nodes
       fun forall_edges f = A.appi (fn (i,es) => 
             app (fn (j,e) => if i <= j then f(i,j,e) else ()) es)
                               adj
       fun none _ = []

   in  G.GRAPH {
          name            = name,
          graph_info      = graph_info,
          new_id          = new_id,
          add_node        = add_node,
          add_edge        = add_edge,
          remove_node     = remove_node,
          set_in_edges    = set_edges,
          set_out_edges   = set_edges,
          set_entries     = set_entries,
          set_exits       = set_exits,
          garbage_collect = garbage_collect,
          nodes           = get_nodes,
          edges           = get_edges,
          order           = order,
          size            = size,
          capacity        = capacity,
          out_edges       = adj_edges,
          in_edges        = adj_edges,
          succ            = neighbors,
          pred            = neighbors,
          has_edge        = has_edge,
          has_node        = has_node,
          node_info       = node_info,
          entries         = get_entries,
          exits           = get_exits,
          entry_edges     = none,
          exit_edges      = none,
          forall_nodes    = forall_nodes,
          forall_edges    = forall_edges
       }
   end 

end
