(* digraph.sml
 *
 * COPYRIGHT (c) 2002 Bell Labs, Lucent Technologies
 *
 *  Directed graph in adjacency list format.
 *
 * -- Allen
 *)

functor DirectedGraph(A : ARRAY) : 
sig include GRAPH_IMPLEMENTATION 

    type 'e adjlist   = 'e Graph.edge list A.array
    type 'n nodetable = 'n option A.array

    (* This function exposes the internal representation! *)
    val newGraph : 
        { name  : string,
          info  : 'g,
          succ  : 'e adjlist,
          pred  : 'e adjlist,
          nodes : 'n nodetable
        } -> ('n,'e,'g) Graph.graph
end =
struct

   structure G = Graph
   structure A = A

   type 'e adjlist   = 'e Graph.edge list A.array
   type 'n nodetable = 'n option A.array

   fun newGraph{name,info,succ,pred,nodes} =
   let val node_count    = ref 0
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
          A.foldri(fn(i,SOME n,l) =>(i,n)::l|(_,_,l) => l) [] (nodes,0,NONE)
       fun get_edges() = List.concat(A.foldr op:: [] succ)
       fun order() = !node_count
       fun size()  = !edge_count
       fun capacity() = A.length nodes
       fun add_node(i,n) =
         (case A.sub(nodes,i) 
             of NONE => node_count := 1 + !node_count
              | _    => (); 
          A.update(nodes,i,SOME n)
         )
       fun add_edge(e as (i,j,info)) = 
         (A.update(succ,i,e :: A.sub(succ,i));
          A.update(pred,j,e :: A.sub(pred,j));
          edge_count := 1 + !edge_count)

       fun set_out_edges(i,edges) =
       let fun removePred([],j,es') = A.update(pred,j,es')
             | removePred((e as (i',_,_))::es,j,es') = 
                 removePred(es,j,if i' = i then es' else e::es')
           fun removeEdge(i',j,_) =
                (if i <> i' then raise G.Graph "set_out_edges" else ();
                 removePred(A.sub(pred,j),j,[]))
           fun addPred(e as (_,j,_)) = A.update(pred,j,e :: A.sub(pred,j))
           val old_edges = A.sub(succ,i)
       in  app removeEdge old_edges;
           A.update(succ,i,edges);
           app addPred edges;
           edge_count := !edge_count + length edges - length old_edges
       end

       fun set_in_edges(j,edges) =
       let fun removeSucc([],i,es') = A.update(succ,i,es')
             | removeSucc((e as (_,j',_))::es,i,es') = 
                 removeSucc(es,i,if j' = j then es' else e::es')
           fun removeEdge(i,j',_) =
                (if j <> j' then raise G.Graph "set_in_edges" else ();
                 removeSucc(A.sub(succ,i),i,[]))
           fun addSucc(e as (i,_,_)) = A.update(succ,i,e :: A.sub(succ,i))
           val old_edges = A.sub(pred,j)
       in  app removeEdge old_edges;
           A.update(pred,j,edges);
           app addSucc edges;
           edge_count := !edge_count + length edges - length old_edges
       end

       fun remove_node i =
          case A.sub(nodes,i) of
             NONE => ()
          |  SOME _ => (set_out_edges(i,[]);
                        set_in_edges(i,[]);
                        A.update(nodes,i,NONE);
                        node_count := !node_count - 1;
                        garbage_nodes := i :: !garbage_nodes)

       fun remove_nodes ns = app remove_node ns
       fun set_entries ns = entries := ns
       fun set_exits ns   = exits := ns
       fun get_entries()  = !entries
       fun get_exits()    = !exits
       fun out_edges n = A.sub(succ,n)
       fun in_edges n = A.sub(pred,n)
       fun get_succ n = map #2 (A.sub(succ,n))
       fun get_pred n = map #1 (A.sub(pred,n))
       fun has_edge(i,j) = List.exists (fn (_,k,_) => j = k) (A.sub(succ,i))
       fun has_node n = case A.sub(nodes,n) of
                           SOME _ => true | NONE => false
       fun node_info n = case A.sub(nodes,n) of
                            SOME x => x 
                          | NONE => raise G.NotFound
       fun forall_nodes f = 
           A.appi (fn (i,SOME x) => f(i,x) | _ => ()) (nodes,0,NONE)
       fun forall_edges f = A.app (List.app f) succ

   in  G.GRAPH {
          name            = name,
          graph_info      = info,
          new_id          = new_id,
          add_node        = add_node,
          add_edge        = add_edge,
          remove_node     = remove_node,
          set_in_edges    = set_in_edges,
          set_out_edges   = set_out_edges,
          set_entries     = set_entries,
          set_exits       = set_exits,
          garbage_collect = garbage_collect,
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
          entries         = get_entries,
          exits           = get_exits,
          entry_edges     = fn _ => [],
          exit_edges      = fn _ => [],
          forall_nodes    = forall_nodes,
          forall_edges    = forall_edges
       }
   end 

   fun graph(name,info,n) = 
   let val succ  = A.array(n,[])
       val pred  = A.array(n,[])
       val nodes = A.array(n,NONE)
   in  newGraph{name=name,info=info,nodes=nodes,succ=succ,pred=pred} end
end

structure DirectedGraph = DirectedGraph(DynArray)

