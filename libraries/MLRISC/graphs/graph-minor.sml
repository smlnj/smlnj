(*
 *  Graph minor.
 *  Allows contraction of nodes.  
 *  Remove self-edges during contraction. 
 *  
 *  -- Allen
 *) 
signature GRAPH_MINOR_VIEW =
sig

   val minor : ('n,'e,'g) Graph.graph
            -> ('n * 'n * 'e Graph.edge list -> 'n)
            -> { view  : ('n,'e,'g) Graph.graph,
                 union : Graph.node_id * Graph.node_id -> bool,
                 ==    : Graph.node_id * Graph.node_id -> bool,
                 partition : Graph.node_id -> Graph.node_id list 
               }

end

structure GraphMinorView : GRAPH_MINOR_VIEW =
struct

   structure G = Graph
   structure U = URef
   structure H = HashArray

   datatype ('n,'e) node = 
      NODE of { key   : int,
                data  : 'n,
                nodes : Graph.node_id list,
                succ  : 'e G.edge list,
                pred  : 'e G.edge list
              }

   fun minor(G.GRAPH G : ('n,'e,'g) Graph.graph) merge_nodes =
   let fun unimplemented _ = raise G.Readonly
       val N     = #capacity G ()
       val table = H.array'(N,fn _ => raise G.NotFound)
       fun get n = let val NODE x = U.!!(H.sub(table,n)) in x end
       val _ = #forall_nodes G 
                (fn (n,n') => 
                    H.update(table,n,
                       U.uRef(NODE{key=n,
                                   data=n',
                                   nodes=[n],
                                   succ= #out_edges G n,
                                   pred= #in_edges G n})))
       fun same(i,j) = U.equal (H.sub(table,i),H.sub(table,j))
       fun partition i = #nodes(get i) 
       val size  = ref (#size G ())
       val order = ref (#order G ())
       fun out_edges n = #succ(get n)
       fun in_edges n = #pred(get n)
       fun succ n  = map #2 (out_edges n)
       fun pred n  = map #1 (in_edges n) 
       fun nodes() = 
           let val found = H.array(10,false)
               fun collect((node as (n,_))::nodes,nodes') =
                   if H.sub(found,n) then collect(nodes,nodes')
                   else let val ns = partition n
                        in  app (fn n => H.update(found,n,true)) ns;
                            collect(nodes,node::nodes')
                        end
                 | collect([],nodes') = nodes'
           in collect(#nodes G (),[])
           end
       fun edges() = List.concat (map (fn (n,_) => out_edges n) (nodes()))
       fun has_edge(i,j) =
           List.exists (fn (_,j',_) => j = j') (out_edges i)
       fun has_node n = (H.sub(table,n);true) handle G.NotFound => false 
       fun node_info n = #data(get n)
       fun forall_nodes f = app f (nodes())
       fun forall_edges f = app f (edges())
       fun merge(NODE{key=k1,data=d1,succ=s1,pred=p1,nodes=n1},
                 NODE{key=k2,data=d2,succ=s2,pred=p2,nodes=n2}) =
       let fun key i = #key(get i)
           fun partition([],others,self) = (others,self)
             | partition((e as (i,j,_))::es,others,self) =
                let val k_i = key i
                    val k_j = key j
                in   if (k_i = k1 orelse k_i = k2) andalso   
                        (k_j = k1 orelse k_j = k2) then 
                          partition(es,others,e::self)  
                     else partition(es,e::others,self)
                end
           val (s,s') = partition(s1@s2,[],[])
           val (p,p') = partition(p1@p2,[],[])
           val n = NODE{key=k1,
                        data=merge_nodes(d1,d2,s'),
                        nodes=n1@n2, 
                        succ=s,
                        pred=p
                       } 
           val _ = order := !order - 1
           val _ = size  := !size - length s'
       in  n
       end
       fun union(i,j) = U.unify merge (H.sub(table,i),H.sub(table,j))
       val view =
       G.GRAPH
       { name            = #name G,
         graph_info      = #graph_info G,
         new_id          = unimplemented,
         add_node        = unimplemented,
         add_edge        = unimplemented,
         remove_node     = unimplemented,
         set_in_edges    = unimplemented,
         set_out_edges   = unimplemented,
         set_entries     = unimplemented,
         set_exits       = unimplemented,
         garbage_collect = unimplemented,
         nodes           = nodes,
         edges           = edges,
         order           = fn () => !order,
         size            = fn () => !size,
         capacity        = #capacity G,
         out_edges       = out_edges,
         in_edges        = in_edges,
         succ            = succ,
         pred            = pred,
         has_edge        = has_edge,
         has_node        = has_node,
         node_info       = node_info,
         entries         = #entries G,
         exits           = #exits G,
         entry_edges     = #entry_edges G,
         exit_edges      = #exit_edges G,
         forall_nodes    = forall_nodes, 
         forall_edges    = forall_edges
       }
   in  { view  = view,
         union = union,
         ==    = same,
         partition = partition
       }
   end
end

