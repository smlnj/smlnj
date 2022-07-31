(*
 * This is a generic module for computing the control dependence graph
 * from a graph with an entry and an exit.  
 * The graph is treated as a control flow graph.  
 * The edge predicate is used to determine whether an edge should be
 * treated as a branch edge.
 *
 * -- Allen
 *)

functor ControlDependenceGraph
   (structure Dom       : DOMINATOR_TREE
    structure GraphImpl : GRAPH_IMPLEMENTATION
   ) : CONTROL_DEPENDENCE_GRAPH =

struct

    structure Dom = Dom
    structure G   = Graph
    structure GI  = GraphImpl

    type ('n,'e,'g) cdg = ('n,'e,'g) Graph.graph

    fun control_dependence_graph' f_node f_edge f_graph is_conditional
             (PDom as G.GRAPH pdom) =
    let val G.GRAPH cfg        = Dom.cfg PDom
        val N                  = #capacity cfg ()
        val cdg_info           = f_graph (#graph_info cfg)
        val CDG as G.GRAPH cdg = GI.graph("CDG", cdg_info, N)
        val ipdom              = Dom.idom PDom
        val add_edge           = fn e => #add_edge cdg (f_edge e)
        val out_edges          = #out_edges cfg

        (* create the control dependence nodes *)
        val _ = #forall_nodes cfg (fn n => #add_node cdg (f_node n))
 
        (* create the control dependence edges *)
        val _ = #forall_nodes cfg 
         (fn node as (X,bb) =>
             let val ipdom_X = ipdom X
                 fun loop (X,Z,L) =
                     if ipdom_X = ~1 orelse ipdom_X <> Z then
                     (* Z is immediately control dependent on X *)
                       (add_edge (X,Z,L);
                        case ipdom Z of
                          ~1 => ()
                        |  Z => loop (X,Z,L))
                     else ()
             in
                 app (fn (X,Z,L) => 
                           (* Z is a successor of X on label L *)
                           if is_conditional L then loop(X,Z,L)
                           else ()
                     ) (out_edges X)
             end)
    in
        CDG
    end

    fun control_dependence_graph is_conditional =
          control_dependence_graph' 
          (fn n => n) 
          (fn e => e) 
          (fn g => g) is_conditional

end
