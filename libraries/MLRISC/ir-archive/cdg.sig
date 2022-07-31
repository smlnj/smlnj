(*
 * This is a generic module for computing the control dependence graph
 * from any graph with an entry and an exit.  
 * The graph is treated as a control flow graph.  
 * The edge predicate is used to determine whether an edge should be
 * treated as a branch edge.
 *
 * -- Allen
 *)

signature CONTROL_DEPENDENCE_GRAPH =
sig

    structure Dom : DOMINATOR_TREE

    type ('n,'e,'g) cdg = ('n,'e,'g) Graph.graph

    val control_dependence_graph : 
          ('e -> bool) ->
          ('n,'e,'g) Dom.postdominator_tree ->
          ('n,'e,'g) cdg 

    val control_dependence_graph' : 
          ('n Graph.node -> 'n2 Graph.node) ->
          ('e Graph.edge -> 'e2 Graph.edge) ->
          ('g -> 'g2) ->
          ('e -> bool) ->
          ('n,'e,'g) Dom.postdominator_tree ->
          ('n2,'e2,'g2) cdg 

end

