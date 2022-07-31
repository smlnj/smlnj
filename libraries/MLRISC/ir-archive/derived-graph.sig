(*
 * Compute Tarjan's dominator derived graph from a dominator tree.
 * This is used partly to computing path expressions.  Alternatively,
 * it can also be used for testing for reducibility.  In particular,
 * cycles involving more than one node represent irreducible loops
 * in the flow graph.
 *
 * -- Allen
 *)

signature DERIVED_GRAPH =
sig
   structure Dom : DOMINATOR_TREE
   type ('n,'e) derived_graph = ('n,'e Graph.edge,unit) Graph.graph

   val derived_graph :   (* O(n+e) *)
        ('n,'e,'g) Dom.dominator_tree -> ('n,'e) derived_graph
end
