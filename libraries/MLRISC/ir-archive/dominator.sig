(*
 * This is the signature of a dominator tree.
 * The dominator tree includes lots of query methods.
 * 
 * -- Allen
 *)

signature DOMINATOR_TREE =
sig

    structure GI : GRAPH_IMPLEMENTATION

    exception Dominator

    type ('n,'e,'g) dom_info

    (* Dominator/postdominator trees *) 
    type ('n,'e,'g) dominator_tree =
       ('n,unit,('n,'e,'g) dom_info) Graph.graph
    type ('n,'e,'g) postdominator_tree = 
       ('n,unit,('n,'e,'g) dom_info) Graph.graph

    type node = Graph.node_id

       (* Compute the (post)dominator tree from a flowgraph *)
    val makeDominator : ('n,'e,'g) Graph.graph -> ('n,'e,'g) dominator_tree 
    val makePostdominator : ('n,'e,'g) Graph.graph -> 
                                ('n,'e,'g) postdominator_tree 

    (* The following methods work on both dominator/postdominator trees.
     * When operating on a postdominator tree, the interpretation of these
     * methods are reversed in the obvious manner.
     *) 

        (* Extract the original CFG *)
    val cfg        : ('n,'e,'g) dominator_tree -> ('n,'e,'g) Graph.graph

        (* The height of the dominator tree *)
    val max_levels : ('n,'e,'g) dominator_tree -> int

        (* Return a map from node id -> level (level(root) = 0) *)
    val levelsMap  : ('n,'e,'g) dominator_tree -> int Array.array

        (* Return a map from node id i -> the node_id j,
         * where j is the level 1 node that dominates i.
         * Special case: if i = ENTRY, then j = ENTRY.
         * This table is cached.
         *)
    val entryPos   : ('n,'e,'g) dominator_tree -> int Array.array

        (* Return a map from node id -> immediate (post)dominator *)
    val idomsMap   : ('n,'e,'g) dominator_tree -> int Array.array

        (* Immediately (post)dominates? *)
    val immediately_dominates : ('n,'e,'g) dominator_tree -> node * node -> bool

        (* (Post)dominates? *)
    val dominates : ('n,'e,'g) dominator_tree -> node * node -> bool

        (* Strictly (post)dominates? *)
    val strictly_dominates : ('n,'e,'g) dominator_tree -> node * node -> bool

        (* Immediate (post)dominator of a node (~1 if none) *)
    val idom : ('n,'e,'g) dominator_tree -> node -> node

        (* Nodes that the node immediately (post)dominates *)
    val idoms : ('n,'e,'g) dominator_tree -> node -> node list

        (* Nodes that the node (post)dominates (includes self) *)
    val doms : ('n,'e,'g) dominator_tree -> node -> node list

        (* Return the level of a node in the tree *)
    val level : ('n,'e,'g) dominator_tree -> node -> int 

        (* Return the least common ancestor of a pair of nodes *)
    val lca : ('n,'e,'g) dominator_tree -> node * node -> node 

    (* The following methods require both the dominator and postdominator trees.
     *) 
        (* Are two nodes control equivalent? *)
    val control_equivalent :
          ('n,'e,'g) dominator_tree * ('n,'e,'g) postdominator_tree ->
              node * node -> bool

        (* Compute the control equivalent partitions of a graph *)
    val control_equivalent_partitions : 
          ('n,'e,'g) dominator_tree * ('n,'e,'g) postdominator_tree ->
              node list list

end

