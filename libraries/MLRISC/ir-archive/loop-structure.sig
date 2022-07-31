(* loop-structure.sig
 *
 * COPYRIGHT (c) 2002 Bell Labs, Lucent Technologies.
 *
 * This module is responsible for locating loop structures (intervals).
 * All loops have only one single entry (via the header) but
 * potentially multiple exits, i.e. the header dominates all nodes
 * within the loop.   Other definitions are used for ``loops'' and ``headers''
 * in the literature.  We choose a structural definition that has nicer
 * properties.
 * 
 * -- Allen
 *)

signature LOOP_STRUCTURE =
sig

   structure Dom : DOMINATOR_TREE
   structure GI  : GRAPH_IMPLEMENTATION

   (*
    * DEF: An edge i -> j  is a backedge iff j dom i.
    *      Here, j is the header, and i -> j \in backedges(j) 
    *      A loop is identified by its header h.  
    *)
   datatype ('n,'e,'g) loop = 
      LOOP of { nesting    : int,
                header     : Graph.node_id,
                loop_nodes : Graph.node_id list,
                backedges  : 'e Graph.edge list,
                exits      : 'e Graph.edge list
              }

   type ('n,'e,'g) loop_info

   type ('n,'e,'g) loop_structure = 
        (('n,'e,'g) loop,unit, ('n,'e,'g) loop_info) Graph.graph 

   val dom            : ('n,'e,'g) loop_structure ->
                        ('n,'e,'g) Dom.dominator_tree 

          (* O(n+e) *)
   val loop_structure : ('n,'e,'g) Dom.dominator_tree ->
                        ('n,'e,'g) loop_structure 

       (* return an array mapping node id -> nesting level *) 
   val nesting_level : ('n,'e,'g) loop_structure -> Graph.node_id Array.array

       (* return an array mapping node id -> header that it belongs to *) 
   val header        : ('n,'e,'g) loop_structure -> Graph.node_id Array.array

       (* given a header, return the set of entry edges into the loop *)
   val entryEdges    : ('n,'e,'g) loop_structure -> Graph.node_id -> 
                             'e Graph.edge list

end    

