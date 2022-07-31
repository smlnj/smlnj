(*
 * This module includes some very basic CFG transformations.
 *
 * -- Allen
 *)
signature CFG_UTIL =
sig

   structure CFG : CONTROL_FLOW_GRAPH

   (*=======================================================================
    * Get a label from a block
    *=======================================================================*)
   val labelOf : CFG.cfg -> Graph.node_id -> Label.label

   (*=======================================================================
    *  Update the label of the branch instruction in a block
    *  to be consistent with the control flow edges.  
    *  This is an NOP if the CFG is already consistent.
    *=======================================================================*)
   val updateJumpLabel : CFG.cfg -> Graph.node_id -> unit

   (*=======================================================================
    *  Copy an edge
    *=======================================================================*)
   val copyEdge : CFG.edge_info -> CFG.edge_info

   (*=======================================================================
    *  Merge a control flow edge.  Return false if merging is unsuccessful.
    *=======================================================================*)
   val mergeEdge : CFG.cfg -> CFG.edge -> bool

   (*=======================================================================
    *  Eliminate the jump (insert a jump)
    *     at the end of the current block if it is feasible.
    *  Return true iff it is successful.
    *=======================================================================*)
   val eliminateJump : CFG.cfg -> Graph.node_id -> bool
   val insertJump    : CFG.cfg -> Graph.node_id -> bool

   (*=======================================================================
    *  Split a control flow edge, return a new edge and the new block.
    *  If the jump flag is true, then a jump is always placed in the 
    *  new block; otherwise, we try to eliminate the jump when feasible.
    *=======================================================================*)
   val splitEdge  : CFG.cfg -> 
                      { kind : CFG.block_kind,
                        edge : CFG.edge,
                        jump : bool
                      } ->
                      { edge : CFG.edge,
                        node : CFG.node
                      }
   (*=======================================================================
    *  Test if an edge is critical.  An edge i->j is critical iff 
    *  there are multiple entries into j and multiple exits out of i,
    *  i.e. it is both a merge and a split node.
    *=======================================================================*)
    val isMerge        : CFG.cfg -> Graph.node_id -> bool
    val isSplit        : CFG.cfg -> Graph.node_id -> bool
    val hasSideExits   : CFG.cfg -> Graph.node_id -> bool
    val isCriticalEdge : CFG.cfg -> CFG.edge -> bool

   (*=======================================================================
    *  Split all critical edges in the CFG.
    *  This may introduce extra jumps into the program.
    *=======================================================================*)
    val splitAllCriticalEdges : CFG.cfg -> unit

   (*=======================================================================
    *  Check whether two blocks are necessary connected.
    *  Blocks i and j are connected iff i must be layout before j.
    *=======================================================================*)
    val mustPreceed : CFG.cfg -> Graph.node_id * Graph.node_id -> bool

   (*=======================================================================
    *  Tail duplicate a region until it only has a single entry.
    *  Return the set of new nodes and new edges.  The region is represented
    *  as a subgraph view.
    *=======================================================================*)
    val tailDuplicate : CFG.cfg -> 
                        { subgraph : CFG.cfg,
                          root     : Graph.node_id
                        } -> 
                        { nodes : CFG.node list, 
                          edges : CFG.edge list
                        } 

   (*=======================================================================
    *  Remove all unreachable code 
    *=======================================================================*)
    val removeUnreachableCode : CFG.cfg -> unit

   (*=======================================================================
    *  Try to merge all edges
    *=======================================================================*)
    val mergeAllEdges : CFG.cfg -> unit

end

