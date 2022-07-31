(*
 * This signature abstracts out the capability of the flowgraph
 * used in the SSA infrastruture.  This way makes it easier to 
 * substitute another CFG implementation in the future.
 *
 * -- Allen (leunga@cs.nyu.edu)
 *)
signature SSA_FLOWGRAPH =
sig

   structure I : INSTRUCTIONS
   structure W : FREQ

   type info      (* graph info *)
   type block     (* basic block type *)
   type edge_info (* edge information *)
   type cfg = (block, edge_info, info) Graph.graph (* the cfg *)

   (* 
    * Operations on the cfg
    *)
   val regmap         : cfg -> I.C.regmap
   val annotations    : cfg -> Annotations.annotations ref

   (* 
    * Operations on basic blocks
    *)
   val headerText     : block -> string
   val insns          : block -> I.instruction list ref 
   val freq           : block -> W.freq ref
   val liveOut        : block -> I.C.cellset 

   (* 
    * Operations on edges
    *)
   val setBranch      : cfg * Graph.node_id * bool -> I.instruction  
   val branchOf       : edge_info -> bool option

   (* 
    * Viewing
    *)
   val viewStyle      : cfg -> (block, edge_info, info) GraphLayout.style 

end

signature SSA_FLOWGRAPH_LIVENESS =
sig

   structure CFG : SSA_FLOWGRAPH
   structure I   : INSTRUCTIONS
       sharing CFG.I = I

   val liveness :
       { cfg     : CFG.cfg,
         liveOut : CFG.block Graph.node -> I.C.cell list,
         defUse  : CFG.block Graph.node -> I.C.cell list * I.C.cell list
       } -> unit

   val getLiveness : CFG.cfg -> Graph.node_id ->
                           {livein: I.C.cell list, liveout: I.C.cell list}

end
