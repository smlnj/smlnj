(*
 * MLRISC IR
 *
 * This is for performing whole program analysis.
 * All optimizations are based on this representation.
 * It provides a few useful views: dominator tree, control dependence graph,
 * loop nesting (interval) structure etc. Also there is a mechanism to
 * incrementally attach additional views to the IR.  The SSA infrastructure
 * is implemented in such a manner.
 *
 * -- Allen
 *)

signature MLRISC_IR =
sig

   structure I    : INSTRUCTIONS
   structure CFG  : CONTROL_FLOW_GRAPH
   structure Dom  : DOMINATOR_TREE
   structure CDG  : CONTROL_DEPENDENCE_GRAPH
   structure Loop : LOOP_STRUCTURE
   structure Util : CFG_UTIL
      sharing Util.CFG = CFG
      sharing CFG.I = I 
      sharing Loop.Dom = CDG.Dom = Dom
  
   type cfg  = CFG.cfg  
   type IR   = CFG.cfg  (* The IR looks just like a CFG! *)
   type dom  = (CFG.block,CFG.edge_info,CFG.info) Dom.dominator_tree
   type pdom = (CFG.block,CFG.edge_info,CFG.info) Dom.postdominator_tree
   type cdg  = (CFG.block,CFG.edge_info,CFG.info) CDG.cdg
   type loop = (CFG.block,CFG.edge_info,CFG.info) Loop.loop_structure
 
   (*
    *  Extract various views from an IR.
    *  These are computed by need.
    *) 
   val dom   : IR -> dom
   val pdom  : IR -> pdom
   val doms  : IR -> dom * pdom
   val cdg   : IR -> cdg
   val loop  : IR -> loop

   (*
    *  Signal that the IR has been changed
    *) 
   val changed : IR -> unit  

   (*
    *  View as a picture  
    *) 
   val view  : string -> IR -> unit       (* view some facet of the IR *)
   val views : string list -> IR -> unit  (* view a set of facets *) 
   val viewSubgraph : IR -> cfg -> unit   (* view a subgraph of the IR *)

   (*
    *  This function allows the client to design a new view and extend
    *  the functionality of the IR
    *) 
   val memo : string -> (IR -> 'facet) -> IR -> 'facet
   val addLayout : string -> (IR -> GraphLayout.layout) -> unit

end

