(*
 * This module constructs an SSA graph from an control flow graph.
 * The control flow graph is kept abstract so that we can specialize
 * this module to various representations.
 * 
 * -- Allen (leunga@cs.nyu.edu)
 *)
signature CFG2SSA =  
sig
   structure SSA : SSA
   structure CFG : SSA_FLOWGRAPH
     sharing SSA.CFG = CFG

   val buildSSA : {cfg:CFG.cfg, dom:CFG.cfg -> SSA.dom} -> SSA.ssa

end
