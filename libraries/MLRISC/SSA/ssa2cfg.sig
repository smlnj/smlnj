(* 
 * This module reconstructs the CFG from an SSA graph
 *
 * -- Allen (leunga@cs.nyu.edu)
 *)

signature SSA2CFG =
sig

   structure SSA : SSA
   structure CFG : SSA_FLOWGRAPH
      sharing SSA.CFG = CFG

   val buildCFG : SSA.ssa -> CFG.cfg
end
