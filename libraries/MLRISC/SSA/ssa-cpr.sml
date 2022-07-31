(*
 * This module performs critical path reduction.
 * Jobs include:
 *   1. Transform conditional branches into conditional moves.
 *   2. Migrate branches out of loops
 *
 * -- Allen (leunga@cs.nyu.edu)
 *)
functor SSACPR(SSA : SSA) : SSA_OPTIMIZATION =
struct

   structure SSA  = SSA
   structure CFG  = SSA.CFG
   structure RTL  = SSA.RTL
   structure T    = RTL.T
   structure G    = Graph
   structure A    = Array

   type flowgraph = SSA.ssa

   val name = "Critical Path Reduction"

   infix ||-
 
   fun run(SSA as G.GRAPH ssa) =
   let val Dom as G.GRAPH dom = SSA.dom SSA
       val CFG as G.GRAPH cfg = SSA.cfg SSA
       val [ENTRY] = #entries dom ()
       val {sources, phis, ops, sinks, ...} = SSA.nodes SSA

       fun walk X = 
       let 
       in  app walk (#succ dom X)
       end

   in  walk ENTRY;
       SSA
   end

end
