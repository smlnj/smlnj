(*
 * Partial redundancy elimination.
 * This is my own algorithm. 
 *
 * -- Allen (leunga@cs.nyu.edu)
 *)
functor SSAPRE(SSA : SSA) : SSA_OPTIMIZATION =
struct
   structure SSA  = SSA
   structure SP   = SSA.SP
   structure RTL  = SSA.RTL
   structure Dom  = SSA.Dom
   structure T    = RTL.T
   structure G    = Graph 
   structure A    = Array
  
   type flowgraph = SSA.ssa

   fun error msg = MLRiscErrorMsg.error("SSAPRE",msg)

   val preRemoved = MLRiscControl.getCounter "ssa-partial-redundancy-removed"

   val name = "Partial Redundancy Elimination"

   (*
    * Redundancy elimination is driven by frequency.
    * Given:
    *
    *    t <- phi(t1, t2, ..., tn)
    *    ...
    *    v <- f(t) 
    *
    * f(t) is partially redundant if it is cheaper to transform it
    * into: 
    *
    *    v1 <- f(v1)
    *    v2 <- f(v2)
    *    ...
    *    vn <- f(vn)
    * 
    *    v <- phi(v1, v2, ..., vn)
    *    t <- phi(t1, t2, ..., tn)
    *)

   fun run(SSA as G.GRAPH ssa) = 
   let val Dom as G.GRAPH dom = SSA.dom SSA

       val dominates = Dom.dominates Dom
       val levels    = Dom.levelsMap Dom

       val defSite = SSA.defSite SSA
       val block   = SSA.block SSA
       val uses    = SSA.uses SSA
       val defs    = SSA.defs SSA
       val rtl     = SSA.rtl SSA
       val freqTbl = SSA.freqTbl SSA 

       val showOp  = SSA.showOp SSA
       val showVal = SSA.showVal SSA

       fun process i = 
           case rtl i of
             T.PHI{preds, ...} => hoistAllUses(i,preds)
           | _ => ()

           (* hoist uses of phi-node i *)
       and hoistAllUses(i,preds) =
           let val [t]  = defs i
               val uses_i = uses i

               (* find partially redundant phi nodes *)
           in  if List.exists (fn v => v = t) uses_i then
                  print("PRE "^showOp i^"\n") else ()
           end
          
   in  SSA.forallNodes SSA process;
       SSA
   end

end
