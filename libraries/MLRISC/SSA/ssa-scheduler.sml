(* 
 * Global DAG Scheduling on the SSA graph.
 *
 * -- Allen (leunga@cs.nyu.edu)
 *)
functor SSAScheduler(SSA : SSA) : SSA_OPTIMIZATION =
struct
   structure SSA  = SSA
   structure SP   = SSA.SP
   structure RTL  = SSA.RTL
   structure T    = RTL.T
   structure G    = Graph 
   structure W8A  = Word8Array
   structure A    = Array
  
   type flowgraph = SSA.ssa

   fun error msg = MLRiscErrorMsg.error("SSAScheduler",msg)

   val name = "Global Scheduling"

   fun run(SSA as G.GRAPH ssa) = 
   let val CFG as G.GRAPH cfg = SSA.cfg SSA
   in  SSA
   end
end
