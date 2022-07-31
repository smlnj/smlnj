(*
 * This module performs gc-safety repair in SSA form.
 *)
functor GCSafety
   (structure SSA       : SSA
    structure GCProps   : GC_PROPERTIES
    structure InsnProps : INSN_PROPERTIES
    structure SSAProps  : SSA_PROPERTIES
       sharing SSA.I = GCProps.I = InsnProps.I = SSAProps.I
   ) : GC_SAFETY =
struct
   structure SSA = SSA
   structure G   = Graph
   structure A   = Array

   fun gcSafety(SSA as G.GRAPH ssa) = 
   let val N = #capacity ssa
   in  SSA
   end

end
