(*
 * This module is for GC-safe code motion.
 * It generates the appropriate set of control dependences to limit
 * code motion that cannot be correctly repaired by subsequent passes.  
 *)
signature GC_CODE_MOTION =
sig
   structure IR : MLRISC_IR
   structure GC : GC_TYPE

   val gcSafeCodeMotion : IR.IR -> IR.IR

end
