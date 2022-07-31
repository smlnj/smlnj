(*
 * GC Code motion
 *)
functor GCCodeMotion
   (structure IR : MLRISC_IR
    structure GC : GC_TYPE
   ) : GC_CODE_MOTION =
struct

   structure IR  = IR
   structure CFG = IR.CFG
   structure GC  = GC
   structure G   = Graph

   fun gcSafeCodeMotion(IR as G.GRAPH cfg) =
       IR

end
