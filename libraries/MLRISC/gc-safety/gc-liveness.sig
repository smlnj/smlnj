(*
 * This module is responsible for computing typed liveness 
 * information needed garbage collection-safe global optimizations.
 *)
signature GC_LIVENESS =
sig
   structure IR : MLRISC_IR
   structure GC : GC_TYPE
   structure GCTypeMap : GC_TYPEMAP
     sharing GCTypeMap.GC = GC

   val liveness : IR.IR -> {liveIn:GCTypeMap.typemap,
                            liveOut:GCTypeMap.typemap} Array.array
end
