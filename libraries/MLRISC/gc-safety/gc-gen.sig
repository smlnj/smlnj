(*
 * This module is reponsible for generating garbage collection 
 * code for all gc-points in the program.  That is, we delay the generation
 * of garbage collection code until all optimizations have been performed.
 * A callback is invoked at all GC safe-points with the appropriate type 
 * information.  The callback is responsible for generating the appropriate
 * code to save and restore all roots and call the garbage collector.
 *)
signature GC_CALLBACK =
sig
   structure C  : CELLS
   structure GC : GC_TYPE
   structure T  : MLTREE
   val callgcCallback :  
        { id          : int,                        (* basic block id *)
          msg         : string,                     (* some auxiliary text *)
          gcLabel     : Label.label,                (* label of gc block *)
          returnLabel : Label.label,                (* label of return block *)
          roots       : (C.cell * GC.gctype) list,  (* root set *)
          stream      : (T.stm,T.mlrisc list) T.stream 
                          (* code generator *)
        } -> unit
end
