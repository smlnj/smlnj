(*
 * This is a generic module for performing (forward) dataflow 
 * analysis on the SSA graph.  Returns an array of dataflow values 
 * indexed by variables.
 * 
 * -- Allen (leunga@cs.nyu.edu)
 *)
signature SSA_DATAFLOW_ANALYSIS =
sig
   structure SSA : SSA

   val dataflowAnalysis : 
       { ssa      : SSA.ssa,              (* SSA graph *)
         bot      : 'D,                   (* bottom element *)
         top      : 'D,                   (* top element *)
         ==       : 'D * 'D -> bool,      (* equality *)
         const    : SSA.value -> 'D,      (* map constants to lattice value *)
         meet     : 'D list -> 'D,        (* meet *)
         transfer : {rtl:SSA.rtl,         (* transfer function *)
                     inputs:'D list, 
                     defs:SSA.value list
                    } -> 'D list
       } -> 'D Array.array     (* SSA variable -> dataflow value *)

end
