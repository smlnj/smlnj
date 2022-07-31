(*
 * A customizable list scheduler that works on a region at a time.
 * A region is a subset of the control flow graph.
 *
 * -- Allen
 *)
signature LIST_SCHEDULER = 
sig

   structure I          : INSTRUCTIONS
   structure IR         : MLRISC_IR
   structure DDG        : SCHEDULER_DDG
   structure CFG        : CONTROL_FLOW_GRAPH
     sharing DDG.I = IR.I = I
     sharing IR.CFG = CFG 

   val listScheduler : 
       { cpu_info   : DDG.SchedProps.cpu_info,
         blockIdTbl : int Array.array,  
         ranking    : DDG.node Graph.node * DDG.node Graph.node -> bool,
         cfg        : CFG.cfg,  (* the entire program! *)
         region     : CFG.cfg,  (* current region *)
         ddg        : (DDG.node, DDG.edge) DDG.ddg
       } -> unit

end
