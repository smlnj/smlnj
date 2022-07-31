(*
 * Various global scheduling algorithm have this signature
 *
 * -- Allen
 *)

signature GLOBAL_SCHEDULING_ALGORITHM =
sig

   structure IR  : MLRISC_IR
   structure CFG : CONTROL_FLOW_GRAPH
   structure DDG : SCHEDULER_DDG
     sharing DDG.I = CFG.I
     sharing IR.CFG = CFG

   val schedule : DDG.SchedProps.cpu_info (* architecture *) ->
                  { ir     : IR.IR,  (* The overall IR *)
                    region : IR.cfg, (* Subregion for scheduling *)
                    numberOfInstructions : int,
                    blockIdTbl : int Array.array
                  } -> unit

end
