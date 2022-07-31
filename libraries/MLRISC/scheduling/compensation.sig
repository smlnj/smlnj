(*
 * This is the signature for inserting compensation code.
 *
 * -- Allen
 *)

signature SCHEDULING_COMPENSATION =
sig

   structure CFG : CONTROL_FLOW_GRAPH
   structure DDG : SCHEDULER_DDG

end
