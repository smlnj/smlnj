(*
 * Module for inserting compensation code.
 *
 * -- Allen
 *)

functor SchedulingCompensation
   (structure CFG : CONTROL_FLOW_GRAPH
    structure DDG : SCHEDULER_DAG
      sharing CFG.I = DDG.I
   ) : SCHEDULING_COMPENSATION =
struct

end
