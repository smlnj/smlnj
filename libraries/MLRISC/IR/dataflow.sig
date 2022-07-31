(* 
 * Signatures used by the iterative data flow analyzers.
 * 
 * -- Allen
 *)

signature DATAFLOW_ANALYZER =
sig

   structure CFG : CONTROL_FLOW_GRAPH

   type dataflow_info

   val analyze : CFG.cfg * dataflow_info -> dataflow_info

end

signature DATAFLOW_PROBLEM =
sig

   structure CFG : CONTROL_FLOW_GRAPH

   type domain
   type dataflow_info

   val forward   : bool
   val bot       : domain
   val ==        : domain * domain -> bool
   val join      : domain list -> domain
   val prologue  : CFG.cfg * dataflow_info ->
                       CFG.block Graph.node ->
                           { input    : domain,
                             output   : domain,
                             transfer : domain -> domain
                           }
   val epilogue  : CFG.cfg * dataflow_info ->
                       { node   : CFG.block Graph.node,
                         input  : domain,
                         output : domain
                       } -> unit
end

