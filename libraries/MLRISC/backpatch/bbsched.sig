signature BBSCHED = sig
  structure CFG : CONTROL_FLOW_GRAPH

  val bbsched : (CFG.cfg * CFG.node list) -> unit
  val finish : unit -> unit
  val cleanUp : unit -> unit
end
