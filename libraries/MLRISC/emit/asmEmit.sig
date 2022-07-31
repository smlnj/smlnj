(* asmEmit.sig
 *
 * COPYRIGHT (c) 2001 Bell Labs, Lucent Technologies
 *)

signature ASSEMBLY_EMITTER =
sig

   structure CFG : CONTROL_FLOW_GRAPH

   val asmEmit : (CFG.cfg * CFG.node list) -> unit

end
