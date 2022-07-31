(* cfg-optimization.sig
 *
 * COPYRIGHT (c) 2001 Bell Labs, Lucent Technologies
 *)

signature CFG_OPTIMIZATION = sig
   structure CFG : CONTROL_FLOW_GRAPH

(*   include MLRISC_OPTIMIZATION  where type flowgraph = CFG.cfg *)

   val name : string                  (* name of optimization *)
   val run  : CFG.cfg -> CFG.cfg  (* run optimization *)

end 

