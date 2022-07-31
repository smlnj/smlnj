(* block-placement.sig
 *
 * COPYRIGHT (c) 2001 Bell Labs, Lucent Technologies
 *)

(* Perform code block placement *)

(* When several blocks are successors to the unique entry node, 
 * then block with the lowest block id appears first.
 * This usually corresponds to what one wants when doing dynamic 
 * code generation.
 *)
signature BLOCK_PLACEMENT = sig
  structure CFG : CONTROL_FLOW_GRAPH

  val blockPlacement : CFG.cfg -> (CFG.cfg * CFG.node list)

end
