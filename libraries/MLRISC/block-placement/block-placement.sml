(* block-placement.sml
 *
 * COPYRIGHT (c) 2002 Bell Labs, Lucent Technologies
 *)

functor BlockPlacement 
   (structure CFG : CONTROL_FLOW_GRAPH
    structure Props : INSN_PROPERTIES
	where I = CFG.I)

   : BLOCK_PLACEMENT =

struct
  structure CFG = CFG

  structure DefaultPlacement = DefaultBlockPlacement(CFG)

  structure WeightedPlacement = 
     WeightedBlockPlacementFn
	  (structure CFG = CFG 
	   structure InsnProps = Props)

  val placementFlag = MLRiscControl.mkFlag
			  ("weighted-block-placement",
			   "whether MLRISC does weighted block placement")

  fun blockPlacement(cfg as Graph.GRAPH G) =
	if !placementFlag
	  then WeightedPlacement.blockPlacement cfg
	  else DefaultPlacement.blockPlacement cfg

end
