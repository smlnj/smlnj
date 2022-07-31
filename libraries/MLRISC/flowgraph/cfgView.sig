(* cfgView.sig -- graphical viewing utilities for cfg 
 *
 * Copyright (c) 2001 Bell Laboratories.
 *)

signature CFG_VIEW = sig
   structure CFG : CONTROL_FLOW_GRAPH
   val viewStyle      : CFG.cfg -> (CFG.block, CFG.edge_info, CFG.info) GraphLayout.style
   val viewLayout     : CFG.cfg -> GraphLayout.layout
   val headerText     : CFG.block -> string
   val footerText     : CFG.block -> string
   val subgraphLayout : {cfg : CFG.cfg, subgraph : CFG.cfg } -> GraphLayout.layout
end
