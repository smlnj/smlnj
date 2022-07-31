(*
 * Rank functions used for scheduling.
 * 
 * -- Allen
 *)

signature SCHEDULING_RANKS =
sig

   structure I   : INSTRUCTIONS
   structure DDG : SCHEDULER_DDG
      sharing DDG.I = I

   type edge
   val rank : ('node,edge) DDG.ddg -> 
                'node Graph.node * 'node Graph.node -> bool

end
