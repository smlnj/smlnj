(*
 * This is Reif and Tarjan's algorithm (SIAM J Computing 1981) 
 * for computing approximate birthpoints for expressions.   
 * For each basic block B,
 *   idef(B) = { v | v is defined on some path between B's idom and B }
 *
 * -- Allen
 *)
signature MLRISC_IDEFS =
sig

   structure Dom : DOMINATOR_TREE
   structure CFG : CONTROL_FLOW_GRAPH
   structure I   : INSTRUCTIONS
   structure C   : CELLS
      sharing CFG.I = I
      sharing C     = I.C

   val idefs : 
       (I.instruction -> C.cell list * C.cell list) ->
       CFG.cfg ->
       { idefuse     : unit -> 
           (C.SortedCells.sorted_cells * 
            C.SortedCells.sorted_cells) Array.array,
         ipostdefuse : unit -> 
           (C.SortedCells.sorted_cells * 
            C.SortedCells.sorted_cells) Array.array
       }
end

