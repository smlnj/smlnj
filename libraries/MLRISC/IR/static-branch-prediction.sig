(*
 * This module performs static branch prediction using heuristics
 * similar to Ball and Larus'
 *
 * -- Allen
 *)

signature STATIC_BRANCH_PREDICTION = 
sig

   structure IR : MLRISC_IR

   val profile : {loopMultiplier:int} -> IR.IR -> unit

end

