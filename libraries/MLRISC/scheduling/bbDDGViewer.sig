(*
 * View a scheduler DDG constructed for basic block scheduling
 *
 * -- Allen
 *)
signature BASIC_BLOCK_SCHEDULER_DDG_VIEWER =
sig

   structure DDG : SCHEDULER_DDG
   structure I   : INSTRUCTIONS
     sharing DDG.I = I

   val toString : I.instruction -> string

   val view : (I.instruction,DDG.latency) DDG.ddg -> unit

end

