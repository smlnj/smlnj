(*
 * View a scheduler DDG constructed for basic block scheduling
 *
 * -- Allen
 *)
signature GLOBAL_SCHEDULER_DDG_VIEWER =
sig

   structure IR  : MLRISC_IR
   structure DDG : SCHEDULER_DDG
   structure I   : INSTRUCTIONS
     sharing DDG.I = I

   val view : IR.IR -> (DDG.node,DDG.edge) DDG.ddg -> unit

end

