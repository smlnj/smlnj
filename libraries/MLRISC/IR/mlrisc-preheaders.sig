(*
 * This module just insert preheaders
 *
 * -- Allen
 *)
signature INSERT_PREHEADERS =
sig

   structure IR : MLRISC_IR
 
   val insert_preheaders : IR.IR -> unit

end

