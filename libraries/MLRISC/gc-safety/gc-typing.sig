(*
 * This module is responsible for propagating gc type information.
 *)
signature GC_TYPING =
sig

   structure IR : MLRISC_IR 
   structure GC : GC_TYPE

   val gcTyping : IR.IR -> IR.IR

end
