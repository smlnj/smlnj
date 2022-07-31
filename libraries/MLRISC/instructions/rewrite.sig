(*
 * Signature for rewriting (renaming) cells inside instructions.
 *)

signature REWRITE_INSTRUCTIONS =
sig

   structure I : INSTRUCTIONS

                                    (* from      to *) 
   val rewriteDef : I.instruction * CellsBasis.cell * CellsBasis.cell -> I.instruction
   val rewriteUse : I.instruction * CellsBasis.cell * CellsBasis.cell -> I.instruction
   val frewriteDef : I.instruction * CellsBasis.cell * CellsBasis.cell -> I.instruction
   val frewriteUse : I.instruction * CellsBasis.cell * CellsBasis.cell -> I.instruction
end
