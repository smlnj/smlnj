(*
 * sparcPseudoInstr.sig --- Sparc pseudo instructions 
 *)

signature SPARC_PSEUDO_INSTR = sig
   structure I : SPARCINSTR

   type format1 = 
       {r:CellsBasis.cell, i:I.operand, d:CellsBasis.cell} * 
       (I.operand -> CellsBasis.cell) -> I.instruction list

   type format2 = 
       {i:I.operand, d:CellsBasis.cell} * 
       (I.operand -> CellsBasis.cell) -> I.instruction list
   (* 
    * Signed and unsigned multiplications.
    * These are all 32 bit operations 
    *)
   val umul32     : format1 (* unsigned/non-trapping *)
   val smul32     : format1 (* signed/non-trapping *)
   val smul32trap : format1 (* trap on overflow *)
   val udiv32     : format1 (* unsigned/non-trapping *)
   val sdiv32     : format1 (* signed/non-trapping *)
   val sdiv32trap : format1 (* trap on overflow/zero *)

       (* convert integer into floating point *)
   val cvti2d : format2
   val cvti2s : format2
   val cvti2q : format2

       (* 32-bit overflow detection *)
   val overflowtrap32 : I.instruction list
 
       (* 64-bit overflow detection *)
   val overflowtrap64 : I.instruction list

end

