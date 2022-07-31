(*
 * This is the abstract interface for propagating gc info from an 
 * instruction set.
 *)

signature GC_PROPERTIES =
sig

   structure I  : INSTRUCTIONS
   structure GC : GC_TYPE

   (*
    * This function is responsible for propagating gc type information
    * from the input operands to the output operands of an instruction.
    * The propagation is performed conservatively; that is, when in doubt,
    * we leave the gc-type unspecified. 
    *)

   val propagate : { lookup : I.C.cell -> GC.gctype,
                     update : I.C.cell * GC.gctype -> unit
                   } -> I.instruction -> unit

end
