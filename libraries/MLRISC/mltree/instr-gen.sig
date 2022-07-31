(*
 * Generate a linear sequence of instructions
 *)
signature INSTR_GEN =
sig
   structure C   : CELLS
   structure I   : INSTRUCTIONS
   structure S   : INSTRUCTION_STREAM
   structure CFG : CONTROL_FLOW_GRAPH 

   sharing I.C = C 
   sharing CFG.P = S.P

   (* 
    * This function creates an instruction stream, which can be 
    * used to emit instruction into the instruction list.
    *)
   val newStream : I.instruction list ref -> 
                     (I.instruction, Annotations.annotations, 'a, CFG.cfg) S.stream

end
