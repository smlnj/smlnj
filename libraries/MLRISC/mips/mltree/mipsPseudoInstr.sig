(* mipsPseudoInstr.sig --- mips pseudo instructions *)

signature MIPS_PSEUDO_INSTR = 
sig
   structure I : MIPSINSTR
   structure T : MLTREE
   structure C : MIPSCELLS
     sharing C = I.C
     sharing I.T = T
  
   type reduceOpnd = I.operand -> C.cell
end 

