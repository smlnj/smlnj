(* alphaPseudoInstr.sig --- alpha pseudo instructions *)

signature ALPHA_PSEUDO_INSTR = 
sig
   structure I : ALPHAINSTR
   structure T : MLTREE
   structure C : ALPHACELLS
     sharing C = I.C
     sharing I.T = T
   structure CB: CELLS_BASIS = CellsBasis
  
   type reduceOpnd = I.operand -> CB.cell

   val divlv : {ra:CB.cell, rb:I.operand, rc:CB.cell} * reduceOpnd -> I.instruction list
   val divl  : {ra:CB.cell, rb:I.operand, rc:CB.cell} * reduceOpnd -> I.instruction list
   val divlu : {ra:CB.cell, rb:I.operand, rc:CB.cell} * reduceOpnd -> I.instruction list
   val remlv : {ra:CB.cell, rb:I.operand, rc:CB.cell} * reduceOpnd -> I.instruction list
   val reml  : {ra:CB.cell, rb:I.operand, rc:CB.cell} * reduceOpnd -> I.instruction list
   val remlu : {ra:CB.cell, rb:I.operand, rc:CB.cell} * reduceOpnd -> I.instruction list
   val divqv : {ra:CB.cell, rb:I.operand, rc:CB.cell} * reduceOpnd -> I.instruction list
   val divq  : {ra:CB.cell, rb:I.operand, rc:CB.cell} * reduceOpnd -> I.instruction list
   val divqu : {ra:CB.cell, rb:I.operand, rc:CB.cell} * reduceOpnd -> I.instruction list
   val remqv : {ra:CB.cell, rb:I.operand, rc:CB.cell} * reduceOpnd -> I.instruction list
   val remq  : {ra:CB.cell, rb:I.operand, rc:CB.cell} * reduceOpnd -> I.instruction list
   val remqu : {ra:CB.cell, rb:I.operand, rc:CB.cell} * reduceOpnd -> I.instruction list

   val cvtls : {opnd:I.operand, fd:CB.cell} * reduceOpnd -> I.instruction list
   val cvtlt : {opnd:I.operand, fd:CB.cell} * reduceOpnd -> I.instruction list
   val cvtqs : {opnd:I.operand, fd:CB.cell} * reduceOpnd -> I.instruction list
   val cvtqt : {opnd:I.operand, fd:CB.cell} * reduceOpnd -> I.instruction list

   val cvtsl : {mode:T.rounding_mode, fs:CB.cell, rd:CB.cell} -> I.instruction list
   val cvttl : {mode:T.rounding_mode, fs:CB.cell, rd:CB.cell} -> I.instruction list
   val cvtsq : {mode:T.rounding_mode, fs:CB.cell, rd:CB.cell} -> I.instruction list
   val cvttq : {mode:T.rounding_mode, fs:CB.cell, rd:CB.cell} -> I.instruction list
end 

