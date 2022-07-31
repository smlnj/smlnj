signature LABEL_COMP = sig
  structure T : MLTREE
  structure I : INSTRUCTIONS

  type reduce = 
    {stm: T.stm -> unit, 
     rexp: T.rexp -> CellsBasis.cell, 
     emit:I.instruction -> unit 
    }
    (* functions to emit MLRISC statements or register expressions *)

  val ldLabelEA : 
    (I.instruction -> unit) -> T.labexp -> (CellsBasis.cell * I.operand)
    (* generate a label operand to use as an effective address *)

  val ldLabelOpnd : 
    (I.instruction -> unit) -> 
       {label:T.labexp, pref:CellsBasis.cell option} -> I.operand
    (* generate a label operand to be used by immediate instructions *)

  val doJmp : reduce * T.stm  -> unit
    (* compile a jump involving a label *)

  val doCall : reduce * T.stm -> unit
    (* compile a call involving a label *)

end

