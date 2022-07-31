signature X86SHUFFLE = sig
  structure I : X86INSTR

  type t = {tmp:I.operand option, dst:CellsBasis.cell list, src:CellsBasis.cell list}

  val shuffle : t -> I.instruction list
  val shufflefp : t -> I.instruction list
end
