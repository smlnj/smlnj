signature MEMORY_REGISTERS = sig
  structure I : X86INSTR
  val memReg : {reg:I.operand, base: CellsBasis.cell} -> I.ea
end
