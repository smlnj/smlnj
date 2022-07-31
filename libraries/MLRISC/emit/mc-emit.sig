
signature MC_EMIT = sig
  structure I : INSTRUCTIONS

  val emitInstr : I.instruction -> Word8Vector.vector
end

  
