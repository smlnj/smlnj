signature BASIC_BLOCK_ALIASING =
sig

   structure I : INSTRUCTIONS

   val aliasing : unit -> (I.instruction -> I.C.cell list * I.C.cell list)

end
