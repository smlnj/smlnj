signature EXPAND_COPIES =
sig
   structure I : INSTRUCTIONS
   val expandCopies : I.instruction -> I.instruction list
end
