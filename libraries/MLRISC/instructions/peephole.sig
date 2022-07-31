signature PEEPHOLE =
sig
   structure I : INSTRUCTIONS

   (* Instructions are in reversed order *)
   val peephole : I.instruction list -> I.instruction list

end
