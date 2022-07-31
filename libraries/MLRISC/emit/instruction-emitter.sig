(*
 * This is the abstract interface for all instruction emitters, i.e.
 * assemblers and machine code emitters.
 *
 * -- Allen
 *)

signature INSTRUCTION_EMITTER =
sig

   structure I : INSTRUCTIONS
   structure S : INSTRUCTION_STREAM

   (* 
    * Create a new stream.  The argument is a list of 
    * annotations that can affect the output format.
    *)
   val makeStream : Annotations.annotations ->
		    (I.instruction,'b,'c,'d) S.stream

end
