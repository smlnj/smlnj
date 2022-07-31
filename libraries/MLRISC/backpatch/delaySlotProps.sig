(* 
 * Architectures that require branch delay slots should implement this module 
 *
 *)
signature DELAY_SLOT_PROPERTIES =
sig
   structure I : INSTRUCTIONS

   datatype delay_slot = 
     D_NONE        (* no delay slot *)
   | D_ERROR       (* an error *)    
   | D_ALWAYS      (* one delay slot *)
   | D_TAKEN       (* delay slot is only active when branch is taken *)
   | D_FALLTHRU    (* delay slot is only active when branch is not taken *)

       (* size of delay slot in bytes *)
   val delaySlotSize : int 

       (* Return the delay slot properties of an instruction *)
   val delaySlot : { instr : I.instruction, backward : bool } -> 
		   { n    : bool,       (* is the nullified bit on? *)
		     nOn  : delay_slot, (* delay type when nullified *)
		     nOff : delay_slot, (* delay type when not nullified *)
		     nop  : bool        (* is there a nop padded? *) 
		   } 

       (* Change the delay slot properties of an instruction *)
   val enableDelaySlot : 
	 {instr : I.instruction, n:bool, nop:bool} -> I.instruction

       (* is there any dependency conflict? *)
   val conflict : {src:I.instruction,dst:I.instruction} -> bool

       (* can delaySlot fit within the delay slot of jmp? *)
   val delaySlotCandidate : 
       { jmp : I.instruction, delaySlot : I.instruction } -> bool

       (* change the branch target of an instruction *)
   val setTarget : I.instruction * Label.label -> I.instruction

end

