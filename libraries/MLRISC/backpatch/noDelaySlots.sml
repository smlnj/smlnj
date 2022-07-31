(* 
 * This is a default description for architectures without *any* delay slots.
 * By using this dummy module the architecture can use the spanDep.sml 
 * module for span dependency resolution.
 *
 * -- Allen
 *)

functor NoDelaySlots(I : INSTRUCTIONS) : DELAY_SLOT_PROPERTIES =
struct
   structure I = I

   datatype delay_slot = 
     D_NONE        (* no delay slot *)
   | D_ERROR       (* an error *)    
   | D_ALWAYS      (* one delay slot *)
   | D_TAKEN       (* delay slot is only active when branch is taken *)
   | D_FALLTHRU    (* delay slot is only active when branch is not taken *)

       (* size of delay slot in bytes *)
   val delaySlotSize = 0 

       (* Return the delay slot properties of an instruction *)
   fun delaySlot{instr,backward} =
        { n   = false,   (* is the nullified bit on? *)
          nOn = D_ERROR, (* delay type when nullified *)
          nOff= D_NONE,  (* delay type when not nullified *)
          nop = false    (* is there a nop padded? *) 
        } 

       (* Change the delay slot properties of an instruction *)
   fun enableDelaySlot{instr,n,nop} = instr 

       (* is there any dependency conflict? *)
   fun conflict{src,dst} = true

       (* can delaySlot fit within the delay slot of jmp? *)
   fun delaySlotCandidate{jmp, delaySlot} = false

       (* change the branch target of an instruction *)
   fun setTarget(instr,label) = instr

end
