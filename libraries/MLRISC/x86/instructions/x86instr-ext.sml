(* x86instr-ext.sml
 *
 * COPYRIGHT (c) 2000 Bell Labs, Lucent Technologies
 *
 * extensions to the x86 instruction set.
 *)

structure X86InstrExt = struct
  datatype fsz = single | double | extended

  datatype ('s, 'r, 'f, 'c) sext 
    (* push an integer value onto the H/W stack *)
    = PUSHL of 'r
    | POP of 'r

    (* FSTPS/L/T is a way of pulling things off the floating point 
     * stack and must therefore take FREG f as argument 
     *)
    | FSTPS of 'f
    | FSTPL of 'f
    | FSTPT of 'f

    | LEAVE
    | RET of 'r

    | LOCK_CMPXCHGL of ('r * 'r)
		       
    | PAUSE                         (* improves performance of spin-wait loops *) 
    (* performs a serializing operation on all load-from-memory and store-to-memory 
     * operations issued prior to the mfence instruction.
     *)
    | MFENCE
    (* performs a serializing operation on all load-to-memory operations issued prior to
     * the lfence instruction.
     *)
    | LFENCE
    (* performs a serializing operation on all store-to-memory operations issued prior to
     * the sfence instruction.
     *)
    | SFENCE


end
