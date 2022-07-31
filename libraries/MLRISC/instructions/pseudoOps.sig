(* pseudo-ops.sml --- description of assembly pseudo-ops
 * 
 * COPYRIGHT (c) 1996 AT&T Bell Laboratories.
 *
 *)

signature PSEUDO_OPS = sig
  type pseudo_op

  val toString : pseudo_op -> string

  val emitValue : {pOp:pseudo_op, loc:int, emit:Word8.word -> unit} -> unit
    (* emit value of pseudo op give current location counter and output
     * stream. The value emitted should respect the endianness of the
     * target machine.
     *)

  val sizeOf : pseudo_op * int -> int
    (* Size of the pseudo_op in bytes given the current location counter
     * The location counter is provided in case some pseudo ops are 
     * dependent on alignment considerations.
     *)

  val adjustLabels : pseudo_op * int -> bool
    (* adjust the value of labels in the pseudo_op given the current
     * location counter.
     *)
  
end


