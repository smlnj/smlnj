(* pseudo-ops.sig
 *
 * COPYRIGHT (c) 2001 Bell Labs, Lucent Technologies
 *
 * MLRISC pseudo-ops
 * Ties together the assembler and client pseudo-ops
 *)

signature PSEUDO_OPS = sig
  structure T : MLTREE
  structure Client : CLIENT_PSEUDO_OPS where AsmPseudoOps.T = T

  type pseudo_op = (T.labexp, Client.pseudo_op) PseudoOpsBasisTyp.pseudo_op

  val toString : pseudo_op -> string
  val emitValue : {pOp: pseudo_op, loc: int, emit: Word8.word -> unit} -> unit
    (* identical to that in pseudo-ops-basis.sig *)

  val sizeOf : pseudo_op * int -> int
    (* identical to that in pseudo-ops-basis.sig *)

  val adjustLabels : pseudo_op * int -> bool
    (* adjust the value of labels in the pseudo_op given the current
     * location counter.
     *)
  

end

