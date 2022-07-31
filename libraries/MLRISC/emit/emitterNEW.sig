(* emitterNEW.sig
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

(** emitter - emit assembly or machine code **)

(* Note:
 *	assembly code: Each of the emit functions outputs the 
 * appropriate assembly instructions to a file. The stream to
 * this file can be hardwired.
 *
 *      machine code: Each of the emit functions outputs the 
 * appropriate binary output to a bytearray created in a special
 * structure reserved for this purpose.
 *
 *)
signature EMITTER_NEW = sig
  structure I : INSTRUCTIONS
  structure P : PSEUDO_OPS

  val defineLabel  : Label.label -> unit
  val emitInstr : I.instruction * int IntHashTable.hash_table -> unit
  val comment : string -> unit
  val pseudoOp : P.pseudo_op -> unit
  val init : int -> unit
end  




