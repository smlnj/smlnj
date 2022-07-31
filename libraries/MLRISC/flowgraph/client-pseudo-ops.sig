(* client-pseudo-ops.sig
 *
 * COPYRIGHT (c) 2001 Bell Labs, Lucent Technologies
 *
 * Client pseudo-ops. Must be all related to data and not code.
 *)


signature CLIENT_PSEUDO_OPS = sig
  structure AsmPseudoOps : PSEUDO_OPS_BASIS

  type pseudo_op

  val toString : pseudo_op -> string
  val emitValue : {pOp: pseudo_op, loc: int, emit: Word8.word -> unit} -> unit
  val sizeOf : pseudo_op * int -> int
  val adjustLabels : pseudo_op * int -> bool
end







