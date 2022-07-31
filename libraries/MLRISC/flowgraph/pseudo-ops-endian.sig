(* 
 *
 * COPYRIGHT (c) 2001 Lucent Technologies, Bell Laboratories.
 *
 *)

signature PSEUDO_OPS_ENDIAN = sig
  structure T : MLTREE

  type 'a pseudo_op = (T.labexp, 'a) PseudoOpsBasisTyp.pseudo_op

  val emitValue : {pOp : 'a pseudo_op, loc: int, emit : Word8.word -> unit} -> unit
    (* identical to that in pseudo-ops-basis.sig *)

  val sizeOf : 'a pseudo_op * int -> int
    (* identical to that in pseudo-ops-basis.sig *)
end