(* pseudo-ops-basis.sig
 *
 * COPYRIGHT (c) 2001 Bell Labs, Lucent Technologies
 *
 * Interface to host assembler. 
 * Will handle all constructors in PseudoOpsBasisTyp except for
 * client extensions (EXT)
 *)


signature PSEUDO_OPS_BASIS = sig
  structure T : MLTREE
  type 'a pseudo_op = (T.labexp, 'a) PseudoOpsBasisTyp.pseudo_op 

  val toString  : 'a pseudo_op -> string
  val lexpToString : T.labexp -> string
  val defineLabel : Label.label -> string

  val emitValue : {pOp: 'a pseudo_op, loc: int, emit: Word8.word -> unit} -> unit
    (* emit value of pseudo op give current location counter and output
     * stream. The value emitted should respect the endianness of the
     * target machine.
     *)

  val sizeOf : 'a pseudo_op * int -> int
    (* Size of the pseudo_op in bytes given the current location counter
     * The location counter is provided in case some pseudo ops are 
     * dependent on alignment considerations.
     *)

  val wordSize : int
end

