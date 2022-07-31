(* as-pseudo-ops.sig
 *
 * COPYRIGHT (c) 2006 The Fellowship of SML/NJ (www.smlnj.org)
 * All rights reserved.
 *
 * An interface to the string related functions to emit pseudo-ops
 * for assemblers.
 *)

signature AS_PSEUDO_OPS =
  sig
    structure T : MLTREE
    val lexpToString : T.labexp -> string
    val toString : (T.labexp, 'a) PseudoOpsBasisTyp.pseudo_op -> string
    val defineLabel : Label.label -> string
  end

