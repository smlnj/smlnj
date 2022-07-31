(* mltree-hash.sig
 *
 * COPYRIGHT (c) 2001 Lucent Technologies, Bell Laboratories.
 *
 * Utilities to hash mltree expressions
 *)

signature MLTREE_HASH = sig
  structure T  : MLTREE
  val hash     : T.labexp -> word

  val hashStm   : T.stm -> word
  val hashRexp  : T.rexp -> word
  val hashFexp  : T.fexp -> word
  val hashCCexp : T.ccexp -> word

end