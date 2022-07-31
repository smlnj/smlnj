(* bit-flags.sml
 *
 * COPYRIGHT (c) 2003 The Fellowship of SML/NJ
 *
 * Signature for bit flags.
 *
 *)

signature BIT_FLAGS = sig

    eqtype flags

    val toWord   : flags -> SysWord.word
    val fromWord : SysWord.word -> flags

    val all : flags
    val flags : flags list -> flags	(* union *)
    val intersect : flags list -> flags	(* intersection *)
    val clear : flags * flags -> flags	(* set difference flipped *)
    val allSet : flags * flags -> bool  (* subseteq *)
    val anySet : flags * flags -> bool 	(* non-empty intersection *)

end
