(* atom-sig.sml
 *
 * COPYRIGHT (c) 1996 by AT&T Research
 *
 * AUTHOR:	John Reppy
 *		AT&T Bell Laboratories
 *		Murray Hill, NJ 07974
 *		jhr@research.att.com
 *
 * TODO: add a gensym operation?
 *)

signature ATOM =
  sig

    type atom
	(* Atoms are hashed strings that support fast equality testing. *)

    val atom : string -> atom
    val atom' : substring -> atom
	(* Map a string/substring to the corresponding unique atom. *)
    val toString : atom -> string
	(* return the string representation of the atom *)
    val sameAtom : (atom * atom) -> bool
	(* return true if the atoms are the same *)
    val compare : (atom * atom) -> order
	(* compare two atoms for their relative order; note that this is
	 * not lexical order!
	 *)
    val hash : atom -> word
	(* return a hash key for the atom *)

  end (* signature ATOM *)
