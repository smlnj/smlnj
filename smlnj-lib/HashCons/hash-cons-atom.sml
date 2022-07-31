(* hash-cons-atom.sml
 *
 * COPYRIGHT (c) 2011 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure HashConsAtom = HashConsGroundFn (
  struct
    type hash_key = Atom.atom
    val sameKey = Atom.same
    val hashVal = Atom.hash
  end)
