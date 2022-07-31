(* atom-redblack-map.sml
 *
 * COPYRIGHT (c) 1999 Bell Labs, Lucent Technologies.
 *
 * Functional sets of atoms.
 *)

structure AtomRedBlackSet =
  RedBlackSetFn (
    struct
      type ord_key = Atom.atom
      val compare = Atom.compare
    end)
