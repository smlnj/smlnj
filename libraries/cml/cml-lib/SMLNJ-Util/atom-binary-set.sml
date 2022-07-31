(* atom-binary-map.sml
 *
 * COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies.
 *
 * Functional sets of atoms.
 *)

structure AtomBinarySet =
  BinarySetFn (
    struct
      type ord_key = Atom.atom
      val compare = Atom.compare
    end)
