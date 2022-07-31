(* atom-binary-map.sml
 *
 * COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies.
 *
 * Functional finite maps with atom keys.
 *)

structure AtomBinaryMap =
  BinaryMapFn (
    struct
      type ord_key = Atom.atom
      val compare = Atom.compare
    end)
