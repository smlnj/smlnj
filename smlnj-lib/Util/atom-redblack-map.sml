(* atom-redblack-map.sml
 *
 * COPYRIGHT (c) 1999 Bell Labs, Lucent Technologies.
 *
 * Functional finite maps with atom keys.
 *)

structure AtomRedBlackMap =
  RedBlackMapFn (
    struct
      type ord_key = Atom.atom
      val compare = Atom.compare
    end)
