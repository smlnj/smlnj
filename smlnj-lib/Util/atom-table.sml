(* atom-table.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *
 * Hash tables of atoms.
 *)

structure AtomTable = HashTableFn (struct
      type hash_key = Atom.atom
      val hashVal = Atom.hash
      val sameKey = Atom.same
    end);

