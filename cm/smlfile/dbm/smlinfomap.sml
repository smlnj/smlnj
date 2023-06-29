(*
 * Dictionaries indexed by SmlInfo.info.
 *   Uses SML/NJ library implementation of binary maps.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
structure SmlInfoMap =
RedBlackMapFn
  (struct
     type ord_key = SmlInfo.info
     val compare = SmlInfo.compareInfo  (* File.compareFile on file components *)
   end)
