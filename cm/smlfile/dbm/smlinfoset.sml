(* cm/smlfile/dbm/smlinfoset.sml
 *
 * Sets of SmlInfo.info items representing sets of sml files.
 *
 * Uses SML/NJ library implementation (RedBlack trees) of finite sets.
 *
 * (C) 2023 The Fellowship of SML/NJ
 *
 * Author: Matthias Blume (matthias.blume@gmail.com)
 * Edited: DBM 2023.6
 *)

structure SmlInfoSet =
RedBlackSetFn
  (struct
     type ord_key = SmlInfo.info
     val compare = SmlInfo.compare
   end)
