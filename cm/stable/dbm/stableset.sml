(* cm/stable/dbm/stableset.sml
 *
 * "Stable module" sets: sets of BinInfo.infos representing stable
 *  library binfiles).
 *
 * Uses SML/NJ library implementation (RedBlack trees) of finite sets.
 *
 * (C) 2023 The Fellowship of SML/NJ
 *
 * Author: Matthias Blume (matthias.blume@gmail.com)
 *)

structure StableSet =
RedBlackSetFn
  (struct
     type ord_key = BinInfo.info
     val compare = BinInfo.compare
   end)
