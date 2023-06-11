(* cm/paths/dbm/filemap.sml
 *
 * File dictionaries.
 *   Uses SML/NJ library red-black tree implementation of binary maps
 *   over files.
 *
 * (C) 2023 The Fellowship of SML/NJ
 *
 * Author: Matthias Blume (matthias.blume@gmail.com)
 * Edited: DBM (2023.6)
 *)

(* structure name changed: SrcPathMap --> FileMap *)

structure FileMap =
  RedBlackMapFn (struct
		   type ord_key = File.file
		   val compare = File.compare (* by stable ids *)
		 end)
