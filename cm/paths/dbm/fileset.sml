(* cm/paths/dbm/fileset.sml
 *
 * File sets.
 *   Uses SML/NJ library red-black tree implementation of sets.
 *
 * (C) 2023, The Fellowship of SML/NJ
 *
 * Author: Matthias Blume (matthias.blume@gmail.com)
 * Edited: DBM (2023.6)
 *)

structure FileSet =
  RedBlackMapFn (struct
		   type ord_key = File.file
		   val compare = File.compare (* by stable ids *)
		 end)
