(* intlist-map.sml
 *
 *  (C) 2002, Lucent Technologies, Bell Labs
 *
 * author: Matthias Blume (blume@research.bell-labs.com)
 *)
structure IntListMap = RedBlackMapFn (type ord_key = int list
				      val compare = List.collate Int.compare)
