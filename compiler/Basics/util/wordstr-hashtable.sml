(* wordstr-hashtable.sml
 *
 *   A hashtable of strings which are already explicitly paired with
 *   their respective hash value.
 *
 * Copyright (c) 2004 by The Fellowship of SML/NJ
 *
 * Author: Matthias Blume (blume@tti-c.org)
 *)
structure WordStringHashTable =
    HashTableFn (struct type hash_key = word * string
			fun hashVal (k : hash_key) = #1 k
			fun sameKey ((h, s) : hash_key, (h', s')) =
			    h = h' andalso s = s'
		 end)
