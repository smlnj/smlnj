(* symbol-hashtable.sml
 *
 *   Hash table of symbols.
 *
 * Copyright (c) 2004 by The Fellowship of SML/NJ
 *
 * Author: Matthias Blume (blume@tti-c.org)
 *)
structure SymbolHashTable =
    HashTableFn (struct type hash_key = Symbol.symbol
			val hashVal = Symbol.number
			val sameKey = Symbol.eq
		 end)
