(* hash-key-sig.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Abstract hash table keys.  This is the argument signature for the hash table
 * functor (see hash-table-sig.sml and hash-table.sml).
 *
 * AUTHOR:  John Reppy
 *	    University of Chicago
 *	    https://cs.uchicago.edu/~jhr
 *)

signature HASH_KEY =
  sig
    type hash_key

    val hashVal : hash_key -> word
	(* Compute an unsigned integer key from a hash key. *)

    val sameKey : (hash_key * hash_key) -> bool
	(* Return true if two keys are the same.
	 * NOTE: if sameKey(h1, h2), then it must be the
	 * case that (hashVal h1 = hashVal h2).
	 *)

  end (* HASH_KEY *)
