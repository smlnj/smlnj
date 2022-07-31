(* hash-table-sig.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * The signature of the polymorphic hash table structure.
 *
 * AUTHOR:  John Reppy
 *	    University of Chicago
 *	    https://cs.uchicago.edu/~jhr
 *)

signature HASH_TABLE =
  sig

    type ('a, 'b) hash_table
	(* type of hash table mapping 'a to 'b *)

    val mkTable : (('a -> word) * (('a * 'a) -> bool)) -> (int * exn)
	  -> ('a,'b) hash_table
	(* Given a hashing function and an equality predicate, create a new table;
	 * the int is a size hint and the exception is to be raised by find.
	 *)

    val clear : ('a, 'b) hash_table -> unit
	(* remove all elements from the table *)

    val insert : ('a, 'b) hash_table -> ('a * 'b) -> unit
	(* Insert an item.  If the key already has an item associated with it,
	 * then the old item is discarded.
	 *)

    val inDomain : ('a, 'b) hash_table -> 'a -> bool
	(* return true, if the key is in the domain of the table *)

    val lookup : ('a, 'b) hash_table -> 'a -> 'b
	(* Find an item, the table's exception is raised if the item doesn't exist *)

    val find : ('a, 'b) hash_table -> 'a -> 'b option
	(* Look for an item, return NONE if the item doesn't exist *)

    val remove : ('a, 'b) hash_table -> 'a -> 'b
	(* Remove an item, returning the item.  The table's exception is raised if
	 * the item doesn't exist.
	 *)

    val numItems : ('a, 'b) hash_table ->  int
	(* Return the number of items in the table *)

    val listItems  : ('a, 'b) hash_table -> 'b list
    val listItemsi : ('a, 'b) hash_table -> ('a * 'b) list
	(* Return a list of the items (and their keys) in the table *)

    val app  : ('b -> unit) -> ('a, 'b) hash_table -> unit
    val appi : (('a * 'b) -> unit) -> ('a, 'b) hash_table -> unit
	(* Apply a function to the entries of the table *)

    val map  : ('b -> 'c) -> ('a, 'b) hash_table -> ('a, 'c) hash_table
    val mapi : (('a * 'b) -> 'c) -> ('a, 'b) hash_table -> ('a, 'c) hash_table
	(* Map a table to a new table that has the same keys *)

    val fold  : (('b *'c) -> 'c) -> 'c -> ('a, 'b) hash_table -> 'c
    val foldi : (('a * 'b * 'c) -> 'c) -> 'c -> ('a, 'b) hash_table -> 'c
	(* Fold a function over the elements of a table *)

    val modify  : ('b -> 'b) -> ('a, 'b) hash_table -> unit
    val modifyi : (('a * 'b) -> 'b) -> ('a, 'b) hash_table -> unit
	(* modify the hash-table items in place *)

    val filter  : ('b -> bool) -> ('a, 'b) hash_table -> unit
    val filteri : (('a * 'b) -> bool) -> ('a, 'b) hash_table -> unit
	(* remove any hash table items that do not satisfy the given
	 * predicate.
	 *)

    val copy : ('a, 'b) hash_table -> ('a, 'b) hash_table
	(* Create a copy of a hash table *)

    val bucketSizes : ('a, 'b) hash_table -> int list
	(* returns a list of the sizes of the various buckets.  This is to
	 * allow users to gauge the quality of their hashing function.
	 *)

  end (* HASH_TABLE *)
