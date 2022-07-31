(* mono-hash-table-sig.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * The result signature of the hash table functor (see hash-table.sml).
 *
 * AUTHOR:  John Reppy
 *	    University of Chicago
 *	    https://cs.uchicago.edu/~jhr
 *)

signature MONO_HASH_TABLE =
  sig

    structure Key : HASH_KEY

    type 'a hash_table

    val mkTable : (int * exn) -> 'a hash_table
	(* Create a new table; the int is a size hint and the exception
	 * is to be raised by find.
	 *)

    val clear : 'a hash_table -> unit
	(* remove all elements from the table *)

    val insert : 'a hash_table -> (Key.hash_key * 'a) -> unit
	(* Insert an item.  If the key already has an item associated with it,
	 * then the old item is discarded.
	 *)

    val inDomain : 'a hash_table -> Key.hash_key -> bool
	(* return true, if the key is in the domain of the table *)

    val lookup : 'a hash_table -> Key.hash_key -> 'a
	(* Find an item, the table's exception is raised if the item doesn't exist *)

    val find : 'a hash_table -> Key.hash_key -> 'a option
	(* Look for an item, return NONE if the item doesn't exist *)

    val remove : 'a hash_table -> Key.hash_key -> 'a
	(* Remove an item, returning the item.  The table's exception is raised if
	 * the item doesn't exist.
	 *)

    val numItems : 'a hash_table ->  int
	(* Return the number of items in the table *)

    val listItems  : 'a hash_table -> 'a list
    val listItemsi : 'a hash_table -> (Key.hash_key * 'a) list
	(* Return a list of the items (and their keys) in the table *)

    val app  : ('a -> unit) -> 'a hash_table -> unit
    val appi : ((Key.hash_key * 'a) -> unit) -> 'a hash_table -> unit
	(* Apply a function to the entries of the table *)

    val map  : ('a -> 'b) -> 'a hash_table -> 'b hash_table
    val mapi : ((Key.hash_key * 'a) -> 'b) -> 'a hash_table -> 'b hash_table
	(* Map a table to a new table that has the same keys *)

    val fold  : (('a * 'b) -> 'b) -> 'b -> 'a hash_table -> 'b
    val foldi : ((Key.hash_key * 'a * 'b) -> 'b) -> 'b -> 'a hash_table -> 'b

    val modify  : ('a -> 'a) -> 'a hash_table -> unit
    val modifyi : ((Key.hash_key * 'a) -> 'a) -> 'a hash_table -> unit
	(* modify the hash-table items in place *)

(** Also mapPartial?? *)
    val filter  : ('a -> bool) -> 'a hash_table -> unit
    val filteri : ((Key.hash_key * 'a) -> bool) -> 'a hash_table -> unit
	(* remove any hash table items that do not satisfy the given
	 * predicate.
	 *)

    val copy : 'a hash_table -> 'a hash_table
	(* Create a copy of a hash table *)

    val bucketSizes : 'a hash_table -> int list
	(* returns a list of the sizes of the various buckets.  This is to
	 * allow users to gauge the quality of their hashing function.
	 *)

  end (* MONO_HASH_TABLE *)
