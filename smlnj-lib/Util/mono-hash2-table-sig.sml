(* mono-hash2-table-sig.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Hash tables that are keyed by two keys (in different domains).
 *
 * AUTHOR:  John Reppy
 *	    University of Chicago
 *	    https://cs.uchicago.edu/~jhr
 *)

signature MONO_HASH2_TABLE =
  sig

    structure Key1 : HASH_KEY
    structure Key2 : HASH_KEY

    type 'a hash_table

    val mkTable : (int * exn) -> 'a hash_table
	(* Create a new table; the int is a size hint and the exception
	 * is to be raised by find.
	 *)

    val clear : 'a hash_table -> unit
	(* remove all elements from the table *)

    val insert : 'a hash_table -> (Key1.hash_key * Key2.hash_key * 'a) -> unit
	(* Insert an item.  If the key already has an item associated with it,
	 * then the old item is discarded.
	 *)

    val inDomain1 : 'a hash_table -> Key1.hash_key -> bool
    val inDomain2 : 'a hash_table -> Key2.hash_key -> bool
	(* return true, if the key is in the domain of the table *)

    val lookup1 : 'a hash_table -> Key1.hash_key -> 'a
    val lookup2 : 'a hash_table -> Key2.hash_key -> 'a
	(* Find an item, the table's exception is raised if the item doesn't exist *)

    val find1 : 'a hash_table -> Key1.hash_key -> 'a option
    val find2 : 'a hash_table -> Key2.hash_key -> 'a option
	(* Look for an item, return NONE if the item doesn't exist *)

    val remove1 : 'a hash_table -> Key1.hash_key -> 'a
    val remove2 : 'a hash_table -> Key2.hash_key -> 'a
	(* Remove an item, returning the item.  The table's exception is raised if
	 * the item doesn't exist.
	 *)

    val numItems : 'a hash_table ->  int
	(* Return the number of items in the table *)

    val listItems  : 'a hash_table -> 'a list
    val listItemsi : 'a hash_table -> (Key1.hash_key * Key2.hash_key * 'a) list
	(* Return a list of the items (and their keys) in the table *)

    val app  : ('a -> unit) -> 'a hash_table -> unit
    val appi : ((Key1.hash_key * Key2.hash_key * 'a) -> unit) -> 'a hash_table
		-> unit
	(* Apply a function to the entries of the table *)

    val map  : ('a -> 'b) -> 'a hash_table -> 'b hash_table
    val mapi : ((Key1.hash_key * Key2.hash_key * 'a) -> 'b) -> 'a hash_table
		-> 'b hash_table
	(* Map a table to a new table that has the same keys *)

(* TODO: add mapPartial and mapPartiali *)

    val fold  : (('a * 'b) -> 'b) -> 'b -> 'a hash_table -> 'b
    val foldi : ((Key1.hash_key * Key2.hash_key * 'a * 'b) -> 'b) -> 'b
		-> 'a hash_table -> 'b

(* TODO: add modify and modifyi *)

    val filter  : ('a -> bool) -> 'a hash_table -> unit
    val filteri : ((Key1.hash_key * Key2.hash_key * 'a) -> bool) -> 'a hash_table
		-> unit
	(* remove any hash table items that do not satisfy the given
	 * predicate.
	 *)

    val copy : 'a hash_table -> 'a hash_table
	(* Create a copy of a hash table *)

    val bucketSizes : 'a hash_table -> (int list * int list)
	(* returns a list of the sizes of the various buckets.  This is to
	 * allow users to gauge the quality of their hashing function.
	 *)

  end (* MONO_HASH2_TABLE *)
