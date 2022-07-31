(* hash-cons-map-sig.sml
 *
 * COPYRIGHT (c) 2011 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature HASH_CONS_MAP =
  sig

    type 'a obj = 'a HashCons.obj

    type ('a, 'b) map

    val empty : ('a, 'b) map
	(* The empty map *)

    val singleton : ('a obj * 'b) -> ('a, 'b) map
	(* return the specified singleton map *)

    val insert  : ('a, 'b) map * 'a obj * 'b -> ('a, 'b) map
    val insert' : (('a obj * 'b) * ('a, 'b) map) -> ('a, 'b) map
	(* Insert an item. *)

    val insertWith  : (('b * 'b) -> 'b)
	  -> ('a, 'b) map * 'a obj * 'b -> ('a, 'b) map
	(* Insert an item with a combining function to resolve collisions.
	 * The first argument to the combining function is the existing value,
	 * and the second argument is the value being inserted into the map.
	 *)
    val insertWithi :  (('a obj * 'b * 'b) -> 'b)
	  -> ('a, 'b) map * 'a obj * 'b -> ('a, 'b) map
	(* Like insertWith, except that the combining function also takes the
	 * key as an argument.
	 *)

    val find : ('a, 'b) map * 'a obj -> 'b option
	(* Look for an item, return NONE if the item doesn't exist *)

    val lookup : ('a, 'b) map * 'a obj -> 'b
	(* look for an item, raise the NotFound exception if it doesn't exist *)

    val inDomain : (('a, 'b) map * 'a obj) -> bool
	(* return true, if the key is in the domain of the map *)

    val remove : ('a, 'b) map * 'a obj -> ('a, 'b) map * 'b
	(* Remove an item, returning new map and value removed.
         * Raises LibBase.NotFound if not found.
	 *)

    val isEmpty : ('a, 'b) map -> bool
	(* Return true if and only if the map is empty *)

    val numItems : ('a, 'b) map ->  int
	(* Return the number of items in the map *)

    val listItems  : ('a, 'b) map -> 'b list
    val listItemsi : ('a, 'b) map -> ('a obj * 'b) list
	(* Return an ordered list of the items (and their keys) in the map. *)

    val listKeys : ('a, 'b) map -> 'a obj list
	(* return an ordered list of the keys in the map. *)

    val collate : ('b * 'b -> order) -> (('a, 'b) map * ('a, 'b) map) -> order
	(* given an ordering on the map's range, return an ordering
	 * on the map.
	 *)

    val unionWith  : ('b * 'b -> 'b) -> (('a, 'b) map * ('a, 'b) map)
	  -> ('a, 'b) map
    val unionWithi : ('a obj * 'b * 'b -> 'b) -> (('a, 'b) map * ('a, 'b) map)
	  -> ('a, 'b) map
	(* return a map whose domain is the union of the domains of the two input
	 * maps, using the supplied function to define the map on elements that
	 * are in both domains.
	 *)

    val intersectWith  : ('b * 'c -> 'd) -> (('a, 'b) map * ('a, 'c) map)
	  -> ('a, 'd) map
    val intersectWithi : ('a obj * 'b * 'c -> 'd) -> (('a, 'b) map * ('a, 'c) map)
	  -> ('a, 'd) map
	(* return a map whose domain is the intersection of the domains of the
	 * two input maps, using the supplied function to define the range.
	 *)

    val mergeWith : ('b option * 'c option -> 'd option)
	  -> (('a, 'b) map * ('a, 'c) map) -> ('a, 'd) map
    val mergeWithi : ('a obj * 'b option * 'c option -> 'd option)
	  -> (('a, 'b) map * ('a, 'c) map) -> ('a, 'd) map
	(* merge two maps using the given function to control the merge. For
	 * each key k in the union of the two maps domains, the function
	 * is applied to the image of the key under the map.  If the function
	 * returns SOME y, then (k, y) is added to the resulting map.
	 *)

    val app  : ('b -> unit) -> ('a, 'b) map -> unit
    val appi : (('a obj * 'b) -> unit) -> ('a, 'b) map -> unit
	(* Apply a function to the entries of the map. *)

    val map  : ('b -> 'c) -> ('a, 'b) map -> ('a, 'c) map
    val mapi : ('a obj * 'b -> 'c) -> ('a, 'b) map -> ('a, 'c) map
	(* Create a new map by applying a map function to the
         * name/value pairs in the map.
         *)

    val fold  : ('b * 'c -> 'c) -> 'c -> ('a, 'b) map -> 'c
    val foldi : ('a obj * 'b * 'c -> 'c) -> 'c -> ('a, 'b) map -> 'c
	(* Apply a folding function to the entries of the map *)

    val foldl  : ('b * 'c -> 'c) -> 'c -> ('a, 'b) map -> 'c
    val foldli : ('a obj * 'b * 'c -> 'c) -> 'c -> ('a, 'b) map -> 'c
    val foldr  : ('b * 'c -> 'c) -> 'c -> ('a, 'b) map -> 'c
    val foldri : ('a obj * 'b * 'c -> 'c) -> 'c -> ('a, 'b) map -> 'c
	(* these functions are DEPRECATED *)

    val filter  : ('b -> bool) -> ('a, 'b) map -> ('a, 'b) map
    val filteri : ('a obj * 'b -> bool) -> ('a, 'b) map -> ('a, 'b) map
	(* Filter out those elements of the map that do not satisfy the
	 * predicate.
	 *)

    val mapPartial  : ('b -> 'c option) -> ('a, 'b) map -> ('a, 'c) map
    val mapPartiali : ('a obj * 'b -> 'c option) -> ('a, 'b) map -> ('a, 'c) map
	(* map a partial function over the elements of a map. *)

    val exists : ('b -> bool) -> ('a, 'b) map -> bool
    val existsi : ('a obj * 'b -> bool) -> ('a, 'b) map -> bool
	(* check the elements of a map with a predicate and return true if
	 * any element satisfies the predicate. Return false otherwise.
	 *)

    val all : ('b -> bool) -> ('a, 'b) map -> bool
    val alli : ('a obj * 'b -> bool) -> ('a, 'b) map -> bool
	(* check the elements of a map with a predicate and return true if
	 * they all satisfy the predicate. Return false otherwise.
	 *)

  end
