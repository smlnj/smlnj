(* ord-map-sig.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * COPYRIGHT (c) 1996 by AT&T Research.  See COPYRIGHT file for details.
 *
 * Abstract signature of an applicative-style finite maps (dictionaries)
 * structure over ordered monomorphic keys.
 *)

signature ORD_MAP =
  sig

    structure Key : ORD_KEY
	(* the map's domain and its comparison function *)

    type 'a map

    val empty : 'a map
	(* The empty map *)

    val isEmpty : 'a map -> bool
	(* Return true if and only if the map is empty *)

    val singleton : (Key.ord_key * 'a) -> 'a map
	(* return the specified singleton map *)

    val insert  : 'a map * Key.ord_key * 'a -> 'a map
    val insert' : ((Key.ord_key * 'a) * 'a map) -> 'a map
	(* Insert an item. *)

    val insertWith  : ('a * 'a -> 'a) -> 'a map * Key.ord_key * 'a -> 'a map
	(* Insert an item with a combining function to resolve collisions.
	 * The first argument to the combining function is the existing value,
	 * and the second argument is the value being inserted into the map.
	 *)
    val insertWithi : (Key.ord_key * 'a * 'a -> 'a) -> 'a map * Key.ord_key * 'a -> 'a map
	(* Like insertWith, except that the combining function also takes the
	 * key as an argument.
	 *)

    val find : 'a map * Key.ord_key -> 'a option
	(* Look for an item, return NONE if the item doesn't exist *)

    val lookup : 'a map * Key.ord_key -> 'a
	(* look for an item, raise the NotFound exception if it doesn't exist *)

    val inDomain : ('a map * Key.ord_key) -> bool
	(* return true, if the key is in the domain of the map *)

    val findAndRemove : 'a map * Key.ord_key -> ('a map * 'a) option
        (* If an item with the specified key exists in the map, then it
         * is removed and the residual map and the item are returned.
         * Otherwise, `NONE` is returned.
         *)

    val remove : 'a map * Key.ord_key -> 'a map * 'a
	(* Remove an item, returning new map and value removed.
         * Raises LibBase.NotFound if not found.
	 *)

    val first : 'a map -> 'a option
    val firsti : 'a map -> (Key.ord_key * 'a) option
	(* return the first item in the map (or NONE if it is empty) *)

    val numItems : 'a map ->  int
	(* Return the number of items in the map *)

    val listItems  : 'a map -> 'a list
    val listItemsi : 'a map -> (Key.ord_key * 'a) list
	(* Return an ordered list of the items (and their keys) in the map. *)

    val listKeys : 'a map -> Key.ord_key list
	(* return an ordered list of the keys in the map. *)

    val unionWith  : ('a * 'a -> 'a) -> ('a map * 'a map) -> 'a map
    val unionWithi : (Key.ord_key * 'a * 'a -> 'a) -> ('a map * 'a map) -> 'a map
	(* return a map whose domain is the union of the domains of the two input
	 * maps, using the supplied function to define the map on elements that
	 * are in both domains.
	 *)

    val intersectWith  : ('a * 'b -> 'c) -> ('a map * 'b map) -> 'c map
    val intersectWithi : (Key.ord_key * 'a * 'b -> 'c) -> ('a map * 'b map) -> 'c map
	(* return a map whose domain is the intersection of the domains of the
	 * two input maps, using the supplied function to define the range.
	 *)

    val mergeWith : ('a option * 'b option -> 'c option)
	  -> ('a map * 'b map) -> 'c map
    val mergeWithi : (Key.ord_key * 'a option * 'b option -> 'c option)
	  -> ('a map * 'b map) -> 'c map
	(* merge two maps using the given function to control the merge. For
	 * each key k in the union of the two maps domains, the function
	 * is applied to the image of the key under the map.  If the function
	 * returns SOME y, then (k, y) is added to the resulting map.
	 *)

    val equiv : ('a * 'b -> bool) -> ('a map * 'b map) -> bool
        (* `equiv rngEq (f, g)` returns true if `f` and `g`` have equal domains
         * and if for every `x` in their domain, `rngEq(f x, g x) = true`.
         *)
    val collate : ('a * 'b -> order) -> ('a map * 'b map) -> order
        (* Given two maps `f` and `g`, and a comparison function `rngCmp` on their
         * range types, return the order of the maps.
         *)
    val extends : ('a * 'b -> bool) -> ('a map * 'b map) -> bool
        (* `extends rngEx (f, g)` returns true if the domain of `g` is a
         * subset of the domain of `f` and for every `x` in the domain of `g`,
         * `rngEx(g x, f x) = true`.
         *)

    val app  : ('a -> unit) -> 'a map -> unit
    val appi : ((Key.ord_key * 'a) -> unit) -> 'a map -> unit
	(* Apply a function to the entries of the map in map order. *)

    val map  : ('a -> 'b) -> 'a map -> 'b map
    val mapi : (Key.ord_key * 'a -> 'b) -> 'a map -> 'b map
	(* Create a new map by applying a map function to the
         * name/value pairs in the map.
         *)

    val foldl  : ('a * 'b -> 'b) -> 'b -> 'a map -> 'b
    val foldli : (Key.ord_key * 'a * 'b -> 'b) -> 'b -> 'a map -> 'b
	(* Apply a folding function to the entries of the map
         * in increasing map order.
         *)

    val foldr  : ('a * 'b -> 'b) -> 'b -> 'a map -> 'b
    val foldri : (Key.ord_key * 'a * 'b -> 'b) -> 'b -> 'a map -> 'b
	(* Apply a folding function to the entries of the map
         * in decreasing map order.
         *)

    val filter  : ('a -> bool) -> 'a map -> 'a map
    val filteri : (Key.ord_key * 'a -> bool) -> 'a map -> 'a map
	(* Filter out those elements of the map that do not satisfy the
	 * predicate.  The filtering is done in increasing map order.
	 *)

    val mapPartial  : ('a -> 'b option) -> 'a map -> 'b map
    val mapPartiali : (Key.ord_key * 'a -> 'b option) -> 'a map -> 'b map
	(* map a partial function over the elements of a map in increasing
	 * map order.
	 *)

    val exists : ('a -> bool) -> 'a map -> bool
    val existsi : (Key.ord_key * 'a -> bool) -> 'a map -> bool
	(* check the elements of a map with a predicate and return true if
	 * any element satisfies the predicate. Return false otherwise.
	 * Elements are checked in key order.
	 *)

    val all : ('a -> bool) -> 'a map -> bool
    val alli : (Key.ord_key * 'a -> bool) -> 'a map -> bool
	(* check the elements of a map with a predicate and return true if
	 * they all satisfy the predicate. Return false otherwise.  Elements
	 * are checked in key order.
	 *)

  end (* ORD_MAP *)
