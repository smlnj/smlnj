(* mono-hash-map-sig.sml
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 * Imperative finite maps.
 *)

signature MONO_HASH_MAP =
  sig

    (* the map's domain and its hash and equality functions *)
    structure Key : HASH_KEY

    (* the map's domain *)
    type key = Key.hash_key
    type 'a map

    (* The empty set; argument specifies initial table size *)
    val mkEmpty : int -> 'a map

    (* is the map empty? *)
    val isEmpty : 'a map -> bool

    (* remove all of the `(key, item)` pairs from the map *)
    val clear : 'a map -> unit

    (* resize the table based on the number of items *)
    val resize : 'a map -> unit

    (* Create a singleton map *)
    val mkSingleton : key * 'a -> 'a map

    (* returns a copy of the map *)
    val copy : 'a map -> 'a map

    (* Insert an item. *)
    val insert  : 'a map * key * 'a -> unit
    val insert' : ((key * 'a) * 'a map) -> unit
    val insertc : 'a map -> (key * 'a) -> unit

    (* Insert an item with a combining function to resolve collisions.
     * The first argument to the combining function is the existing value,
     * and the second argument is the value being inserted into the map.
     *)
    val insertWith  : ('a * 'a -> 'a) -> 'a map * key * 'a -> unit
    (* Like insertWith, except that the combining function also takes the
     * key as an argument.
     *)
    val insertWithi : (key * 'a * 'a -> 'a) -> 'a map * key * 'a -> unit

    (* Look for an item, return NONE if the item doesn't exist *)
    val find : 'a map * key -> 'a option

    (* look for an item, raise the NotFound exception if it doesn't exist *)
    val lookup : 'a map * key -> 'a

    (* return true, if the key is in the domain of the map *)
    val inDomain : ('a map * key) -> bool

    (* If an item with the specified key exists in the map, then it
     * is removed and the residual map and the item are returned.
     * Otherwise, `NONE` is returned.
     *)
    val findAndRemove : 'a map * key -> 'a option

    (* Remove a key, returning the value associated with the removed key.
     * Raises LibBase.NotFound if not found.
     *)
    val remove : 'a map * key -> 'a

    (* Return the number of items in the map *)
    val numItems : 'a map ->  int

    (* Return a list of the items (and their keys) in the map. *)
    val listItems  : 'a map -> 'a list
    val listItemsi : 'a map -> (key * 'a) list

    (* return a list of the keys in the map. *)
    val listKeys : 'a map -> key list

    (* return a new map whose domain is the union of the domains of the two
     * input maps, using the supplied function to define the map on elements
     * that are in both domains.
     *)
    val unionWith  : ('a * 'a -> 'a) -> ('a map * 'a map) -> 'a map
    val unionWithi : (key * 'a * 'a -> 'a) -> ('a map * 'a map) -> 'a map

    (* return a new map whose domain is the intersection of the domains of
     * the two input maps, using the supplied function to define the range.
     *)
    val intersectWith  : ('a * 'b -> 'c) -> ('a map * 'b map) -> 'c map
    val intersectWithi : (key * 'a * 'b -> 'c) -> ('a map * 'b map) -> 'c map

    (* merge two maps using the given function to control the merge. For
     * each key k in the union of the two maps domains, the function
     * is applied to the image of the key under the map.  If the function
     * returns SOME y, then (k, y) is added to the resulting map.
     *)
    val mergeWith : ('a option * 'b option -> 'c option)
	  -> ('a map * 'b map) -> 'c map
    val mergeWithi : (key * 'a option * 'b option -> 'c option)
	  -> ('a map * 'b map) -> 'c map

    (* `equiv rngEq (f, g)` returns true if `f` and `g`` have equal domains
     * and if for every `x` in their domain, `rngEq(f x, g x) = true`.
     *)
    val equiv : ('a * 'b -> bool) -> ('a map * 'b map) -> bool

    (* `extends rngEx (f, g)` returns true if the domain of `g` is a
     * subset of the domain of `f` and for every `x` in the domain of `g`,
     * `rngEx(g x, f x) = true`.
     *)
    val extends : ('a * 'b -> bool) -> ('a map * 'b map) -> bool

    (* Apply a function to the entries of the map *)
    val app  : ('a -> unit) -> 'a map -> unit
    val appi : ((key * 'a) -> unit) -> 'a map -> unit

    (* Create a new map by applying a map function to the
     * name/value pairs in the map.
     *)
    val map  : ('a -> 'b) -> 'a map -> 'b map
    val mapi : (key * 'a -> 'b) -> 'a map -> 'b map

    (* Apply a folding function to the entries of the map *)
    val fold  : ('a * 'b -> 'b) -> 'b -> 'a map -> 'b
    val foldi : (key * 'a * 'b -> 'b) -> 'b -> 'a map -> 'b

    (* Filter out those elements of the map that do not satisfy the
     * predicate.
     *)
    val filter  : ('a -> bool) -> 'a map -> 'a map
    val filteri : (key * 'a -> bool) -> 'a map -> 'a map

    (* map a partial function over the elements of a map *)
    val mapPartial  : ('a -> 'b option) -> 'a map -> 'b map
    val mapPartiali : (key * 'a -> 'b option) -> 'a map -> 'b map

    (* check the elements of a map with a predicate and return true if
     * any element satisfies the predicate. Return false otherwise.
     *)
    val exists : ('a -> bool) -> 'a map -> bool
    val existsi : (key * 'a -> bool) -> 'a map -> bool

    (* check the elements of a map with a predicate and return true if
     * they all satisfy the predicate. Return false otherwise.
     *)
    val all : ('a -> bool) -> 'a map -> bool
    val alli : (key * 'a -> bool) -> 'a map -> bool

  end
