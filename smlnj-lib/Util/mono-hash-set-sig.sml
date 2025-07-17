(* mono-hash-set-sig.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature MONO_HASH_SET =
  sig

    structure Key : HASH_KEY

    type item = Key.hash_key
    type set

    (* The empty set; argument specifies initial table size *)
    val mkEmpty : int -> set

    (* Create a singleton set *)
    val mkSingleton : item -> set

    (* create a set from a list of items *)
    val mkFromList : item list -> set

    (* returns a copy of the set *)
    val copy : set -> set

    (* Return a list of the items in the set *)
    val toList : set -> item list

    (* Insert an item. *)
    val add  : set * item -> unit
    val add' : item * set -> unit
    val addc : set -> item -> unit

    (* Insert items from list. *)
    val addList : set * item list -> unit

    (* Remove the item, if it is in the set.  Otherwise the set is unchanged. *)
    val subtract  : set * item -> unit
    val subtract' : item * set -> unit
    val subtractc : set -> item -> unit

    (* Subtract a list of items from the set. *)
    val subtractList : set * item list -> unit

    (* Remove an item.  Return false if the item was not present. *)
    val delete : set * item -> bool

    (* Return true if and only if item is an element in the set *)
    val member : set * item -> bool

    (* Return true if and only if the set is empty *)
    val isEmpty : set -> bool

    (* Return true if and only if the first set is a subset of the second *)
    val isSubset : (set * set) -> bool

    (* are the two sets disjoint? *)
    val disjoint : set * set -> bool

    (* Return the number of items in the table *)
    val numItems : set ->  int

    (* Create a new set by applying a map function to the elements
     * of the set.
     *)
    val map : (item -> item) -> set -> set

    (* Create a new set by mapping a partial function over the
     * items in the set.
     *)
    val mapPartial : (item -> item option) -> set -> set

    (* Apply a function to the entries of the set. *)
    val app : (item -> unit) -> set -> unit

    (* Apply a folding function to the entries of the set. *)
    val fold : (item * 'b -> 'b) -> 'b -> set -> 'b

    (* partition a set into two based using the given predicate.  Returns two
     * sets, where the first contains those elements for which the predicate is
     * true and the second contains those elements for which the predicate is
     * false.
     *)
    val partition : (item -> bool) -> set -> (set * set)

    (* filter a set by removing those elements for which the predicate
     * is false.
     *)
    val filter : (item -> bool) -> set -> unit

    (* check the elements of a set with a predicate and return true if
     * any element satisfies the predicate. Return false otherwise.
     * Elements are checked in key order.
     *)
    val exists : (item -> bool) -> set -> bool

    (* check the elements of a set with a predicate and return true if
     * they all satisfy the predicate. Return false otherwise.  Elements
     * are checked in key order.
     *)
    val all : (item -> bool) -> set -> bool

    (* find an element in the set for which the predicate is true *)
    val find : (item -> bool) -> set -> item option

  (* DEPRECATED FUNCTIONS *)

    (* the `listItems` function is deprecated in favor of `toList` *)
    val listItems : set -> item list

    (* The `without` function is deprecated in favor of `subtract`, whose name
     * is consistent with the other set-like APIs.
     *)
    val without : set * item -> unit

  end (* MONO_HASH_SET *)
