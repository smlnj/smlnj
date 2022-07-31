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

    val mkEmpty : int -> set
	(* The empty set; argument specifies initial table size *)

    val mkSingleton : item -> set
	(* Create a singleton set *)

    val mkFromList : item list -> set
	(* create a set from a list of items *)

    val copy : set -> set
	(* returns a copy of the set *)

    val toList : set -> item list
	(* Return a list of the items in the set *)

    val add  : set * item -> unit
    val addc : set -> item -> unit
	(* Insert an item. *)

    val addList : set * item list -> unit
	(* Insert items from list. *)

    val subtract  : set * item -> unit
    val subtractc : set -> item -> unit
	(* Remove the item, if it is in the set.  Otherwise the set is unchanged.
 	 * The `without` function is deprecated in favor of `subtract`, whose name
	 * is consistent with the other set-like APIs.
	 *)

    val subtractList : set * item list -> unit
	(* Subtract a list of items from the set. *)

    val delete : set * item -> bool
	(* Remove an item.  Return false if the item was not present. *)

    val member : set * item -> bool
	(* Return true if and only if item is an element in the set *)

    val isEmpty : set -> bool
	(* Return true if and only if the set is empty *)

    val isSubset : (set * set) -> bool
	(* Return true if and only if the first set is a subset of the second *)

    val numItems : set ->  int
	(* Return the number of items in the table *)

    val map : (item -> item) -> set -> set
	(* Create a new set by applying a map function to the elements
	 * of the set.
         *)

    val mapPartial : (item -> item option) -> set -> set
	(* Create a new set by mapping a partial function over the
	 * items in the set.
	 *)

    val app : (item -> unit) -> set -> unit
	(* Apply a function to the entries of the set. *)

    val fold : (item * 'b -> 'b) -> 'b -> set -> 'b
	(* Apply a folding function to the entries of the set. *)

    val partition : (item -> bool) -> set -> (set * set)
	(* partition a set into two based using the given predicate.  Returns two
	 * sets, where the first contains those elements for which the predicate is
	 * true and the second contains those elements for which the predicate is
	 * false.
	 *)

    val filter : (item -> bool) -> set -> unit
	(* filter a set by removing those elements for which the predicate
	 * is false.
	 *)

    val exists : (item -> bool) -> set -> bool
	(* check the elements of a set with a predicate and return true if
	 * any element satisfies the predicate. Return false otherwise.
	 * Elements are checked in key order.
	 *)

    val all : (item -> bool) -> set -> bool
	(* check the elements of a set with a predicate and return true if
	 * they all satisfy the predicate. Return false otherwise.  Elements
	 * are checked in key order.
	 *)

    val find : (item -> bool) -> set -> item option
	(* find an element in the set for which the predicate is true *)

  (* DEPRECATED FUNCTIONS *)

    val listItems : set -> item list
    val without : set * item -> unit

  end (* MONO_HASH_SET *)
