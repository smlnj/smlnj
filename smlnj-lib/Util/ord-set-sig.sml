(* ordset-sig.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * Signature for a set of values with an order relation.
 *)

signature ORD_SET =
  sig

    structure Key : ORD_KEY
	(* the set elements and their comparison function *)

    type item = Key.ord_key
    type set

    val empty : set
	(* The empty set *)

    val singleton : item -> set
	(* Create a singleton set *)

    val fromList : item list -> set
	(* create a set from a list of items *)

    val toList : set -> item list
	(* Return an ordered list of the items in the set.
         * Added in SML/NJ 110.80.
         *)

    val add  : set * item -> set
    val add' : (item * set) -> set
	(* Add an item. *)

    val addList : set * item list -> set
	(* Add a list of items. *)

    val subtract  : set * item -> set
    val subtract' : (item * set) -> set
	(* Subtract an item from a set; has no effect if the item is not in the set *)

    val subtractList : set * item list -> set
	(* Subtract a list of items from the set. *)

    val delete : set * item -> set
	(* Remove an item. Raise NotFound if not found. *)

    val member : set * item -> bool
	(* Return true if and only if item is an element in the set *)

    val isEmpty : set -> bool
	(* Return true if and only if the set is empty *)

    val minItem : set -> item
	(* return the smallest element of the set (raises Empty if the set is empty).
         * Added in SML/NJ 110.80.
         *)

    val maxItem : set -> item
	(* return the largest element of the set (raises Empty if the set is empty).
         * Added in SML/NJ 110.80.
         *)

    val equal : (set * set) -> bool
	(* Return true if and only if the two sets are equal *)

    val compare : (set * set) -> order
	(* does a lexical comparison of two sets *)

    val isSubset : (set * set) -> bool
	(* Return true if and only if the first set is a subset of the second *)

    val disjoint : set * set -> bool
	(* are the two sets disjoint? *)

    val numItems : set ->  int
	(* Return the number of items in the table *)

    val union : set * set -> set
        (* Union *)

    val intersection : set * set -> set
        (* Intersection *)

    val difference : set * set -> set
        (* Difference *)

    val map : (item -> item) -> set -> set
	(* Create a new set by applying a map function to the elements
	 * of the set.
         *)

    val mapPartial : (item -> item option) -> set -> set
	(* Create a new set by mapping a partial function over the
	 * items in the set.
	 *)

    val app : (item -> unit) -> set -> unit
	(* Apply a function to the entries of the set
         * in increasing order
         *)

    val foldl : (item * 'b -> 'b) -> 'b -> set -> 'b
	(* Apply a folding function to the entries of the set
         * in increasing order
         *)

    val foldr : (item * 'b -> 'b) -> 'b -> set -> 'b
	(* Apply a folding function to the entries of the set
         * in decreasing order
         *)

    val partition : (item -> bool) -> set -> (set * set)
	(* partition a set into two based using the given predicate.  Returns two
	 * sets, where the first contains those elements for which the predicate is
	 * true and the second contains those elements for which the predicate is
	 * false.
	 *)

    val filter : (item -> bool) -> set -> set
	(* filter a set by the given predicate returning only those elements for
	 * which the predicate is true.
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

  end (* ORD_SET *)
