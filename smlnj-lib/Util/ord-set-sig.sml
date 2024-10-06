(* ordset-sig.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *
 * Signature for a set of values with an order relation.
 *)

signature ORD_SET =
  sig

    structure Key : ORD_KEY
	(* the set elements and their comparison function *)

    type item = Key.ord_key
    type set

    (* the empty set *)
    val empty : set

    (* create a singleton set *)
    val singleton : item -> set

    (* create a set from a list of items *)
    val fromList : item list -> set

    (* return an ordered list of the items in the set.
     * Added in SML/NJ 110.80
     *)
    val toList : set -> item list

    (* add an item. *)
    val add  : set * item -> set
    val add' : (item * set) -> set

    (* add a list of items. *)
    val addList : set * item list -> set

    (* subtract an item from a set; has no effect if the item is not in the set *)
    val subtract  : set * item -> set
    val subtract' : (item * set) -> set

    (* subtract a list of items from the set. *)
    val subtractList : set * item list -> set

    (* delete an item from the set. Raise NotFound if not found. *)
    val delete : set * item -> set

    (* return true if and only if item is an element in the set *)
    val member : set * item -> bool

    (* return true if and only if the set is empty *)
    val isEmpty : set -> bool

    (* return the smallest element of the set (raises Empty if the set is empty).
     * Added in SML/NJ 110.80.
     *)
    val minItem : set -> item

    (* return the largest element of the set (raises Empty if the set is empty).
     * Added in SML/NJ 110.80.
     *)
    val maxItem : set -> item

    (* return true if and only if the two sets are equal *)
    val equal : (set * set) -> bool

    (* lexical comparison of two sets *)
    val compare : (set * set) -> order

    (* Return true if and only if the first set is a subset of the second *)
    val isSubset : (set * set) -> bool

    (* are the two sets disjoint? *)
    val disjoint : set * set -> bool

    (* return the number of items in the set *)
    val numItems : set ->  int

    (* return the union of two sets *)
    val union : set * set -> set

    (* return the intersection of two sets *)
    val intersection : set * set -> set

    (* return the difference of two sets (i.e., the second subtracted from the first) *)
    val difference : set * set -> set

    (* combine two sets using the given predicate; this function is a generalization
     * of the `union`, `intersection`, and `difference` operations.
     * Added in SML/NJ 110.99.6
     *)
    val combineWith : (item * bool * bool -> bool) -> set * set -> set

    (* create a new set by applying a map function to the elements of the set. *)
    val map : (item -> item) -> set -> set

    (* create a new set by mapping a partial function over the items in the set. *)
    val mapPartial : (item -> item option) -> set -> set

    (* apply a function to the entries of the set in increasing order *)
    val app : (item -> unit) -> set -> unit

    (* apply a folding function to the entries of the set in increasing order *)
    val foldl : (item * 'b -> 'b) -> 'b -> set -> 'b

    (* apply a folding function to the entries of the set in decreasing order *)
    val foldr : (item * 'b -> 'b) -> 'b -> set -> 'b

    (* partition a set into two based using the given predicate.  Returns two
     * sets, where the first contains those elements for which the predicate is
     * true and the second contains those elements for which the predicate is
     * false.
     *)
    val partition : (item -> bool) -> set -> (set * set)

    (* filter a set by the given predicate returning only those elements for
     * which the predicate is true.
     *)
    val filter : (item -> bool) -> set -> set

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
    val listItems : set -> item list

  end (* ORD_SET *)
