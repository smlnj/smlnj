(* int-set.sig
 *
 * COPYRIGHT (c) 2023 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature INT_SET =
  sig

    structure Key : ORD_KEY where type ord_key = word

    type item = Key.ord_key
    type set

    (* The empty set *)
    val empty : set

    (* Create a singleton set *)
    val singleton : item -> set

    (* create a set from a list of items *)
    val fromList : item list -> set

    (* Return an ordered list of the items in the set. *)
    val toList : set -> item list

    (* Add an item. *)
    val add  : set * item -> set
    val add' : (item * set) -> set

    (* Add a list of items. *)
    val addList : set * item list -> set

    (* Subtract an item from a set; has no effect if the item is not in the set *)
    val subtract  : set * item -> set
    val subtract' : (item * set) -> set

    (* Subtract a list of items from the set. *)
    val subtractList : set * item list -> set

    (* Remove an item. Raise NotFound if not found. *)
    val delete : set * item -> set

    (* Return true if and only if item is an element in the set *)
    val member : set * item -> bool

    (* Return true if and only if the set is empty *)
    val isEmpty : set -> bool

    (* Return true if and only if the two sets are equal *)
    val equal : (set * set) -> bool

    (* lexical comparison of two sets *)
    val compare : (set * set) -> order

    (* Return true if and only if the first set is a subset of the second *)
    val isSubset : (set * set) -> bool

    (* are the two sets disjoint? *)
    val disjoint : set * set -> bool

    (* Return the number of items in the set *)
    val numItems : set ->  int

    (* Union *)
    val union : set * set -> set

    (* Intersection *)
    val intersection : set * set -> set

    (* Difference *)
    val difference : set * set -> set

    (* Create a new set by applying a map function to the elements of the set. *)
    val map : (item -> item) -> set -> set

    (* Create a new set by mapping a partial function over the
     * items in the set.
     *)
    val mapPartial : (item -> item option) -> set -> set

    (* Apply a function to the entries of the set in increasing order *)
    val app : (item -> unit) -> set -> unit

    (* Apply a folding function to the entries of the set in increasing order *)
    val foldl : (item * 'b -> 'b) -> 'b -> set -> 'b

    (* Apply a folding function to the entries of the set in decreasing order *)
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

    (* for debugging *)
    val dump : (item -> string) -> TextIO.outstream * set -> unit

   end
