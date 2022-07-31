(* hash-cons-set-sig.sml
 *
 * COPYRIGHT (c) 2011 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Finnite sets of hash-consed objects.
 *)


signature HASH_CONS_SET =
  sig

    type 'a obj = 'a HashCons.obj

    type 'a set

    val empty : 'a set
	(* The empty set *)

    val singleton : 'a obj -> 'a set
	(* Create a singleton set *)

    val fromList : 'a obj list -> 'a set
	(* create a set from a list of items *)

    val add  : 'a set * 'a obj -> 'a set
    val add' : ('a obj * 'a set) -> 'a set
	(* Insert an 'a obj. *)

    val addList : 'a set * 'a obj list -> 'a set
	(* Insert items from list. *)

    val subtract  : 'a set * 'a obj -> 'a set
    val subtract' : ('a obj * 'a set) -> 'a set
	(* Subtract an 'a obj from a set; has no effect if the 'a obj is not in the set *)

    val subtractList : 'a set * 'a obj list -> 'a set
	(* Subtract a list of items from the set. *)

    val delete : 'a set * 'a obj -> 'a set
	(* Remove an 'a obj. Raise NotFound if not found. *)

    val member : 'a set * 'a obj -> bool
	(* Return true if and only if 'a obj is an element in the set *)

    val isEmpty : 'a set -> bool
	(* Return true if and only if the set is empty *)

    val equal : ('a set * 'a set) -> bool
	(* Return true if and only if the two sets are equal *)

    val compare : ('a set * 'a set) -> order
	(* does a lexical comparison of two sets *)

    val isSubset : ('a set * 'a set) -> bool
	(* Return true if and only if the first set is a subset of the second *)

    val disjoint : 'a set * 'a set -> bool
	(* are the two sets disjoint? *)

    val numItems : 'a set ->  int
	(* Return the number of items in the table *)

    val toList : 'a set -> 'a obj list
	(* Return an ordered list of the items in the set *)

    val listItems : 'a set -> 'a obj list
	(* Return an ordered list of the items in the set.  This function is
	 * deprecated in favor of `toList`
	 *)

    val union : 'a set * 'a set -> 'a set
        (* Union *)

    val intersection : 'a set * 'a set -> 'a set
        (* Intersection *)

    val difference : 'a set * 'a set -> 'a set
        (* Difference *)

    val map : ('a obj -> 'b obj) -> 'a set -> 'b set
	(* Create a new set by applying a function to the elements
	 * of the set.
         *)

    val mapPartial : ('a obj -> 'b obj option) -> 'a set -> 'b set
	(* Create a new set by applying a partial function to the elements
	 * of the set.
         *)

    val app : ('a obj -> unit) -> 'a set -> unit
	(* Apply a function to the entries of the set *)

    val fold : ('a obj * 'b -> 'b) -> 'b -> 'a set -> 'b
	(* Apply a folding function to the entries of the set *)

    val foldl : ('a obj * 'b -> 'b) -> 'b -> 'a set -> 'b
    val foldr : ('a obj * 'b -> 'b) -> 'b -> 'a set -> 'b
	(* these functions are DEPRECATED *)

    val partition : ('a obj -> bool) -> 'a set -> ('a set * 'a set)
	(* partition a set into two based using the given predicate.  Returns two
	 * sets, where the first contains those elements for which the predicate is
	 * true and the second contains those elements for which the predicate is
	 * false.
	 *)

    val filter : ('a obj -> bool) -> 'a set -> 'a set
	(* filter a set by the given predicate returning only those elements for
	 * which the predicate is true.
	 *)

    val all : ('a obj -> bool) -> 'a set -> bool
	(* check the elements of a set with a predicate and return true if
	 * they all satisfy the predicate. Return false otherwise.
	 *)

    val exists : ('a obj -> bool) -> 'a set -> bool
	(* check the elements of a set with a predicate and return true if
	 * any element satisfies the predicate. Return false otherwise.
	 *)

    val find : ('a obj -> bool) -> 'a set -> 'a obj option
	(* find an element in the set for which the predicate is true *)

  end
