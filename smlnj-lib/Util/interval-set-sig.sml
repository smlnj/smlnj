(* interval-set-sig.sml
 *
 * COPYRIGHT (c) 2005 John Reppy (http://www.cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * This signature is the interface to sets over a discrete ordered domain, where the
 * sets are represented by intervals.  It is meant for representing dense sets (e.g.,
 * unicode character classes).
 *)

signature INTERVAL_SET =
  sig

    structure D : INTERVAL_DOMAIN

    type item = D.point
    type interval = (item * item)
    type set

  (* the empty set and the set of all elements *)
    val empty : set
    val universe : set

  (* a set of a single element *)
    val singleton : item -> set

    val fromList : item list -> set

  (* set that covers the given interval *)
    val interval : item * item -> set

    val isEmpty : set -> bool
    val isUniverse : set -> bool

    val member : set * item -> bool

  (* return the list of items in the set *)
    val toList : set -> item list

  (* return a list of intervals that represents the set *)
    val intervals : set -> interval list

  (* add a single element to the set *)
    val add : set * item -> set
    val add' : item * set -> set

  (* add an interval to the set *)
    val addInt : set * interval -> set
    val addInt' : interval * set -> set

  (* set operations *)
    val complement : set -> set
    val union : (set * set) -> set
    val intersect : (set * set) -> set
    val difference : (set * set) -> set

  (* iterators on elements *)
    val app    : (item -> unit) -> set -> unit
    val foldl  : (item * 'a -> 'a) -> 'a -> set -> 'a
    val foldr  : (item * 'a -> 'a) -> 'a -> set -> 'a
    val filter : (item -> bool) -> set -> set
    val all    : (item -> bool) -> set -> bool
    val exists : (item -> bool) -> set -> bool

  (* iterators on intervals *)
    val appInt    : (interval -> unit) -> set -> unit
    val foldlInt  : (interval * 'a -> 'a) -> 'a -> set -> 'a
    val foldrInt  : (interval * 'a -> 'a) -> 'a -> set -> 'a
    val filterInt : (interval -> bool) -> set -> set
    val allInt    : (interval -> bool) -> set -> bool
    val existsInt : (interval -> bool) -> set -> bool

  (* ordering on sets *)
    val compare : set * set -> order
    val isSubset : set * set -> bool

  (* DEPRECATED FUNCTIONS *)
    val listItems : set -> item list

  end
