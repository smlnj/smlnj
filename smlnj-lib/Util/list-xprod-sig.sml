(* list-xprod-sig.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Functions for computing with the Cartesian product of two lists.
 *)

signature LIST_XPROD =
  sig

    val app : (('a * 'b) -> unit) -> ('a list * 'b list) -> unit
	(* apply a function to the Cartesian product of two lists *)

    val map : (('a * 'b) -> 'c) -> ('a list * 'b list) -> 'c list
	(* map a function across the Cartesian product of two lists *)

    val fold : (('a * 'b * 'c) -> 'c) -> 'c -> ('a list * 'b list) -> 'c
	(* fold a function across the Cartesian product of two lists *)

  (* DEPRECATED FUNCTIONS *)

    val appX : (('a * 'b) -> unit) -> ('a list * 'b list) -> unit
    val mapX : (('a * 'b) -> 'c) -> ('a list * 'b list) -> 'c list
    val foldX : (('a * 'b * 'c) -> 'c) -> ('a list * 'b list) -> 'c -> 'c

  end; (* LIST_XPROD *)
