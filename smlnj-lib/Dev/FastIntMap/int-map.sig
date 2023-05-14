(* int-map.sig
 *
 * COPYRIGHT (c) 2023 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature INT_MAP =
  sig

    type 'a t

    val empty : 'a t
    val isEmpty : 'a t -> bool
    val singleton : word * 'a -> 'a t
    val find : 'a t * word -> 'a option
    val insert : 'a t * word * 'a -> 'a t
    val numItems : 'a t -> int
    val listItems : 'a t -> 'a list
    val unionWith : ('a * 'a -> 'a) -> 'a t * 'a t -> 'a t

   end
