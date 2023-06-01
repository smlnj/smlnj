(* int-set.sig
 *
 * COPYRIGHT (c) 2023 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature INT_SET =
  sig

    type t

    val empty : t
    val isEmpty : t -> bool
    val singleton : word -> t
    val member : t * word -> bool
    val add : t * word -> t
    val numItems : t -> int
    val fromList : word list -> t
    val toList : t -> word list

    val union : t * t -> t
    val intersection : t * t -> t
    val difference : t * t -> t

    (* for debugging *)
    val dump : TextIO.outstream * 'a t -> unit

   end
