(* array-slice.sig
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature ARRAY_SLICE_2004 = sig

    type 'a slice
    val length : 'a slice -> int
    val sub    : 'a slice * int -> 'a
    val update : 'a slice * int * 'a -> unit

    val full     : 'a Array.array -> 'a slice
    val slice    : 'a Array.array * int * int option -> 'a slice
    val subslice : 'a slice * int * int option -> 'a slice

    val base     : 'a slice -> 'a Array.array * int * int
    val vector   : 'a slice -> 'a Vector.vector

    val copy     : { src : 'a slice, dst : 'a Array.array,
		     di : int }
		   -> unit
    val copyVec  : { src : 'a VectorSlice.slice, dst : 'a Array.array,
		     di : int }
		   -> unit

    val isEmpty  : 'a slice -> bool
    val getItem  : 'a slice -> ('a * 'a slice) option

    val appi     : (int * 'a -> unit) -> 'a slice -> unit
    val app      : ('a -> unit) -> 'a slice -> unit
    val modifyi  : (int * 'a -> 'a) -> 'a slice -> unit
    val modify   : ('a -> 'a) -> 'a slice -> unit
    val foldli   : (int * 'a * 'b -> 'b) -> 'b -> 'a slice -> 'b
    val foldri   : (int * 'a * 'b -> 'b) -> 'b -> 'a slice -> 'b
    val foldl    : ('a * 'b -> 'b) -> 'b -> 'a slice -> 'b
    val foldr    : ('a * 'b -> 'b) -> 'b -> 'a slice -> 'b

    val findi    : (int * 'a -> bool) -> 'a slice -> (int * 'a) option
    val find     : ('a -> bool) -> 'a slice -> 'a option
    val exists   : ('a -> bool) -> 'a slice -> bool
    val all      : ('a -> bool) -> 'a slice -> bool
    val collate  : ('a * 'a -> order) -> 'a slice * 'a slice -> order
end

(* includes Basis Library proposal 2018-002 *)
signature ARRAY_SLICE_2018 =
  sig
    include ARRAY_SLICE_2004

    val triml : int -> 'a slice -> 'a slice
    val trimr : int -> 'a slice -> 'a slice
    val splitAt : 'a slice * int -> 'a slice * 'a slice
    val getVec : 'a slice * int -> ('a vector * 'a slice) option

  end

signature ARRAY_SLICE = ARRAY_SLICE_2018
