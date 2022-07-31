(* mono-array-slice.sig
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature MONO_ARRAY_SLICE_2004 = sig

    type elem
    type array
    type slice
    type vector
    type vector_slice

    val length : slice -> int
    val sub    : slice * int -> elem
    val update : slice * int * elem -> unit

    val full     : array -> slice
    val slice    : array * int * int option -> slice
    val subslice : slice * int * int option -> slice

    val base   : slice -> array * int * int
    val vector : slice -> vector

    val copy    : { src : slice, dst : array, di : int } -> unit
    val copyVec : { src: vector_slice, dst : array, di : int } -> unit

    val isEmpty : slice -> bool
    val getItem : slice -> (elem * slice) option

    val appi    : (int * elem -> unit) -> slice -> unit
    val app     : (elem -> unit) -> slice -> unit
    val modifyi : (int * elem -> elem) -> slice -> unit
    val modify  : (elem -> elem) -> slice -> unit

    val foldli : (int * elem * 'a -> 'a) -> 'a -> slice -> 'a
    val foldri : (int * elem * 'a -> 'a) -> 'a -> slice -> 'a
    val foldl  : (elem * 'a -> 'a) -> 'a -> slice -> 'a
    val foldr  : (elem * 'a -> 'a) -> 'a -> slice -> 'a

    val findi  : (int * elem -> bool) -> slice -> (int * elem) option
    val find   : (elem -> bool) -> slice -> elem option
    val exists : (elem -> bool) -> slice -> bool
    val all    : (elem -> bool) -> slice -> bool
    val collate: (elem * elem -> order) -> slice * slice -> order

end

(* includes Basis Library proposal 2018-002 *)
signature MONO_ARRAY_SLICE_2018 =
  sig
    include MONO_ARRAY_SLICE_2004

    val triml : int -> slice -> slice
    val trimr : int -> slice -> slice
    val splitAt : slice * int -> slice * slice
    val getVec : slice * int -> (vector * slice) option

  end

signature MONO_ARRAY_SLICE = MONO_ARRAY_SLICE_2018
