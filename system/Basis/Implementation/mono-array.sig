(* mono-array.sig
 *
 * COPYRIGHT (c) 2015 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Generic interface for monomorphic array structures.
 *)

signature MONO_ARRAY_2004 =
  sig

    eqtype array
    type elem
    type vector

    val maxLen : int

  (* array creation functions *)
    val array    : int * elem -> array
    val fromList : elem list -> array
    val tabulate : int * (int -> elem) -> array

    val length   : array -> int
    val sub      : array * int -> elem
    val update   : array * int * elem -> unit

    val vector   : array -> vector
    val copy     : { src : array, dst : array, di : int } -> unit
    val copyVec  : { src : vector, dst : array, di : int } -> unit


    val appi   : (int * elem -> unit) -> array -> unit
    val app    : (elem -> unit) -> array -> unit
    val modifyi: (int * elem -> elem) -> array -> unit
    val modify : (elem -> elem) -> array -> unit

    val foldli : (int * elem * 'a -> 'a) -> 'a -> array -> 'a
    val foldri : (int * elem * 'a -> 'a) -> 'a -> array -> 'a
    val foldl  : (elem * 'a -> 'a) -> 'a -> array -> 'a
    val foldr  : (elem * 'a -> 'a) -> 'a -> array -> 'a

    val findi   : (int * elem -> bool) -> array -> (int * elem) option
    val find    : (elem -> bool) -> array -> elem option
    val exists  : (elem -> bool) -> array -> bool
    val all     : (elem -> bool) -> array -> bool
    val collate : (elem * elem -> order) -> array * array -> order

  end

(* includes Basis Library proposal 2015-003 *)
signature MONO_ARRAY_2015 =
  sig
    include MONO_ARRAY_2004

    val toList     : array -> elem list
    val fromVector : vector -> array
    val toVector   : array -> vector
  end

signature MONO_ARRAY = MONO_ARRAY_2015
