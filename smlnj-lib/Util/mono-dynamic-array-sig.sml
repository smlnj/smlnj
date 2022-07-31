(* mono-dynamic-array-sig.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Signature for monomorphic unbounded arrays.
 *
 * TODO: add the missing operations that the DynamicArray structure
 * provides.
 *)

signature MONO_DYNAMIC_ARRAY =
  sig
    type elem
    type array

    val array : (int * elem) -> array
      (* array (sz, e) creates an unbounded array all of whose elements
       * are initialized to e.  sz (>= 0) is used as a
       * hint of the potential range of indices.  Raises Size if a
       * negative hint is given.
       *)

    val subArray : array * int * int -> array
      (* subArray (a,lo,hi) creates a new array with the same default
       * as a, and whose values in the range [0,hi-lo] are equal to
       * the values in b in the range [lo, hi].
       * Raises Size if lo > hi
       *)

    val fromList : elem list * elem -> array
      (* arrayoflist (l, v) creates an array using the list of values l
       * plus the default value v.
       *)

    val toList : array -> elem list
      (* return the array's contents as a list *)

    val tabulate: int * (int -> elem) * elem -> array
      (* tabulate (sz,fill,dflt) acts like Array.tabulate, plus
       * stores default value dflt.  Raises Size if sz < 0.
       *)

    val default : array -> elem
      (* default returns array's default value *)

    val sub : array * int -> elem
      (* sub (a,idx) returns value of the array at index idx.
       * If that value has not been set by update, it returns the default value.
       * Raises Subscript if idx < 0
       *)

    val update : array * int * elem -> unit
      (* update (a,idx,v) sets the value at index idx of the array to v.
       * Raises Subscript if idx < 0
       *)

    val bound : array -> int
      (* bound returns an upper bound on the index of values that have been
       * changed.
       *)

    val truncate : array * int -> unit
      (* truncate (a,sz) makes every entry with index > sz the default value *)

(** what about iterators??? **)
(*
    val vector : array -> 'a vector
    val copy : {di:int, dst:array, src:array} -> unit
    val copyVec : {di:int, dst:array, src:'a vector} -> unit
    val appi : (int * 'a -> unit) -> array -> unit
    val app : ('a -> unit) -> array -> unit
    val modifyi : (int * 'a -> 'a) -> array -> unit
    val modify : ('a -> 'a) -> array -> unit
    val foldli : (int * 'a * 'b -> 'b) -> 'b -> array -> 'b
    val foldri : (int * 'a * 'b -> 'b) -> 'b -> array -> 'b
    val foldl : ('a * 'b -> 'b) -> 'b -> array -> 'b
    val foldr : ('a * 'b -> 'b) -> 'b -> array -> 'b
    val findi : (int * 'a -> bool) -> array -> (int * 'a) option
    val find : ('a -> bool) -> array -> 'a option
    val exists : ('a -> bool) -> array -> bool
    val all : ('a -> bool) -> array -> bool
    val collate : ('a * 'a -> order) -> array * array -> order
*)

  end (* MONO_DYNAMIC_ARRAY *)

