(* mono-vector.sig
 *
 * COPYRIGHT (c) 2015 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Generic interface for monomorphic vector structures.
 *)

signature MONO_VECTOR_2004 =
  sig

    type vector
    type elem

    val maxLen : int

  (* vector creation functions *)
    val fromList : elem list -> vector
    val tabulate : int * (int -> elem) -> vector

    val length   : vector -> int
    val sub      : vector * int -> elem
    val concat   : vector list -> vector

    val update : vector * int * elem -> vector

    val appi   : (int * elem -> unit) -> vector -> unit
    val app    : (elem -> unit) -> vector -> unit
    val mapi   : (int * elem -> elem) -> vector -> vector
    val map    : (elem -> elem) -> vector -> vector
    val foldli : (int * elem * 'a -> 'a) -> 'a -> vector -> 'a
    val foldri : (int * elem * 'a -> 'a) -> 'a -> vector -> 'a
    val foldl  : (elem * 'a -> 'a) -> 'a -> vector -> 'a
    val foldr  : (elem * 'a -> 'a) -> 'a -> vector -> 'a

    val findi  : (int * elem -> bool) -> vector -> (int * elem) option
    val find   : (elem -> bool) -> vector -> elem option
    val exists : (elem -> bool) -> vector -> bool
    val all    : (elem -> bool) -> vector -> bool
    val collate: (elem * elem -> order) -> vector * vector -> order

  end

(* includes Basis Library proposal 2015-003 *)
signature MONO_VECTOR_2015 =
  sig
    include MONO_VECTOR_2004

    val toList  : vector -> elem list
    val append  : vector * elem -> vector
    val prepend : elem * vector -> vector

  end

signature MONO_VECTOR = MONO_VECTOR_2015
