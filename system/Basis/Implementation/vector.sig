(* vector.sig
 *
 * COPYRIGHT (c) 2015 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature VECTOR_2004 = 
  sig
    eqtype 'a vector

    val maxLen   : int

    val fromList : 'a list -> 'a vector
    val tabulate : int * (int -> 'a) -> 'a vector

    val length   : 'a vector -> int
    val sub      : 'a vector * int -> 'a 

    val update   : 'a vector * int * 'a -> 'a vector
    val concat   : 'a vector list -> 'a vector

    val appi   : (int * 'a -> unit) -> 'a vector -> unit
    val app    : ('a -> unit) -> 'a vector -> unit
    val mapi   : (int * 'a -> 'b) -> 'a vector -> 'b vector
    val map    : ('a -> 'b) -> 'a vector -> 'b vector
    val foldli : (int * 'a * 'b -> 'b) -> 'b -> 'a vector -> 'b
    val foldri : (int * 'a * 'b -> 'b) -> 'b -> 'a vector -> 'b
    val foldl  : (('a * 'b) -> 'b) -> 'b -> 'a vector -> 'b
    val foldr  : (('a * 'b) -> 'b) -> 'b -> 'a vector -> 'b

    val findi   : (int * 'a -> bool) -> 'a vector -> (int * 'a) option
    val find    : ('a -> bool) -> 'a vector -> 'a option
    val exists  : ('a -> bool) -> 'a vector -> bool
    val all     : ('a -> bool) -> 'a vector -> bool
    val collate : ('a * 'a -> order) -> 'a vector * 'a vector -> order
  end

(* includes Basis Library proposal 2015-003 *)
signature VECTOR_2015 =
  sig
    include VECTOR_2004

    val toList  : 'a vector -> 'a list
    val append  : 'a vector * 'a -> 'a vector
    val prepend : 'a * 'a vector -> 'a vector

  end

signature VECTOR = VECTOR_2015
