(* list-pair.sig
 *
 * COPYRIGHT (c) 2015 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * If lists are of unequal length, the excess elements from the
 * tail of the longer one are ignored. No exception is raised.
 *
 *)

signature LIST_PAIR_2004 =
  sig

    exception UnequalLengths
    val zip    : ('a list * 'b list) -> ('a * 'b) list
    val zipEq  : ('a list * 'b list) -> ('a * 'b) list
    val unzip  : ('a * 'b) list -> ('a list * 'b list)
    val map    : ('a * 'b -> 'c) -> ('a list * 'b list) -> 'c list
    val mapEq  : ('a * 'b -> 'c) -> ('a list * 'b list) -> 'c list
    val app    : ('a * 'b -> unit) -> ('a list * 'b list) -> unit
    val appEq  : ('a * 'b -> unit) -> ('a list * 'b list) -> unit
    val foldl  : (('a * 'b * 'c) -> 'c) -> 'c -> ('a list * 'b list) -> 'c
    val foldr  : (('a * 'b * 'c) -> 'c) -> 'c -> ('a list * 'b list) -> 'c
    val foldlEq: (('a * 'b * 'c) -> 'c) -> 'c -> ('a list * 'b list) -> 'c
    val foldrEq: (('a * 'b * 'c) -> 'c) -> 'c -> ('a list * 'b list) -> 'c
    val all    : ('a * 'b -> bool) -> ('a list * 'b list) -> bool
    val allEq  : ('a * 'b -> bool) -> ('a list * 'b list) -> bool
    val exists : ('a * 'b -> bool) -> ('a list * 'b list) -> bool

  end (* signature LIST_PAIR *)

(* includes Basis Library proposal 2015-003 *)
signature LIST_PAIR_2015 =
  sig

    include LIST_PAIR_2004

    val appi		: (int * 'a * 'b -> unit) -> 'a list * 'b list -> unit
    val appiEq		: (int * 'a * 'b -> unit) -> 'a list * 'b list -> unit
    val mapi		: (int * 'a * 'b -> 'c) -> 'a list * 'b list -> 'c list
    val mapiEq		: (int * 'a * 'b -> 'c) -> 'a list * 'b list -> 'c list
    val mapPartial	: ('a * 'b -> 'c option) -> 'a list * 'b list -> 'c list
    val mapPartialEq	: ('a * 'b -> 'c option) -> 'a list * 'b list -> 'c list
    val mapPartiali	: (int * 'a * 'b -> 'c option) -> 'a list * 'b list -> 'c list
    val mapPartialiEq	: (int * 'a * 'b -> 'c option) -> 'a list * 'b list -> 'c list
    val foldli		: (int * 'a * 'b * 'c -> 'c) -> 'c -> 'a list * 'b list -> 'c
    val foldliEq	: (int * 'a * 'b * 'c -> 'c) -> 'c -> 'a list * 'b list -> 'c
    val foldri		: (int * 'a * 'b * 'c -> 'c) -> 'c -> 'a list * 'b list -> 'c
    val foldriEq	: (int * 'a * 'b * 'c -> 'c) -> 'c -> 'a list * 'b list -> 'c

  (* added 2015-10-24 *)
    val unzipMap	: ('a -> 'b * 'c) -> 'a list -> 'b list * 'c list
    val unzipMapi	: (int * 'a -> 'b * 'c) -> 'a list -> 'b list * 'c list

  (* added 2015-10-27 *)
    val find		: ('a * 'b -> bool) -> 'a list * 'b list -> ('a * 'b) option
    val findi		: (int * 'a * 'b -> bool) -> 'a list * 'b list -> (int * 'a * 'b) option

  end

signature LIST_PAIR = LIST_PAIR_2015
