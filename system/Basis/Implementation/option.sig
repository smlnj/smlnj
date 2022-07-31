(* option.sig
 *
 * COPYRIGHT (c) 2015 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature OPTION_2004 =
  sig
    datatype 'a option = NONE | SOME of 'a

    exception Option

    val getOpt         : ('a option * 'a) -> 'a
    val isSome         : 'a option -> bool
    val valOf          : 'a option -> 'a
    val filter         : ('a -> bool) -> 'a -> 'a option
    val join           : 'a option option -> 'a option
    val app            : ('a -> unit) -> 'a option -> unit
    val map            : ('a -> 'b) -> 'a option -> 'b option
    val mapPartial     : ('a -> 'b option) -> 'a option -> 'b option
    val compose        : (('a -> 'b) * ('c -> 'a option)) -> 'c -> 'b option
    val composePartial : (('a -> 'b option) * ('c -> 'a option)) -> 'c -> 'b option

  end;

(* added for Basis Library proposal 2015-003 *)
signature OPTION_2015 =
  sig

    include OPTION_2004

    val isNone : 'a option -> bool
    val fold : ('a * 'b -> 'b) -> 'b -> 'a option -> 'b

  end

signature OPTION = OPTION_2015
