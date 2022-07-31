(* integer.sig
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

signature INTEGER =
  sig

    eqtype int

    val precision : Int.int option
    val minInt : int option
    val maxInt : int option

    val toLarge   : int -> LargeInt.int
    val fromLarge : LargeInt.int -> int
    val toInt     : int -> Int.int
    val fromInt   : Int.int -> int

    val ~ : int -> int
    val + : int * int -> int
    val - : int * int -> int
    val * : int * int -> int
    val div : int * int -> int
    val mod : int * int -> int
    val quot : int * int -> int
    val rem : int * int -> int

    val min : (int * int) -> int
    val max : (int * int) -> int

    val abs : int -> int

    val sign     : int -> Int.int
    val sameSign : (int * int) -> bool

    val >  : int * int -> bool
    val >= : int * int -> bool
    val <  : int * int -> bool
    val <= : int * int -> bool
    val compare : (int * int) -> order

    val toString   : int -> string
    val fromString : string -> int option
    val scan :
	  StringCvt.radix -> (char, 'a) StringCvt.reader
	    -> (int, 'a) StringCvt.reader
    val fmt  : StringCvt.radix -> int -> string

  end;


