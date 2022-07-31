(* time.sig
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

signature TIME =
  sig

    eqtype time

    exception Time

    val zeroTime : time

    val fromReal : real -> time
    val toReal   : time -> real

    val toSeconds        : time -> LargeInt.int
    val fromSeconds      : LargeInt.int -> time
    val toMilliseconds   : time -> LargeInt.int
    val fromMilliseconds : LargeInt.int -> time
    val toMicroseconds   : time -> LargeInt.int
    val fromMicroseconds : LargeInt.int -> time
    val toNanoseconds    : time -> LargeInt.int
    val fromNanoseconds  : LargeInt.int -> time

    val +  : (time * time) -> time
    val -  : (time * time) -> time

    val compare : (time * time) -> order

    val <  : (time * time) -> bool
    val <= : (time * time) -> bool
    val >  : (time * time) -> bool
    val >= : (time * time) -> bool

    val now : unit -> time

    val toString   : time -> string
    val fromString : string -> time option
    val fmt : int -> time -> string
    val scan : (char, 'a) StringCvt.reader -> (time, 'a) StringCvt.reader

  end (* TIME *)
