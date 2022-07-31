(* bool.sig
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

signature BOOL =
  sig

    (*    datatype bool = true | false *)
    datatype bool = datatype PrimTypes.bool
    val not : bool -> bool

    val toString   : bool -> string
    val fromString : string -> bool option
    val scan : (char, 'a) StringCvt.reader -> (bool, 'a) StringCvt.reader

  end


