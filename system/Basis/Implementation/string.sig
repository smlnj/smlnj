(* string.sig
 *
 * COPYRIGHT (c) 2015 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature STRING_2004 =
  sig
    eqtype char
    eqtype string

    val maxSize : int
    val size      : string -> int

    val sub       : string * int -> char

    val str       : char -> string
    val extract   : string * int * int option -> string
    val substring : string * int * int -> string

    val ^         : string * string -> string
    val concat    : string list -> string
    val concatWith : string -> string list -> string

    val implode   : char list -> string
    val explode   : string -> char list
    val map       : (char -> char) -> string -> string
    val translate : (char -> string) -> string -> string
    val tokens    : (char -> bool) -> string -> string list
    val fields    : (char -> bool) -> string -> string list

    val isPrefix    : string -> string -> bool
    val isSubstring : string -> string -> bool
    val isSuffix    : string -> string -> bool

    val compare  : string * string -> order
    val collate  : (char * char -> order) -> string * string -> order

    val <  : (string * string) -> bool
    val <= : (string * string) -> bool
    val >  : (string * string) -> bool
    val >= : (string * string) -> bool

    val toString    : string -> String.string
    val scan        : (char, 'a) StringCvt.reader -> (string, 'a) StringCvt.reader
    val fromString  : String.string -> string option
    val toCString   : string -> String.string
    val fromCString : String.string -> string option

  end

(* includes Basis Library proposal 2015-003 *)
signature STRING_2015 =
  sig
    include STRING_2004

    val rev           : string -> string
    val implodeRev    : char list -> string

    val concatWithMap : string -> ('a -> string) -> 'a list -> string

  end

signature STRING = STRING_2015

