(* csv-sig.sml
 *
 * COPYRIGHT (c) 2015 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Support for reading and writing comma-separated-value files.
 *)

signature CSV =
  sig

  (* a sequence type representing a CSV row *)
    type 'a seq

  (* convert a CSV line to a sequence of its fields; returns NONE on error *)
    val fromString : string -> string seq option

  (* convert a sequence to a string *)
    val toString : string seq -> string

    val fmt : ('a -> string) -> 'a seq -> string

  end
