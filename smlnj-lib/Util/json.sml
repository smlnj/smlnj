(* json.sml
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 * A representation of JSON values as a SML datatype.  See the JSON library
 * for operations on these values.
 *)

structure JSON =
  struct

    datatype value
      = OBJECT of (string * value) list
      | ARRAY of value list
      | NULL
      | BOOL of bool
      | INT of IntInf.int
      | FLOAT of real
      | STRING of string	(* note that string is assumed to be UTF-8 *)

  end
