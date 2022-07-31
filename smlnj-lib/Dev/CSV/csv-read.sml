(* csv-read.sml
 *
 * COPYRIGHT (c) 2022 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure CSVReadList = CSVReadFn (
  struct

    type row = string list

    val fromList = Fn.id
    val toList = Fn.id

  end)

structure CSVReadVector = CSVReadFn (
  struct

    type row = string vector

    val fromList = Vector.fromList
    val toList = Vector.toList

  end)
