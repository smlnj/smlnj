(* text-prim-io.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure TextPrimIO = PrimIO (
    structure Vector = CharVector
    structure Array = CharArray
    structure VectorSlice = CharVectorSlice
    structure ArraySlice = CharArraySlice
    val someElem = #"\000"
    type pos = Position.int
    val compare = Position.compare);

