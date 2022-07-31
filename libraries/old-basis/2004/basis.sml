(* basis-2004.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This file rebinds various basis module names to their 2004 versions.
 *)

(* rebind basis signatures to their 2004 versions *)
signature ARRAY = ARRAY_2004
signature ARRAY_SLICE = ARRAY_SLICE_2004
signature LIST = LIST_2004
signature LIST_PAIR = LIST_PAIR_2004
signature MATH = MATH_2004
signature MONO_ARRAY = MONO_ARRAY_2004
signature MONO_ARRAY_SLICE = MONO_ARRAY_SLICE_2004
signature MONO_VECTOR = MONO_VECTOR_2004
signature MONO_VECTOR_SLICE = MONO_VECTOR_SLICE_2004
signature OPTION = OPTION_2004
signature REAL = REAL_2004
signature STRING = STRING_2004
signature TEXT = TEXT_2004
signature VECTOR = VECTOR_2004
signature VECTOR_SLICE = VECTOR_SLICE_2004
signature WORD = WORD_2004

(* rebind basis structures using 2004 signatures *)
structure Array : ARRAY = Array
structure ArraySlice : ARRAY_SLICE = ArraySlice
structure List : LIST = List
structure ListPair : LIST_PAIR = ListPair
structure Option : OPTION = Option
structure Math : MATH = Math
structure Real : REAL = Real
structure Real64 : REAL = Real64
structure Real64Array : MONO_ARRAY = Real64Array
structure Real64ArraySlice : MONO_ARRAY_SLICE = Real64ArraySlice
structure Real64Vector : MONO_VECTOR = Real64Vector
structure Real64VectorSlice : MONO_VECTOR_SLICE = Real64VectorSlice
structure Text : TEXT = Text
structure Vector : VECTOR = Vector
structure VectorSlice : VECTOR_SLICE = VectorSlice
structure Word : WORD = Word
structure Word8 : WORD = Word8
structure Word32 : WORD = Word32
structure Word64 : WORD = Word64
structure Word8Array : MONO_ARRAY = Word8Array
structure Word8ArraySlice : MONO_ARRAY_SLICE = Word8ArraySlice
structure Word8Vector : MONO_VECTOR = Word8Vector
structure Word8VectorSlice : MONO_VECTOR_SLICE = Word8VectorSlice

(* the Text modules are extracted from the Text structure *)
structure CharArray : MONO_ARRAY = Text.CharArray
structure CharArraySlice : MONO_ARRAY_SLICE = Text.CharArraySlice
structure CharVector : MONO_VECTOR = Text.CharVector
structure CharVectorSlice : MONO_VECTOR_SLICE = Text.CharVectorSlice
structure String : STRING = Text.String
