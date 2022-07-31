(* text.sig
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature TEXT_2004 =
  sig
    structure Char            : CHAR
    structure String          : STRING_2004
    structure Substring       : SUBSTRING
    structure CharVector      : MONO_VECTOR_2004
    structure CharArray       : MONO_ARRAY_2004
    structure CharVectorSlice : MONO_VECTOR_SLICE_2004
    structure CharArraySlice  : MONO_ARRAY_SLICE_2004
    sharing type Char.char = String.char = Substring.char
	= CharVector.elem = CharArray.elem
	= CharVectorSlice.elem = CharArraySlice.elem
    sharing type Char.string = String.string = Substring.string
	= CharVector.vector = CharArray.vector
	= CharVectorSlice.vector = CharArraySlice.vector
    sharing type CharArray.array = CharArraySlice.array
    sharing type CharArraySlice.vector_slice = CharVectorSlice.slice
  end;

(* TEXT signature with extensions from SML Basis Library
 * proposals 2015-003 and 2018-002.
 *)
signature TEXT_2018 =
  sig
    structure Char            : CHAR
    structure String          : STRING_2015
    structure Substring       : SUBSTRING
    structure CharVector      : MONO_VECTOR_2015
    structure CharArray       : MONO_ARRAY_2015
    structure CharVectorSlice : MONO_VECTOR_SLICE_2018
    structure CharArraySlice  : MONO_ARRAY_SLICE_2018
    sharing type Char.char = String.char = Substring.char
	= CharVector.elem = CharArray.elem
	= CharVectorSlice.elem = CharArraySlice.elem
    sharing type Char.string = String.string = Substring.string
	= CharVector.vector = CharArray.vector
	= CharVectorSlice.vector = CharArraySlice.vector
    sharing type CharArray.array = CharArraySlice.array
    sharing type CharArraySlice.vector_slice = CharVectorSlice.slice
  end;

signature TEXT = TEXT_2018

