(* leb128-sig.sml
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

signature LEB128 =
  sig

    type ('ty, 'src) decoder =
          (Word8.word, 'src) StringCvt.reader -> 'src -> ('ty * 'src) option

    (* signed decodings *)
    val decodeInt       : (Int.int, 'a) decoder
    val decodeNativeInt : (NativeInt.int, 'a) decoder
    val decodeInt64     : (Int64.int, 'a) decoder
    val decodeIntInf    : (IntInf.int, 'a) decoder

    (* unsigned decodings *)
    val decodeWord       : (Word.word, 'src) decoder
    val decodeNativeWord : (NativeWord.word, 'src) decoder
    val decodeWord64     : (Word64.word, 'src) decoder
    val decodeUIntInf    : (IntInf.int, 'src) decoder

    type 'ty encoder = 'ty -> Word8Vector.vector

    (* signed encodings *)
    val encodeInt       : Int.int encoder
    val encodeNativeInt : Int64.int encoder
    val encodeInt64     : Int64.int encoder
    val encodeIntInf    : IntInf.int encoder

    (* unsigned encodings *)
    val encodeWord       : Word.word encoder
    val encodeNativeWord : Word64.word encoder
    val encodeWord64     : Word64.word encoder
    val encodeUIntInf    : IntInf.int encoder

    (* return the size of the signed encodings *)
    val sizeOfInt       : Int.int -> int
    val sizeOfNativeInt : NativeInt.int -> int
    val sizeOfInt64     : Int64.int -> int
    val sizeOfIntInf    : IntInf.int -> int

    (* return the size of the unsigned encodings *)
    val sizeOfWord       : Word.word -> int
    val sizeOfNativeWord : NativeWord.word -> int
    val sizeOfWord64     : Word64.word -> int
    val sizeOfUIntInf    : IntInf.int -> int

  end
