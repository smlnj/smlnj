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

    type ('ty, 'dst) encoder = ('dst * Word8.word -> 'dst) -> ('dst * 'ty) -> 'dst

    (* signed encodings *)
    val encodeInt       : (Int.int, 'dst) encoder
    val encodeNativeInt : (NativeInt.int, 'dst) encoder
    val encodeInt64     : (Int64.int, 'dst) encoder
    val encodeIntInf    : (IntInf.int, 'dst) encoder

    (* unsigned encodings *)
    val encodeWord       : (Word.word, 'dst) encoder
    val encodeNativeWord : (NativeWord.word, 'dst) encoder
    val encodeWord64     : (Word64.word, 'dst) encoder
    val encodeUIntInf    : (IntInf.int, 'dst) encoder

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

    (* encode as byte vector *)
    val intToBytes       : Int.int -> Word8Vector.vector
    val nativeIntToBytes : NativeInt.int -> Word8Vector.vector
    val int64ToBytes     : Int64.int -> Word8Vector.vector
    val intInfToBytes    : IntInf.int -> Word8Vector.vector

    (* encode as byte vector *)
    val wordToBytes       : Word.word -> Word8Vector.vector
    val nativeWordToBytes : NativeWord.word -> Word8Vector.vector
    val word64ToBytes     : Word64.word -> Word8Vector.vector
    val uIntInfToBytes    : IntInf.int -> Word8Vector.vector

  end
