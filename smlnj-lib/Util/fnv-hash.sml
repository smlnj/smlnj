(* fnv-hash.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * The interface to an implementation of the Fowler–Noll–Vo (FNV) hashing
 * algorithm.  We use the 64-bit FNV-1a algorithm.
 *
 * See https://en.wikipedia.org/wiki/Fowler–Noll–Vo_hash_function for details.
 *)

structure FNVHash : sig

    val offsetBasis : Word64.word

    val hashByte : Word8.word * Word64.word -> Word64.word
    val hashChar : char * Word64.word -> Word64.word

    val hashString : string -> word
    val hashSubstring : substring -> word

  end = struct

  (* values from https://en.wikipedia.org/wiki/Fowler–Noll–Vo_hash_function *)
    val offsetBasis : Word64.word = 0wxcbf29ce484222325
    val prime : Word64.word = 0wx00000100000001B3

    fun hashOne (b, h) = Word64.xorb(b, h) * prime

    fun hashByte (b, h) = hashOne (Word64.fromLargeWord(Word8.toLargeWord b), h)

    fun hashChar (c, h) = hashOne (Word64.fromInt(Char.ord c), h)

    fun hashString s = Word.fromLarge (CharVector.foldl hashChar offsetBasis s)
    fun hashSubstring ss = Word.fromLarge (Substring.foldl hashChar offsetBasis ss)

  end
