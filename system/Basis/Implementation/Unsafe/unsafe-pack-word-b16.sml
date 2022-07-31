(* unsafe-pack-word-b16.sml
 *
 * COPYRIGHT (c) 2013 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This is the unsafe non-native implementation of 16-bit big-endian packing
 * operations.
 *)

local
  structure Word = WordImp
  structure LargeWord = LargeWordImp
  structure Word8 = Word8Imp
in
structure UnsafePackWord16Big : PACK_WORD =
  struct
    structure W = LargeWord
    structure W8 = Word8
    structure W8V = InlineT.Word8Vector
    structure W8A = InlineT.Word8Array

    val bytesPerElem = 2
    val isBigEndian = true

    fun mkWord (b1, b2) =
	  W.orb(W.<<(Word8.toLargeWord b1, 0w8), Word8.toLargeWord b2)

    fun signExt w = W.-(W.xorb(0wx8000, w), 0wx8000)

    fun subVec (vec, i) = let
	  val k = i+i
	  in
	    mkWord (W8V.sub(vec, k), W8V.sub(vec, k+1))
	  end
    fun subVecX(vec, i) = signExt (subVec (vec, i))

    fun subArr (arr, i) = let
	  val k = i+i
	  in
	    mkWord (W8A.sub(arr, k), W8A.sub(arr, k+1))
	  end
    fun subArrX(arr, i) = signExt (subArr (arr, i))

    fun update (arr, i, w) = let
	  val k = i+i
	  in
	    W8A.update (arr, k, W8.fromLargeWord(W.>>(w, 0w8)));
	    W8A.update (arr, k+1, W8.fromLargeWord w)
	  end

  end
end (* local *)
