(* pack-word-l32.sml
 *
 * COPYRIGHT (c) 2013 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This is the non-native implementation of 32-bit little-endian packing
 * operations.
 *
 *)

local
    structure Word = WordImp
    structure LargeWord = LargeWordImp
    structure Word8 = Word8Imp
in
structure PackWord32Little : PACK_WORD =
  struct
    structure W = LargeWord
    structure W8 = Word8
    structure W8V = InlineT.Word8Vector
    structure W8A = InlineT.Word8Array

    val bytesPerElem = 4
    val isBigEndian = false

  (* convert the byte length into word32 length (n div 4), and check the index *)
    fun chkIndex (len, i) = let
	  val len = Word.toIntX(Word.>>(Word.fromInt len, 0w2))
	  in
	    if (InlineT.Int.ltu(i, len)) then () else raise Subscript
	  end

  (* scale word32 index to byte index *)
    fun scale i = Word.toIntX(Word.<<(Word.fromInt i, 0w2))

  (* make a word32 from little-endian-order bytes [b1, b2, b3, b4] *)
    fun mkWord (b1, b2, b3, b4) =
	  W.orb (W.<<(Word8.toLargeWord b4, 0w24),
	  W.orb (W.<<(Word8.toLargeWord b3, 0w16),
	  W.orb (W.<<(Word8.toLargeWord b2,  0w8),
		      Word8.toLargeWord b1)))

    fun signExt w = W.-(W.xorb(0wx80000000, w), 0wx80000000)

    fun subVec (vec, i) = let
	  val _ = chkIndex (W8V.length vec, i)
	  val k = scale i
	  in
	    mkWord (W8V.sub(vec, k), W8V.sub(vec, k+1),
	      W8V.sub(vec, k+2), W8V.sub(vec, k+3))
	  end
    fun subVecX (vec, i) = signExt (subVec (vec, i))

    fun subArr (arr, i) = let
	  val _ = chkIndex (W8A.length arr, i)
	  val k = scale i
	  in
	    mkWord (W8A.sub(arr, k), W8A.sub(arr, k+1),
	      W8A.sub(arr, k+2), W8A.sub(arr, k+3))
	  end
    fun subArrX (arr, i) = signExt (subArr (arr, i))

    fun update (arr, i, w) = let
	  val _ = chkIndex (W8A.length arr, i)
	  val k = scale i
	  in
	    W8A.update (arr, k,   W8.fromLargeWord w);
	    W8A.update (arr, k+1, W8.fromLargeWord(W.>>(w,  0w8)));
	    W8A.update (arr, k+2, W8.fromLargeWord(W.>>(w, 0w16)));
	    W8A.update (arr, k+3, W8.fromLargeWord(W.>>(w, 0w24)))
	  end

  end
end (* local *)
