(* pack-word-l64.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Implementation of PackWord64Little for 32-bit targets.
 *)

local
  structure LargeWord = LargeWordImp
in
structure PackWord64Little : PACK_WORD =
  struct

    structure Word = InlineT.Word
    structure W8 = Word8Imp
    structure W32 = InlineT.Word32
    structure W64 = InlineT.Word64
    structure W8V = InlineT.Word8Vector
    structure W8A = InlineT.Word8Array

  (* fast add avoiding the overflow test *)
    infix ++
    fun x ++ y = InlineT.Int.fast_add(x, y)

    val bytesPerElem = 8
    val isBigEndian = false

  (* convert the byte length into word64 length (n div 8), and check the index *)
    fun chkIndex (len, i) = let
	  val len = Word.toIntX(Word.rshiftl(Word.fromInt len, 0w3))
	  in
	    if (InlineT.Int.ltu(i, len)) then () else raise Subscript
	  end

  (* scale word64 index to byte index *)
    fun scale i = Word.toIntX(Word.lshift(Word.fromInt i, 0w3))

    val w8ToW32 = W32.fromLarge o W8.toLarge
    val w32ToW8 = W8.fromLarge o W32.toLarge

  (* make a word32 from little-endian-order bytes [b1, b2, b3, b4] *)
    fun mkWord32 (b1, b2, b3, b4) =
	  W32.orb (W32.lshift(w8ToW32 b4, 0w24),
	  W32.orb (W32.lshift(w8ToW32 b3, 0w16),
	  W32.orb (W32.lshift(w8ToW32 b2,  0w8),
		          w8ToW32 b1)))

    fun subVec (vec, i) = let
	  val _ = chkIndex (W8V.length vec, i)
	  val k = scale i
	  in
	    W64.intern(
	      mkWord32 (W8V.sub(vec, k++4), W8V.sub(vec, k++5),
	        W8V.sub(vec, k++6), W8V.sub(vec, k++7)),
	      mkWord32 (W8V.sub(vec, k), W8V.sub(vec, k++1),
	        W8V.sub(vec, k++2), W8V.sub(vec, k++3)))
	  end
  (* since LargeWord is 64-bits, no sign extension is required *)
    val subVecX = subVec

    fun subArr (arr, i) = let
	  val _ = chkIndex (W8A.length arr, i)
	  val k = scale i
	  in
	    W64.intern(
	      mkWord32 (W8A.sub(arr, k++4), W8A.sub(arr, k++5),
	        W8A.sub(arr, k++6), W8A.sub(arr, k++7)),
	      mkWord32 (W8A.sub(arr, k), W8A.sub(arr, k++1),
	        W8A.sub(arr, k++2), W8A.sub(arr, k++3)))
	  end
  (* since LargeWord is 64-bits, no sign extension is required *)
    val subArrX = subArr

    fun update (arr, i, w) = let
	  val _ = chkIndex (W8A.length arr, i)
	  val k = scale i
	  val (hi, lo) = W64.extern w
	  in
	    W8A.update (arr, k,   w32ToW8 lo);
	    W8A.update (arr, k+1, w32ToW8(W32.rshiftl(lo,  0w8)));
	    W8A.update (arr, k+2, w32ToW8(W32.rshiftl(lo, 0w16)));
	    W8A.update (arr, k+3, w32ToW8(W32.rshiftl(lo, 0w24)));
	    W8A.update (arr, k,   w32ToW8 hi);
	    W8A.update (arr, k+1, w32ToW8(W32.rshiftl(hi,  0w8)));
	    W8A.update (arr, k+2, w32ToW8(W32.rshiftl(hi, 0w16)));
	    W8A.update (arr, k+3, w32ToW8(W32.rshiftl(hi, 0w24)))
	  end

  end
end (* local *)
