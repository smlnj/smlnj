(* pack-word-l64.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Implementation of PackWord64Little for 64-bit targets.
 *)

local
  structure LargeWord = LargeWordImp
in
structure PackWord64Little : PACK_WORD =
  struct

    structure Word = InlineT.Word
    structure W8 = Word8Imp
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

    val w8ToW64 = W64.fromLarge o W8.toLarge
    val w64ToW8 = W8.fromLarge o W64.toLarge

  (* make a word64 from little-endian-order bytes [b1, ..., b8] *)
    fun mkWord (b1, b2, b3, b4, b5, b6, b7, b8) = let
	  val w = w8ToW64 b8
	  val w = W64.orb(W64.lshift(w, 0w8), w8ToW64 b7)
	  val w = W64.orb(W64.lshift(w, 0w8), w8ToW64 b6)
	  val w = W64.orb(W64.lshift(w, 0w8), w8ToW64 b5)
	  val w = W64.orb(W64.lshift(w, 0w8), w8ToW64 b4)
	  val w = W64.orb(W64.lshift(w, 0w8), w8ToW64 b3)
	  val w = W64.orb(W64.lshift(w, 0w8), w8ToW64 b2)
	  val w = W64.orb(W64.lshift(w, 0w8), w8ToW64 b1)
	  in
	    w
	  end

    fun subVec (vec, i) = let
	  val _ = chkIndex (W8V.length vec, i)
	  val k = scale i
	  in
	    mkWord (
	      W8V.sub(vec, k), W8V.sub(vec, k++1),
	      W8V.sub(vec, k++2), W8V.sub(vec, k++3),
	      W8V.sub(vec, k++4), W8V.sub(vec, k++5),
	      W8V.sub(vec, k++6), W8V.sub(vec, k++7))
	  end
  (* since LargeWord is 64-bits, no sign extension is required *)
    val subVecX = subVec

    fun subArr (arr, i) = let
	  val _ = chkIndex (W8A.length arr, i)
	  val k = scale i
	  in
	    mkWord (
	      W8A.sub(arr, k), W8A.sub(arr, k++1),
	      W8A.sub(arr, k++2), W8A.sub(arr, k++3),
	      W8A.sub(arr, k++4), W8A.sub(arr, k++5),
	      W8A.sub(arr, k++6), W8A.sub(arr, k++7))
	  end
  (* since LargeWord is 64-bits, no sign extension is required *)
    val subArrX = subArr

    fun update (arr, i, w) = let
	  val _ = chkIndex (W8A.length arr, i)
	  val k = scale i
	  in
	    W8A.update (arr, k,   w64ToW8(W64.rshiftl(w, 0w56)));
	    W8A.update (arr, k+1, w64ToW8(W64.rshiftl(w, 0w48)));
	    W8A.update (arr, k+2, w64ToW8(W64.rshiftl(w, 0w40)));
	    W8A.update (arr, k+3, w64ToW8(W64.rshiftl(w, 0w32)));
	    W8A.update (arr, k+4, w64ToW8(W64.rshiftl(w, 0w24)));
	    W8A.update (arr, k+5, w64ToW8(W64.rshiftl(w, 0w16)));
	    W8A.update (arr, k+6, w64ToW8(W64.rshiftl(w,  0w8)));
	    W8A.update (arr, k+7, w64ToW8      w)
	  end

  end
end (* local *)
