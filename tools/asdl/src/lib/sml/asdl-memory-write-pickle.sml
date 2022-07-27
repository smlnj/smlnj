(* asdl-memory-write-pickle.sml
 *
 * COPYRIGHT (c) 2021 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Basic pickler to memory operation.
 *)

structure ASDLMemoryWritePickle : sig

    include ASDL_WRITE_PICKLE

  (* byte-level output to support primitive picklers *)
    val output1 : outstream * Word8.word -> unit

  (* pickle to a vector *)
    val toVector : (outstream * 'a -> unit) -> 'a -> Word8Vector.vector

  end where type outstream = Word8Buffer.buf
  = struct

    structure W = Word
    structure W8 = Word8
    structure W8B = Word8Buffer

    val << = W.<< and >> = W.>>
    val ++ = W.orb
    val & = W.andb
    val !! = W.notb
    infix 5 << >>
    infix 6 ++
    infix 7 &

    fun toByte w = W8.fromLarge (W.toLarge w)

    type outstream = W8B.buf

  (* byte-level output to support primitive picklers *)
    val output1 = W8B.add1

    fun writeBool (buf, false) = W8B.add1 (buf, 0w1)
      | writeBool (buf, true) = W8B.add1 (buf, 0w2)

    fun writeBoolOption (outS, NONE) = W8B.add1 (outS, 0w0)
      | writeBoolOption (outS, SOME b) = writeBool (outS, b)

  (* write an unsigned signed integer.  We assume that the value is in the
   * range 0..2^30 - 1
   *)
    fun writeUInt (buf, w) = if (w <= 0wx3f)
	    then W8B.add1(buf, toByte w)
	  else if (w <= 0wx3fff)
	    then ( (* two bytes *)
	      W8B.add1(buf, toByte(0wx40 ++ (w >> 0w8)));
	      W8B.add1(buf, toByte w))
	  else if (w <= 0wx3fffff)
	    then ( (* three bytes *)
	      W8B.add1(buf, toByte(0wx80 ++ (w >> 0w16)));
	      W8B.add1(buf, toByte(w >> 0w8));
	      W8B.add1(buf, toByte w))
	    else ( (* four bytes *)
	      W8B.add1(buf, toByte(0wxc0 ++ (w >> 0w24)));
	      W8B.add1(buf, toByte(w >> 0w16));
	      W8B.add1(buf, toByte(w >> 0w8));
	      W8B.add1(buf, toByte w))

  (* write a signed integer.  We assume that the value is in the range -2^29..2^29 - 1 *)
    fun writeInt (buf, n) = let
	  val (sign, w) = if (n < 0)
		then (0wx20, W.fromInt(~(n+1)))
		else (0w0, Word.fromInt n)
	  in
	    if (w <= 0wx1f)
	      then W8B.add1(buf, toByte(sign ++ w))
	    else if (w <= 0wx1fff)
	      then ( (* two bytes *)
		W8B.add1(buf, toByte(0wx40 ++ sign ++ (w >> 0w8)));
		W8B.add1(buf, toByte w))
	    else if (w <= 0wx1fffff)
	      then ( (* three bytes *)
		W8B.add1(buf, toByte(0wx80 ++ sign ++ (w >> 0w16)));
		W8B.add1(buf, toByte(w >> 0w8));
		W8B.add1(buf, toByte w))
	      else ( (* four bytes *)
		W8B.add1(buf, toByte(0wxc0 ++ sign ++ (w >> 0w24)));
		W8B.add1(buf, toByte(w >> 0w16));
		W8B.add1(buf, toByte(w >> 0w8));
		W8B.add1(buf, toByte w))
	  end

    fun writeInteger (buf, n) = let
	  val (sign, n) = if n < 0 then (0wx40, ~n) else (0wx00, n)
	(* convert to sequence of 7-bit chunks in big-endian order.  Note that
	 * first chunk in result is only 6 bits to allow for the sign.
	 *)
	  fun lp (n, bs) = if (n < 64)
		then output (W.fromLargeInt n ++ sign, bs)
		else lp (IntInf.~>>(n, 0w7), (W.fromLargeInt n & 0wx7f) :: bs)
	(* output bytes to buffer with continuation bits set as necessary *)
	  and output (b, []) = W8B.add1(buf, toByte b)
	    | output (b, b'::br) = (
		W8B.add1(buf, toByte(0wx80 ++ b));
		output(b', br))
	  in
	    lp (n, [])
	  end

    fun writeString (buf, s) = (
	  writeUInt(buf, Word.fromInt(size s));
	  W8B.addVec(buf, Byte.stringToBytes s))

    fun writeIdentifier (buf, id) = writeString (buf, Atom.toString id)

  (* utility functions for sum-type tags *)
    fun writeTag8 (buf, tag) = W8B.add1(buf, toByte tag)
    fun writeTag16 (buf, tag) = (
	  W8B.add1(buf, toByte(tag >> 0w8));
	  W8B.add1(buf, toByte tag));

  (* write to a vector *)
    fun toVector write x = let
	  val buf = W8B.new 64
	  in
	    write (buf, x);
	    W8B.contents buf
	  end

  end
