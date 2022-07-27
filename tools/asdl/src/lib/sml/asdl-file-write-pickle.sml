(* asdl-file-write-pickle.sml
 *
 * COPYRIGHT (c) 2021 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure ASDLFileWritePickle : sig

    include ASDL_WRITE_PICKLE

  (* byte-level output to support primitive picklers *)
    val output1 : outstream * Word8.word -> unit

  (* pickle to/from files *)
    val toFile : (outstream * 'a -> unit) -> (string * 'a) -> unit

  end where type outstream = BinIO.outstream
  = struct

    structure W = Word
    structure W8 = Word8

    val << = W.<< and >> = W.>>
    val ++ = W.orb
    val & = W.andb
    val !! = W.notb
    infix 5 << >>
    infix 6 ++
    infix 7 &

    type outstream = BinIO.outstream

  (* byte-level output to support primitive picklers *)
    val output1 = BinIO.output1

    fun toByte w = W8.fromLarge (W.toLarge w)

    fun writeBool (outS, false) = BinIO.output1(outS, 0w1)
      | writeBool (outS, true) = BinIO.output1(outS, 0w2)

    fun writeBoolOption (outS, NONE) = BinIO.output1(outS, 0w0)
      | writeBoolOption (outS, SOME b) = writeBool (outS, b)

  (* write an unsigned signed integer.  We assume that the value is in the
   * range 0..2^30 - 1
   *)
    fun writeUInt (outS, w) = if (w <= 0wx3f)
	    then BinIO.output1(outS, toByte w)
	  else if (w <= 0wx3fff)
	    then ( (* two bytes *)
	      BinIO.output1(outS, toByte(0wx40 ++ (w >> 0w8)));
	      BinIO.output1(outS, toByte w))
	  else if (w <= 0wx3fffff)
	    then ( (* three bytes *)
	      BinIO.output1(outS, toByte(0wx80 ++ (w >> 0w16)));
	      BinIO.output1(outS, toByte(w >> 0w8));
	      BinIO.output1(outS, toByte w))
	    else ( (* four bytes *)
	      BinIO.output1(outS, toByte(0wxc0 ++ (w >> 0w24)));
	      BinIO.output1(outS, toByte(w >> 0w16));
	      BinIO.output1(outS, toByte(w >> 0w8));
	      BinIO.output1(outS, toByte w))

  (* encode a signed integer.  We assume that the value is in the range -2^29..2^29 - 1 *)
    fun writeInt (outS, n) = let
	  val (sign, w) = if (n < 0)
		then (0wx20, W.fromInt(~(n+1)))
		else (0w0, Word.fromInt n)
	  in
	    if (w <= 0wx1f)
	      then BinIO.output1(outS, toByte(sign ++ w))
	    else if (w <= 0wx1fff)
	      then ( (* two bytes *)
		BinIO.output1(outS, toByte(0wx40 ++ sign ++ (w >> 0w8)));
		BinIO.output1(outS, toByte w))
	    else if (w <= 0wx1fffff)
	      then ( (* three bytes *)
		BinIO.output1(outS, toByte(0wx80 ++ sign ++ (w >> 0w16)));
		BinIO.output1(outS, toByte(w >> 0w8));
		BinIO.output1(outS, toByte w))
	      else ( (* four bytes *)
		BinIO.output1(outS, toByte(0wxc0 ++ sign ++ (w >> 0w24)));
		BinIO.output1(outS, toByte(w >> 0w16));
		BinIO.output1(outS, toByte(w >> 0w8));
		BinIO.output1(outS, toByte w))
	  end

    fun writeInteger (outS, n) = let
	  val (sign, n) = if n < 0 then (0wx40, ~n) else (0wx00, n)
	(* convert to sequence of 7-bit chunks in big-endian order.  Note that
	 * first chunk in result is only 6 bits to allow for the sign.
	 *)
	  fun lp (n, bs) = if (n < 64)
		then output (W.fromLargeInt n ++ sign, bs)
		else lp (IntInf.~>>(n, 0w7), (W.fromLargeInt n & 0wx7f) :: bs)
	(* output bytes to buffer with continuation bits set as necessary *)
	  and output (b, []) = BinIO.output1(outS, toByte b)
	    | output (b, b'::br) = (
		BinIO.output1(outS, toByte(0wx80 ++ b));
		output(b', br))
	  in
	    lp (n, [])
	  end

    fun writeString (outS, s) = (
	  writeUInt(outS, Word.fromInt(size s));
	  BinIO.output(outS, Byte.stringToBytes s))

    fun writeIdentifier (outS, id) = writeString (outS, Atom.toString id)

  (* utility functions for sum-type tags *)
    fun writeTag8 (outS, tag) = BinIO.output1(outS, toByte tag)
    fun writeTag16 (outS, tag) = (
	  BinIO.output1(outS, toByte(tag >> 0w8));
	  BinIO.output1(outS, toByte tag));

    fun toFile write (file, x) = let
	  val outS = BinIO.openOut file
	  in
	    write (outS, x) handle exn => (BinIO.closeOut outS; raise exn);
	    BinIO.closeOut outS
	  end

  end
