(* asdl-file-read-pickle.sml
 *
 * COPYRIGHT (c) 2021 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure ASDLFileReadPickle : sig

    include ASDL_READ_PICKLE

  (* byte-level input to support primitive picklers *)
    val input1 : instream -> Word8.word

  (* pickle from files *)
    val fromFile : (instream -> 'a) -> string -> 'a

  end where type instream = BinIO.instream
  = struct

    structure W = Word
    structure W8 = Word8
    structure W8V = Word8Vector

    val << = W.<< and >> = W.>>
    val ++ = W.orb
    val & = W.andb
    val !! = W.notb
    infix 5 << >>
    infix 6 ++
    infix 7 &

    type instream = BinIO.instream

  (* byte-level input to support primitive picklers *)
    fun input1 inS = (case BinIO.input1 inS
	   of SOME b => b
	    | NONE => raise ASDL.DecodeError
	  (* end case *))

    fun fromByte b = W.fromLarge (W8.toLarge b)

  (* read a single byte as a word *)
    fun readByte inS = (case BinIO.input1 inS
	   of SOME b => fromByte b
	    | NONE => raise ASDL.DecodeError
	  (* end case *))

    fun readBool inS = (case BinIO.input1 inS
	   of SOME 0w1 => false
	    | SOME 0w2 => true
	    | _ => raise ASDL.DecodeError
	  (* end case *))

    fun readBoolOption inS = (case BinIO.input1 inS
	   of SOME 0w0 => NONE
	    | SOME 0w1 => SOME false
	    | SOME 0w2 => SOME true
	    | _ => raise ASDL.DecodeError
	  (* end case *))

    fun readUInt inS = let
	  val b0 = readByte inS
	  val nb = b0 & 0wxc0
	  val res = b0 & 0wx3f
	  in
	    if (nb = 0w0) then res
	    else let
	      val b1 = readByte inS
	      val res = (res << 0w8) + b1
	      in
		if (nb = 0wx40) then res
		else let
		  val b2 = readByte inS
		  val res = (res << 0w8) + b2
		  in
		    if (nb = 0wx80) then res
		    else let
		      val b3 = readByte inS
		      in
			(res << 0w8) + b3
		      end
		  end
	      end
	  end

    fun readInt inS = let
	  val b0 = readByte inS
	  val nb = b0 & 0wxc0
	  val isNeg = (b0 & 0wx20 <> 0w0)
	  val res = b0 & 0wx1f
	  fun return w = if (b0 & 0wx20 <> 0w0)
		then ~1 - Word.toIntX w
		else Word.toIntX w
	  in
	    if (nb = 0w0) then return res
	    else let
	      val b1 = readByte inS
	      val res = (res << 0w8) + b1
	      in
		if (nb = 0wx40) then return res
		else let
		  val b2 = readByte inS
		  val res = (res << 0w8) + b2
		  in
		    if (nb = 0wx80) then return res
		    else let
		      val b3 = readByte inS
		      in
			return((res << 0w8) + b3)
		      end
		  end
	      end
	  end

    fun readInteger inS = let
	(* get first byte, which include sign *)
	  val b0 = readByte inS
	(* mask out sign bit *)
	  val b0' = (b0 & 0wxbf)
	  val sign = (b0 <> b0')
	  fun return n = if sign then ~n else n
	(* get bytes in big-endian order *)
	  fun lp n = let
		val b = readByte inS
	      (* mask out continuation bit *)
		val b' = (b & 0wx7f)
		val n = n + W.toLargeInt b'
		in
		  if (b = b')
		    then return n
		    else lp (IntInf.<<(n, 0w7))
		end
	(* initial byte *)
	  val b0'' = (b0' & 0wx3f)
	  val n = W.toLargeInt b0''
	  in
	    if (b0'' = b0')
	      then return n (* only one byte *)
	      else lp (IntInf.<<(n, 0w7))
	  end

    fun readString inS = let
	  val len = W.toIntX (readUInt inS)
	  val bytes = BinIO.inputN (inS, len)
	  in
	    if W8V.length bytes <> len
	      then raise ASDL.DecodeError
	      else Byte.bytesToString bytes
	  end

    fun readIdentifier inS =  Atom.atom(readString inS)

  (* utility functions for sum-type tags *)
    val readTag8 = readByte
    fun readTag16 inS = ((readByte inS) << 0w8) ++ (readByte inS)

    fun fromFile read file = let
	  val inS = BinIO.openIn file
	  in
	    (read inS handle exn => (BinIO.closeIn inS; raise exn))
	    before BinIO.closeIn inS
	  end

  end
