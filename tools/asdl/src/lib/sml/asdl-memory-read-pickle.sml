(* asdl-memory-pickle.sml
 *
 * COPYRIGHT (c) 2021 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Basic unpickler operations from memory.
 *)

structure ASDLMemoryReadPickle : sig

    include ASDL_READ_PICKLE

  (* byte-level input to support primitive picklers *)
    val input1 : instream -> Word8.word

  (* open a vector as an input stream *)
    val openVector : Word8Vector.vector -> instream

  (* has all the input been consumed? *)
    val endOfStream : instream -> bool

  (* unpickle from a vector *)
    val fromVector : (instream -> 'a)
	  -> Word8Vector.vector
	  -> 'a

  end = struct

    structure W = Word
    structure W8 = Word8
    structure W8V = Word8Vector
    structure W8S = Word8VectorSlice

    val << = W.<< and >> = W.>>
    val ++ = W.orb
    val & = W.andb
    val !! = W.notb
    infix 5 << >>
    infix 6 ++
    infix 7 &

    fun fromByte w = W.fromLarge (W8.toLarge w)
    fun getByte (data, ix) = fromByte (W8V.sub(data, ix))

    datatype instream = Instrm of {
	data : W8V.vector,
	idx : int ref,
	len : int
      }

  (* byte-level input to support primitive picklers *)
    fun input1 (Instrm{data, idx as ref ix, len}) =
          if (ix < len)
            then (
              idx := ix+1;
              W8V.sub(data, ix))
            else raise ASDL.DecodeError

    fun openVector v = Instrm{
	    data = v,
	    idx = ref 0,
	    len = W8V.length v
	  }

    fun endOfStream (Instrm{idx, len, ...}) = (!idx >= len)

    fun get1 arg = fromByte(input1 arg)

    fun get2 (Instrm{data, idx as ref ix, len}) = let
	  val n = ix + 2
	  in
	    if (n <= len)
	      then (
	        idx := n;
	        (getByte(data, ix), getByte(data, ix+1)))
	      else raise ASDL.DecodeError
	  end

    fun get3 (Instrm{data, idx as ref ix, len}) = let
	  val n = ix + 3
	  in
	    if (n <= len)
	      then (
	        idx := n;
	        (getByte(data, ix), getByte(data, ix+1), getByte(data, ix+2)))
	      else raise ASDL.DecodeError
	  end

    fun getSlice (Instrm{data, idx as ref ix, len}, n) = let
	  val ix' = ix + n
	  in
	    if (ix' <= len)
	      then (
		idx := ix';
		W8S.slice(data, ix, SOME n))
	      else raise ASDL.DecodeError
	  end

    fun readBool inS = (case get1 inS
	   of 0w1 => false
	    | 0w2 => true
	    | _ => raise ASDL.DecodeError
	  (* end case *))

    fun readBoolOption inS = (case get1 inS
	   of 0w0 => NONE
	    | 0w1 => SOME false
	    | 0w2 => SOME true
	    | _ => raise ASDL.DecodeError
	  (* end case *))

    fun readUInt inS = let
	  val b0 = get1 inS
	  val nb = b0 & 0wxc0
	  val res = b0 & 0wx3f
	  in
	    if (nb = 0w0) then res
	    else if (nb = 0wx40)
	      then let
		val b1 = get1 inS
		in
		  (res << 0w8) + b1
		end
	    else if (nb = 0wx80)
	      then let
		val (b1, b2) = get2 inS
		in
		  (res << 0w16) + (b1 << 0w8) + b2
		end
	      else let
		val (b1, b2, b3) = get3 inS
		in
		  (res << 0w24) + (b1 << 0w16) + (b2 << 0w8) + b3
		end
	  end

    fun readInt inS = let
	  val b0 = get1 inS
	  val nb = b0 & 0wxc0
	  val isNeg = (b0 & 0wx20 <> 0w0)
	  val res = b0 & 0wx1f
	  fun return w = if (b0 & 0wx20 <> 0w0)
		then (~1 - Word.toIntX w)
		else Word.toIntX w
	  in
	    if (nb = 0w0) then return res
	    else if (nb = 0wx40)
	      then let
		val b1 = get1 inS
		in
		  return ((res << 0w8) + b1)
		end
	    else if (nb = 0wx80)
	      then let
		val (b1, b2) = get2 inS
		in
		  return ((res << 0w16) + (b1 << 0w8) + b2)
		end
	      else let
		val (b1, b2, b3) = get3 inS
		in
		  return ((res << 0w24) + (b1 << 0w16) + (b2 << 0w8) + b3)
		end
	  end

    fun readInteger inS = let
	(* get first byte, which include sign *)
	  val b0 = get1 inS
	(* mask out sign bit *)
	  val b0' = (b0 & 0wxbf)
	  val sign = (b0 <> b0')
	  fun return n = if sign then ~n else n
	(* get bytes in big-endian order *)
	  fun lp n = let
		val b = get1 inS
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

    fun readString inS = (case W.toIntX (readUInt inS)
	   of 0 => ""
	    | len => Byte.unpackStringVec(getSlice (inS, len))
	  (* end case *))

    fun readIdentifier inS = Atom.atom(readString inS)

  (* utility functions for sum-type tags *)
    fun readTag8 inS = get1 inS
    fun readTag16 inS = let
	  val b0 = get1 inS
	  val b1 = get1 inS
	  in
	    (b0 << 0w8) ++ b1
	  end

    fun fromVector decode v = decode (openVector v)

  end
