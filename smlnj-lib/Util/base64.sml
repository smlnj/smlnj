(* base64.sml
 *
 * COPYRIGHT (c) 2012 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Support for Base64 encoding/decoding as specified by RFC 4648.
 *
 *	http://www.ietf.org/rfc/rfc4648.txt
 *)

structure Base64 : BASE64 =
  struct

    structure W8 = Word8
    structure W8V = Word8Vector
    structure W8A = Word8Array
    structure UCV = Unsafe.CharVector
    structure UW8V = Unsafe.Word8Vector

  (* encoding table *)
    val encTbl = "\
	    \ABCDEFGHIJKLMNOPQRSTUVWXYZ\
	    \abcdefghijklmnopqrstuvwxyz\
	    \0123456789+/\
	  \"
    val padChar = #"="
    fun incByte b = UCV.sub(encTbl, Word8.toIntX b) 

  (* return true if a character is in the base64 alphabet *)
    val isBase64 = Char.contains encTbl

  (* encode a triple of bytes into four base-64 characters *)
    fun encode3 (b1, b2, b3) = let
	  val c1 = W8.>>(b1, 0w2)
	  val c2 = W8.orb(W8.<<(W8.andb(b1, 0wx3), 0w4), W8.>>(b2, 0w4))
	  val c3 = W8.orb(W8.<<(W8.andb(0wxF, b2), 0w2), W8.>>(b3, 0w6))
	  val c4 = W8.andb(0wx3f, b3)
	  in
	    (incByte c1, incByte c2, incByte c3, incByte c4)
	  end

  (* encode a pair of bytes into three base-64 characters plus a padding character *)
    fun encode2 (b1, b2) = let
	  val c1 = W8.>>(b1, 0w2)
	  val c2 = W8.orb(W8.<<(W8.andb(b1, 0wx3), 0w4), W8.>>(b2, 0w4))
	  val c3 = W8.<<(W8.andb(0wxF, b2), 0w2)
	  in
	    (incByte c1, incByte c2, incByte c3, padChar)
	  end

  (* encode a byte into two base-64 characters plus two padding characters *)
    fun encode1 b1 = let
	  val c1 = W8.>>(b1, 0w2)
	  val c2 = W8.<<(W8.andb(b1, 0wx3), 0w4)
	  in
	    (incByte c1, incByte c2, padChar, padChar)
	  end

    local
      fun encode64 (vec, start, len) = let
	    val outLen = 4 * Int.quot(len + 2, 3)
	    val outBuf = Unsafe.CharVector.create outLen
	    val nTriples = Int.quot(len, 3)
	    val extra = Int.rem(len, 3)
	    fun insBuf (i, (c1, c2, c3, c4)) = let
		  val idx = 4*i
		  in
		    UCV.update(outBuf, idx,   c1);
		    UCV.update(outBuf, idx+1, c2);
		    UCV.update(outBuf, idx+2, c3);
		    UCV.update(outBuf, idx+3, c4)
		  end
	    fun loop (i, idx) = if (i < nTriples)
		  then (
		    insBuf(i, encode3(UW8V.sub(vec, idx), UW8V.sub(vec, idx+1), UW8V.sub(vec, idx+2)));
		    loop (i+1, idx+3))
		  else (case extra
		     of 1 => insBuf(i, encode1(UW8V.sub(vec, idx)))
		      | 2 => insBuf(i, encode2(UW8V.sub(vec, idx), UW8V.sub(vec, idx+1)))
		      | _ => ()
		    (* end case *))
	    in
	      loop (0, start);
	      outBuf
	    end
    in

    fun encode vec = encode64 (vec, 0, W8V.length vec)

    fun encodeSlice slice = encode64 (Word8VectorSlice.base slice)

    end (* local *)

  (* raised if a Base64 string does not end in a complete encoding quantum (i.e., 4
   * characters including padding characters).
   *)
    exception Incomplete

  (* raised if an invalid Base64 character is encountered during decode.  The int
   * is the position of the character and the char is the invalid character.
   *)
    exception Invalid of (int * char)

  (* decoding tags *)
    val errCode : W8.word = 0w255
    val spCode : W8.word = 0w65
    val padCode : W8.word = 0w66
    val decTbl = let
	  val tbl = W8A.array(256, errCode)
	  fun ins (w, c) = W8A.update(tbl, Char.ord c, w)
	  in
	  (* add space codes *)
	    ins(spCode, #"\t");
	    ins(spCode, #"\n");
	    ins(spCode, #"\r");
	    ins(spCode, #" ");
	  (* add decoding codes *)
	    CharVector.appi (fn (i, c) => ins(Word8.fromInt i, c)) encTbl;
	  (* convert to vector *)
	    W8V.tabulate (256, fn i => W8A.sub(tbl, i))
	  end
    val strictDecTbl = let
	  val tbl = W8A.array(256, errCode)
	  fun ins (w, c) = W8A.update(tbl, Char.ord c, w)
	  in
	  (* add decoding codes *)
	    CharVector.appi (fn (i, c) => ins(Word8.fromInt i, c)) encTbl;
	  (* convert to vector *)
	    W8V.tabulate (256, fn i => W8A.sub(tbl, i))
	  end

    fun decode64 decTbl (s, start, len) = let
	  fun decodeChr c = W8V.sub(decTbl, Char.ord c)
	  fun getc i = if (i < len)
		then let
		  val c = String.sub(s, start+i)
		  val b = decodeChr c
		  in
		    if (b = errCode) then raise Invalid(i, c)
		    else if (b = spCode) then getc (i+1)
		    else (b, i+1)
		  end
		else raise Incomplete
	(* first we deal with possible padding.  There are three possible situations:
	 *   1. the final quantum is 24 bits, so there is no padding
	 *   2. the final quantum is 16 bits, so there are three code characters and
	 *      one pad character.
	 *   3. the final quantum is 8 bits, so there are two code characters and
	 *      two pad characters.
	 *)
	  val (lastQ, len, tailLen) = let
		fun getTail (i, n, chrs) = if (i < 0)
			then raise Incomplete
		      else if (n < 4)
			then (case String.sub(s, start+i)
			   of #"=" => getTail (i-1, n+1, (#"=", i)::chrs)
			    | c => let
				  val b = decodeChr c
				  in
				    if (b = spCode)
				      then getTail (i-1, n, chrs) (* skip whitespace *)
				    else if (b = errCode)
				      then raise Invalid(i, c)
				      else getTail (i-1, n+1, (c, i)::chrs)
				  end
			  (* end case *))
			else (i, chrs)
		fun cvt (c, i) = let
		      val b = decodeChr c
		      in
			if (b < 0w64) then b else raise Invalid(i, c)
		      end
		in
		  case getTail (len-1, 0, [])
		   of (len, [ci0, ci1, (#"=", _), (#"=", _)]) => let
			val c0 = cvt ci0
			val c1 = cvt ci1
			val b0 = W8.orb(W8.<<(c0, 0w2), W8.>>(c1, 0w4))
			in
			  ([b0], len, 1)
			end
		    | (len, [ci0, ci1, ci2, (#"=", _)]) => let
			val c0 = cvt ci0
			val c1 = cvt ci1
			val c2 = cvt ci2
			val b0 = W8.orb(W8.<<(c0, 0w2), W8.>>(c1, 0w4))
			val b1 = W8.orb(W8.<<(c1, 0w4), W8.>>(c2, 0w2))
			in
			  ([b0, b1], len, 2)
			end
		    | (_, [_, _, _, _]) => ([], len, 0) (* fallback to regular path below *)
		    | (_, []) => ([], len, 0)
		    | _ => raise Incomplete
		  (* end case *)
		end
	(* compute upper bound on number of output bytes *)
	  val nBytes = 3 * Word.toIntX(Word.>>(Word.fromInt len + 0w3, 0w2)) + tailLen
	  val buffer = W8A.array(nBytes, 0w0)
	  fun cvt (inIdx, outIdx) = if (inIdx < len)
		then let
		  val (c0, i) = getc inIdx
		  val (c1, i) = getc i
		  val (c2, i) = getc i
		  val (c3, i) = getc i
		  val b0 = W8.orb(W8.<<(c0, 0w2), W8.>>(c1, 0w4))
		  val b1 = W8.orb(W8.<<(c1, 0w4), W8.>>(c2, 0w2))
		  val b2 = W8.orb(W8.<<(c2, 0w6), c3)
		  in
		    W8A.update(buffer, outIdx, b0);
		    W8A.update(buffer, outIdx+1, b1);
		    W8A.update(buffer, outIdx+2, b2);
		    cvt (i, outIdx+3)
		  end
		else outIdx
	  val outLen = cvt (0, 0) (*handle Subscript => raise Incomplete*)
	(* deal with the last quantum *)
	  val outLen = (case lastQ
		 of [b0, b1] => (
		      W8A.update(buffer, outLen, b0);
		      W8A.update(buffer, outLen+1, b1);
		      outLen+2)
		  | [b0] => (
		      W8A.update(buffer, outLen, b0);
		      outLen+1)
		  | _ => outLen
		(* end case *))
	  in
	    Word8ArraySlice.vector(Word8ArraySlice.slice(buffer, 0, SOME outLen))
	  end

    fun decode s = decode64 decTbl (s, 0, size s)
    fun decodeSlice ss = decode64 decTbl (Substring.base ss)

    fun decodeStrict s = decode64 strictDecTbl (s, 0, size s)
    fun decodeSliceStrict ss = decode64 strictDecTbl (Substring.base ss)

  end

(* simple test code

val v = Word8Vector.tabulate(256, fn i => Word8.fromInt i);
val enc = Base64.encode v
val v' = Base64.decode enc
val ok = (v = v')

*)
