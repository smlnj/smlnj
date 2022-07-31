(* utf8.sml
 *
 * COPYRIGHT (c) 2020 John Reppy (http://www.cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Routines for working with UTF8 encoded strings.
 *
 *	Unicode value		        1st byte    2nd byte    3rd byte    4th byte
 *	-----------------------	        --------    --------    --------    --------
 *	00000 00000000 0xxxxxxx	        0xxxxxxx
 *	00000 00000yyy yyxxxxxx	        110yyyyy    10xxxxxx
 *	00000 zzzzyyyy yyxxxxxx	        1110zzzz    10yyyyyy	10xxxxxx
 *      wwwzz zzzzyyyy yyxxxxxx         11110www    10zzzzzz    10yyyyyy    10xxxxxx
 *
 *)

structure UTF8 :> UTF8 =
  struct

    structure W = Word
    structure SS = Substring

    type wchar = W.word

    fun w2c w = Char.chr(W.toInt w)

    val maxCodePoint : wchar = 0wx0010FFFF

  (* maximum values for the first byte for each encoding length *)
    val max1Byte : W.word = 0wx7f (* 0xxx xxxx *)
    val max2Byte : W.word = 0wxdf (* 110x xxxx *)
    val max3Byte : W.word = 0wxef (* 1110 xxxx *)
    val max4Byte : W.word = 0wxf7 (* 1111 0xxx *)

  (* bit masks for the first byte for each encoding length *)
    val mask2Byte : W.word = 0wx1f
    val mask3Byte : W.word = 0wx0f
    val mask4Byte : W.word = 0wx07

    exception Incomplete
	(* raised by some operations when applied to incomplete strings. *)

  (* add a continuation byte to the end of wc.  Continuation bytes have
   * the form 0b10xxxxxx.
   *)
    fun getContByte getc (wc, ss) = (case (getc ss)
	   of NONE => raise Incomplete
	    | SOME(c, ss') => let
		val b = W.fromInt(Char.ord c)
		in
		  if (W.andb(0wxc0, b) = 0wx80)
		    then (W.orb(W.<<(wc, 0w6), W.andb(0wx3f, b)), ss')
		    else raise Incomplete
		end
	  (* end case *))

  (* convert a character reader to a wide-character reader *)
    fun getu getc = let
	  val getContByte = getContByte getc
	  fun get strm = (case getc strm
		 of NONE => NONE
		  | SOME(c, ss) => let
		      val w = W.fromInt(Char.ord c)
		      val (wc, ss) = if (w <= max1Byte)
			  then (w, ss)
			else if (w <= max2Byte)
			  then getContByte (W.andb(mask2Byte, w), ss)
			else if (w <= max3Byte)
			  then getContByte(getContByte(W.andb(mask3Byte, w), ss))
			else if (w <= max4Byte)
			  then getContByte(getContByte(getContByte(W.andb(mask4Byte, w), ss)))
			  else raise Incomplete
		      in
			SOME(wc, ss)
		      end
		(* end case *))
	  in
	    get
	  end

    fun isAscii (wc : wchar) = (wc <= max1Byte)
    fun toAscii (wc : wchar) = w2c(W.andb(0wx7f, wc))
    fun fromAscii c = W.andb(0wx7f, W.fromInt(Char.ord c))

  (* return a printable string representation of a wide character *)
    fun toString wc =
	  if isAscii wc
	    then Char.toCString(toAscii wc)
	  else if (wc <= max2Byte)
	    then "\\u" ^ (StringCvt.padLeft #"0" 4 (W.toString wc))
	  (* NOTE: the following is not really SML syntax *)
	    else "\\u" ^ (StringCvt.padLeft #"0" 8 (W.toString wc))

  (* return a list of characters that is the UTF8 encoding of a wide character *)
    fun encode' (wc, chrs) = if (wc <= 0wx7f)
	    then w2c wc :: chrs
	  else if (wc <= 0wx7ff)
	    then w2c(W.orb(0wxc0, W.>>(wc, 0w6))) ::
	      w2c(W.orb(0wx80, W.andb(wc, 0wx3f))) :: chrs
	  else if (wc <= 0wxffff)
	    then w2c(W.orb(0wxe0, W.>>(wc, 0w12))) ::
	      w2c(W.orb(0wx80, W.andb(W.>>(wc, 0w6), 0wx3f))) ::
	      w2c(W.orb(0wx80, W.andb(wc, 0wx3f))) :: chrs
	  else if (wc <= maxCodePoint)
	    then w2c(W.orb(0wxf0, W.>>(wc, 0w18))) ::
	      w2c(W.orb(0wx80, W.andb(W.>>(wc, 0w12), 0wx3f))) ::
	      w2c(W.orb(0wx80, W.andb(W.>>(wc, 0w6), 0wx3f))) ::
	      w2c(W.orb(0wx80, W.andb(wc, 0wx3f))) :: chrs
	    else raise Domain

    fun encode wc = String.implode(encode'(wc, []))

    val getContByte = getContByte SS.getc

    fun getWC (c1, ss) = let
	  val w = W.fromInt(Char.ord c1)
	  val (wc, ss) = if (w <= max1Byte)
	      then (w, ss)
	    else if (w <= max2Byte)
	      then getContByte (W.andb(mask2Byte, w), ss)
	    else if (w <= max3Byte)
	      then getContByte(getContByte(W.andb(mask3Byte, w), ss))
	    else if (w <= max4Byte)
	      then getContByte(getContByte(getContByte(W.andb(mask4Byte, w), ss)))
	      else raise Incomplete
	  in
	    (wc, ss)
	  end

  (* return the number of Unicode characters *)
    fun size s = let
	  fun len (ss, n) = (case SS.getc ss
		 of NONE => n
		  | SOME arg => let
		      val (_, ss) = getWC arg
		      in
			len (ss, n+1)
		      end
		(* end case *))
	  in
	    len (SS.full s, 0)
	  end

    fun map f s = let
	  fun mapf (ss, chrs) = (case SS.getc ss
		 of NONE => String.implodeRev chrs
		  | SOME arg => let
		      val (wc, ss) = getWC arg
		      in
			mapf (ss, List.revAppend(encode'(wc, []), chrs))
		      end
		(* end case *))
	  in
	    mapf (SS.full s, [])
	  end

    fun app f s = let
	  fun appf ss = (case SS.getc ss
		 of NONE => ()
		  | SOME arg => let
		      val (wc, ss) = getWC arg
		      in
			f wc; appf ss
		      end
		(* end case *))
	  in
	    appf (SS.full s)
	  end

  (* fold a function over the Unicode characters in the string *)
    fun fold f = let
	  fun foldf (ss, acc) = (case SS.getc ss
		 of NONE => acc
		  | SOME arg => let
		      val (wc, ss) = getWC arg
		      in
			foldf (ss, f (wc, acc))
		      end
		(* end case *))
	  in
	    fn init => fn s => foldf (SS.full s, init)
	  end

    fun all pred s = let
	  fun allf ss = (case SS.getc ss
		 of NONE => true
		  | SOME arg => let
		      val (wc, ss) = getWC arg
		      in
			pred wc andalso allf ss
		      end
		(* end case *))
	  in
	    allf (SS.full s)
	  end

    fun exists pred s = let
	  fun existsf ss = (case SS.getc ss
		 of NONE => true
		  | SOME arg => let
		      val (wc, ss) = getWC arg
		      in
			pred wc orelse existsf ss
		      end
		(* end case *))
	  in
	    existsf (SS.full s)
	  end

  (* return the list of wide characters that are encoded by a string *)
    fun explode s = List.rev(fold (op ::) [] s)

    fun implode wcs = String.implode(List.foldr encode' [] wcs)

  end
