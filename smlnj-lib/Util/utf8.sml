(* utf8.sml
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://smlnj.org)
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
    val replacementCodePoint : wchar = 0wxFFFD

  (* maximum values for the first byte for each encoding length *)
    val max1Byte : W.word = 0wx7f (* 0xxx xxxx *)
    val max2Byte : W.word = 0wxdf (* 110x xxxx *)
    val max3Byte : W.word = 0wxef (* 1110 xxxx *)
    val max4Byte : W.word = 0wxf7 (* 1111 0xxx *)

    (* bit masks for the first byte for each encoding length *)
    val mask2Byte : W.word = 0wx1f
    val mask3Byte : W.word = 0wx0f
    val mask4Byte : W.word = 0wx07

    (* bit mask for continuation bytes *)
    val maskContByte : W.word = 0wx3f

    exception Incomplete
    exception Invalid

    (* what to do when there is an incomplete UTF-8 sequence *)
    datatype decode_strategy
      = STRICT
      | REPLACE

    (* a simple state machine for getting a valid UTF8 byte sequence.
     * See https://unicode.org/mail-arch/unicode-ml/y2003-m02/att-0467/01-The_Algorithm_to_Valide_an_UTF-8_String
     * for a description of the state machine.
     *)
    fun getWC strategy getc = let
          fun getByte (inS, k) = (case getc inS
                 of SOME(c, inS') => k (Word.fromInt(ord c), inS')
                  | NONE => (case strategy
                       of STRICT => raise Incomplete
                        | REPLACE => SOME(replacementCodePoint, inS)
                     (* end case *))
                (* end case *))
          fun inRange (minB : word, b, maxB) = ((b - minB) <= maxB - minB)
          (* add the bits of a continuation byte to the accumulator *)
          fun addContBits (accum, b) = W.orb(W.<<(accum, 0w6), W.andb(0wx3f, b))
          (* handles an invalid byte in the input stream *)
          fun invalid inS = (case strategy
                 of STRICT => raise Invalid
                  | REPLACE => SOME(replacementCodePoint, inS)
                (* end case *))
          (* handles last byte for all multi-byte sequences *)
          fun stateA (inS, accum) = getByte (inS, fn (b, inS') =>
                  if inRange(0wx80, b, 0wxbf)
                    then SOME(addContBits(accum, b), inS')
                    else invalid inS)
          (* handles second/third byte for three/four-byte sequences *)
          and stateB (inS, accum) = getByte (inS, fn (b, inS') =>
                  if inRange(0wx80, b, 0wxbf)
                    then stateA (inS', addContBits(accum, b))
                    else invalid inS)
          (* byte0 = 0b1110_0000 (3-byte sequence) *)
          and stateC (inS, accum) = getByte (inS, fn (b, inS') =>
                  if inRange(0wxa0, b, 0wxbf)
                    then stateA (inS', addContBits(accum, b))
                    else invalid inS)
          (* byte0 = 0b1110_1101 (3-byte sequence) *)
          and stateD (inS, accum) = getByte (inS, fn (b, inS') =>
                  if inRange(0wx80, b, 0wx9f)
                    then stateA (inS', addContBits(accum, b))
                    else invalid inS)
          (* byte0 = 0b1111_0001 .. 0b1111_0011 (4-byte sequence) *)
          and stateE (inS, accum) = getByte (inS, fn (b, inS') =>
                  if inRange(0wx80, b, 0wxbf)
                    then stateB (inS', addContBits(accum, b))
                    else invalid inS)
          (* byte0 = 0b1111_0000 (4-byte sequence) *)
          and stateF (inS, accum) = getByte (inS, fn (b, inS') =>
                  if inRange(0wx90, b, 0wxbf)
                    then stateB (inS', addContBits(accum, b))
                    else invalid inS)
          (* byte0 = 0b1111_1000 (4-byte sequence) *)
          and stateG (inS, accum) = getByte (inS, fn (b, inS') =>
                  if inRange(0wx80, b, 0wx8f)
                    then stateB (inS', addContBits(accum, b))
                    else invalid inS)
          and start inS = (case getc inS
                 of SOME(c, inS) => let
                      val byte0 = Word.fromInt(ord c)
                      in
                        if (byte0 <= 0wx7f)
                          then SOME(byte0, inS) (* ASCII character *)
                        else if inRange(0wxc2, byte0, 0wxdf)
                          then stateA (inS, W.andb(byte0, mask2Byte))
                        else if inRange(0wxe1, byte0, 0wxec)
                        orelse inRange(0wxee, byte0, 0wxef)
                          then stateB (inS, W.andb(byte0, mask3Byte))
                        else if (byte0 = 0wxe0)
                          then stateC (inS, W.andb(byte0, mask3Byte))
                        else if (byte0 = 0wxed)
                          then stateD (inS, W.andb(byte0, mask3Byte))
                        else if inRange(0wxf1, byte0, 0wxf3)
                          then stateE (inS, W.andb(byte0, mask4Byte))
                        else if (byte0 = 0wxf0)
                          then stateF (inS, W.andb(byte0, mask4Byte))
                        else if (byte0 = 0wxf4)
                          then stateG (inS, W.andb(byte0, mask4Byte))
                          else invalid inS
                      end
                  | NONE => NONE
                (* end case *))
          in
            start
          end

    fun getWCStrict getc = getWC STRICT getc

    val getu = getWCStrict

    fun isAscii (wc : wchar) = (wc <= max1Byte)
    fun toAscii (wc : wchar) = w2c(W.andb(0wx7f, wc))
    fun fromAscii c = W.andb(0wx7f, W.fromInt(Char.ord c))

  (* return a printable string representation of a wide character *)
    fun toString wc =
	  if isAscii wc
	    then Char.toCString(toAscii wc)
	  else if (wc <= 0wxFFFF)
	    then "\\u" ^ (StringCvt.padLeft #"0" 4 (W.toString wc))
	    (* NOTE: the following is Successor ML syntax *)
	    else "\\U" ^ (StringCvt.padLeft #"0" 8 (W.toString wc))

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
	    else raise Invalid

    fun encode wc = String.implode(encode'(wc, []))

    (* consume one valid UTF8 character from a substring *)
    fun eatOneUTF8Char ss = let
          fun getByte ss = (case SS.getc ss
                 of SOME(c, ss') => (Word.fromInt(ord c), ss')
                  | NONE => raise Incomplete
                (* end case *))
          fun inRange (minB : word, b, maxB) = ((b - minB) <= maxB - minB)
          (* handles last byte for all multi-byte sequences *)
          fun stateA ss = let
                val (b, ss) = getByte ss
                in
                  if inRange(0wx80, b, 0wxbf)
                    then ss
                    else raise Invalid
                end
          (* handles second/third byte for three/four-byte sequences *)
          and stateB ss = let
                val (b, ss) = getByte ss
                in
                  if inRange(0wx80, b, 0wxbf)
                    then stateA ss
                    else raise Invalid
                end
          (* byte0 = 0b1110_0000 (3-byte sequence) *)
          and stateC ss = let
                val (b, ss) = getByte ss
                in
                  if inRange(0wxa0, b, 0wxbf)
                    then stateA ss
                    else raise Invalid
                end
          (* byte0 = 0b1110_1101 (3-byte sequence) *)
          and stateD ss = let
                val (b, ss) = getByte ss
                in
                  if inRange(0wx80, b, 0wx9f)
                    then stateA ss
                    else raise Invalid
                end
          (* byte0 = 0b1111_0001 .. 0b1111_0011 (4-byte sequence) *)
          and stateE ss = let
                val (b, ss) = getByte ss
                in
                  if inRange(0wx80, b, 0wxbf)
                    then stateB ss
                    else raise Invalid
                end
          (* byte0 = 0b1111_0000 (4-byte sequence) *)
          and stateF ss = let
                val (b, ss) = getByte ss
                in
                  if inRange(0wx90, b, 0wxbf)
                    then stateB ss
                    else raise Invalid
                end
          (* byte0 = 0b1111_1000 (4-byte sequence) *)
          and stateG ss = let
                val (b, ss) = getByte ss
                in
                  if inRange(0wx80, b, 0wx8f)
                    then stateB ss
                    else raise Invalid
                end
          in
            case SS.getc ss
             of SOME(c, ss) => let
                  val byte0 = Word.fromInt(ord c)
                  in
                    if (byte0 <= 0wx7f)
                      then ss (* ASCII character *)
                    else if inRange(0wxc2, byte0, 0wxdf)
                      then stateA ss
                    else if inRange(0wxe1, byte0, 0wxec)
                    orelse inRange(0wxee, byte0, 0wxef)
                      then stateB ss
                    else if (byte0 = 0wxe0)
                      then stateC ss
                    else if (byte0 = 0wxed)
                      then stateD ss
                    else if inRange(0wxf1, byte0, 0wxf3)
                      then stateE ss
                    else if (byte0 = 0wxf0)
                      then stateF ss
                    else if (byte0 = 0wxf4)
                      then stateG ss
                      else raise Invalid
                  end
              | NONE => ss
            (* end case *)
          end

    (* return the number of Unicode characters in a substring *)
    fun size' ss = let
	  fun len (ss, n) = if SS.isEmpty ss
                then n
                else len (eatOneUTF8Char ss, n+1)
          in
	    len (ss, 0)
	  end

    (* return the number of Unicode characters in a string *)
    fun size s = size' (SS.full s)

    (* get wide characters from substrings *)
    val getWCFromSubstring = getWCStrict SS.getc

    fun map f s = let
	  fun mapf (ss, chrs) = (case getWCFromSubstring ss
		 of NONE => String.implodeRev chrs
		  | SOME(wc, ss) => mapf (ss, List.revAppend(encode'(wc, []), chrs))
		(* end case *))
	  in
	    mapf (SS.full s, [])
	  end

    fun app f s = let
	  fun appf ss = (case getWCFromSubstring ss
		 of NONE => ()
		  | SOME(wc, ss) => (f wc; appf ss)
		(* end case *))
	  in
	    appf (SS.full s)
	  end

  (* fold a function over the Unicode characters in the string *)
    fun fold f = let
	  fun foldf (ss, acc) = (case getWCFromSubstring ss
		 of NONE => acc
		  | SOME(wc, ss) => foldf (ss, f (wc, acc))
		(* end case *))
	  in
	    fn init => fn s => foldf (SS.full s, init)
	  end

    fun all pred s = let
	  fun allf ss = (case getWCFromSubstring ss
		 of NONE => true
		  | SOME(wc, ss) => pred wc andalso allf ss
		(* end case *))
	  in
	    allf (SS.full s)
	  end

    fun exists pred s = let
	  fun existsf ss = (case getWCFromSubstring ss
		 of NONE => true
		  | SOME(wc, ss) => pred wc orelse existsf ss
		(* end case *))
	  in
	    existsf (SS.full s)
	  end

  (* return the list of wide characters that are encoded by a string *)
    fun explode s = List.rev(fold (op ::) [] s)

    fun implode wcs = String.implode(List.foldr encode' [] wcs)

  end
