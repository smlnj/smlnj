(* num-scan32.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * The string conversion for the largest fixed-size int and word types.
 * These are used for fixed-precision types that have 32 or fewer bits.
 *)

structure NumScan32 : sig

    val scanWord : StringCvt.radix
	  -> (char, 'a) StringCvt.reader -> (Word32.word, 'a) StringCvt.reader
    val scanInt  : StringCvt.radix
	  -> (char, 'a) StringCvt.reader -> (Int32.int, 'a) StringCvt.reader

  end = struct

    structure W = InlineT.Word32
    structure I = InlineT.Int32
    structure U = ScanUtil

    type word32 = Word32.word

    fun toWord32 w = InlineT.Word32.fromLarge(InlineT.Word.toLarge w)

    val largestWordDiv10 : word32 = 0w429496729	(* 2^32-1 divided by 10 *)
    val largestWordMod10 : word32 = 0w5		(* remainder *)

    val largestNegInt : word32 = 0wx80000000
    val largestPosInt : word32 = 0wx7fffffff
    val minInt : Int32.int = ~2147483648

  (* for power of 2 bases (2, 8 & 16), we can check for overflow by looking
   * at the hi (1, 3 or 4) bits.
   *)
    fun chkOverflow mask w =
	  if (W.andb(mask, w) = 0w0) then () else raise Overflow

    fun scanBin isWord getc cs = (case (U.scanPrefix (U.binPat isWord) getc cs)
	   of NONE => NONE
	    | (SOME{neg, next, rest}) => let
		val chkOverflow = chkOverflow 0wx80000000
		fun cvt (w, rest) = (case (getc rest)
		       of NONE => SOME{neg=neg, word=w, rest=rest}
			| SOME(c, rest') => let val d = U.code c
			    in
			      if (d <= 0w1)
				then (
				  chkOverflow w;
				  cvt(W.+(W.lshift(w, 0w1), toWord32 d), rest'))
				else SOME{neg=neg, word=w, rest=rest}
			    end
		      (* end case *))
		in
		  cvt (toWord32 next, rest)
		end
	  (* end case *))

    fun scanOct isWord getc cs = (case (U.scanPrefix (U.octPat isWord) getc cs)
	   of NONE => NONE
	    | (SOME{neg, next, rest}) => let
		val chkOverflow = chkOverflow 0wxE0000000
		fun cvt (w, rest) = (case (getc rest)
		       of NONE => SOME{neg=neg, word=w, rest=rest}
			| SOME(c, rest') => let val d = U.code c
			    in
			      if (d <= 0w7)
				then (
				  chkOverflow w;
				  cvt(W.+(W.lshift(w, 0w3), toWord32 d), rest'))
				else SOME{neg=neg, word=w, rest=rest}
			    end
		      (* end case *))
		in
		  cvt (toWord32 next, rest)
		end
	  (* end case *))

    fun scanDec isWord getc cs = (case (U.scanPrefix (U.decPat isWord) getc cs)
	   of NONE => NONE
	    | (SOME{neg, next, rest}) => let
		fun cvt (w : word32, rest) = (case (getc rest)
		       of NONE => SOME{neg=neg, word=w, rest=rest}
			| SOME(c, rest') => let val d = U.code c
			    in
			      if (d <= 0w9)
				then (
				  if (W.>=(w, largestWordDiv10)
				  andalso (W.<(largestWordDiv10, w)
				    orelse W.<(largestWordMod10, toWord32 d)))
				    then raise Overflow
				    else ();
				  cvt (W.+(W.*(0w10, w), toWord32 d), rest'))
				else SOME{neg=neg, word=w, rest=rest}
			    end
		      (* end case *))
		in
		  cvt (toWord32 next, rest)
		end
	  (* end case *))

    fun scanHex isWord getc cs = (case (U.scanPrefix (U.hexPat isWord) getc cs)
	   of NONE => NONE
	    | (SOME{neg, next, rest}) => let
		val chkOverflow = chkOverflow 0wxF0000000
		fun cvt (w, rest) = (case (getc rest)
		       of NONE => SOME{neg=neg, word=w, rest=rest}
			| SOME(c, rest') => let val d = U.code c
			    in
			      if (d <= 0w15)
				then (
				  chkOverflow w;
				  cvt(W.+(W.lshift(w, 0w4), toWord32 d), rest'))
				else SOME{neg=neg, word=w, rest=rest}
			    end
		      (* end case *))
		in
		  cvt (toWord32 next, rest)
		end
	  (* end case *))

    fun finalWord scanFn getc cs = (case (scanFn true getc cs)
	   of NONE => NONE
	    | (SOME{neg, word, rest}) => SOME(word, rest)
	  (* end case *))

    fun scanWord StringCvt.BIN = finalWord scanBin
      | scanWord StringCvt.OCT = finalWord scanOct
      | scanWord StringCvt.DEC = finalWord scanDec
      | scanWord StringCvt.HEX = finalWord scanHex

    local
      (* Type check Bug test case
	   fun test x = InlineT.Int32.fromLarge (InlineT.Word32.toLargeIntX x)
       *)
      val fromWord32 = InlineT.Int32.fromLarge o InlineT.Word32.toLargeIntX
    in

    fun finalInt scanFn getc cs = (case (scanFn false getc cs)
           of NONE => NONE
	    | (SOME{neg=true, word, rest}) =>
		if W.<(word, largestNegInt)
                  then SOME(InlineT.Int32.~(fromWord32 word), rest)
		else if W.<(largestNegInt, word)
                  then raise Overflow
                  else SOME(minInt, rest)
	    | (SOME{word, rest, ...}) =>
		if W.<(largestPosInt, word)
		  then raise Overflow
                  else SOME(fromWord32 word, rest)
          (* end case *))

    end (* local *)

    fun scanInt StringCvt.BIN = finalInt scanBin
      | scanInt StringCvt.OCT = finalInt scanOct
      | scanInt StringCvt.DEC = finalInt scanDec
      | scanInt StringCvt.HEX = finalInt scanHex

  end
