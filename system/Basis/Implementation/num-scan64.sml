(* num-scan64.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * The string conversion for 64-bit int and word types.
 * These are used for fixed-precision types that have more than 32 bits.
 *)

structure NumScan64 : sig

    val scanWord : StringCvt.radix
	  -> (char, 'a) StringCvt.reader -> (Word64.word, 'a) StringCvt.reader
    val scanInt  : StringCvt.radix
	  -> (char, 'a) StringCvt.reader -> (Int64.int, 'a) StringCvt.reader

  end = struct

    structure W = InlineT.Word64
    structure I = InlineT.Int64
    structure U = ScanUtil

    type word64 = Word64.word

    val toWord64 = InlineT.Word.toWord64

    val largestWordDiv10 : word64 = 0w1844674407370955161	(* 2^64-1 divided by 10 *)
    val largestWordMod10 : word64 = 0w5				(* remainder *)

    val largestNegInt : word64 = 0wx8000000000000000
    val largestPosInt : word64 = 0wx7fffffffffffffff
    val minInt64 : Int64.int = ~9223372036854775808

  (* for power of 2 bases (2, 8 & 16), we can check for overflow by looking
   * at the hi (1, 3 or 4) bits.
   *)
    fun chkOverflow mask w =
	  if (W.andb(mask, w) = 0w0) then () else raise Overflow

    fun scanBin isWord getc cs = (case (U.scanPrefix (U.binPat isWord) getc cs)
	   of NONE => NONE
	    | (SOME{neg, next, rest}) => let
		val chkOverflow = chkOverflow largestNegInt
		fun cvt (w, rest) = (case (getc rest)
		       of NONE => SOME{neg=neg, word=w, rest=rest}
			| SOME(c, rest') => let val d = U.code c
			    in
			      if (d <= 0w1)
				then (
				  chkOverflow w;
				  cvt(W.+(W.lshift(w, 0w1), toWord64 d), rest'))
				else SOME{neg=neg, word=w, rest=rest}
			    end
		      (* end case *))
		in
		  cvt (toWord64 next, rest)
		end
	  (* end case *))

    fun scanOct isWord getc cs = (case (U.scanPrefix (U.octPat isWord) getc cs)
	   of NONE => NONE
	    | (SOME{neg, next, rest}) => let
		val chkOverflow = chkOverflow 0wxE000000000000000
		fun cvt (w, rest) = (case (getc rest)
		       of NONE => SOME{neg=neg, word=w, rest=rest}
			| SOME(c, rest') => let val d = U.code c
			    in
			      if (d <= 0w7)
				then (
				  chkOverflow w;
				  cvt(W.+(W.lshift(w, 0w3), toWord64 d), rest'))
				else SOME{neg=neg, word=w, rest=rest}
			    end
		      (* end case *))
		in
		  cvt (toWord64 next, rest)
		end
	  (* end case *))

    fun scanDec isWord getc cs = (case (U.scanPrefix (U.decPat isWord) getc cs)
	   of NONE => NONE
	    | (SOME{neg, next, rest}) => let
		fun cvt (w : word64, rest) = (case (getc rest)
		       of NONE => SOME{neg=neg, word=w, rest=rest}
			| SOME(c, rest') => let val d = U.code c
			    in
			      if (d <= 0w9)
				then (
				  if (W.>=(w, largestWordDiv10)
				  andalso (W.<(largestWordDiv10, w)
				    orelse W.<(largestWordMod10, toWord64 d)))
				    then raise Overflow
				    else ();
				  cvt (W.+(W.*(0w10, w), toWord64 d), rest'))
				else SOME{neg=neg, word=w, rest=rest}
			    end
		      (* end case *))
		in
		  cvt (toWord64 next, rest)
		end
	  (* end case *))

    fun scanHex isWord getc cs = (case (U.scanPrefix (U.hexPat isWord) getc cs)
	   of NONE => NONE
	    | (SOME{neg, next, rest}) => let
		val chkOverflow = chkOverflow 0wxF000000000000000
		fun cvt (w, rest) = (case (getc rest)
		       of NONE => SOME{neg=neg, word=w, rest=rest}
			| SOME(c, rest') => let val d = U.code c
			    in
			      if (d <= 0w15)
				then (
				  chkOverflow w;
				  cvt(W.+(W.lshift(w, 0w4), toWord64 d), rest'))
				else SOME{neg=neg, word=w, rest=rest}
			    end
		      (* end case *))
		in
		  cvt (toWord64 next, rest)
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
      val fromWord64 = InlineT.Int64.fromLarge o InlineT.Word64.toLargeIntX
    in

    fun finalInt scanFn getc cs = (case (scanFn false getc cs)
           of NONE => NONE
	    | (SOME{neg=true, word, rest}) =>
		if W.<(word, largestNegInt)
                  then SOME(InlineT.Int64.~(fromWord64 word), rest)
		else if W.<(largestNegInt, word)
                  then raise Overflow
                  else SOME(minInt64, rest)
	    | (SOME{word, rest, ...}) =>
		if W.<(largestPosInt, word)
		  then raise Overflow
                  else SOME(fromWord64 word, rest)
          (* end case *))

    end (* local *)

    fun scanInt StringCvt.BIN = finalInt scanBin
      | scanInt StringCvt.OCT = finalInt scanOct
      | scanInt StringCvt.DEC = finalInt scanDec
      | scanInt StringCvt.HEX = finalInt scanHex

  end
