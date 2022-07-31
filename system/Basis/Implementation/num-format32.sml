(* num-format32.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * The word to string conversion for the largest word and int types.
 * These are used for fixed-precision types that have 32 or fewer bits.
 *)

structure NumFormat32 : sig

    val fmtWord : StringCvt.radix -> Word32.word -> string
    val fmtInt  : StringCvt.radix -> Int32.int -> string

  end = struct

    structure W = InlineT.Word32
    structure I = InlineT.Int32

    val iadd = InlineT.Int.fast_add

    fun mkDigit (w : Word32.word) =
	  InlineT.CharVector.sub("0123456789ABCDEF", W.toInt w)

    fun wordToBin w = let
	  fun mkBit w = if (W.andb(w, 0w1) = 0w0) then #"0" else #"1"
	  fun f (0w0, n, l) = (iadd(n, 1), #"0" :: l)
	    | f (0w1, n, l) = (iadd(n, 1), #"1" :: l)
	    | f (w, n, l) = f(W.rshiftl(w, 0w1), iadd(n, 1), (mkBit w) :: l)
	  in
	    f (w, 0, [])
	  end
    fun wordToOct w = let
	  fun f (w, n, l) = if (w < 0w8)
		then (iadd(n, 1), (mkDigit w) :: l)
		else f(W.rshiftl(w, 0w3), iadd(n, 1), mkDigit(W.andb(w, 0wx7)) :: l)
	  in
	    f (w, 0, [])
	  end
    fun wordToDec w = let
	  fun f (w, n, l) = if (w < 0w10)
		then (iadd(n, 1), (mkDigit w) :: l)
		else let val j = w div 0w10
		  in
		    f (j,  iadd(n, 1), mkDigit(w - 0w10*j) :: l)
		  end
	  in
	    f (w, 0, [])
	  end
    fun wordToHex w = let
	  fun f (w, n, l) = if (w < 0w16)
		then (iadd(n, 1), (mkDigit w) :: l)
		else f(W.rshiftl(w, 0w4), iadd(n, 1), mkDigit(W.andb(w, 0wxf)) :: l)
	  in
	    f (w, 0, [])
	  end

    fun fmtW StringCvt.BIN = wordToBin
      | fmtW StringCvt.OCT = wordToOct
      | fmtW StringCvt.DEC = wordToDec
      | fmtW StringCvt.HEX = wordToHex

    fun fmtWord radix = PreString.implode o (fmtW radix)

    local
      val i2w = W.fromLargeInt o I.toLarge

    (* signed string representation of minInt; these could be computed as
     * follows, but that creates a circularity.
     *
     *  val minInt = W.lshift(0w1, Word.fromInt(W.wordSize - 1))
     *  val minIntBin = IntInf.fmt StringCvt.BIN (W.toLargeIntX minInt)
     *  val minIntOct = IntInf.fmt StringCvt.OCT (W.toLargeIntX minInt)
     *  val minIntDec = IntInf.fmt StringCvt.DEC (W.toLargeIntX minInt)
     *  val minIntHex = IntInf.fmt StringCvt.HEX (W.toLargeIntX minInt)
     *)
      val minInt = W.lshift(0w1, 0w31)
      val minIntBin = "~10000000000000000000000000000000"
      val minIntOct = "~20000000000"
      val minIntDec = "~2147483648"
      val minIntHex = "~80000000"

    in
    fun fmtInt radix = let
	(* format using the appropriate word formatter *)
	  fun fmt (fmtW, minIntStr) i = if i2w i = minInt
		then minIntStr
		else let
		    val w32 = i2w (if (i < 0) then ~i else i)
		    val (n, digits) = fmtW w32
		  in
		    if (i < 0) then PreString.implode(iadd(n,1), #"~"::digits)
		    else PreString.implode(n, digits)
		  end
	  in
	    case radix
	     of StringCvt.BIN => fmt (wordToBin, minIntBin)
	      | StringCvt.OCT => fmt (wordToOct, minIntOct)
	      | StringCvt.DEC => fmt (wordToDec, minIntDec)
	      | StringCvt.HEX => fmt (wordToHex, minIntHex)
	    (* end case *)
	  end
    end (* local *)

  end
