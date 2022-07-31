(* format.sml
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * AUTHOR:  John Reppy
 *	    AT&T Bell Laboratories
 *	    Murray Hill, NJ 07974
 *	    jhr@research.att.com
 *
 * TODO
 *   - field widths in scan
 *   - add PREC of (int * fmt_item) constructor to allow dynamic control of
 *     precision.
 *   - precision in %d, %s, ...
 *   - * flag in scan (checks, but doesn't scan input)
 *   - %n specifier in scan
 *)

structure Format : FORMAT =
  struct

    structure SS = Substring
    structure SC = StringCvt

    open FmtFields

    exception BadFmtList

    fun padLeft (str, pad) = SC.padLeft #" " pad str
    fun padRight (str, pad) = SC.padRight #" " pad str
    fun zeroLPad (str, pad) = SC.padLeft #"0" pad str
    fun zeroRPad (str, pad) = SC.padRight #"0" pad str

  (* int to string conversions (for positive integers only) *)
    local
      val (maxInt8, maxInt10, maxInt16) = (case LargeInt.maxInt
	     of (SOME n) => let
		  val maxP1 = LargeWord.fromLargeInt n + 0w1
		  in
		    ( LargeWord.fmt SC.OCT maxP1,
		      LargeWord.fmt SC.DEC maxP1,
		      LargeWord.fmt SC.HEX maxP1
		    )
		  end
	      | NONE => ("", "", "")
	    (* end case *))
    in
  (* MaxInt is used to represent the absolute value of the largest negative
   * representable integer.
   *)
    datatype posint = PosInt of LargeInt.int | MaxInt
    fun intToOctal MaxInt = maxInt8
      | intToOctal (PosInt i) = LargeInt.fmt SC.OCT i
    fun intToStr MaxInt = maxInt10
      | intToStr (PosInt i) = LargeInt.toString i
    fun intToHex MaxInt = maxInt16
      | intToHex (PosInt i) = LargeInt.fmt SC.HEX i
    fun intToHeX i = String.map Char.toUpper (intToHex i)
    end (* local *)

  (* word to string conversions *)
    val wordToOctal = LargeWord.fmt SC.OCT
    val wordToStr = LargeWord.fmt SC.DEC
    val wordToHex = LargeWord.fmt SC.HEX
    fun wordToHeX i = String.map Char.toUpper (wordToHex i)

    fun compileFormat str = let
	  val split = SS.splitl (fn #"%" => false | _ => true)
	  fun scan (ss, l) =
		if (SS.isEmpty ss)
		  then rev l
		  else let val (ss1, ss2) = split ss
		    in
		      case (SS.getc ss2)
		       of (SOME(#"%", ss')) => let val (field, ss3) = scanField ss'
			    in
			      scan(ss3, field::(Raw ss1)::l)
			    end
			| _ => rev((Raw ss1)::l)
		      (* end case *)
		    end
	  in
	    scan (SS.full str, [])
	  end

    fun format s = let
	  val fmts = compileFormat s
	  fun doField (flags : field_flags, wid, ty, arg) = let
		fun padFn s = (case wid
		       of NoPad => s
			| Wid i => padLeft(s, i)
		      (* end case *))
		fun zeroPadFn (sign, s) = (case wid
		       of NoPad => raise BadFormat
			| (Wid i) => zeroLPad(s, i - (String.size sign))
		      (* end case *))
		fun trimFn (NONE, s) = padFn s
		  | trimFn (SOME maxWid, s) = let
		      val s = if (size s > maxWid)
			    then String.substring(s, 0, maxWid)
			    else s
		      in
			padFn s
		      end
		fun negate i = ((PosInt(~i)) handle _ => MaxInt)
		fun doSign i = (case (i < 0, #sign flags, #neg_char flags)
		       of (false, AlwaysSign, _) => ("+", PosInt i)
			| (false, BlankSign, _) => (" ", PosInt i)
			| (false, _, _) => ("", PosInt i)
			| (true, _, TildeSign) => ("~", negate i)
			| (true, _, _) => ("-", negate i)
		      (* end case *))
		fun doRealSign sign = (case (sign, #sign flags, #neg_char flags)
		       of (false, AlwaysSign, _) => "+"
			| (false, BlankSign, _) => " "
			| (false, _, _) => ""
			| (true, _, TildeSign) => "~"
			| (true, _, _) => "-"
		      (* end case *))
		fun doExpSign (exp, isCap) = let
		      val e = if isCap then "E" else "e"
		      fun mkExp e = zeroLPad(Int.toString e, 2)
		      in
			case (exp < 0, #neg_char flags)
			 of (false, _) => [e, mkExp exp]
			  | (true, TildeSign) => [e, "~", mkExp(~exp)]
			  | (true, _) => [e, "-", mkExp(~exp)]
			(* end case *)
		      end
		fun octal i = let
		      val (sign, i) = doSign i
		      val sign = if (#base flags) then sign^"0" else sign
		      val s = intToOctal i
		      in
		        if (#zero_pad flags)
			  then sign ^ zeroPadFn(sign, s)
			  else padFn (sign ^ s)
		      end
		fun decimal i = let
		      val (sign, i) = doSign i
		      val s = intToStr i
		      in
			if (#zero_pad flags)
			  then sign ^ zeroPadFn(sign, s)
		          else padFn (sign ^ s)
		      end
		fun hexidecimal i = let
		      val (sign, i) = doSign i
		      val sign = if (#base flags) then sign^"0x" else sign
		      val s = intToHex i
		      in
		        if (#zero_pad flags)
			  then sign ^ zeroPadFn(sign, s)
			  else padFn (sign ^ s)
		      end
	        fun capHexidecimal i = let
		      val (sign, i) = doSign i
		      val sign = if (#base flags) then sign^"0X" else sign
		      val s = intToHeX i
		      in
		        if (#zero_pad flags)
			  then sign ^ zeroPadFn(sign, s)
			  else padFn (sign ^ s)
		      end
	      (* word formatting *)
		fun doWordSign () = (case (#sign flags)
		       of AlwaysSign => "+"
			| BlankSign => " "
			| _ => ""
		      (* end case *))
		fun octalW i = let
		      val sign = doWordSign ()
		      val sign = if (#base flags) then sign^"0" else sign
		      val s = wordToOctal i
		      in
		        if (#zero_pad flags)
			  then sign ^ zeroPadFn(sign, s)
			  else padFn (sign ^ s)
		      end
		fun decimalW i = let
		      val sign = doWordSign ()
		      val s = wordToStr i
		      in
			if (#zero_pad flags)
			  then sign ^ zeroPadFn(sign, s)
		          else padFn (sign ^ s)
		      end
		fun hexidecimalW i = let
		      val sign = doWordSign ()
		      val sign = if (#base flags) then sign^"0x" else sign
		      val s = wordToHex i
		      in
		        if (#zero_pad flags)
			  then sign ^ zeroPadFn(sign, s)
			  else padFn (sign ^ s)
		      end
	        fun capHexidecimalW i = let
		      val sign = doWordSign ()
		      val sign = if (#base flags) then sign^"0X" else sign
		      val s = wordToHeX i
		      in
		        if (#zero_pad flags)
			  then sign ^ zeroPadFn(sign, s)
			  else padFn (sign ^ s)
		      end
		in
		  case (ty, arg)
		   of (OctalField, LINT i) => octal i
		    | (OctalField, INT i) => octal(Int.toLarge i)
		    | (OctalField, WORD w) => octalW (Word.toLargeWord w)
		    | (OctalField, LWORD w) => octalW w
		    | (OctalField, WORD8 w) => octalW (Word8.toLargeWord w)
		    | (IntField, LINT i) => decimal i
		    | (IntField, INT i) => decimal(Int.toLarge i)
		    | (IntField, WORD w) => decimalW (Word.toLargeWord w)
		    | (IntField, LWORD w) => decimalW w
		    | (IntField, WORD8 w) => decimalW (Word8.toLargeWord w)
		    | (HexField, LINT i) => hexidecimal i
		    | (HexField, INT i) => hexidecimal(Int.toLarge i)
		    | (HexField, WORD w) => hexidecimalW (Word.toLargeWord w)
		    | (HexField, LWORD w) => hexidecimalW w
		    | (HexField, WORD8 w) => hexidecimalW (Word8.toLargeWord w)
		    | (CapHexField, LINT i) => capHexidecimal i
		    | (CapHexField, INT i) => capHexidecimal(Int.toLarge i)
		    | (CapHexField, WORD w) => capHexidecimalW (Word.toLargeWord w)
		    | (CapHexField, LWORD w) => capHexidecimalW w
		    | (CapHexField, WORD8 w) => capHexidecimalW (Word8.toLargeWord w)
		    | (CharField, CHR c) => padFn(String.str c)
		    | (BoolField, BOOL false) => padFn "false"
		    | (BoolField, BOOL true) => padFn "true"
		    | (StrField prec, ATOM s) => trimFn(prec, Atom.toString s)
		    | (StrField prec, STR s) => trimFn(prec, s)
		    | (RealField{prec, format}, REAL r) =>
			if (Real.isFinite r)
			  then (case format
			     of F_Format => let
				  val {sign, mantissa} =
					RealFormat.realFFormat(r, prec)
				  val sign = doRealSign sign
				  in
				    if ((prec = 0) andalso (#base flags))
				      then padFn(concat[sign, mantissa, "."])
				      else padFn(sign ^ mantissa)
				  end
			      | E_Format isCap => let
				  val {sign, mantissa, exp} =
					RealFormat.realEFormat(r, prec)
				  val sign = doRealSign sign
				  val expStr = doExpSign(exp, isCap)
				  in
				    if ((prec = 0) andalso (#base flags))
				      then padFn(concat(sign :: mantissa :: "."
					:: expStr))
				      else padFn(concat(sign :: mantissa :: expStr))
				  end
			      | G_Format isCap => let
				  val prec = if (prec = 0) then 1 else prec
				  val {sign, whole, frac, exp} =
					RealFormat.realGFormat(r, prec)
				  val sign = doRealSign sign
				  val expStr = (case exp
					 of SOME e => doExpSign(e, isCap)
					  | NONE => []
					(* end csae *))
				  val num = if (#base flags)
					then let
					  val diff =
						prec - ((size whole) + (size frac))
					  in
					    if (diff > 0)
					      then zeroRPad(frac, (size frac)+diff)
					      else frac
					  end
					else if (frac = "")
					  then ""
					  else ("." ^ frac)
				  in
				    padFn(concat(sign::whole::num::expStr))
				  end
			    (* end case *))
			else if Real.==(Real.negInf, r)
			  then doRealSign true ^ "inf"
			else if Real.==(Real.posInf, r)
			  then doRealSign false ^ "inf"
			  else "nan"
		    | (_, LEFT(w, arg)) =>
			StringCvt.padLeft #" " w (doField (flags, wid, ty, arg))
		    | (_, RIGHT(w, arg)) =>
			StringCvt.padRight #" " w (doField (flags, Wid w, ty, arg))
		    | _ => raise BadFmtList
		  (* end case *)
		end
	  fun doArgs ([], [], l) = SS.concat(rev l)
	    | doArgs ((Raw s)::rf, args, l) = doArgs(rf, args, s::l)
	    | doArgs (Field(flags, wid, ty)::rf, arg::ra, l) =
		doArgs (rf, ra, SS.full (doField (flags, wid, ty, arg)) :: l)
	    | doArgs _ = raise BadFmtList
	  in
	    fn args => doArgs (fmts, args, [])
	  end (* format *)

    fun formatf fmt = let
	  val f = format fmt
	  in
	    fn consumer => fn args => consumer(f args)
	  end

  end (* Format *)
