(* ieee-real.sml
 *
 * COPYRIGHT (c) 2022 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure IEEEReal : IEEE_REAL =
  struct

    exception Unordered

    open IEEERealTypes

    val ctlRoundingMode : int option -> int =
	    CInterface.c_function "SMLNJ-Math" "ctlRoundingMode"

    fun intToRM 0 = TO_NEAREST
      | intToRM 1 = TO_ZERO
      | intToRM 2 = TO_POSINF
      | intToRM 3 = TO_NEGINF
      | intToRM _ = raise Match (* shut up compiler *)

    fun setRoundingMode' m = (ctlRoundingMode (SOME m); ())

    fun setRoundingMode TO_NEAREST	= setRoundingMode' 0
      | setRoundingMode TO_ZERO		= setRoundingMode' 1
      | setRoundingMode TO_POSINF	= setRoundingMode' 2
      | setRoundingMode TO_NEGINF	= setRoundingMode' 3

    fun getRoundingMode () = intToRM (ctlRoundingMode NONE)

(* could implement this using FloatRep.toExact *)
    fun toString {class, sign, digits, exp} = let
	  fun fmtExp 0 = []
	    | fmtExp i = ["E", IntImp.toString i]
	  fun fmtDigits ([], tail) = tail
	    | fmtDigits (d::r, tail) =
	      (IntImp.toString d) :: fmtDigits(r, tail)
	  in
	    case (sign, class, digits)
	     of (true, ZERO, _) => "~0.0"
	      | (false, ZERO, _) => "0.0"
	      | (true, (NORMAL|SUBNORMAL), []) => "~0.0"
	      | (false, (NORMAL|SUBNORMAL), []) => "0.0"
	      | (true, (NORMAL|SUBNORMAL), _) =>
		  StringImp.concat("~0." :: fmtDigits(digits, fmtExp exp))
	      | (false, (NORMAL|SUBNORMAL), _) =>
		  StringImp.concat("0." :: fmtDigits(digits, fmtExp exp))
	      | (true, INF, _) => "~inf"
	      | (false, INF, _) => "inf"
	      | (_, NAN, []) => "nan"
	      | (_, NAN, _) =>
		  StringImp.concat("nan(" :: fmtDigits(digits, [")"]))
	    (* end case *)
	  end

    fun scan getc = let
          val scan' = StringToFRep.scan getc
          in
            fn cs => (case scan' cs
                 of SOME(frep, cs) => SOME(FloatRep.toDecimalApprox frep, cs)
                  | NONE => NONE
                (* end case *))
          end

  (* use "scan" to implement "fromString" *)
    fun fromString s = StringCvt.scanString scan s

  end;
