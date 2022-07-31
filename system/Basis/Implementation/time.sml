(* time.sml
 *
 * COPYRIGHT (c) 2007 The Fellowship of SML/NJ (http://smlnj.org)
 * All rights reserved.
 *)

structure TimeImp : sig

    include TIME

  (* export these for the benefit of, e.g., Posix.ProcEnv.times: *)
    val fractionsPerSecond : LargeInt.int
    val toFractions   : time -> LargeInt.int
    val fromFractions : LargeInt.int -> time

  end = struct

    structure PB = PreBasis
    structure LInt = LargeIntImp
    structure Real = RealImp
    structure Int = IntImp
    structure Int32 = Int32Imp
    structure String = StringImp

  (* get time type from type-only structure *)
    open Time

    exception Time

    infix quot
    val op quot = LInt.quot

    val zeroTime = PB.TIME { usec = 0 }

    val fractionsPerSecond : LargeInt.int = 1000000
    fun toFractions (PB.TIME { usec }) = usec
    fun fromFractions usec = (PB.TIME { usec = usec })

  (* rounding is towards ZERO *)
    fun toSeconds (PB.TIME { usec }) = usec quot 1000000
    fun fromSeconds sec = PB.TIME { usec = sec * 1000000 }
    fun toMilliseconds (PB.TIME { usec }) = usec quot 1000
    fun fromMilliseconds msec = PB.TIME { usec = msec * 1000 }
    fun toMicroseconds (PB.TIME { usec }) = usec
    fun fromMicroseconds usec = PB.TIME { usec = usec }
    fun toNanoseconds (PB.TIME { usec }) = usec * 1000
    fun fromNanoseconds nsec = PB.TIME { usec = nsec quot 1000 }

    fun fromReal rsec = PB.TIME{ usec = Real.toLargeInt IEEEReal.TO_ZERO (rsec * 1.0e6) }
    fun toReal (PB.TIME{usec}) = Real.fromLargeInt usec * 1.0e~6

    local
      val gettimeofday : unit -> Word64.word =
	    CInterface.c_function "SMLNJ-Time" "timeofday"
    in
    fun now () = fromNanoseconds (Word64Imp.toLargeInt (gettimeofday ()))
    end (* local *)

    val rndv : LInt.int vector =  #[50000, 5000, 500, 50, 5]

    fun fmt' (prec, PB.TIME{ usec }) = let
	  val (neg, usec) = if usec < 0 then (true, ~usec) else (false, usec)
	  fun fmtInt i = LInt.fmt StringCvt.DEC i
	  fun fmtSec (true, i) = "~" ^ fmtInt i
	    | fmtSec (false, i) = fmtInt i
	  fun isEven i = LInt.rem (i, 2) = 0
	  in
	    if prec = 0
	      then let
		val (sec, usec) = IntInfImp.quotRem (usec, 1000000)
		val sec = (case LInt.compare (usec, 500000)
		       of LESS => sec
			| GREATER => sec + 1
			| EQUAL => if isEven sec then sec else sec + 1
		      (* end  case *))
		in
		  fmtSec (neg, sec)
		end
	    else if prec >= 6
	      then let
	        val (sec, usec) = IntInfImp.quotRem (usec, 1000000)
		in
		  concat [
		      fmtSec (neg, sec), ".",
		      StringCvt.padLeft #"0" 6 (fmtInt usec),
		      StringCvt.padLeft #"0" (prec - 6) ""
		    ]
		end
	      else let
		val rnd = Vector.sub (rndv, prec - 1)
		val (whole, frac) = IntInfImp.quotRem (usec, 2 * rnd)
		val whole = (case LInt.compare (frac, rnd)
		       of LESS => whole
			| GREATER => whole + 1
			| EQUAL => if isEven whole then whole else whole + 1
		      (* end  case *))
		val rscl = 2 * Vector.sub (rndv, 5 - prec)
		val (sec, frac) = IntInfImp.quotRem (whole, rscl)
		in
		  concat [fmtSec (neg, sec), ".", StringCvt.padLeft #"0" prec (fmtInt frac)]
		end
	  end

    fun fmt prec = if (prec < 0) then raise Size else (fn t => fmt'(prec, t))


  (* scan a time value; this has the syntax:
   *
   *  [+-~]?([0-9]+(.[0-9]+)? | .[0-9]+)
   *)
    fun scan getc s = let

	fun digv c = Int.toLarge (Char.ord c - Char.ord #"0")

	fun whole s = let
	    fun loop (s, n, m, ret) =
		case getc s of
		    NONE => ret (n, s, m)
		  | SOME (c, s') =>
		      if Char.isDigit c then
			  loop (s', 10 * n + digv c, m + 1, SOME)
		      else ret (n, s, m)
	in
	    loop (s, 0, 0, fn _ => NONE)
	end

	fun time (negative, s) = let
	    fun pow10 p = IntInfImp.pow (10, p)
	    fun return (usec, s) =
		SOME (fromMicroseconds (if negative then ~usec else usec), s)
	    fun fractional (wh, s) =
		case whole s of
		    SOME (n, s, m) => let
			fun done fr = return (wh * 1000000 + fr, s)
		    in
			if m > 6 then done (n div pow10 (m - 6))
			else if m < 6 then done (n * pow10 (6 - m))
			else done n
		    end
		  | NONE => NONE
	    fun withwhole s =
		case whole s of
		    NONE => NONE
		  | SOME (wh, s', _) =>
		      (case getc s' of
			   SOME (#".", s'') => fractional (wh, s'')
			 | _ => return (wh * 1000000, s'))
	in
	    case getc s of
		NONE => NONE
	      | SOME (#".", s') => fractional (0, s')
	      | _ => withwhole s
	end

	fun sign s =
	    case getc s of
		NONE => NONE
	      | SOME ((#"-" | #"~"), s') => time (true, s')
	      | SOME (#"+", s') => time (false, s')
	      | _ => time (false, s)
    in
	sign (StringCvt.skipWS getc s)
    end

    val toString   = fmt 3
    val fromString = PB.scanString scan

    local
	fun binop usecoper (PB.TIME t1, PB.TIME t2) =
	    usecoper (#usec t1, #usec t2)
    in

    val op + = binop (fromMicroseconds o op +)
    val op - = binop (fromMicroseconds o op -)
    val compare = binop LInt.compare
    val op < = binop op <
    val op <= = binop op <=
    val op > = binop op >
    val op >= = binop op >=

    end

  end (* TIME *)
