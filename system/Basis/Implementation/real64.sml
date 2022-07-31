(* real64.sml
 *
 * COPYRIGHT (c) 2022 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Real64Imp : REAL =
  struct

    structure I = InlineT.Int
    structure W = InlineT.Word
    structure W64 = InlineT.Word64
    structure Math = Math64

    infix 4 == !=
    type real = real

    fun *+(a:real,b,c) = a*b+c
    fun *-(a:real,b,c) = a*b-c

    val op == = InlineT.Real64.==
    val op != = InlineT.Real64.!=

    fun unordered(x:real,y) = Bool.not(x>y orelse x <= y)
    fun ?= (x, y) = (x == y) orelse unordered(x, y)

  (* maximum finite 64-bit real value *)
    val maxFinite = Real64Values.maxFinite
  (* minimum normalized positive real value *)
    val minNormalPos = Real64Values.minNormalPos
  (* minimum positive real value (denormalized) *)
    val minPos = Real64Values.minPos
  (* positive infinity *)
    val posInf = Real64Values.posInf
  (* negative infinity *)
    val negInf = Real64Values.negInf

    val radix = 2
    val precision = 53			(* hidden bit gets counted, too *)

  (* these functions are implemented in base/system/smlnj/init/pervasive.sml *)
    val floor = floor
    val ceil = ceil
    val trunc = trunc
    val round = round

  (* This is the IEEE double-precision maxint == 2^52 *)
    val maxInt = 4503599627370496.0

    local
    (* realround mode x returns x rounded to the nearest integer using the
     * given rounding mode.
     * May be applied to inf's and nan's.
     *)
      fun realround mode x = let
	    val saveMode = IEEEReal.getRoundingMode ()
	    in
	      IEEEReal.setRoundingMode mode;
	      (if x>=0.0 then x+maxInt-maxInt else x-maxInt+maxInt)
		before IEEEReal.setRoundingMode saveMode
	    end
    in
    val realFloor = realround IEEEReal.TO_NEGINF
    val realCeil = realround IEEEReal.TO_POSINF
    val realTrunc = realround IEEEReal.TO_ZERO
    val realRound = realround IEEEReal.TO_NEAREST
    end

    val abs : real -> real = InlineT.Real64.abs
    val fromInt : int -> real = InlineT.Real64.from_int

    fun toInt IEEEReal.TO_NEGINF = floor
      | toInt IEEEReal.TO_POSINF = ceil
      | toInt IEEEReal.TO_ZERO = trunc
      | toInt IEEEReal.TO_NEAREST = round

    fun toLarge x = x
    fun fromLarge _ x = x

    fun sign x = if (x < 0.0) then ~1
	  else if (x > 0.0) then 1
	  else if (x != x) then raise Domain
	  else 0
    val signBit : real -> bool = InlineT.Real64.signBit

    fun sameSign (x, y) = signBit x = signBit y

    fun copySign (x, y) = (* may not work if x is Nan *)
           if sameSign(x,y) then x else ~x

    fun compare (x, y) =
	  if x<y then General.LESS
	  else if x>y then General.GREATER
	  else if x == y then General.EQUAL
	  else raise IEEEReal.Unordered

    fun compareReal (x, y) =
	  if x<y then IEEEReal.LESS
	  else if x>y then IEEEReal.GREATER
	  else if x == y then IEEEReal.EQUAL
	  else IEEEReal.UNORDERED

  (* classification of IEEE reals *)

    fun isFinite x = negInf < x andalso x < posInf
    fun isNan x = (x != x)

    fun isNormal x = let
	  val biasExp = W.andb(
		W.fromLarge(W64.rshiftl(InlineT.Real64.toBits x, 0w52)),
		0wx7ff)
	  in
	    (0w0 < biasExp) andalso (biasExp < 0w2047)
	  end

    fun class x = let (* does not distinguish between quiet and signalling NaN *)
	  val bits = InlineT.Real64.toBits x
	  in
	    if (W64.andb(0wx7fffffffffffffff, bits) = 0w0)
	      then IEEEReal.ZERO
	    else let
	      val signAndExp = W.fromLarge(W64.rshiftl(bits, 0w52))
	      in
		case W.andb(signAndExp, 0wx7ff)
		 of 0w0 => IEEEReal.SUBNORMAL
		  | 0w2047 => if (W64.andb(0wxfffffffffffff, bits) = 0w0)
		      then IEEEReal.INF
		      else IEEEReal.NAN
		  | _ => IEEEReal.NORMAL
		(* end case *)
	      end
	  end

    fun toManExp x = let
	  val bits = InlineT.Real64.toBits x
	  in
	    if (W64.andb(0wx7fffffffffffffff, bits) = 0w0)
	      then {man = x, exp = 0} (* +/- zero *)
	      else (case W.andb(W.fromLarge(W64.rshiftl(bits, 0w52)), 0wx7ff)
		 of 0w0 => let (* subnormal *)
		      val {man, exp} = toManExp(1048576.0 * x)
		      in
			{man = man, exp = exp - 20}
		      end
		  | 0w2047 => {man = x, exp = 0}	(* both NaNs and infinities *)
		  | biasExp => let
		      val exp = W.toIntX biasExp - 1022
		      in
			{man = Assembly.A.scalb(x, ~exp), exp=exp}
		      end
		(* end case *))
	  end

    fun fromManExp {man=m, exp=e:int} = let
        (* is `x` a zero, nan, or infinity? *)
          fun isSpecial x = let
                val bits = InlineT.Real64.toBits x
                in
                  (W64.andb(0wx7fffffffffffffff, bits) = 0w0)
                  orelse (W.andb(W.fromLarge(W64.rshiftl(bits, 0w52)), 0wx7ff) = 0w2047)
                end
        (* iterative implementation *)
          fun fromManExp' (m, e) =
                if (m >= 0.5 andalso m <= 1.0 orelse m <= ~0.5 andalso m >= ~1.0)
                  then if e > 1020
                    then if e > 1050 then if m>0.0 then posInf else negInf
                         else let fun f(i,x) = if i=0 then x else f(i-1,x+x)
                                 in f(e-1020,  Assembly.A.scalb(m,1020))
                                end
                    else if e < ~1020
                         then if e < ~1200 then 0.0
                           else let fun f(i,x) = if i=0 then x else f(i-1, x*0.5)
                                 in f(1020-e, Assembly.A.scalb(m, ~1020))
                                end
                         else Assembly.A.scalb(m,e)  (* This is the common case! *)
                  else let
                    val {man=m', exp=e'} = toManExp m
                    in
                      fromManExp' (m', e'+ e)
                    end
          in
            if isSpecial m
              then m
              else fromManExp' (m, e)
          end

  (* the conversion between reals and IntInf.int are target dependent *)
    val fromLargeInt = IntInfToReal64.cvt
    fun toLargeInt mode x = Real64ToIntInf.cvt (mode, x)

  (* whole and split could be implemented more efficiently if we had
   * control over the rounding mode; but for now we don't.
   *)
    fun whole x = if x>0.0
	    then if x > 0.5
	      then x-0.5+maxInt-maxInt
	      else whole(x+1.0)-1.0
	  else if x<0.0
	    then if x < ~0.5
	      then x+0.5-maxInt+maxInt
	      else whole(x-1.0)+1.0
	    else x

    fun split x = let
	  val w = whole x
	  val f = x-w
	  in
	    if abs f == 1.0
	      then {whole=w+f, frac=0.0}
	      else {whole=w, frac=f}
	  end

    fun realMod x = let
	  val f = x - whole x
	  in
	    if abs f == 1.0 then 0.0 else f
	  end

    fun rem(x,y) = y * #frac(split(x/y))

    fun checkFloat x = if x>negInf andalso x<posInf then x
                       else if isNan x then raise General.Div
			 else raise General.Overflow

    fun nextAfter (r, t) = if (r == t) then r
	  else if isNan r then r
	  else if isNan t then t
	  else if not (isFinite r) then r
	  else if (r < t)
	    then raise Fail "Real.nextAfter unimplemented"  (* next biggest value *)
	    else raise Fail "Real.nextAfter unimplemented"  (* next smallest value *)

    val min : real * real -> real = InlineT.Real64.min
    val max : real * real -> real = InlineT.Real64.max

    fun toDecimal _ = raise Fail "Real.toDecimal unimplemented"
    fun fromDecimal _ = raise Fail "Real.fromDecimal unimplemented"

    val fmt = RealFormat.fmtReal
    val toString = fmt (StringCvt.GEN NONE)
    val scan = RealScan.scanReal
    val fromString = StringCvt.scanString scan

    val ~ = InlineT.Real64.~
    val op +  = InlineT.Real64.+
    val op -  = InlineT.Real64.-
    val op *  = InlineT.Real64.*
    val op /  = InlineT.Real64./

    val op >  = InlineT.Real64.>
    val op <  = InlineT.Real64.<
    val op >= = InlineT.Real64.>=
    val op <= = InlineT.Real64.<=

  end (* Real64 *)
