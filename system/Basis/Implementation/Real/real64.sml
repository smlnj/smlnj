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

(* TODO: these should be defined in InlineT *)
    fun *+ (a:real,b,c) = a*b+c
    fun *- (a:real,b,c) = a*b-c

(* TODO: this function should be defined in InlineT *)
    (* bitcast a Word64.word to a Real64.real *)
    fun fromBits (b : Word64.word) : real = let
          val r : real ref = InlineT.cast(ref b)
          in
            !r
          end
    val toBits = InlineT.Real64.toBits

    val op == = InlineT.Real64.==
    val op != = InlineT.Real64.!=

    fun unordered (x:real,y) = Bool.not(x>y orelse x <= y)
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

    (* some useful constants *)
    val nExpBits = 0w11                 (* # of exponent bits in double-precision real *)
    val nFracBits = 0w52                (* # of stored fractional (aka mantissa) bits *)
    val bias = 1023                     (* exponent bias for normalized numbers *)
    val signBitMask = W64.lshift(0w1, 0w63)
    val fracMask = W64.lshift(0w1, nFracBits) - 0w1
    val expMask = W64.lshift(0w1, nExpBits) - 0w1
    val expAndFracMask = W64.notb signBitMask

    (* bit representation of positive infinity *)
    val infBits = W64.lshift(expMask, nFracBits)

    val radix = 2
    val precision = W.toIntX nFracBits + 1      (* hidden bit gets counted, too *)

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
		W.fromLarge(W64.rshiftl(toBits x, 0w52)),
		0wx7ff)
	  in
	    (0w0 < biasExp) andalso (biasExp < 0w2047)
	  end

    fun class x = let (* does not distinguish between quiet and signalling NaN *)
	  val bits = toBits x
	  in
	    if (W64.andb(expAndFracMask, bits) = 0w0)
	      then IEEEReal.ZERO
	    else let
	      val signAndExp = W.fromLarge(W64.rshiftl(bits, nFracBits))
	      in
		case W.andb(signAndExp, W.fromLarge expMask)
		 of 0w0 => IEEEReal.SUBNORMAL
		  | 0w2047 => if (W64.andb(fracMask, bits) = 0w0)
		      then IEEEReal.INF
		      else IEEEReal.NAN
		  | _ => IEEEReal.NORMAL
		(* end case *)
	      end
	  end

    fun toManExp x = let
	  val bits = toBits x
	  in
	    if (W64.andb(expAndFracMask, bits) = 0w0)
	      then {man = x, exp = 0} (* +/- zero *)
	      else (case W.andb(W.fromLarge(W64.rshiftl(bits, nFracBits)), W.fromLarge expMask)
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
                val bits = toBits x
                in
                  (W64.andb(expAndFracMask, bits) = 0w0)
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

  (* split a real number into whole and fractional parts *)
    fun split r = let
          val bits = toBits r
          val sign = W64.andb(bits, signBitMask) <> 0w0
          val expBits = W64.andb(W64.rshiftl(bits, nFracBits), expMask)
          val fracBits = W64.andb(bits, fracMask)
          val exp = W.toIntX(W.fromLarge expBits) - bias
          fun zero () = if sign then ~0.0 else 0.0
          in
            if (exp < W.toIntX nFracBits)
              then if (exp < 0)
                (* abs(r) < 1 *)
                then {whole = zero(), frac = r}
                else let
                  (* mask for fractional bits *)
                  val mask = W64.rshiftl(0wx000fffffffffffff, W.fromInt exp)
                  in
                    if (W64.andb(mask, bits) = 0w0)
                      (* `r` is integral *)
                      then {whole = r, frac = zero()}
                      else let
                        val whole = fromBits (W64.andb(bits, W64.notb mask))
                        in
                          {whole = whole, frac = r - whole}
                        end
                  end
            else if (expBits < 0wx7ff)
              (* no fractional part *)
              then {whole = r, frac = zero()}
            else if (fracBits = 0w0)
              (* Infinity *)
              then {whole = r, frac = zero{}}
              (* NaN *)
              else {whole = r, frac = r}
          end

    fun realMod x = #frac (split x)

    fun rem(x,y) = y * #frac(split(x/y))

    fun checkFloat x = if x>negInf andalso x<posInf then x
                       else if isNan x then raise General.Div
			 else raise General.Overflow

    fun nextAfter (r, t) = if (r == t)
            then r
          else let
            (* split the bits of an IEEE double into sign, exp, and fractional parts *)
            fun split x = let
                  val bits = toBits x
                  val sign = W64.andb(signBitMask, bits)
                  val exp = W64.andb(W64.rshiftl(bits, nFracBits), expMask)
                  val frac = W64.andb(bits, fracMask)
                  in
                    (bits, sign, exp, frac)
                  end
            val (rBits, rSign, rExp, rFrac) = split r
            val (tBits, tSign, tExp, tFrac) = split t
            (* increment the magnitude of r by one ulp *)
            fun stepUp () = let
                  val ef = W64.andb(rBits, expAndFracMask) + 0w1
                  in
                    if (ef > expAndFracMask)
                      (* overflow, so return ±infinity *)
                      then fromBits(W64.orb(rSign, infBits))
                      else fromBits(W64.orb(rSign, ef))
                  end
            (* decrement the magnitude of r by one ulp *)
            fun stepDn () = let
                  val ef = W64.andb(rBits, expAndFracMask) - 0w1
                  in
                    if (ef < 0wx0010000000000000)
                      (* underflow, so return ±zero *)
                      then fromBits rSign
                      else fromBits(W64.orb(rSign, ef))
                  end
            in
              if (rExp = expMask)
                then r  (* r is either a NaN or infinity so return it *)
              else if (tExp = expMask)
                then if (tFrac <> 0w0)
                  then t (* t is a NaN, so return it *)
                  else if (rSign = tSign)
                    (* t is infinity with the same sign as r, so make r bigger *)
                    then stepUp ()
                    (* t is infinity with the opposite sign as r, so make r smaller *)
                    else stepDn ()
              (* both r and t are normal/subnormal numbers *)
              else if (W64.orb(rExp, rFrac) = 0w0)
                (* rExp = 0 && rFrac = 0 ==> r = ±0.0 *)
                then fromBits(W64.orb(tSign, 0w1)) (* ± minimum subnormal *)
              (* bit r and t are non-zero normal/subnormal numbers *)
              else if (rSign <> tSign)
                (* when different signs, move `r` toward 0 *)
                then stepDn()
              else if (rBits > tBits)
                (* either `r < t < 0` or `0 < t < r`, so move `r` toward 0 *)
                then stepDn()
                (* either `t < r < 0` or `0 < r < t`, so move `r` toward 0 *)
                else stepUp ()
            end

    val min : real * real -> real = InlineT.Real64.min
    val max : real * real -> real = InlineT.Real64.max

    fun toDecimal r = FloatRep.toDecimalApprox(Real64ToFRep.cvt r)
    fun fromDecimal d = (case FloatRep.fromDecimalApprox d
           of SOME frep => SOME(FRepToReal64.cvt(FloatRep.normalize64 frep))
            | NONE => NONE
          (* end case *))

    fun fmt mode = let
          val fmt' = FRepToString.fmt mode
          in
            fn r => fmt' (Real64ToFRep.cvt r)
          end

    val toString = FRepToString.toString o Real64ToFRep.cvt

    fun scan getc = let
          val scan' = StringToFRep.scan getc
          in
            fn cs => (case scan' cs
                 of SOME(frep, cs) => SOME(FRepToReal64.cvt frep, cs)
                  | NONE => NONE
                (* end case *))
          end

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
