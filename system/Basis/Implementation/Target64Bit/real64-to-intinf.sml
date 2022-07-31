(* real64-to-intinf.sml
 *
 * COPYRIGHT (c) 2021 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Conversion from Real64.real to IntInf.int for 64-bit machines.
 *)

structure Real64ToIntInf : sig

    val cvt : IEEEReal.rounding_mode * Real64.real -> IntInf.int

  end = struct

    structure W = InlineT.Word
    structure W64 = InlineT.Word64
    structure IntInf = IntInfImp

    fun w64tow w = W.fromLarge (W64.toLarge w)

  (* convert the mantissa to IntInf.int; note that it is guaranteed
   * to be representable in one digit.
   *)
  fun mkIntInf (_, 0w0) = 0
    | mkIntInf (neg, n) =
        CoreIntInf.abstract (CoreIntInf.BI{ negative = neg, digits = [w64tow n] })

    fun cvt (mode, x) = let
          val bits = InlineT.Real64.toBits x
          val neg = (W64.chkRshiftl(bits, 0w63) <> 0w0)
          val exp = W64.andb(W64.chkRshiftl(bits, 0w52), 0wx7ff)
          val mant = W64.andb(bits, 0wxfffffffffffff)
          in
            if (exp = 0wx7ff)
              then if (mant = 0w0)
                then raise Overflow         (* infinity *)
                else raise Domain           (* NaN *)
              else if (exp = 0w0)
                then ( (* number is zero or denormalized *)
                  case mode
                   of IEEEReal.TO_ZERO => 0
                    | IEEEReal.TO_POSINF =>
                        if neg orelse (mant = 0w0) then 0 else 1
                    | IEEEReal.TO_NEGINF =>
                        if neg andalso (mant <> 0w0) then ~1 else 0
                    | IEEEReal.TO_NEAREST => 0
                  (* end case *))
                else let  (* x = -1^sign * 2^(exp - 1023) * 1.mant *)
                  (* add implicit leading '1' bit = 2^52 to the mantissa *)
                  val mant = mant + 0wx10000000000000
                  (* the mantissa is currently scaled by 2^52, so we need to include
                   * that in the unbiased exponent used to shift it.
                   *)
                  val bias = 0w1023 + 0w52
                  in
                    if (exp < bias)
                      then let
                      (* in this case, we are shifting right, which loses some of the
                       * mantissa bits, so we need to worry about the rounding mode.
                       *)
                        val shiftAmt = w64tow (bias - exp)
                        (* truncate bits *)
                        fun trunc n = mkIntInf (neg, W64.chkRshiftl(n, shiftAmt))
                        (* truncate bits and then add one *)
                        fun truncPlusOne n = mkIntInf (neg, W64.chkRshiftl(n, shiftAmt) + 0w1)
                        (* test for a zero fraction; note that the mantissa is
                         * in the range 2^52 .. 2^53-1, so any shift > 52 will
                         * result in zero.  In that case, the fraction is non-zero.
                         * When the shiftAmt <= 52, we mask the bits that are being
                         * shifted away to check for a zero fraction.
                         *)
                        fun fracIsZero () = (shiftAmt <= 0w52)
                              andalso (W64.andb(W64.chkLshift(0w1, shiftAmt) - 0w1, mant) = 0w0)
                        in
                          case mode
                           of IEEEReal.TO_ZERO => trunc mant
                            | IEEEReal.TO_POSINF =>
                                if neg orelse fracIsZero()
                                (* < 0 so we can ignore any fractional bits *)
                                  then trunc mant
                                (* > 0 so non-zero fractional bits => round up *)
                                  else truncPlusOne mant
                            | IEEEReal.TO_NEGINF =>
                                if neg andalso not (fracIsZero())
                                (* > 0 so non-zero fractional bits => round down, which
                                 * means that the mantissa gets **bigger**
                                 *)
                                  then truncPlusOne mant
                                (* < 0 so we can ignore any fractional bits *)
                                  else trunc mant
                            | IEEEReal.TO_NEAREST => if (shiftAmt > 0w52)
                                then if (shiftAmt = 0w53)
                                  (* 0.5 <= abs(x) < 1 *)
                                  then if (mant = 0wx10000000000000)
                                    then 0 (* abs(x) = 0.5, so round to 0 *)
                                    else mkIntInf (neg, 0w1)
                                  else 0 (* all mantissa bits are shifted away *)
                                else let
                                  val w = W64.chkRshiftl(mant, shiftAmt)
                                  val frac = W64.andb(mant, W64.chkLshift(0w1, shiftAmt) - 0w1)
                                  val whole =
                                        if frac = 0w0
                                          then w
                                          else let
                                            val mid = W64.chkLshift(0w1, shiftAmt - 0w1)
                                            in
                                              if (frac < mid)
                                                then w (* round absolute value down *)
                                              else if (frac > mid)
                                                then w + 0w1 (* round absolute value up *)
                                              (* round to even for ties *)
                                              else if (W64.andb(w, 0w1) = 0w0)
                                                then w
                                                else w + 0w1 (* round to even *)
                                            end
                                  in
                                    mkIntInf (neg, whole)
                                  end
                          (* end case *)
                        end
                      else let
                        val n = mkIntInf (neg, mant)
                        in
                          if (bias < exp)
                            then IntInf.<<(n, w64tow (exp - bias))
                            else n
                        end
                  end
          end (* cvt *)

  end
