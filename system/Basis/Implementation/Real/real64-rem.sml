(* real64-rem.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 * This is an implementation of `Real.rem` that returns the **exact** result.
 * It is based on the C code given at
 *
 *       https://stackoverflow.com/questions/26342823/implementation-of-fmod-function
 *)

structure Real64Rem : sig

    val rem : real * real -> real

  end = struct

    structure I64 = InlineT.Int64
    structure W64 = InlineT.Word64
    structure R64 = InlineT.Real64
    structure I = InlineT.Int
    structure W = InlineT.Word

    val posNaN = Real64Values.posNaN

    val kSignBit = W64.lshift(0w1, 0w63)
    val kExpAndMatMask = W64.notb kSignBit

    val kMantBits = 0w52
    val kMantIntBit = W64.lshift(0w1, kMantBits)
    val kMantMask = kMantIntBit - 0w1
    val upscale = R64.from_int64(I64.fromLarge(W64.toLargeInt kMantIntBit))

    val kExpBits = 0w11
    val kExpBias = W64.lshift(0w1, kExpBits) - 0w1
    fun getExponent bits = W64.toIntX(W64.andb(W64.rshiftl(bits, kMantBits), kExpBias))

    fun rem (x, y) = let
        val bitsx = R64.toBits x
        val ex = getExponent bitsx
        val bitsy = R64.toBits y
        val ey = getExponent bitsy
        in
          if (ex = 2047)
            then posNaN (* x is NaN or infinite *)
          else if (ey = 2047) andalso (W64.andb(bitsy, kMantMask) <> 0w0)
            then if (W64.andb(bitsy, kMantMask) = 0w0)
              then x (* y is infinite *)
              else posNaN (* y is NaN *)
          else if (W64.andb(bitsy, kExpAndMatMask) = 0w0)
            then posNaN (* y is zero *)
          else if (W64.lshift(bitsx, 0w1) >= W64.lshift(bitsy, 0w1))
            then let (* abs(x) >= abs(y) *)
              (* normalize operand *)
              fun normalizeArg (r, bits, exp) = let
                  val (bits, exp) = if exp = 0
                        then let
                          val bits = R64.toBits(upscale * r)
                          in
                            (bits, getExponent bits - W.toInt kMantBits)
                          end
                        else (bits, exp)
                  in
                    (W64.orb(W64.andb(bits, kMantMask), kMantIntBit), exp)
                  end
              val (ix, ex) = normalizeArg (x, bitsx, ex)
              val (iy, ey) = normalizeArg (y, bitsy, ey)
              (* binary long division *)
              fun divLp (ix, ex) = if (ex > ey)
                    then let
                      val ix = if (ix >= iy) then ix - iy else ix
                      in
                        divLp (W64.lshift(ix, 0w1), ex-1)
                      end
                    else (ix, ex)
              val (ix, ex) = divLp (ix, ex)
              (* ensure remainder is less than divisor *)
              val ix = if (ix >= iy) then ix - iy else ix
              (* generate the final result *)
              val bits = if ix = 0w0
                    then ix
                    else let
                      (* normalize the result *)
                      fun normalize (ix, ex) = if (ix < kMantIntBit)
                            then normalize (W64.lshift(ix, 0w1), ex-1)
                            else (ix, ex)
                      val (ix, ex) = normalize (ix, ex)
                      in
                        (* combine exponent and mantissa *)
                        if (ex > 0)
                          then W64.+(W64.lshift(W64.fromInt(ex-1), kMantBits), ix)
                          else W64.rshiftl(ix, W.fromInt(1 - ex))
                      end
              in
                (* set the sign and cast back to real *)
                R64.fromBits(W64.orb(W64.andb(bitsx, kSignBit), bits))
              end
            else x (* abs(x) < abs(y) *)
        end

  end
