(* real64-nextafter.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 * This is an implementation of `Real64.nextAfter`.
 *
 * Note that the SML Basis Library specification states that
 *
 *      ..., if `r` is ±∞ it returns ±∞.
 *
 * This semantics differs from that of the C library, which will return
 * `maxFinite` for `nextAfter (posInf, 0.0)`.
 *)

structure Real64NextAfter : sig

    val nextAfter : real * real -> real

  end = struct

    structure I64 = InlineT.Int64
    structure W64 = InlineT.Word64
    structure R64 = InlineT.Real64
    structure I = InlineT.Int
    structure W = InlineT.Word

    (* signed comparison of two 64-bit words *)
    fun i64LT (w1, w2) = I64.<(
          I64.fromLarge(W64.toLargeIntX w1),
          I64.fromLarge(W64.toLargeIntX w2))

    val kSignBit = W64.lshift(0w1, 0w63)
    val kExpAndMatMask = W64.notb kSignBit

    val kMantBits = 0w52
    val kMantIntBit = W64.lshift(0w1, kMantBits)
    val kMantMask = kMantIntBit - 0w1
    val upscale = R64.from_int64(I64.fromLarge(W64.toLargeInt kMantIntBit))

    val kExpBits = 0w11
    val kExpBias = W64.lshift(0w1, kExpBits) - 0w1
    val kExpMask = W64.lshift(kExpBias, kMantBits)
    fun getExponent bits = W64.toIntX(W64.andb(W64.rshiftl(bits, kMantBits), kExpBias))

    (* the non-sign bits of +/- infinity *)
    val infBits = kExpMask

    fun nextAfter (x, y) = let
        val bitsx = R64.toBits x
        val bitsy = R64.toBits y
        val expx = getExponent bitsx
        val expy = getExponent bitsy
        (* copy y's sign into z *)
        fun copySignFromY z = W64.orb(W64.andb(bitsy, kSignBit), z)
        in
          if (expx = 2047)
            then if (W64.andb(bitsx, kMantMask) <> 0w0)
            orelse ((expy = 2047) andalso (W64.andb(bitsy, kMantMask) <> 0w0))
              (* either `x` or `y` is a NaN, so return a NaN *)
              then (x + y)
              (* `x` is an infinity and `y` is not a NaN, so return `x`.
               * Note that that this is where the semantics of the SML Basis
               * Library's `Real.nextAfter` differs from that of the C Library.
               *)
              else x
          else if (expy = 2047) andalso (W64.andb(bitsx, kMantMask) <> 0w0)
            (* `y` is a NaN, so return a NaN *)
            then y
          (* here we know that neither `x` or `y` is a NaN and that `x` is finite *)
          else if (bitsx = bitsy)
            then y
          else if (W64.lshift(bitsx, 0w1) = 0w0)
            (* x = 0: return the minimum subnormal with the same sign as `y` *)
            then R64.fromBits (copySignFromY 0w1)
            else let
              val ix = if (W64.andb(bitsx, kSignBit) = 0w0)
                    (* x > 0 *)
                    then if (i64LT(bitsx, bitsy))
                      (* 0 < x < y: return x + ulp *)
                      then bitsx + 0w1
                      (* 0 < x && y < x: return x - ulp *)
                      else bitsx - 0w1
                    (* x < 0 *)
                    else if (W64.andb(bitsy, kSignBit) = 0w0)
                      then bitsx - 0w1 (* x < 0 <= y: return x - ulp *)
                    else if (W64.lshift(bitsx, 0w1) < W64.lshift(bitsy, 0w1))
                      then bitsx - 0w1 (* x < y < 0: return x - ulp *)
                      else bitsx + 0w1 (* y < x < 0: return x + ulp *)
              in
                if (W64.andb(ix, kExpMask) = kExpMask)
                  (* the mantissa overflowed into the exponent, which was already
                   * the maximum, so return an infinity.
                   *)
                  then R64.fromBits (copySignFromY infBits)
                  else R64.fromBits ix
              end
        end (* nextAfter *)

  end (* Real64NextAfter *)

(* tests:
 *
 *      0wx8000000000000000     -0
 *      0wx0000000000000000     +0
 *      0wx000fffffffffffff     largest positive subnormal
 *      0wx0000000000000001     smallest positive subnormal
 *      0wx3fefffffffffffff     the largest number less than 1
 *      0wx3ff0000000000000     1
 *      0wx3ff0000000000001     the smallest number greater than 1
 *      0wx7fefffffffffffff     largest normal number
 *      0wx7ff0000000000000     positive infinity
 *      0wx7ff8000000000001     a NaN
 *
 *      nextAfter(zero, 1.0)
 *      nextAfter(zero, -1.0)
 *      nextAfter(maxSubNormal, 1.0)
 *      nextAfter(maxFloat, +infinity)
 *
 *)


