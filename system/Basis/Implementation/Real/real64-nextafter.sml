(* real64-nextafter.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 * This is an implementation of `Real64.nextAfter`.
 *)

structure Real64NextAfter : sig

    val nextAfter : real * real -> real

  end = struct

    structure I64 = InlineT.Int64
    structure W64 = InlineT.Word64
    structure R64 = InlineT.Real64
    structure I = InlineT.Int
    structure W = InlineT.Word

local
  structure W64 = struct
      open Word64
      val rshiftl = >>
      val lshift = <<
    end

    (* signed comparison of two 64-bit words *)
    fun i64LT (w1, w2) = I64.<(
          I64.fromLarge(W64.toLargeIntX w1),
          I64.fromLarge(W64.toLargeIntX w2))

    val fromBits = Unsafe.Real64.castFromWord
    val toBits = Unsafe.Real64.castToWord

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
in

    fun nextAfter (x, y) = let
        val bitsx = toBits x
        val bitsy = toBits y
        fun isNan bits = let
              val exp = getExponent bits
              in
                (exp = 2047) andalso (W64.andb(bits, kMantMask) <> 0w0)
              end
        (* copy y's sign into z *)
        fun copySignFromY z = W64.orb(W64.andb(bitsy, kSignBit), z)
        in
          if (isNan bitsx orelse isNan bitsy)
            then (x + y) (* return a NaN *)
          else if (bitsx = bitsy)
            then return y;
          else if (W64.shiftl(bitsx, 0w1) = 0w0)
            (* x = 0: return the minimum subnormal with the same sign as `y` *)
            then fromBits (copySignFromY 0w1)
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
                    else if (W64.shiftl(bitsx, 0w1) < W64.shiftl(bitsy, 0w1))
                      then bitsx - 0w1 (* x < y < 0: return x - ulp *)
                      else bitsx + 0w1 (* y < x < 0: return x + ulp *)
              in
                if (W64.andb(ix, kExpMask) = kExpMask)
                  (* the mantissa overflowed into the exponent, which was already
                   * the maximum, so return an infinity.
                   *)
                  then fromBits (copySignFromY infBits)
                  else fromBits ix
              end
        end (* nextAfter *)

end (* local *)

(* tests:
 *
 *      nextAfter(zero, 1.0)
 *      nextAfter(zero, -1.0)
 *      nextAfter(maxSubNormal, 1.0)
 *      nextAfter(maxFloat, +infinity)
 *)


