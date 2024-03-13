(* real.sig
 *
 * COPYRIGHT (c) 2022 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * The Standard ML Basis Library interface to floating-point structures.
 *)

(* the REAL signature without the Math submodule *)
signature REAL_WO_MATH =
  sig

    type real

    val radix     : Int.int
    val precision : Int.int
	(* the number of digits (each 0..radix-1) in mantissa *)

    val maxFinite    : real   (* maximum finite number *)
    val minPos       : real   (* minimum non-zero positive number *)
    val minNormalPos : real   (* minimum non-zero normalized number *)

    val posInf : real
    val negInf : real

    val + : real * real -> real
    val - : real * real -> real
    val * : real * real -> real
    val / : real * real -> real
    val rem : real * real -> real
    val *+ : real * real * real -> real
    val *- : real * real * real -> real
    val ~ : real -> real

    val abs      : real -> real
    val min      : real * real -> real
    val max      : real * real -> real

    val sign     : real -> int
    val signBit  : real -> bool
    val sameSign : real * real -> bool
    val copySign : real * real -> real

    val compare : real * real -> order
    val compareReal : real * real -> IEEEReal.real_order

    val < : real * real -> bool
    val <= : real * real -> bool
    val > : real * real -> bool
    val >= : real * real -> bool

    val == : real * real -> bool
    val != : real * real -> bool
    val ?= : real * real -> bool
    val unordered : real * real -> bool

    val isFinite : real -> bool
    val isNan : real -> bool
    val isNormal : real -> bool

    val class : real -> IEEEReal.float_class

    val toManExp : real -> {man: real, exp: int}
    val fromManExp : {man: real, exp: int} -> real
    val split : real -> {whole: real, frac: real}
    val realMod : real -> real

    val nextAfter  : real * real -> real
    val checkFloat : real -> real

    val realFloor : real -> real
    val realCeil  : real -> real
    val realTrunc : real -> real
    val realRound : real -> real
    val floor : real -> Int.int
    val ceil  : real -> Int.int
    val trunc : real -> Int.int
    val round : real -> Int.int
    val toInt : IEEEReal.rounding_mode -> real -> int
    val toLargeInt : IEEEReal.rounding_mode -> real -> LargeInt.int
    val fromInt  : Int.int -> real
    val fromLargeInt  : LargeInt.int -> real

    val fmt  : StringCvt.realfmt -> real -> string
    val toString   : real -> string
    val scan : (char, 'a) StringCvt.reader -> (real, 'a) StringCvt.reader
    val fromString : string -> real option

    val toLarge : real -> LargeReal.real
    val fromLarge: IEEEReal.rounding_mode -> LargeReal.real -> real

    val toDecimal   : real -> IEEEReal.decimal_approx
    val fromDecimal : IEEEReal.decimal_approx -> real option

  end (* signature REAL_WO_MATH *)

(* REAL signature from 2004 Basis specification *)
signature REAL_2004 =
  sig
    include REAL_WO_MATH
    structure Math : MATH_2004 where type real = real
  end

(* REAL signature with Basis Library proposal 2022-1 *)
signature REAL_2022 =
  sig
    include REAL_WO_MATH
    structure Math : MATH_2022 where type real = real
  end

signature REAL = REAL_2022
