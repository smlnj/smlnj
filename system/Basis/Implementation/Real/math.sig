(* math.sig
 *
 * COPYRIGHT (c) 2022 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature MATH_2004 =
  sig
    type real

    val pi : real
    val e  : real
    val sqrt  : real -> real
    val sin   : real -> real
    val cos   : real -> real
    val tan   : real -> real
    val asin  : real -> real
    val acos  : real -> real
    val atan  : real -> real
    val atan2 : real * real -> real
    val exp   : real -> real
    val pow   : real * real -> real
    val ln    : real -> real
    val log10 : real -> real
    val sinh  : real -> real
    val cosh  : real -> real
    val tanh  : real -> real
  end

(* includes Basis Library proposal 2022-001 *)
signature MATH_2022 =
  sig
    include MATH_2004

    val tau : real
  end

signature MATH = MATH_2022
