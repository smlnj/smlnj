(* math64-none.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Math functions for machines that do not have any hardware support
 * for sqrt, etc.
 *
 * The following functions were adapted from the 4.3BSD math library.
 * Eventually, each machine supported should have a hand-coded math
 * functor with more efficient versions of these functions.
 *
 ***************************************************************************
 *                                                                         *
 * Copyright (c) 1985 Regents of the University of California.             *
 *                                                                         *
 * Use and reproduction of this software are granted  in  accordance  with *
 * the terms and conditions specified in  the  Berkeley  Software  License *
 * Agreement (in particular, this entails acknowledgement of the programs' *
 * source, and inclusion of this notice) with the additional understanding *
 * that  all  recipients  should regard themselves as participants  in  an *
 * ongoing  research  project and hence should  feel  obligated  to report *
 * their  experiences (good or bad) with these elementary function  codes, *
 * using "sendbug 4bsd-bugs@BERKELEY", to the authors.                     *
 *                                                                         *
 * K.C. Ng, with Z-S. Alex Liu, S. McDonald, P. Tang, W. Kahan.            *
 * Revised on 5/10/85, 5/13/85, 6/14/85, 8/20/85, 8/27/85, 9/11/85.        *
 *                                                                         *
 ***************************************************************************
 *
 *)

structure Math64 : MATH =
  struct

    structure Word = InlineT.Word
    structure Word64 = InlineT.Word64
    structure Real64 = InlineT.Real64
    structure I = InlineT.Int

    type real = real

    infix 4 ==
    val op +  = Real64.+
    val op -  = Real64.-
    val op *  = Real64.*
    val op /  = Real64./
    val op ~  = Real64.~
    val op <  = Real64.<
    val op <= = Real64.<=
    val op >  = Real64.>
    val op >= = Real64.>=
    val op == = Real64.==

    val lessu : int * int -> bool = I.ltu

    open Math64Common

    val zero = 0.0
    val half = 0.5
    val one = 1.0
    val two = 2.0
    val plusInfinity = 1E300 * 1E300
    val NaN = 0.0 / 0.0

  (* sin/cos *)
    local
      val S0 = ~1.6666666666666463126E~1
      val S1 =  8.3333333332992771264E~3
      val S2 = ~1.9841269816180999116E~4
      val S3 =  2.7557309793219876880E~6
      val S4 = ~2.5050225177523807003E~8
      val S5 =  1.5868926979889205164E~10
    in
    fun sin__S z = (z*(S0+z*(S1+z*(S2+z*(S3+z*(S4+z*S5))))))
    end (* local *)

    local
      val C0 =  4.1666666666666504759E~2
      val C1 = ~1.3888888888865301516E~3
      val C2 =  2.4801587269650015769E~5
      val C3 = ~2.7557304623183959811E~7
      val C4 =  2.0873958177697780076E~9
      val C5 = ~1.1250289076471311557E~11
    in
    fun cos__C z = (z*z*(C0+z*(C1+z*(C2+z*(C3+z*(C4+z*C5))))))
    end (* local *)

    val PIo4   =  7.853981633974483096E~1
    val PIo2   =  1.5707963267948966192E0
    val PI3o4  =  2.3561944901923449288E0
    val PI     =  pi
    val PI2    =  6.2831853071795864769E0
    val oneOver2Pi = 0.1591549430918953357688837633725143620345

    local
      val thresh =  2.6117239648121182150E~1
    in
    fun S y = y + y * sin__S(y*y)
    fun C y = let
	  val yy = y*y
	  val c = cos__C yy
	  val Y = yy/two
	  in
	    if Y < thresh
	      then one - (Y - c)
	      else half - (Y - half - c)
	  end
    end (* local *)

    fun sin x = let (* This function propagages Inf's and Nan's correctly. *)
	(* x may be finite, inf, or nan at this point. *)
	  val xover2pi = x * oneOver2Pi
	  val x = PI2*(xover2pi - realround(xover2pi))
	(* now, probably,  ~pi <= x <= pi, except on vaxes.
	 * x may be a nan, but cannot now be an inf.
	 *)
	  fun lessPIo2 x = if x>PIo4 then C(PIo2-x) else S x
	  fun lessPI x = if x>PIo2 then lessPIo2(PI-x) else lessPIo2 x
	  fun positive x = if x>PI then sin(x-PI2) (* exceedingly rare *)
				   else lessPI x
	  in
	    if x>=0.0
	      then positive x
	      else ~(positive(~x))
	  end

    fun cos x = sin(PIo2 - x)

    fun tan x = sin x / cos x

    fun sqrt (x: real) = (* handles inf's and nan's correctly *)
	  if x>zero
	    then if x < plusInfinity
	      then let
		val k = 6 (* log base 2 of the precision *)
		val n = I.rshift(logb x, 0w1)
		val x = scalb(x, I.~(I.+(n, n)))
		fun iter(0, g) = g
		  | iter(i, g) = iter(I.-(i, 1), half * (g + x/g))
		in
		  scalb(iter(k,one),n)
		end
	      else x
	    else if x<zero
	      then NaN
	      else x

    fun asin x = atan2(x, sqrt(1.0-x*x))
    fun acos x = 2.0 * atan2(sqrt((1.0-x)/(1.0+x)),1.0)

  end
