(* math64-x86.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Math functions for the x86, which has hardware suppport for sqrt, sin, etc.
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

    open Math64Common

    val sqrt = MathInlineT.sqrt
    val sin  = MathInlineT.sine
    val cos  = MathInlineT.cosine
    val tan  = MathInlineT.tangent

    fun asin x = atan2(x, sqrt(1.0-x*x))
    fun acos x = 2.0 * atan2(sqrt((1.0-x)/(1.0+x)),1.0)

  end
