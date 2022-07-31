(* real64-to-intinf.sml
 *
 * COPYRIGHT (c) 2021 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Conversion from Real64.real to IntInf.int for 32-bit machines.
 *)

structure Real64ToIntInf : sig

    val cvt : IEEEReal.rounding_mode * Real64.real -> IntInf.int

  end = struct

    structure W = InlineT.Word
    structure W64 = InlineT.Word64

    infix 4 == !=

    val op == = InlineT.Real64.==
    val op != = InlineT.Real64.!=

  (* we use 2^30 for the radix on 32-bit systems. *)
    val rbase = 1073741824.0

    val precision = 53			(* hidden bit gets counted, too *)

  (* positive infinity *)
    val posInf = Real64Values.posInf
  (* negative infinity *)
    val negInf = Real64Values.negInf
  (* This is the IEEE double-precision maxint == 2^52 *)
    val maxInt = 4503599627370496.0

    fun toManExp x = let
	  val bits = InlineT.Real64.toBits x
	  in
	    if (W64.andb(0wx7fffffffffffffff, bits) = 0w0)
	      then {man = x, exp = 0}
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

    fun fromManExp {man=m, exp=e:int} =
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
	  else let val {man=m',exp=e'} = toManExp m
		in fromManExp { man = m', exp = e'+ e }
	       end

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

  (* split a real number into its whole and fractional parts *)
    fun split x = let
	  val w = whole x
	  val f = x - w
	  in
	    if abs f == 1.0
	      then {whole=w+f, frac=0.0}
	      else {whole=w, frac=f}
	  end

    fun toLargeInt (mode, x) = let
          val (negative, x) = if x < 0.0 then (true, ~x) else (false, x)
          fun feven x = #frac (split (0.5 * x)) == 0.0
          in
            if x < 1.0
              (* if the magnitude is less than 1.0, then
               * we just have to figure out whether to return ~1, 0, or 1
               *)
              then (case mode
                 of IEEEReal.TO_ZERO => 0
                  | IEEEReal.TO_POSINF => if negative then 0 else 1
                  | IEEEReal.TO_NEGINF => if negative then ~1 else 0
                  | IEEEReal.TO_NEAREST =>
                      if x < 0.5 then 0
                      else if x > 0.5
                        then if negative then ~1 else 1
                        else 0 (* 0 is even *)
                (* end case *))
              (* Otherwise we start with an integral value,
               * suitably adjusted according to fractional part
               * and rounding mode:
               *)
              else let
                val { whole, frac } = split x
                val start = (case mode
                       of IEEEReal.TO_NEGINF =>
                            if frac > 0.0 andalso negative
                              then whole + 1.0
                              else whole
                        | IEEEReal.TO_POSINF =>
                            if frac > 0.0 andalso not negative
                              then whole + 1.0
                              else whole
                        | IEEEReal.TO_ZERO => whole
                        | IEEEReal.TO_NEAREST =>
                            if frac > 0.5 then whole + 1.0
                            else if frac < 0.5 then whole
                            else if feven whole then whole
                            else whole + 1.0
                      (* end case *))
                  (* Now, for efficiency, we construct a
                   * fairly "small" whole number with
                   * all the significant bits.  First
                   * we get mantissa and exponent:
                   *)
                  val { man, exp } = toManExp start
                  (* Then we adjust both to make sure the mantissa
                   * is whole:
                   * We know that man is between .5 and 1, so
                   * multiplying man by 2^53 will guarantee wholeness.
                   * However, exp might be < 53 -- which would be
                   * bad.  The correct solution is to multiply
                   * by 2^min(exp,53) and adjust exp by subtracting
                   * min(exp,53):
                   *)
                  val adj = IntImp.min (precision, exp)
                  val man = fromManExp { man = man, exp = adj }
                  val exp = exp - adj
                  (* Now we can construct our bignum digits by
                   * repeated div/mod using the bignum base.
                   * This loop will terminate after two rounds at
                   * the most because we chop off 30 bits each
                   * time:
                   *)
                  fun loop x = if x == 0.0
                        then []
                        else let
                          val { whole, frac } = split (x / rbase)
                          val dig = Assembly.A.floor (frac * rbase)
                          in
                            InlineT.Word.fromInt dig :: loop whole
                          end
                  (* Now we make a bignum out of those digits: *)
                  val iman = CoreIntInf.abstract (CoreIntInf.BI{
                            negative = negative,
                            digits = loop man
                          })
                  in
                    (* Finally, we have to put the exponent back
                     * into the picture:
                     *)
                    IntInfImp.<< (iman, InlineT.Word.fromInt exp)
                  end
          end

    fun cvt (mode, x) = if (x != x)
            then raise Domain
	  else if x == posInf orelse x == negInf
            then raise Overflow
          else toLargeInt (mode, x)

  end
