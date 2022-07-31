(* target64-core-intinf.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Basic IntInf operations for 64-bit targets.  These are required
 * for translating certain primops and IntInf.int literals within the compiler.
 *)

structure CoreIntInf :> sig

  (* We use a 62-bit representation, stored in 63-bit words for digits.
   * This way we avoid the extra boxing that would come with 64-bit values
   * and also have the benefit of an extra bit that can be used to store
   * carries.  The digits are ordered least-significant first and zero
   * is represented by the empty list of digits.
   *)
    datatype rep = BI of { negative : bool, digits : word list }

  (* This is the abstract built-in type "intinf".  It comes with no
   * operations of its own.  We use casts to go back and forth between
   * the representation type "rep" and the abstract type "intinf".
   *)
    type intinf = PrimTypes.intinf

  (* Here are the "cast" operations: *)
    val abstract : rep    -> intinf
    val concrete : intinf -> rep

  (* the number of bits in one "big digit" *)
    val baseBits : word

  (* the actual base: *)
    val base : word

  (* maximum value of a "big digit": *)
    val maxDigit : word

  (* The following conversion functions are copied into structure _Core
   * from where the compiler's "translate" phase will pick them to
   * implement precision-extending and precision-shrinking conversions
   * that involve intinf.  We only provide operations between
   * int64 and intinf.  For precisions less than 64 bits the compiler
   * must insert an additional conversion:
   *)

  (* fit value (2's complement) in int64, raise Overflow if too large *)
    val testInf64   : intinf -> int64
  (* truncate value (2's complement repr) to fit in word64: *)
    val truncInf64  : intinf -> word64
  (* sign-extend int64 into intinf: *)
    val extend64Inf : int64 -> intinf
  (* copy bits from word64 into (non-negative) intinf: *)
    val copy64Inf   : word64 -> intinf

  (* These two directly take the list of digits
   * to be used by the internal representation:
   *)
    val makeNegInf : word list -> intinf
    val makePosInf : word list -> intinf

  (* In the common case where only one big digit is involved, use
   * a shortcut without list allocation:
   *)
    val makeSmallNegInf : word -> intinf
    val makeSmallPosInf : word -> intinf

  (* For ~base < i < base we have lowValue i = i.
   * For other i we have lowValue i = ~base (= neg_base_as_int).
   * This can be used to implement faster pattern-match code for
   * the common case that the pattern consists of small values.
   *)
    val lowValue : intinf -> int
    val neg_base_as_int : int

  (* add one to a list of digits *)
    val natinc : word list -> word list

  (* Various primitive operations for use by the pervasive environment,
   * plus stuff that we have to implement here anyway, so the
   * real structure IntInf can pick them up:
   *)
    val ~ : intinf -> intinf
    val + : intinf * intinf -> intinf
    val - : intinf * intinf -> intinf
    val * : intinf * intinf -> intinf
    val div : intinf * intinf -> intinf
    val mod : intinf * intinf -> intinf
    val quot : intinf * intinf -> intinf
    val rem : intinf * intinf -> intinf
    val < : intinf * intinf -> bool
    val <= : intinf * intinf -> bool
    val > : intinf * intinf -> bool
    val >= : intinf * intinf -> bool
    val compare : intinf * intinf -> Order.order
    val abs : intinf -> intinf
    val pow : intinf * int -> intinf

    val divMod : intinf * intinf -> intinf * intinf
    val quotRem : intinf * intinf -> intinf * intinf

  (* support for scanning and formatting: *)
    val natdivmodd : word list * word -> word list * word
    val natmadd : word * word list * word -> word list

  end = struct

    infixr 5 ::
    val not : bool -> bool = InLine.inl_not

    val wToI64 : word -> int64  = InLine.copy_word_to_int64
    val w64ToI64 : word64 -> int64 = InLine.copy_word64_to_int64
    val wToW64 : word -> word64 = InLine.word_to_word64
    val i64ToW : int64 -> word = InLine.trunc_int64_to_word
    val i64ToW64 : int64 -> word64 = InLine.copy_int64_to_word64
    val w64ToW : word64 -> word = InLine.word64_to_word
    val ~ : int64 -> int64 = InLine.int64_neg
    infix || && >> ^ <<
    val op || : word64 * word64 -> word64 = InLine.word64_orb
    val op ^ : word64 * word64 -> word64 = InLine.word64_xorb
    val op && : word64 * word64 -> word64 = InLine.word64_andb
    val op >> : word64 * word -> word64 = InLine.word64_rshiftl
    val op << : word64 * word -> word64 = InLine.word64_lshift

    (* ******************* *)
    type intinf = PrimTypes.intinf

    (* assuming 62-bit digits (must match actual implementation),
     * least significant digit first,
     * normalized (last digit <> 0) *)

    datatype rep = BI of { negative : bool, digits : word list }

    fun abstract (r : rep) : intinf = InLine.cast r
    fun concrete (i : intinf) : rep = InLine.cast i

    val hBaseBits : word = 0w31 (* half number of bits per digit *)
    val baseBits : word = InLine.word_lshift (hBaseBits, 0w1)
    val base : word = InLine.word_lshift (0w1, baseBits)
    val base64 = wToW64 base
    val maxDigit : word = InLine.word_sub(base, 0w1)
    val maxDigit64 = wToW64 maxDigit
    val maxDigitL : word = 0wx7fffffff	(* lower half of maxDigit *)
    val maxDigitL64 = wToW64 maxDigitL
    val neg_base_as_int : int = ~0x4000000000000000

    val gap : word = InLine.word_sub (0w64, baseBits) (* 64 - baseBits *)
    val slc : word = InLine.word_sub (baseBits, gap)  (* baseBits - gap *)

  (* convert intinf to int64; raise Overflow it result is too large *)
    fun testInf64 i = let
	  val BI { negative, digits } = concrete i
	  fun negif i64 = if negative then ~i64 else i64
	  in
	    case digits
	     of [] => 0
	      | [d] => negif (wToI64 d)
	      | [d0, 0w1] => negif (w64ToI64 (wToW64 d0 || base64))
	      | [0w0, 0w2] => if negative then ~0x8000000000000000
			      else raise Assembly.Overflow
	      | _ => raise Assembly.Overflow
	  end

  (* truncate intinf to word64 *)
    fun truncInf64 i = let
	  val BI { negative, digits } = concrete i
	  val b = (case digits
		 of [] => 0w0
		  | [d] => wToW64 d
		  | d0 :: d1 :: _ => wToW64 d0 || (wToW64 d1 << baseBits)
		(* end case *))
	  in
	    if negative then InLine.word64_neg b else b
	  end

  (* sign-extend an int64 to an intinf *)
    fun extend64Inf i64 = let
	  fun e (negative, w63) = BI{
		    negative = negative,
		    digits = if InLine.word_ge (w63, base)
			then [InLine.word_sub (w63, base), 0w1]
			else [w63]
		  }
	  val i = if InLine.int64_eql(i64, 0)
		  then BI { negative = false, digits = [] }
		else if InLine.int64_gt (i64, 0)
		  then e (false, i64ToW i64)
		else if InLine.int64_eql (i64, ~0x8000000000000000)
		  then BI { negative = true, digits = [0w0, 0w2] }
		  else e (true, i64ToW (~i64))
	  in
	    abstract i
	  end

  (* zero-extend a word64 to an intinf *)
    fun copy64Inf w64 = let
	  val digits = if InLine.word64_eql (w64, 0w0)
		  then []
		else if InLine.word64_ge (w64, base64)
		  then [w64ToW (w64 && maxDigit64), w64ToW (w64 >> baseBits)]
		  else [w64ToW w64]
	  in
	    abstract (BI { negative = false, digits = digits })
	  end

    fun makeInf negative digits =
	  abstract (BI { negative = negative, digits = digits })
    val makeNegInf = makeInf true
    val makePosInf = makeInf false

    fun makeSmallInf _ 0w0 =
	  abstract (BI { negative = false, digits = [] })
      | makeSmallInf negative digit =
	  abstract (BI { negative = negative, digits = [digit] })
    val makeSmallNegInf = makeSmallInf true
    val makeSmallPosInf = makeSmallInf false

    fun lowValue i = (case concrete i
	   of BI { digits = [], ... } => 0
	    | BI { digits = [d], negative = false } =>
		InLine.unsigned_word_to_int d
	    | BI { digits = [d], negative = true } =>
		InLine.int_neg (InLine.unsigned_word_to_int d)
	    | _ => neg_base_as_int
	  (* end case *))

    (* concrete->abstract wrappers for unary and binary functions *)
    fun fabs1 f x = abstract (f (concrete x))
    fun fabs2 f (x, y) = abstract (f (concrete x, concrete y))
    fun fabs2c f (x, y) = f (concrete x, concrete y)
    fun fabs22 f (x, y) = let
	  val (a, b) = f (concrete x, concrete y)
	  in
	    (abstract a, abstract b)
	  end

    (* like BI, but make sure that digits = [] implies not negative *)
    fun bi { digits = [], ... } = BI { digits = [], negative = false }
      | bi arg = BI arg

    fun abs' (BI { negative, digits }) =
	BI { negative = false, digits = digits }

    fun neg (BI { digits, negative }) =
	bi { digits = digits, negative = not negative }

    datatype order = datatype Order.order

    fun dcmp (x, y) =
	if InLine.word_lt (x, y) then LESS else
	if InLine.word_gt (x, y) then GREATER else
	EQUAL

    fun natcmp ([], []) = EQUAL
      | natcmp ([], _) = LESS
      | natcmp (_, []) = GREATER
      | natcmp (x :: xs, y :: ys) =
	  (case natcmp (xs, ys) of
	       EQUAL => dcmp (x, y)
	     | unequal => unequal)

    fun lt (BI { negative = false, ... }, BI { negative = true, ... }) = false
      | lt (BI { negative = true, ... }, BI {negative = false, ...}) = true
      | lt (BI { negative, digits = d1 }, BI { digits = d2, ... }) =
	  InLine.= (natcmp (d1, d2), if negative then GREATER else LESS)

    fun gt (x, y) = lt (y, x)
    fun ge (x, y) = not (lt (x, y))
    fun le (x, y) = not (gt (x, y))

    fun adddig (d1, d2) : {carry: bool, res: word} =
        let val sum = InLine.word_add (d1, d2)
        in
	    {carry = InLine.word_ge (sum, base),
	     res = InLine.word_andb (sum, maxDigit) }
        end

    (* add one to nat *)
    fun natinc [] = [0w1]
      | natinc (x :: xs) =
	  if InLine.word_eql (x, maxDigit) then 0w0 :: natinc xs
	  else InLine.word_add (x, 0w1) :: xs

    fun natdec (0w0 :: xs) = maxDigit :: natdec xs
      | natdec [0w1] = []
      | natdec (x :: xs) = InLine.word_sub (x, 0w1) :: xs
      | natdec [] = raise Assembly.Overflow (* should never happen! *)

    (* add two nats plus 1 (carry) *)
    fun natadd1 (x, []) = natinc x
      | natadd1 ([], y) = natinc y
      | natadd1 (x :: xs, y :: ys) = let
	    val { carry, res } = adddig (x, y)
	    val (carry, res) =
		if InLine.word_eql (res, maxDigit) then (true, 0w0)
		else (carry, InLine.word_add (res, 0w1))
	in
	    res :: natadd01 (carry, xs, ys)
	end

    (* add two nats *)
    and natadd0 (x, []) = x
      | natadd0 ([], y) = y
      | natadd0 (x :: xs, y :: ys) = let
	    val { carry, res } = adddig (x, y)
	in
	    res :: natadd01 (carry, xs, ys)
	end

    (* add two nats with optional carry *)
    and natadd01 (carry, xs, ys) =
	if carry then natadd1 (xs, ys) else natadd0 (xs, ys)

    exception Negative

    (* natsub hopes that xs >= ys + carry, raises Negative otherwise *)
    fun natsub (xs, [], false) = xs
      | natsub (xs, [], true) = natsub (xs, [0w0], true)
      | natsub ([], _, _) = raise Negative
      | natsub (x :: xs, y :: ys, c) = let
	    val y' = if c then InLine.word_add (y, 0w1) else y
	    val (res, carry) =
		if InLine.word_lt (x, y') then
		    (InLine.word_sub (InLine.word_add (x, base), y'), true)
		else (InLine.word_sub (x, y'), false)
	in
	    case natsub (xs, ys, carry) of
		[] => if InLine.word_eql (res, 0w0) then [] else [res]
	      | more => res :: more
	end

    fun natsub0 (xs, ys) = natsub (xs, ys, false)

    fun sub0 (xs, ys) =
	BI { negative = false, digits = natsub0 (xs, ys) }
	handle Negative => bi { negative = true, digits = natsub0 (ys, xs) }

    fun add0 (false, xs1, false, xs2) =
	  BI { negative = false, digits = natadd0 (xs1, xs2) }
      | add0 (true, xs1, true, xs2) =
	  BI { negative = true, digits = natadd0 (xs1, xs2) }
      | add0 (false, xs1, true, xs2) =
	  sub0 (xs1, xs2)
      | add0 (true, xs1, false, xs2) =
	  sub0 (xs2, xs1)

    fun add (BI b1, BI b2) =
	add0 (#negative b1, #digits b1, #negative b2, #digits b2)

    fun sub (BI b1, BI b2) =
	add0 (#negative b1, #digits b1, not (#negative b2), #digits b2)

    fun compare' (BI { negative = true, ... }, BI { negative = false, ... }) =
	  LESS
      | compare' (BI { negative = false, ... }, BI { negative = true, ... }) =
	  GREATER
      | compare' (BI { negative, digits = d1 }, BI { digits = d2, ... }) =
	  if negative then natcmp (d2, d1) else natcmp (d1, d2)

    fun ddmul (x, y) = let
	  fun high w64 = w64 >> hBaseBits
	  fun low w64 = w64 && maxDigitL64
	  fun hl w64 = (high w64, low w64)
	  val (xh, xl) = hl (wToW64 x)
	  val (yh, yl) = hl (wToW64 y)
	  val a = InLine.word64_mul (xh, yh)
	  val c = InLine.word64_mul (xl, yl)
	  (* b = b' - a - c = xh * yl + xl * yh *)
	  val b' = InLine.word64_mul (InLine.word64_add (xh, xl), InLine.word64_add (yh, yl))
	  val b = InLine.word64_sub (b', InLine.word64_add (a, c))
	  val (bh, bl) = hl b
	  val l0 = InLine.word64_add (c, InLine.word64_lshift (bl, hBaseBits))
	  val l64 = l0 && maxDigit64
	  val lc = l0 >> baseBits
	  val h64 = InLine.word64_add (InLine.word64_add (a, bh), lc)
	  in
	    (w64ToW h64, w64ToW l64)
	  end

    fun natmadd (0w0, _, 0w0) = []
      | natmadd (0w0, _, c) = [c]
      | natmadd (_, [], 0w0) = []
      | natmadd (_, [], c) = [c]
      | natmadd (0w1, xs, 0w0) = xs
      | natmadd (0w1, xs, c) = natadd0 (xs, [c])
      | natmadd (w, x :: xs, c) = let
	  val (h, l) = ddmul (w, x)
	  val { carry,  res = l' } = adddig (l, c)
	  val h' = if carry then InLine.word_add (h, 0w1) else h
	  in
	    l' :: natmadd (w, xs, h')
	  end

    fun natmul ([], _) = []
      | natmul (_, []) = []
      | natmul (xs, [0w1]) = xs
      | natmul ([0w1], ys) = ys
      | natmul (x :: xs, ys) =
	  natadd0 (natmadd (x, ys, 0w0), 0w0 :: natmul (xs, ys))

    fun mul (BI x, BI y) =
	bi { negative = InLine.<> (#negative x, #negative y),
	     digits = natmul (#digits x, #digits y) }

    val one = BI { negative = false, digits = [0w1] }
    val zero = BI { negative = false, digits = [] }
    fun posi digits = BI { digits = digits, negative = false }
    fun negi digits = BI { digits = digits, negative = true }
    fun zneg digits = bi { digits = digits, negative = true }

    fun consd (0w0, []) = []
      | consd (x, xs) = x :: xs

    fun scale w = InLine.word_div (base, InLine.word_add (w, 0w1))

  (* returns length-1 and last element *)
    fun length'n'last [] = (0, 0w0)	(* should not happen *)
      | length'n'last [x] = (0, x)
      | length'n'last (_ :: l) = let
	  val (len, last) = length'n'last l
	  in
	    (InLine.int_add (len, 1), last)
	  end

    fun nth (_, []) = 0w0
      | nth (0, x :: _) = x
      | nth (n, _ :: xs) = nth (InLine.int_sub (n, 1), xs)

    (* divide DP number by digit; assumes u < i , i >= base/2 *)
    fun natdivmod2 ((u,v), i) =
	let fun low w = InLine.word_andb (w, maxDigitL)
	    fun high w = InLine.word_rshiftl (w, hBaseBits)
	    val (vh, vl) = (high v, low v)
	    val (ih, il) = (high i, low i)

	    fun adj (q, r, vx) =
		let val x = InLine.word_add (InLine.word_lshift (r, hBaseBits), vx)
		    val y = InLine.word_mul (q, il)
		    fun loop (q, x) =
			if InLine.word_ge (x, y) then (q, InLine.word_sub (x, y))
			else loop (InLine.word_sub (q, 0w1), InLine.word_add (x, i))
		in loop (q, x)
		end

	    val q1 = InLine.word_div (u, ih)
	    val r1 = InLine.word_mod (u, ih)
	    val (q1, r1) = adj (q1, r1, vh)
	    val q0 = InLine.word_div (r1, ih)
	    val r0 = InLine.word_mod (r1, ih)
	    val (q0, r0) = adj (q0, r0, vl)
	in (InLine.word_add (InLine.word_lshift (q1, hBaseBits), q0), r0)
	end

    (* divide bignat by digit>0 *)
    fun natdivmodd (m, 0w1) = (m, 0w0) (* speedup *)
      | natdivmodd (m, i) = let
            val scale = scale i
            val i' = InLine.word_mul (i, scale)
            val m' = natmadd (scale, m, 0w0)
            fun dmi [] = ([], 0w0)
              | dmi (d::r) = let
                    val (qt,rm) = dmi r
                    val (q1,r1) = natdivmod2 ((rm,d), i')
                in
		    (consd (q1,qt), r1)
		end
            val (q,r) = dmi m'
        in
	    (q, InLine.word_div (r, scale))
	end

    (* From Knuth Vol II, 4.3.1, but without opt. in step D3 *)
    fun natdivmod (m, []) = raise Assembly.Div
      | natdivmod ([], n) = ([], []) (* speedup *)
      | natdivmod (d::r, 0w0::s) = let
            val (qt,rm) = natdivmod (r,s)
        in (qt, consd (d, rm)) end (* speedup *)
      | natdivmod (m, [d]) = let
            val (qt, rm) = natdivmodd (m, d)
        in (qt, if InLine.word_eql (rm, 0w0) then [] else [rm]) end
      | natdivmod (m, n) = let
	    val (ln, last) = length'n'last n (* ln >= 1 *)
	    val scale = scale last
            val m' = natmadd (scale, m, 0w0)
            val n' = natmadd (scale, n, 0w0)
            val n1 = nth (ln, n')	(* >= base/2 *)
            fun divl [] = ([], [])
              | divl (d::r) = let
                    val (qt,rm) = divl r
                    val m = consd (d, rm)
                    fun msds ([],_) = (0w0,0w0)
                      | msds ([d],0) = (0w0,d)
                      | msds ([d2,d1],0) = (d1,d2)
                      | msds (d::r,i) = msds (r,InLine.int_sub (i, 1))
                    val (m1,m2) = msds (m, ln)
                    val tq = if InLine.= (m1, n1) then maxDigit
                             else #1 (natdivmod2 ((m1,m2), n1))
                    fun try (q,qn') = (q, natsub0 (m,qn'))
                	handle Negative => try (InLine.word_sub (q,0w1),
						natsub0 (qn', n'))
                    val (q,rr) = try (tq, natmadd (tq,n',0w0))
                in (consd (q,qt), rr) end
            val (qt,rm') = divl m'
            val (rm,_(*0*)) = natdivmodd (rm',scale)
        in
	    (qt,rm)
	end

  (* general case for quotRem *)
    fun quotRem'' ({digits=xDigits, negative=xNeg}, {digits=yDigits, negative=yNeg}) = let
	  val (q, r) = natdivmod (xDigits, yDigits)
	  in (
	    if InLine.<> (xNeg, yNeg) then zneg q else posi q,
	    if xNeg then zneg r else posi r
	  ) end

    fun quotRem' (_, BI { digits = [], ... }) = raise Assembly.Div
      | quotRem' (BI { digits = [], ... }, _) = (zero, zero)
      | quotRem' (BI x, BI y) = quotRem'' (x, y)

  (* general case for divMod *)
    fun divMod'' ({digits=xDigits, negative=xNeg}, {digits=yDigits, negative=yNeg}) =
	  if InLine.= (xNeg, yNeg)
	    then let
	      val (q, r) = natdivmod (xDigits, yDigits)
	      in
		(posi q, if xNeg then zneg r else posi r)
	      end
	    else let
	      val (q, r) = natdivmod (natdec xDigits, yDigits)
	      val mdd = natsub (yDigits, r, true)
	      in
		(negi (natinc q), if xNeg then posi mdd else zneg mdd)
	      end

    fun divMod' (_, BI { digits = [], ... }) = raise Assembly.Div
      | divMod' (BI { digits = [], ... }, _) = (zero, zero)
      | divMod' (BI x, BI y) = divMod'' (x, y)

    fun div' arg = #1 (divMod' arg)
    fun quot' arg = #1 (quotRem' arg)

  (* For mod and rem we special-case a divisor of 2 (common even-odd test) *)
    fun mod' (_, BI { digits = [], ... }) = raise Assembly.Div
      | mod' (BI { digits = [], ... }, _) = zero
      | mod' (BI { digits = low :: _, ... },
	      BI { digits = [0w2], negative }) =
	  if InLine.word_eql (InLine.word_andb (low, 0w1), 0w0) then zero
	  else BI { digits = [0w1], negative = negative }
      | mod' (BI x, BI y) = #2 (divMod'' (x, y))

    fun rem' (_, BI { digits = [], ... }) = raise Assembly.Div
      | rem' (BI { digits = [], ... }, _) = zero
      | rem' (BI { digits = low :: _, negative },
	      BI { digits = [0w2], ... }) =
	  if InLine.word_eql (InLine.word_andb (low, 0w1), 0w0) then zero
	  else BI { digits = [0w1], negative = negative }
      | rem' (BI x, BI y) = #2 (quotRem'' (x, y))

    fun natpow (_, 0) = [0w1]
      | natpow ([], n) = if InLine.int_lt (n, 0) then raise Assembly.Div else []
      | natpow (x, n) =
	  if InLine.int_lt (n, 0) then []
	  else let fun exp (m, 0w0) = [0w1]
		     | exp (m, 0w1) = m
		     | exp (m, n) = let
			   val x = exp (m, InLine.word_rshiftl (n, 0w1))
			   val y = natmul (x, x)
		       in
			   if InLine.word_eql (InLine.word_andb (n, 0w1), 0w0)
                           then y
			   else natmul (y, m)
		       end
	       in
		   exp (x, InLine.int_to_word n)
	       end

    fun pow (_, 0) = abstract (BI { negative = false, digits = [0w1] })
      | pow (i, n) = let
	    val BI { negative, digits } = concrete i
	in
	    abstract (bi { negative = negative andalso
	                              InLine.int_eql (InLine.int_rem (n, 2), 1),
			   digits = natpow (digits, n) })
	end

    val ~ = fabs1 neg
    val - = fabs2 sub
    val + = fabs2 add
    val * = fabs2 mul
    val div = fabs2 div'
    val mod = fabs2 mod'
    val quot = fabs2 quot'
    val rem = fabs2 rem'
    val divMod = fabs22 divMod'
    val quotRem = fabs22 quotRem'
    val < = fabs2c lt
    val > = fabs2c gt
    val <= = fabs2c le
    val >= = fabs2c ge
    val abs = fabs1 abs'
    val compare = fabs2c compare'

  end
