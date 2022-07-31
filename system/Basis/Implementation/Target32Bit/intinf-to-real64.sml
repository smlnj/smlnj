(* intinf-to-real64.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Conversion from IntInf.int to 64-bit reals.  This conversion depends on the target
 * word size, since that determines the number of digits used in the conversion.
 *)

structure IntInfToReal64 : sig

    val cvt : IntInf.int -> Real64.real

  end = struct

    val fromInt32 : Int32.int -> real = InlineT.Real64.from_int32
    fun w2r w = fromInt32(InlineT.Int32.fromLarge(InlineT.Word.toLargeInt w))
    val baseBits = InlineT.Word.toIntX CoreIntInf.baseBits

    val rbase = w2r CoreIntInf.base

  (* some protection against insanity... *)
    val _ = if baseBits < 18  (* i.e., 3 * baseBits < 53 *)
	  then raise Fail "big digits in intinf implementation do not have enough bits"
	  else ()

    fun cvt (x : IntInf.int) = let
	  val CoreIntInf.BI{ negative, digits } = CoreIntInf.concrete x
	  fun dosign (x : real) = if negative then ~x else x
	(* We need at most three "big digits" to get 53 bits of precision...
	 * (See insanity insurance above.)
	 *)
	  fun calc (k, d1, d2, d3, []) =
		dosign (Assembly.A.scalb (w2r d1 +
					  rbase * (w2r d2 + rbase * w2r d3),
					  k))
	    | calc (k, _, d1, d2, d3 :: r) = calc (k + baseBits, d1, d2, d3, r)
	  in
	    case digits
	     of [] => 0.0
	      | [d] => dosign (w2r d)
	      | [d1, d2] => dosign (rbase * w2r d2 + w2r d1)
	      | d1 :: d2 :: d3 :: r => calc (0, d1, d2, d3, r)
	    (* end case *)
	  end

  end
