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

    val fromInt64 : Int64.int -> real = InlineT.Real64.from_int64
    fun w2r w = fromInt64(InlineT.Int64.fromLarge(InlineT.Word.toLargeInt w))
    val baseBits = InlineT.Word.toIntX CoreIntInf.baseBits

    val rbase = w2r CoreIntInf.base

  (* some protection against insanity... *)
    val _ = if baseBits <> 62
	  then raise Fail "unexpected baseBits in intinf implementation"
	  else ()

    fun cvt (x : IntInf.int) = let
	  val CoreIntInf.BI{ negative, digits } = CoreIntInf.concrete x
	  fun dosign (x : real) = if negative then ~x else x
	(* We need at most two "big digits" to get 53 bits of precision...
	 * (See insanity insurance above.)
	 *)
	  fun calc (k, d1, d2, []) =
		dosign (Assembly.A.scalb (w2r d1 + rbase * w2r d2, k))
	    | calc (k, _, d1, d2 :: r) = calc (k + baseBits, d1, d2, r)
	  in
	    case digits
	     of [] => 0.0
	      | [d] => dosign (w2r d)
	      | [d1, d2] => dosign (w2r d1 + rbase * w2r d2)
	      | d1 :: d2 :: r => calc (0, d1, d2, r)
	    (* end case *)
	  end

  end
