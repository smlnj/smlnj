(* real64-values.sml
 *
 * Specific Real64 values that we need both in the real64.sml and num-scan.sml
 * files.
 *
 * COPYRIGHT (c) 2022 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Real64Values : sig

  (* maximum finite 64-bit real value *)
    val maxFinite : real
  (* minimum normalized positive real value *)
    val minNormalPos : real
  (* minimum positive real value (denormalized) *)
    val minPos : real
  (* positive infinity *)
    val posInf : real
  (* negative infinity *)
    val negInf : real
  (* nan with zero sign bit *)
    val posNaN : real
  (* nan with one sign bit *)
    val negNaN : real

  end = struct

    structure Word = InlineT.Word
    structure Word64 = InlineT.Word64
    structure Real64 = InlineT.Real64

  (* The next three values are computed laboriously, partly to
   * avoid problems with inaccurate string->float conversions
   * in the compiler itself.
   *)
    val maxFinite = let
	  fun f (x, i) = if i = 1023 then x else f(x*2.0, i + 1)
	  val y = f(1.0, 0)
	  fun g (z, y, 0) = z
	    | g (z, y, i) = g (z+y, y*0.5, i - 1)
	  in
	    g (0.0, y, 53)
	  end

    val minNormalPos = let
	  fun isNormal x = let
		val biasExp = Word.andb(
		      Word.fromLarge(Word64.rshiftl(InlineT.Real64.toBits x, 0w52)),
		      0wx7ff)
		in
		  (0w0 < biasExp) andalso (biasExp < 0w2047)
		end
	  fun f x = let
		val y = x * 0.5
		in
		  if isNormal y then f y else x
		end
	  in
	    f 1.0
	  end

    local
      (* The x86 uses extended precision (80 bits) internally, therefore
       * it is necessary to write out the result of r * 0.5 to get
       * 64 bit precision.
       *)
      val mem = InlineT.PolyArray.array(1, minNormalPos)
      val update = InlineT.PolyArray.update
      val subscript = InlineT.PolyArray.chkSub
      fun f () = let
	    val r = subscript(mem, 0)
	    val y = r * 0.5
	    in
	       update(mem, 0, y);
	       if Real64.==(subscript(mem, 0), 0.0) then r else f ()
	    end
    in
    val minPos = f()
    end (* local *)

    val posInf = maxFinite * maxFinite
    val negInf = ~posInf
    val (negNaN, posNaN) = let
        (* generate a NaN and then check its bits to figure out the positive
         * and negative NaN values.
         *)
          val nan = 0.0 / 0.0
          in
            if Real64.signBit nan
              then (nan, Real64.~ nan)
              else (Real64.~ nan, nan)
          end

  end
