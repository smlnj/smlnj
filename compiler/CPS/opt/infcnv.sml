(* infcnv.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Expand out any remaining occurences of test_inf, trunc_inf, extend_inf,
 * and copy_inf.  These primops carry a second argument, which is the
 * function that performs the operation.
 *)

structure IntInfCnv : sig

  (* `toInf (prim, sz, [x, f], v, t, e)` generates code to convert a fixed-size
   * number to an intinf.  The arguments are:
   *	prim		-- the primop (COPY or EXTEND) for converting a tagged
   *			   value to the boxed number type
   *	sz		-- the size of the source value
   *	x		-- the value being converted
   *	f		-- the conversion function from the "_Core" structure.
   *	v, t, e		-- \v:t.e is the continuation of the conversion.
   *)
    val toInf : ({from:int, to:int} -> CPS.P.pure) * int * CPS.value list * CPS.lvar * CPS.cty * CPS.cexp
	  -> CPS.cexp

  (* `truncInf (sz, [x, f], v, t, e)` generates code to truncate an IntInf.int
   * to a fixed size representation.  The arguments are:
   *	sz		-- the size of the source value
   *	x		-- the value being converted
   *	f		-- the conversion function from the "_Core" structure.
   *	v, t, e		-- \v:t.e is the continuation of the conversion.
   *)
    val truncInf : int * CPS.value list * CPS.lvar * CPS.cty * CPS.cexp -> CPS.cexp

  (* `testInf (sz, [x, f], v, t, e)` generates code to convert an IntInf.int
   * to a fixed size representation with an overflow check.  The arguments are:
   *	sz		-- the size of the source value
   *	x		-- the value being converted
   *	f		-- the conversion function from the "_Core" structure.
   *	v, t, e		-- \v:t.e is the continuation of the conversion.
   *)
    val testInf : int * CPS.value list * CPS.lvar * CPS.cty * CPS.cexp -> CPS.cexp

end = struct

    structure C = CPS
    structure LV = LambdaVar

    fun bug msg = ErrorMsg.impossible ("IntInfCnv: " ^ msg)

    val boxNumSz = Target.mlValueSz	(* 32 or 64 *)

    val tagNumTy = C.NUMt{tag = true, sz = Target.defaultIntSz}
    val boxNumTy = C.NUMt{tag = false, sz = boxNumSz}

    fun toInf (prim, sz, [x, f], v, t, e) = let
	  val k = LV.mkLvar ()
	  val body = if (sz <= Target.defaultIntSz)
		  then let
		  (* for tagged values, we promote to the boxed type before calling
		   * the conversion function.
		   *)
		    val v' = LV.mkLvar ()
		    in
		      C.PURE (prim{from=sz, to=boxNumSz}, [x], v', boxNumTy,
			C.APP (f, [C.VAR k, C.VAR v']))
		    end
		else if (sz = boxNumSz)
		  then C.APP (f, [C.VAR k, x])
		  else let
		  (* for a 64-bit argument on 32-bit target, we need to extern the
		   * argument to a pair of 32-bit words, before calling the
		   * conversion function.
		   *)
		    val hi = LV.mkLvar ()
		    val lo = LV.mkLvar ()
		    in
		      C.SELECT(0, x, hi, boxNumTy,
		      C.SELECT(1, x, lo, boxNumTy,
			C.APP (f, [C.VAR k, C.VAR hi, C.VAR lo])))
		    end
	  in
	    C.FIX ([(C.CONT, k, [v], [t], e)], body)
	  end
      | toInf _ = bug "toInf: incorrect number of arguments"

    fun truncInf (sz, [x, f], v, t, e) = let
	  val k = LV.mkLvar ()
	  in
	    if (sz <= Target.defaultIntSz)
	      then let
		val v' = LV.mkLvar ()
		val retContBody =
		      C.PURE (C.P.TRUNC{from=boxNumSz, to=sz}, [C.VAR v'], v, t, e)
		in
		  C.FIX (
		    [(C.CONT, k, [v'], [boxNumTy], retContBody)],
		    C.APP (f, [C.VAR k, x]))
		end
	    else if (sz = boxNumSz)
	      then C.FIX ([(C.CONT, k, [v], [t], e)], C.APP (f, [C.VAR k, x]))
	      else let
	      (* for a 64-bit result on 32-bit target, we need to intern the
	       * result, which will be a packed pair of 32-bit words.
	       *)
		val hi = LV.mkLvar ()
		val lo = LV.mkLvar ()
		val retContBody = C.RECORD(C.RK_RAWBLOCK, [
			(C.VAR hi, C.OFFp 0), (C.VAR lo, C.OFFp 0)
		      ], v, e)
		in
		  C.FIX (
		    [(C.CONT, k, [hi, lo], [boxNumTy, boxNumTy], retContBody)],
		    C.APP (f, [C.VAR k, x]))
		end
	  end
      | truncInf _ = bug "truncInf: incorrect number of arguments"

    fun testInf (sz, [x, f], v, t, e) = let
	  val k = LV.mkLvar ()
	  in
	    if (sz <= Target.defaultIntSz)
	      then let
		val v' = LV.mkLvar ()
	      (* NOTE: we may need to lower the TEST from boxNumSz to sz! *)
		val retContBody = TestCnv.test (boxNumSz, sz, [C.VAR v'], v, t, e)
		in
		  C.FIX (
		    [(C.CONT, k, [v'], [boxNumTy], retContBody)],
		    C.APP (f, [C.VAR k, x]))
		end
	    else if (sz = boxNumSz)
	      then C.FIX ([(C.CONT, k, [v], [t], e)], C.APP (f, [C.VAR k, x]))
	      else let
	      (* for a 64-bit result on 32-bit target, we need to intern the
	       * result, which will be a packed pair of 32-bit words.
	       *)
		val hi = LV.mkLvar ()
		val lo = LV.mkLvar ()
		val retContBody = C.RECORD(C.RK_RAWBLOCK, [
			(C.VAR hi, C.OFFp 0), (C.VAR lo, C.OFFp 0)
		      ], v, e)
		in
		  C.FIX (
		    [(C.CONT, k, [hi, lo], [boxNumTy, boxNumTy], retContBody)],
		    C.APP (f, [C.VAR k, x]))
		end
	  end
      | testInf _ = bug "testInf: incorrect number of arguments"

  end
