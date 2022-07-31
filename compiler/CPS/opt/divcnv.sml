(* divcnv.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * The SML Int.div and Int.mod operations round to negative infinity, with
 * the properties:
 *
 *	x div y		= floor(x / y)
 *	x		= y * (x div y) + (x mod y)
 *
 * The signs of the of the arguments and results satisfy the following table:
 *
 *      x       y      (x div y)     (x mod y)
 *     ---     ---     ---------     ---------
 *      +       +          +             +
 *      +       -          -             -
 *      -       +          -             +
 *      -       -          +             -
 *
 * When the signs of x and y are the same, then Int.div and Int.mod return the
 * same results as Int.quot and Int.rem (resp.), but when the signs are different,
 * we must xorArgs the result (assuming that the remainder is non-zero).
 *
 * We use the following implementation:
 *
 *    int div ( int D, int d )
 *    {
 *      int q = D / d;
 *      int r = D % d;
 *	if (((x ^ y) < 0) && (r != 0)) q = q-1;
 *      return q;
 *    }
 *
 *    int mod ( int D, int d )
 *    {
 *      int r = D % d;
 *      if (((x ^ y) < 0) && (r != 0)) r = r+d;
 *      return r;
 *    }
 *
 * For the LLVM backend, the `/` and `%` operations in `div` will be be combined into
 * a single `/` operation (that produces both results).
 *)

structure DivCnv : sig

  (* expand the `div` operation to use native machine arithmetic. *)
    val expandDiv : CPS.value list * CPS.lvar * CPS.cty * CPS.cexp -> CPS.cexp

  (* expand the `mod` operation to use native machine arithmetic. *)
    val expandMod : CPS.value list * CPS.lvar * CPS.cty * CPS.cexp -> CPS.cexp

  end = struct

    structure C = CPS
    structure P = C.P
    structure LV = LambdaVar

    fun bug s = ErrorMsg.impossible ("DivCnv: " ^ s)

    datatype sign = N | P | U	(* negative, positive, unknown *)

  (* bit-width of target word (32 or 64) *)
    val ity = Target.mlValueSz
  (* bit-width of default tagged integer type *)
    val tty = Target.defaultIntSz

    val intTy = C.NUMt{sz = ity, tag=false}
    val tagTy = C.NUMt{sz = tty, tag=true}

    val zero = C.NUM{ival = 0, ty = {sz = ity, tag=false}}
    val one = C.NUM{ival = 1, ty = {sz = ity, tag=false}}
    fun num iv = C.NUM{ival = iv, ty = {sz = ity, tag=false}}

    fun signOf (C.NUM{ival, ...}) = if (ival < 0) then N else P
      | signOf _ = U

    fun arith (oper, args, res, ty, k) =
	  C.ARITH(P.IARITH{oper=oper, sz=ity}, args, res, ty, k)
    fun pure (oper, kind, args, res, ty, k) =
	  C.PURE(P.PURE_ARITH{oper=oper, kind=kind}, args, res, ty, k)
    fun branch (cmp, args, k1, k2) =
	  C.BRANCH(P.CMP{oper=cmp, kind=P.INT ity}, args, LV.mkLvar(), k1, k2)
    fun untag (x, x', k) =
	  C.PURE(P.EXTEND{from=tty, to=ity}, [x], x', intTy, k)
    fun tag (x, x', k) =
	  C.ARITH(P.TEST{from=ity, to=tty}, [x], x', tagTy, k)

    fun expandDiv' (purePrim, x, y, res, k) = let
	  fun divide k = let
		val q = LV.mkLvar()
		val r = LV.mkLvar()
		in
		  if purePrim
		    then pure(P.QUOT, P.INT ity, [x, y], q, intTy,
		      pure(P.REM, P.INT ity, [x, y], r, intTy,
			k (C.VAR q, C.VAR r)))
		    else arith(P.IQUOT, [x, y], q, intTy,
		      arith(P.IREM, [x, y], r, intTy,
			k (C.VAR q, C.VAR r)))
		end
	  fun xorArgs k (q, r) = let
		val sgn = LV.mkLvar()
		in
		  pure(P.XORB, P.UINT ity, [x, y], sgn, intTy, k (C.VAR sgn) (q, r))
		end
	  fun chkSign (tst, args) (q, r) = let
		val jk = LV.mkLvar()
		val q' = LV.mkLvar()
		in
		  C.FIX([(C.CONT, jk, [res], [intTy], k)],
		    branch (tst, args,
		      branch (P.NEQ, [r, zero],
			pure (P.SUB, P.INT ity, [q, one], q', intTy,
			  C.APP(C.VAR jk, [C.VAR q'])),
			C.APP(C.VAR jk, [q])),
		      C.APP(C.VAR jk, [q])))
		end
	  in
	  (* when one of the arguments is a known constant, we can simplify the
	   * sign test to avoid the XORB operation.
	   *)
	    case (signOf x, signOf y)
	     of (U, N) => divide (chkSign (P.GTE, [x, zero]))
	      | (U, P) => divide (chkSign (P.LT, [x, zero]))
	      | (N, U) => divide (chkSign (P.GTE, [y, zero]))
	      | (P, U) => divide (chkSign (P.LT, [y, zero]))
	    (* Note: we subsume the case where both arguments are known into the
	     * general case, since that will be a very rare occurrence (i.e., if
	     * there will be an overflow or if contraction is disabled), so it
	     * is not worth special handling.
	     *)
	      | _ => divide (xorArgs (fn sgn => chkSign (P.LT, [sgn, zero])))
	    (* end case *)
	  end (* expandDiv *)

    fun expandDiv ([_, C.NUM{ival=0, ...}], _, _, _) = bug "impossible divide by zero"
      | expandDiv ([x, y], res, C.NUMt{sz, tag=true}, k) = let
	(* division of tagged numbers, so we wrap it with untagging/tagging code *)
	  val res' = LV.mkLvar()
	  val tagResExp = tag (C.VAR res', res, k)
	  fun untagY x' = (case y
		 of C.NUM{ival, ...} => expandDiv' (true, x', num ival, res', tagResExp)
		  | _ => let
		      val y' = LV.mkLvar()
		      in
			untag (y, y',
			  expandDiv' (true, x', C.VAR y', res', tagResExp))
		      end
		(* end case *))
	  in
	    case x
	     of C.NUM{ival, ...} => untagY (num ival)
	      | _ => let
		  val x' = LV.mkLvar()
		  in
		    untag (x, x', untagY (C.VAR x'))
		  end
	    (* end case *)
	  end
      | expandDiv ([x, y], res, C.NUMt{sz, ...}, k) =
	  if (sz = ity)
	    then expandDiv' (false, x, y, res, k)
	    else bug "expandDiv: bogus argument size"
      | expandDiv _ = bug "expandDiv: bogus arguments"

    fun expandMod' (purePrim, x, y, res, k) = let
	  fun modulo k = let
		val r = LV.mkLvar()
		in
		  if purePrim
		    then pure(P.REM, P.INT ity, [x, y], r, intTy, k(C.VAR r))
		    else arith(P.IREM, [x, y], r, intTy, k(C.VAR r))
		end
	  fun xorArgs k r = let
		val sgn = LV.mkLvar()
		in
		  pure(P.XORB, P.UINT ity, [x, y], sgn, intTy, k (C.VAR sgn) r)
		end
	  fun chkSign (tst, args) r = let
		val jk = LV.mkLvar()
		val r' = LV.mkLvar()
		in
		  C.FIX([(C.CONT, jk, [res], [intTy], k)],
		    branch (tst, args,
		      branch (P.NEQ, [r, zero],
			pure (P.ADD, P.INT ity, [r, y], r', intTy,
			  C.APP(C.VAR jk, [C.VAR r'])),
			C.APP(C.VAR jk, [r])),
		      C.APP(C.VAR jk, [r])))
		end
	  in
	  (* when one of the arguments is a known constant, we can simplify the
	   * sign test to avoid the XORB operation.
	   *)
	    case (signOf x, signOf y)
	     of (U, N) => modulo (chkSign (P.GTE, [x, zero]))
	      | (U, P) => modulo (chkSign (P.LT, [x, zero]))
	      | (N, U) => modulo (chkSign (P.GTE, [y, zero]))
	      | (P, U) => modulo (chkSign (P.LT, [y, zero]))
	    (* Note: we subsume the case where both arguments are known into the
	     * general case, since that will be a very rare occurrence (i.e., if
	     * contraction is disabled), so it is not worth special handling.
	     *)
	      | _ => modulo (xorArgs (fn sgn => chkSign (P.LT, [sgn, zero])))
	    (* end case *)
	  end

    fun expandMod ([_, C.NUM{ival=0, ...}], _, _, _) = bug "impossible modulo zero"
      | expandMod ([x, y], res, C.NUMt{sz, tag=true}, k) = let
	(* modulo of tagged numbers, so we wrap it with untagging/tagging code *)
	  val res' = LV.mkLvar()
	  val tagResExp = tag (C.VAR res', res, k)
	  fun untagY x' = (case y
		 of C.NUM{ival, ...} => expandMod' (true, x', num ival, res', tagResExp)
		  | _ => let
		      val y' = LV.mkLvar()
		      in
			untag (y, y',
			  expandMod' (true, x', C.VAR y', res', tagResExp))
		      end
		(* end case *))
	  in
	    case x
	     of C.NUM{ival, ...} => untagY (num ival)
	      | _ => let
		  val x' = LV.mkLvar()
		  in
		    untag (x, x', untagY (C.VAR x'))
		  end
	    (* end case *)
	  end
      | expandMod ([x, y], res, C.NUMt{sz, ...}, k) =
	  if (sz = ity)
	    then expandMod' (false, x, y, res, k)
	    else bug "expandMod: bogus argument size"
      | expandMod _ = bug "expandMod: bogus arguments"

  end
