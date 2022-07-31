(* lower.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This module implements a lowering pass for the CPS IR that should
 * be run after optimization, but before closure conversion.  The lowering
 * pass makes the following changes:
 *
 *	- expand 64-bit arithmetic to operations on pairs of 32-bit words
 *	  (on 32-bit targets only).
 *
 *	- replace conversions that involve the `IntInf.int` type with
 *	  calls to the pre-basis library code (see `system/smlnj/init`),
 *
 *	- replace the `round-to-negative-infinity` division operations
 *	  (i.e., `div` and `mod`) with native arithmetic operations.
 *
 *	- replace `TEST` and `TESTU` narrowing conversions (other than
 *	  from the native integer size to the tagged integer size) with
 *	  code that tests for overflow and explicitly raises the exception
 *	  if necessary.
 *
 *	- replace `STREQL` with unrolled sequence of word-sized equality
 *	  tests.
 *
 * Note that the bulk of the work is actually done in other modules; this
 * module is responsible for detecting places where things need transforming.
 *)

structure LowerCPS : sig

    val transform : CPS.function -> CPS.function

  end = struct

    structure C = CPS
    structure P = C.P
    structure LV = LambdaVar

    fun bug s = ErrorMsg.impossible ("LowerCPS: " ^ s)

    val ity = Target.mlValueSz
    val tty = Target.defaultIntSz

    fun transform cfun = let
	  fun function (fk, f, formals, tl, e) = let
		fun cexp (C.RECORD(rk, xl, v, e)) =
		      C.RECORD(rk, xl, v, cexp e)
		  | cexp (C.SELECT(i, x, v, t, e)) =
		      C.SELECT(i, x, v, t, cexp e)
		  | cexp (C.OFFSET(i, v, x, e)) =
		      C.OFFSET(i, v, x, cexp e)
		  | cexp (C.APP(x, xl)) =
		      C.APP(x, xl)
		  | cexp (C.FIX(fl, e)) =
		      C.FIX(map function fl, cexp e)
		  | cexp (C.SWITCH(x, v, el)) =
		      C.SWITCH(x, v, map cexp el)
		  | cexp (C.BRANCH(P.STREQL lit, [s], _, e1, e2)) =
		      StrEqlCnv.strEql (s, lit, cexp e1, cexp e2)
		  | cexp (C.BRANCH(P.STREQL _, _, _, _, _)) = bug "bogus STREQL"
		  | cexp (C.BRANCH(b, xl, v, e1, e2)) =
		      C.BRANCH(b, xl, v, cexp e1, cexp e2)
		  | cexp (C.SETTER(s, xl, e)) =
		      C.SETTER(s, xl, cexp e)
		  | cexp (C.LOOKER(l, xl, v, t, e)) =
		      C.LOOKER(l, xl, v, t, cexp e)
		  | cexp (C.PURE(P.COPY_INF sz, args, v, t, e)) =
		      IntInfCnv.toInf (P.COPY, sz, args, v, t, cexp e)
		  | cexp (C.PURE(P.EXTEND_INF sz, args, v, t, e)) =
		      IntInfCnv.toInf (P.EXTEND, sz, args, v, t, cexp e)
		  | cexp (C.PURE(P.TRUNC_INF sz, args, v, t, e)) =
		      IntInfCnv.truncInf (sz, args, v, t, cexp e)
		  | cexp (C.ARITH(P.TEST_INF sz, args, v, t, e)) =
		      IntInfCnv.testInf (sz, args, v, t, cexp e)
		  | cexp (C.ARITH(P.TEST{from, to}, args, v, t, e)) =
		      TestCnv.test(from, to, args, v, t, cexp e)
		  | cexp (C.ARITH(oper as P.TESTU{from, to}, args, v, t, e)) =
		      TestCnv.testu(from, to, args, v, t, cexp e)
		  | cexp (C.ARITH(P.IARITH{oper=P.IDIV, sz}, xl, v, t, e)) =
		      DivCnv.expandDiv (xl, v, t, cexp e)
		  | cexp (C.ARITH(P.IARITH{oper=P.IMOD, sz}, xl, v, t, e)) =
		      DivCnv.expandMod (xl, v, t, cexp e)
		  | cexp (C.ARITH(a, xl, v, t, e)) = C.ARITH(a, xl, v, t, cexp e)
		  | cexp (C.PURE(p, xl, v, t, e)) = C.PURE(p, xl, v, t, cexp e)
		  | cexp (C.RCC(k, s, p, xl, vtl, e)) = C.RCC(k, s, p, xl, vtl, cexp e)
		in
		  (fk, f, formals, tl, cexp e)
		end
	  in
	  (* we expand 64-bit operations first (if required) and then do the rest *)
	    function (Num64Cnv.elim cfun)
	  end

  end
