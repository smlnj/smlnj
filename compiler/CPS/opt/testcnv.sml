(* testcnv.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 *	#define MAX_TO_INT	((1 << (to - 1)) - 1)
 *	#define MIN_TO_INT	-maxToInt - 1
 *
 *	to_int test (from_int x)
 *	{
 *	    if ((x < MIN_TO_INT) || (MAX_TO_INT < x))
 *		raise Overflow;
 *	    else
 *		return (to_int)x;
 *	}
 *
 *	to_int testu (from_uint x)
 *	{
 *	    if ((unsigned)x <= (unsigned)MAX_TO_INT)
 *		return (to_int)x;
 *	    else
 *		raise Overflow;
 *	}
 *)

structure TestCnv : sig

  (* lower a `TEST` (int -> int conversion) to explicit bounds checks; we expect
   * that the `to` type will be a tagged size.
   *)
    val test : int * int * CPS.value list * CPS.lvar * CPS.cty * CPS.cexp -> CPS.cexp

  (* lower a `TESTU` (word -> int conversion) to explicit bounds checks. *)
    val testu : int * int * CPS.value list * CPS.lvar * CPS.cty * CPS.cexp -> CPS.cexp

  end = struct

    structure C = CPS
    structure P = C.P
    structure LV = LambdaVar

    fun bug s = ErrorMsg.impossible ("TestCnv: " ^ s)

  (* bit width of target word (32 or 64) *)
    val ity = Target.mlValueSz
  (* bit width of default tagged integer size (31 or 63) *)
    val tty = Target.defaultIntSz

    fun sLT sz = P.CMP{oper=P.LT, kind=P.INT sz}
    fun uLE sz = P.CMP{oper=P.LTE, kind=P.UINT sz}
    fun branch (cmp, args, k1, k2) = C.BRANCH(cmp, args, LV.mkLvar(), k1, k2)
    fun zero ty = C.NUM{ival=0, ty=ty}

    (* force an overflow trap by adding the 2^63+2^63 *)
    fun mkTrap k = let
	  val tmp = LV.mkLvar()
	  val ty = {tag=false, sz=ity}
	  val n = C.NUM{ival = IntInf.<<(1, Word.fromInt(ity-1)), ty = ty}
	  in
	    C.ARITH(P.IARITH{oper=P.IADD, sz=ity}, [n, n], tmp, C.NUMt ty, k)
	  end

    fun test (from, to, [v], x, ty, k) =
	  if (from = ity) andalso (to = tty)
            (* conversion from native int (e.g., Int64) to default int (e.g., Int63) *)
	    then C.ARITH(P.TEST{from=from, to=to}, [v], x, ty, k)
	  else if (from = to)
	    then C.PURE(P.COPY{from=from, to=to}, [v], x, ty, k)
	  else if (from <= ity) andalso (to < tty)
	    then let
              val toTy = {sz=to, tag=true}
	      val fromIsTagged = (from < ity)
	      fun num iv = C.NUM{ival=iv, ty={sz=from, tag=fromIsTagged}}
	      val maxToInt = IntInf.<<(1, Word.fromInt(to - 1)) - 1
	      val minToInt = ~(maxToInt + 1)
              (* the name of the trap join continuation *)
              val trapK = LV.mkLvar()
              val trapK' = C.VAR trapK
              (* the "fake" join *)
	      val jk = LV.mkLvar()
	      val jk' = C.VAR jk
(*
	      val trap = C.TRAP(C.APP(jk', [v]))
*)
	      val x' = LV.mkLvar()
	      in
		C.FIX([(C.CONT, jk, [x], [C.NUMt toTy], k)],
                C.FIX([(C.CONT, trapK, [LV.mkLvar()], [C.ENUMt], mkTrap(C.APP(jk', [zero toTy])))],
		  branch(sLT from, [v, num minToInt],
		    C.APP(trapK', [C.ENUM 0]),
		    branch(sLT from, [num maxToInt, v],
		      C.APP(trapK', [C.ENUM 0]),
(* FIXME: the following `C.ARITH(C.TEST{from=from, to=tty}` really should be
 * `C.PURE(C.TRUNC{from=from, to=to}", since we have already done the range test,
 * but the `TRUNC` causes the sign extension to be masked out for negative numbers.
 * (see Issue #476)
 *)
		      C.ARITH(P.TEST{from=from, to=tty}, [v], x', C.NUMt toTy,
                        C.APP(jk', [C.VAR x']))))))
	      end
	    else bug "TEST with unexpected precisions"
      | test _ = bug "TEST with bogus arguments"

    fun testu (from, to, [v], x, ty, k) =
	  if (from = to) andalso ((from = ity) orelse (from = tty))
            (* conversion from native or default word type (e.g., Word64 or Word63)
             * to native int type (e.g., Int64 or Int63)
             *)
	    then C.ARITH(P.TESTU{from=from, to=to}, [v], x, ty, k)
	    else let
              val toTy = {sz=to, tag=true}
	      val fromIsTagged = (from < ity)
	      fun num iv = C.NUM{ival=iv, ty={sz=from, tag=fromIsTagged}}
	      val maxToInt = IntInf.<<(1, Word.fromInt(to - 1)) - 1
	      val jk = LV.mkLvar()
	      val jk' = C.VAR jk
	      val x' = LV.mkLvar()
	      in
		C.FIX([(C.CONT, jk, [x], [C.NUMt toTy], k)],
		  branch(uLE from, [v, num maxToInt],
		    C.PURE(P.TRUNC{from=from, to=to}, [v], x', ty,
		      C.APP(jk', [C.VAR x'])),
(*
		    C.TRAP(C.APP(jk', [v]))))
*)
		    mkTrap (C.APP(jk', [zero toTy]))))
	      end
      | testu _ = bug "TESTU with bogus arguments"

  end
