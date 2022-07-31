(* num64cnv.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This module supports the 64-bit int/word types on 32-bit machines
 * by expanding them to pairs of 32-bit words and replacing the primitive
 * operations with 32-bit code.
 *)

structure Num64Cnv : sig

  (* eliminate 64-bit literals and operations on a 32-bit machine.  This function
   * does not rewrite its argument if either the target is a 64-bit machine or
   * the  no 64-bit operations were detected.
   *)
    val elim : CPS.function -> CPS.function

  end = struct

    structure C = CPS
    structure P = C.P
    structure LV = LambdaVar

    fun bug s = ErrorMsg.impossible ("Num64Cnv: " ^ s)

    fun isNum64Ty (C.NUMt{sz = 64, ...}) = true
      | isNum64Ty _ = false
    val pairTy = C.PTRt C.VPT
    val box32Ty = C.PTRt C.VPT
    val raw32ITy = {sz = 32, tag = false}
    val raw32Ty = C.NUMt raw32ITy		(* assuming a 32-bit machine *)
    val tagNumTy = C.NUMt{sz = 31, tag = true}
    val si32 = P.INT 32
    val ui32 = P.UINT 32

  (* split a 64-bit integer/word literal into two 32-bit unsigned values.  We assume
   * that the argument is in the range -2^63 .. 2^64-1, which is the union of the ranges
   * of Int64.int and Word64.word.
   *)
    fun split (n : IntInf.int) = let
	  val n = if (n < 0) then 0x10000000000000000 + n else n
	  val hi = C.NUM{ival = IntInf.~>>(n, 0w32), ty = {sz = 32, tag = false}}
	  val lo = C.NUM{ival = IntInf.andb(n, 0xffffffff), ty = {sz = 32, tag = false}}
	  in
	    (hi, lo)
	  end

  (* short names for various CPS constructs *)
    val zero = C.NUM{ival=0, ty={sz = 32, tag = false}}
    val one = C.NUM{ival=1, ty={sz = 32, tag = false}}
    fun num n = C.NUM{ival=n, ty={sz = 32, tag = false}}
    fun tagNum n = C.NUM{ival=n, ty={sz = 31, tag = true}}
    fun uIf (oper, a, b, tr, fl) = (* unsigned conditional *)
	  C.BRANCH(P.CMP{oper=oper, kind=ui32}, [a, b], LV.mkLvar(), tr, fl)
    fun sIf (oper, a, b, tr, fl) = (* signed conditional *)
	  C.BRANCH(P.CMP{oper=oper, kind=si32}, [a, b], LV.mkLvar(), tr, fl)
    fun ifzero (v, tr, fl) = uIf(P.EQL, v, zero, tr, fl)
    fun pure (rator, args, ty, k) = let
	  val x = LV.mkLvar()
	  in
	    C.PURE(rator, args, x, ty, k(C.VAR x))
	  end
    fun pure_arith32 (oper, args, k) =
	  pure (P.PURE_ARITH{oper=oper, kind=ui32}, args, raw32Ty, k)
    fun taggedArith (oper, args, k) =
	  pure (P.PURE_ARITH{oper=oper, kind=P.UINT 31}, args, tagNumTy, k)
    fun iarith32 (oper, args, k) = let
	  val x = LV.mkLvar()
	  in
	    C.ARITH(P.IARITH{oper=oper, sz=32}, args, x, raw32Ty, k(C.VAR x))
	  end

  (* bitwise equivalence *)
    fun bitEquiv (a, b, k) =
	  pure_arith32 (P.XORB, [a, b], fn a_xor_b =>
	  pure_arith32 (P.NOTB, [a_xor_b], k))

  (* make an application that will be substituted for a primop; we examine
   * the structure of the continuation expression to avoid creating an
   * eta-redex.
   *)
    fun mkApplyWithReturn (f, args, res, resTy, ce) = let
	  fun mkReturn () = let
		val rk = LV.mkLvar()
		in
		  C.FIX(
		    [(C.CONT, rk, [res], [resTy], ce)],
		    C.APP(f, C.VAR rk :: args))
		end
	  in
	    case ce
	     of C.APP(C.VAR g, [C.VAR arg]) => if (arg = res)
		  then C.APP(f, C.VAR g :: args)
		  else mkReturn ()
	      | _ => mkReturn ()
	    (* end case *)
	  end

  (* bind a continuation around a cexp to avoid code duplication; `res` is the variable
   * to use as a parameter for the code `cexp` (we assume that it is a wrapped 64-bit
   * integer)
   *)
    fun join (res, cexp, k) = let
	  val fnId = LV.mkLvar()
	  in
	    C.FIX([(C.CONT, fnId, [res], [pairTy], cexp)],
	      k (fn v => C.APP(C.VAR fnId, [v])))
	  end

    fun toRes64 (hi, lo, res, cexp) =
	  C.RECORD(C.RK_RAWBLOCK, [(hi, C.OFFp 0), (lo, C.OFFp 0)], res, cexp)

  (* given two 32-bit values that comprise a 64-bit number and a continuation `k`,
   * make code to create the 64-bit object
   *)
    fun to64 (hi, lo, k) = let
	  val pair = LV.mkLvar()
	  in
	    toRes64 (hi, lo, pair, k(C.VAR pair))
	  end

  (* given a 64-bit object and a continuation `k`, make code to unpackage the
   * value into two 32-bit values, which are passed to `k`.  Note that we rely
   * on the "last_contract" phase to handle the case where `n` is a literal.
   *)
    fun from64 (n, k) = let
	  val hi = LV.mkLvar()
	  val lo = LV.mkLvar()
	  in
	    C.SELECT(0, n, hi, raw32Ty,
	    C.SELECT(1, n, lo, raw32Ty,
	      k (C.VAR hi, C.VAR lo)))
	  end

  (* given a 64-bit object and a continuation `k`, make code to unpackage the low 32 word,
   * which is passed to `k`.
   *)
    fun getLo32 (n, k) = let
	  val lo = LV.mkLvar()
	  in
	    C.SELECT(1, n, lo, raw32Ty, k(C.VAR lo))
	  end

  (* given a 64-bit object and a continuation `k`, make code to unpackage the high 32 word,
   * which is passed to `k`.
   *)
    fun getHi32 (n, k) = let
	  val hi = LV.mkLvar()
	  in
	    C.SELECT(0, n, hi, raw32Ty, k(C.VAR hi))
	  end

  (* split a 32-bit value into two 16-bit values *)
    fun split32 (n, k) =
	  pure_arith32(P.RSHIFT, [n, num 16], fn hi =>
	  pure_arith32(P.ANDB, [n, num 0xffff], fn lo =>
	    k (hi, lo)))

  (***** Word64 primitive operations *****)

  (*
   * fun add2 ((hi1, lo1), (hi2, lo2)) = let
   *       val hi = hi1 + hi2
   *       val lo = lo1 + lo2
   *     (* from "Hacker's Delight": c = ((lo1 & lo2) | ((lo1 | lo2) & ¬lo)) >> 31 *)
   *       val carry = (((lo1 ++ lo2) & ~~lo) ++ (lo1 & lo2)) >> 0w31
   *       val hi = hi + carry
   *       in
   *         (hi, lo)
   *       end
   *)
    fun w64Add (n1, n2, res, cexp) =
	  from64 (n1, fn (hi1, lo1) =>
	  from64 (n2, fn (hi2, lo2) =>
	  pure_arith32(P.ADD, [hi1, hi2], fn hi =>
	  pure_arith32(P.ADD, [lo1, lo2], fn lo =>
	  pure_arith32(P.ORB, [lo1, lo2], fn lo1_or_lo2 =>
	  pure_arith32(P.NOTB, [lo], fn not_lo =>
	  pure_arith32(P.ANDB, [lo1_or_lo2, not_lo], fn tmp1 =>
	  pure_arith32(P.ANDB, [lo1, lo2], fn lo1_and_lo2 =>
	  pure_arith32(P.ORB, [lo1_and_lo2, tmp1], fn tmp2 =>
	  pure_arith32(P.RSHIFTL, [tmp2, tagNum 31], fn carry =>
	  pure_arith32(P.ADD, [hi, carry], fn hi =>
	    toRes64 (hi, lo, res, cexp))))))))))))

  (*
   * fun sub ((hi1, lo1), (hi2, lo2)) = let
   *       val hi = hi1 - hi2
   *       val lo = lo1 - lo2
   *     (* from "Hacker's Delight": b = ((¬lo1 & lo2) | ((lo1 ≡ lo2) & lo)) >> 31 *)
   *       val b = (((lo1 ^= lo2) & lo) ++ (~~lo1 & lo2)) >> 0w31
   *       val hi = hi - b
   *       in
   *         (hi, lo)
   *       end
   *)
    fun w64Sub (n1, n2, res, cexp) =
	  from64 (n1, fn (hi1, lo1) =>
	  from64 (n2, fn (hi2, lo2) =>
	  pure_arith32(P.SUB, [hi1, hi2], fn hi =>
	  pure_arith32(P.SUB, [lo1, lo2], fn lo =>
	  bitEquiv(lo1, lo2, fn lo1_eqv_lo2 =>
	  pure_arith32(P.ANDB, [lo1_eqv_lo2, lo], fn tmp1 =>
	  pure_arith32(P.NOTB, [lo1], fn not_lo1 =>
	  pure_arith32(P.ANDB, [not_lo1, lo2], fn tmp2 =>
	  pure_arith32(P.ORB, [tmp1, tmp2], fn tmp3 =>
	  pure_arith32(P.RSHIFTL, [tmp3, tagNum 31], fn borrow =>
	  pure_arith32(P.SUB, [hi, borrow], fn hi =>
	    toRes64 (hi, lo, res, cexp))))))))))))

  (*
   * fun orb (W64(hi1, lo1), W64(hi2, lo2)) = W64(hi1 ++ hi2, lo1 ++ lo2)
   *)
    fun w64Orb (n1, n2, res, cexp) =
	  from64 (n1, fn (hi1, lo1) =>
	  from64 (n2, fn (hi2, lo2) =>
	  pure_arith32(P.ORB, [lo1, lo2], fn lo =>
	  pure_arith32(P.ORB, [hi1, hi2], fn hi =>
	    toRes64 (hi, lo, res, cexp)))))

  (*
   * fun xorb (W64(hi1, lo1), W64(hi2, lo2)) = W64(hi1 ^^ hi2, lo1^^ lo2)
   *)
    fun w64Xorb (n1, n2, res, cexp) =
	  from64 (n1, fn (hi1, lo1) =>
	  from64 (n2, fn (hi2, lo2) =>
	  pure_arith32(P.XORB, [lo1, lo2], fn lo =>
	  pure_arith32(P.XORB, [hi1, hi2], fn hi =>
	    toRes64 (hi, lo, res, cexp)))))

  (*
   * fun andb (W64(hi1, lo1), W64(hi2, lo2)) = W64(hi1 & hi2, lo1 & lo2)
   *)
    fun w64Andb (n1, n2, res, cexp) =
	  from64 (n1, fn (hi1, lo1) =>
	  from64 (n2, fn (hi2, lo2) =>
	  pure_arith32(P.ANDB, [lo1, lo2], fn lo =>
	  pure_arith32(P.ANDB, [hi1, hi2], fn hi =>
	    toRes64 (hi, lo, res, cexp)))))

  (*
   * fun notb (W64(hi, lo)) = W64(Word32.notb hi, Word32.notb lo)
   *)
    fun w64Notb (n, res, cexp) =
	  from64 (n, fn (hi, lo) =>
	  pure_arith32(P.NOTB, [hi], fn hi' =>
	  pure_arith32(P.NOTB, [lo], fn lo' =>
	    toRes64 (hi', lo', res, cexp))))

  (*
    fun neg (hi, 0w0) = (W32.~ hi, 0w0)
      | neg (hi, lo) = (W32.notb hi, W32.~ lo)
   *)
    fun w64Neg (n, res, cexp) = join (res, cexp, fn k =>
	  from64(n, fn (hi, lo) =>
	    ifzero(lo,
	      pure_arith32(P.NEG, [hi], fn hi' => to64 (hi', zero, k)),
	      (* else *)
	      pure_arith32(P.NOTB, [hi], fn hi' =>
	      pure_arith32(P.NEG, [lo], fn lo' =>
		to64 (hi', lo', k))))))

  (* logical shift-right, where we know that amt < 0w64
   *
   * fun w63RShiftL ((hi, lo), amt) =
   *	   if (amt < 32)
   *	     then let
   *	       val hi' = (hi >> amt)
   *           val lo' = (lo >> amt) | (hi << (0w32 - amt))
   *           in
   *             (hi', lo')
   *           end
   *         else (0, (hi >> (amt - 0w32)))
   *
   * Note, that while there is a branch-free version of this, it does not work
   * on the x86 architecture, which uses mod-32 shift amounts.
   *)
    fun w64RShiftL (n, amt, res, cexp) = join (res, cexp, fn k =>
	  from64(n, fn (hi, lo) =>
	    sIf(P.LT, amt, tagNum 32,
	      pure_arith32(P.RSHIFTL, [hi, amt], fn hi' =>
	      pure_arith32(P.RSHIFTL, [lo, amt], fn tmp1 =>
	      taggedArith(P.SUB, [tagNum 32, amt], fn tmp2 =>
	      pure_arith32(P.LSHIFT, [hi, tmp2], fn tmp3 =>
	      pure_arith32(P.ORB, [tmp1, tmp3], fn lo' =>
		to64(hi', lo', k)))))),
	      (* else *)
	      taggedArith(P.SUB, [amt, tagNum 32], fn tmp4 =>
	      pure_arith32(P.RSHIFTL, [hi, tmp4], fn lo' =>
		to64(zero, lo', k))))))

  (*arithmetic shift-right, where we know that amt < 0w64
   *
   * fun w63RShift ((hi, lo), amt) =
   *	   if (amt < 32)
   *	     then let
   *	       val hi' = (hi ~>> amt)
   *           val lo' = (lo >> amt) | (hi << (0w32 - amt))
   *           in
   *             (hi', lo')
   *           end
   *         else (hi ~>> 0w31, (hi ~>> (amt - 0w32)))
   *
   * Note, that while there is a branch-free version of this, it does not work
   * on the x86 architecture, which uses mod-32 shift amounts.
   *)
    fun w64RShift (n, amt, res, cexp) = join (res, cexp, fn k =>
	  from64(n, fn (hi, lo) =>
	    sIf(P.LT, amt, tagNum 32,
	      pure_arith32(P.RSHIFT, [hi, amt], fn hi' =>
	      pure_arith32(P.RSHIFT, [lo, amt], fn tmp1 =>
	      taggedArith(P.SUB, [tagNum 32, amt], fn tmp2 =>
	      pure_arith32(P.LSHIFT, [hi, tmp2], fn tmp3 =>
	      pure_arith32(P.ORB, [tmp1, tmp3], fn lo' =>
		to64(hi', lo', k)))))),
	      (* else *)
	      pure_arith32(P.RSHIFT, [hi, tagNum 31], fn hi' =>
	      taggedArith(P.SUB, [amt, tagNum 32], fn tmp4 =>
	      pure_arith32(P.RSHIFTL, [hi, tmp4], fn lo' =>
		to64(hi', lo', k)))))))

  (* shift-left, where we know that amt < 0w64
   *
   * fun w64LShift ((hi, lo), amt) =
   *	   if (amt < 0w32)
   *	     then let
   *	       val hi' = (hi << amt) | (lo >> (0w32 - amt))
   *           val lo' = (lo << amt)
   *           in
   *             (hi', lo')
   *           end
   *         else (lo << (amt - 0w32), 0)
   *
   * Note, that while there is a branch-free version of this, it does not work
   * on the x86 architecture, which uses mod-32 shift amounts.
   *)
    fun w64LShift (n, amt, res, cexp) = join (res, cexp, fn k =>
	  from64(n, fn (hi, lo) =>
	    sIf(P.LT, amt, tagNum 32,
	      pure_arith32(P.LSHIFT, [hi, amt], fn tmp1 =>
	      taggedArith(P.SUB, [tagNum 32, amt], fn tmp2 =>
	      pure_arith32(P.RSHIFTL, [lo, tmp2], fn tmp3 =>
	      pure_arith32(P.ORB, [tmp1, tmp3], fn hi' =>
	      pure_arith32(P.LSHIFT, [lo, amt], fn lo' =>
		to64(hi', lo', k)))))),
	      (* else *)
	      taggedArith(P.SUB, [amt, tagNum 32], fn tmp4 =>
	      pure_arith32(P.LSHIFT, [lo, tmp4], fn hi' =>
		to64(hi', zero, k))))))

  (*
   * fun w64Eql ((hi1, lo1), (hi2, lo2)) =
   *       (W32.orb(W32.xorb(hi1, hi2), W32.xorb(lo1, lo2)) = 0)
   *)
    fun w64Eql (n1, n2, tr, fl) =
	  from64(n1, fn (hi1, lo1) =>
	  from64(n2, fn (hi2, lo2) =>
	    pure_arith32(P.XORB, [hi1, hi2], fn hi' =>
	    pure_arith32(P.XORB, [lo1, lo2], fn lo' =>
	    pure_arith32(P.ORB, [hi', lo'], fn res =>
	      ifzero (res, tr, fl))))))

  (* the basic pattern for comparisons is
   *   fun cmp ((hi1, lo1), (hi2, lo2)) =
   *         cmpHi(hi1, hi2) orelse ((hi1 = hi2) andalso cmpLo(lo1, lo2))
   *)
    local
      fun w64Cmp (cmpHi, cmpLo) (n1, n2, tr, fl) = let
	  (* continuations for the branches so that we can avoid code duplication *)
	    val trFnId = LV.mkLvar()
	    val tr' = C.APP(C.VAR trFnId, [])
	    val flFnId = LV.mkLvar()
	    val fl' = C.APP(C.VAR flFnId, [])
	    in
	    (* NOTE: closure conversion requires that there only be one continuation
	     * function per FIX!
	     *)
	      C.FIX([(C.CONT, trFnId, [], [], tr)],
	      C.FIX([(C.CONT, flFnId, [], [], fl)],
	      (* (hi1 < hi2) orelse ((hi1 = hi2) andalso (lo1 < lo2)) *)
		getHi32(n1, fn hi1 =>
		getHi32(n2, fn hi2 =>
		  uIf(cmpHi, hi1, hi2,
		    tr',
		    uIf(P.EQL, hi1, hi2,
		      getLo32(n1, fn lo1 =>
		      getLo32(n2, fn lo2 =>
			uIf(cmpLo, lo1, lo2, tr', fl'))),
		      fl'))))))
	    end
    in
    val w64Less = w64Cmp (P.LT, P.LT)
    val w64LessEq = w64Cmp (P.LT, P.LTE)
    val w64Greater = w64Cmp (P.GT, P.GT)
    val w64GreaterEq = w64Cmp (P.GT, P.GTE)
    end (* local *)

  (***** Int64 primitive operations *****)

  (*
      fun add64 ((hi1, lo1), (hi2, lo2)) = let
	    val lo = lo1 + lo2
	  (* from "Hacker's Delight": c = ((lo1 & lo2) | ((lo1 | lo2) & ¬lo)) >> 31 *)
	    val carry = ((lo1 & lo2) ++ ((lo1 ++ lo2) & not lo)) >> 0w31
	  (* we add the carry to the smaller hi component before add them; this
	   * check is needed to get Overflow right in the edge cases
	   *)
	    val hi = if InLine.int32_le(hi1, hi2)
		  then InLine.int32_add(InLine.int32_add(hi1, c), hi2)
		  else InLine.int32_add(InLine.int32_add(hi2, c), hi1)
	    in
	      (hi, lo)
	    end
   *)
    fun i64Add (n1, n2, res, cexp) = let
	  val hi = LV.mkLvar()
	  in
	    join (res, cexp, fn k =>
	      from64(n1, fn (hi1, lo1) =>
	      from64(n2, fn (hi2, lo2) =>
	      pure_arith32(P.ADD, [lo1, lo2], fn lo =>
	      pure_arith32(P.ORB, [lo1, lo2], fn lo1_or_lo2 =>
	      pure_arith32(P.NOTB, [lo], fn not_lo =>
	      pure_arith32(P.ANDB, [lo1_or_lo2, not_lo], fn tmp1 =>
	      pure_arith32(P.ANDB, [lo1, lo2], fn lo1_and_lo2 =>
	      pure_arith32(P.ORB, [lo1_and_lo2, tmp1], fn tmp2 =>
	      pure_arith32(P.RSHIFT, [tmp2, tagNum 31], fn carry =>
	      join (hi,
		to64(C.VAR hi, lo, k),
		fn k' =>
		  sIf(P.LTE, hi1, hi2,
		    iarith32(P.IADD, [hi1, carry], fn tmp1 =>
		    iarith32(P.IADD, [tmp1, hi2], k')),
		    (* else *)
		    iarith32(P.IADD, [hi2, carry], fn tmp2 =>
		    iarith32(P.IADD, [tmp2, hi1], k'))))))))))))))
	  end

  (*
      fun sub64 ((hi1, lo1), (hi2, lo2)) = let
	    val lo = lo1 - lo2
	  (* from "Hacker's Delight": b = ((¬lo1 & lo2) | ((lo1 ≡ lo2) & lo)) >> 31 *)
	    val b = ((InLine.word32_notb lo1 & lo2) ++ ((lo1 ^= lo2) & lo)) >> 0w31
	  (* we need this test to get Overflow right in the edge cases *)
	    val hi = if InLine.int32_le(hi1, hi2)
		  then InLine.int32_sub(InLine.int32_sub(hi1, hi2), b)
		  else InLine.int32_sub(InLine.int32_sub(hi1, b), hi2)
	    in
	      (hi, lo)
	    end
   *)
    fun i64Sub (n1, n2, res, cexp) = let
	  val hi = LV.mkLvar()
	  in
	    join (res, cexp, fn k =>
	      from64(n1, fn (hi1, lo1) =>
	      from64(n2, fn (hi2, lo2) =>
	      pure_arith32(P.SUB, [lo1, lo2], fn lo =>
	      bitEquiv(lo1, lo2, fn lo1_eqv_lo2 =>
	      pure_arith32(P.ANDB, [lo1_eqv_lo2, lo], fn tmp1 =>
	      pure_arith32(P.NOTB, [lo1], fn not_lo1 =>
	      pure_arith32(P.ANDB, [not_lo1, lo2], fn tmp2 =>
	      pure_arith32(P.ORB, [tmp1, tmp2], fn tmp3 =>
	      pure_arith32(P.RSHIFT, [tmp3, tagNum 31], fn borrow =>
	      join (hi,
		to64(C.VAR hi, lo, k),
		fn k' =>
		  sIf(P.LTE, hi1, hi2,
		    iarith32(P.IADD, [hi1, hi2], fn tmp1 =>
		    iarith32(P.IADD, [tmp1, borrow], k')),
		    (* else *)
		    iarith32(P.IADD, [hi1, borrow], fn tmp2 =>
		    iarith32(P.IADD, [tmp2, hi2], k'))))))))))))))
	  end

  (*
   * fun neg (hi, 0w0) = (I32.~ hi, 0w0)
   *   | neg (hi, lo) = (W32.notb hi, W32.~ lo)
   *)
    fun i64Neg (n, res, cexp) = join (res, cexp, fn k =>
	  from64(n, fn (hi, lo) =>
	    ifzero(lo,
	      iarith32(P.INEG, [hi], fn hi' => to64 (hi', zero, k)),
	      (* else *)
	      pure_arith32(P.NOTB, [hi], fn hi' =>
	        pure_arith32(P.NEG, [lo], fn lo' =>
		  to64 (hi', lo', k))))))

    val i64Eql = w64Eql

  (* the basic pattern for comparisons is
   *   fun cmp ((hi1, lo1), (hi2, lo2)) =
   *         cmpHi(hi1, hi2) orelse ((hi1 = hi2) andalso cmpLo(lo1, lo2))
   *)
    local
      fun i64Cmp (cmpHi, cmpLo) (n1, n2, tr, fl) = let
	  (* continuations for the branches so that we can avoid code duplication *)
	    val trFnId = LV.mkLvar()
	    val tr' = C.APP(C.VAR trFnId, [])
	    val flFnId = LV.mkLvar()
	    val fl' = C.APP(C.VAR flFnId, [])
	    in
	    (* NOTE: closure conversion requires that there only be one continuation
	     * function per FIX!
	     *)
	      C.FIX([(C.CONT, trFnId, [], [], tr)],
	      C.FIX([(C.CONT, flFnId, [], [], fl)],
	      (* (hi1 < hi2) orelse ((hi1 = hi2) andalso (lo1 < lo2)) *)
		getHi32(n1, fn hi1 =>
		getHi32(n2, fn hi2 =>
		  sIf(cmpHi, hi1, hi2,
		    tr',
		    sIf(P.EQL, hi1, hi2,
		      getLo32(n1, fn lo1 =>
		      getLo32(n2, fn lo2 =>
			uIf(cmpLo, lo1, lo2, tr', fl'))),
		      fl'))))))
	    end
    in
    val i64Less = i64Cmp (P.LT, P.LT)
    val i64LessEq = i64Cmp (P.LT, P.LTE)
    val i64Greater = i64Cmp (P.GT, P.GT)
    val i64GreaterEq = i64Cmp (P.GT, P.GTE)
    end (* local *)

  (***** conversions *****)

  (* signed conversion from 64-bit word with test for overflow *)
    fun test64To (toSz, [x, f], res, resTy, ce) =
	  if (toSz <= Target.defaultIntSz)
	    then let (* need extra conversion from 32-bits to fromSz *)
	      val rk = LV.mkLvar()
	      val v = LV.mkLvar()
	      val ce' = C.ARITH(P.TEST{from=32, to=toSz}, [C.VAR v], res, resTy, ce)
	      in
		C.FIX([(C.CONT, rk, [v], [raw32Ty], ce')], C.APP (f, [C.VAR rk, x]))
	      end
	    else mkApplyWithReturn (f, [x], res, resTy, ce)
      | test64To _ = bug "test64To"

  (* unsigned conversion from 64-bit word with test for overflow *)
    fun testu64To (toSz, [x, f], res, resTy, ce) =
	  if (toSz <= Target.defaultIntSz)
	    then let (* need extra conversion from 32-bits to fromSz *)
	      val rk = LV.mkLvar()
	      val v = LV.mkLvar()
	      val ce' = C.ARITH(P.TESTU{from=32, to=toSz}, [C.VAR v], res, resTy, ce)
	      in
		C.FIX([(C.CONT, rk, [v], [raw32Ty], ce')], C.APP (f, [C.VAR rk, x]))
	      end
	    else mkApplyWithReturn (f, [x], res, resTy, ce)
      | testu64To _ = bug "testu64To"

  (* truncate a 64-bit number to a size <= 32 bit number *)
    fun trunc64To (toSz, n, res, ce) = join (res, ce, fn k =>
	  getLo32 (n, if (toSz = 32)
	    then k
	    else (fn lo => pure(P.TRUNC{from=32, to=toSz}, [lo], tagNumTy, k))))

  (* copy (zero-extend) a number to a 64-bit representation *)
    fun copy64From (64, n, res, ce) = join (res, ce, fn k => k n)
      | copy64From (fromSz, n, res, ce) = join (res, ce, fn k => if (fromSz = 32)
	  then to64 (zero, n, k)
	  else pure(P.COPY{from=fromSz, to=32}, [n], raw32Ty, fn lo =>
	    to64 (zero, lo, k)))

  (* sign-extend a number to a 64-bit representation, where fromSz <= 32 *)
    fun extend64From (fromSz, n, res, ce) = join (res, ce, fn k => if (fromSz = 32)
	  then pure_arith32(P.RSHIFT, [n, tagNum 31], fn hi =>
	    to64 (hi, n, k))
	  else pure(P.EXTEND{from=fromSz, to=32}, [n], raw32Ty, fn lo =>
	    pure_arith32(P.RSHIFT, [lo, tagNum 31], fn hi =>
	      to64 (hi, lo, k))))

  (***** other functions *****)

    fun wrap64 (v, res, cexp) = join (res, cexp, fn k => k v)
    fun unwrap64 (v, res, cexp) = join (res, cexp, fn k => k v)


  (***** main function *****)

  (* check if an expression needs rewriting *)
    fun needsRewrite func = let
	  fun chkTy (C.NUMt{sz=64, ...}) = true
	    | chkTy _ = false
	  fun chkValue (C.NUM{ival, ty={sz=64, ...}}) = true
	    | chkValue _ = false
	  fun chkValues [] = false
	    | chkValues (v::vs) = chkValue v orelse chkValues vs
	  fun chkExp (C.RECORD(_, vs, _, e)) =
		List.exists (chkValue o #1) vs orelse chkExp e
	    | chkExp (C.SELECT(_, v, _, _, e)) = chkValue v orelse chkExp e
	    | chkExp (C.OFFSET(_, v, _, e)) = chkValue v orelse chkExp e
	    | chkExp (C.APP(_, vs)) = chkValues vs
	    | chkExp (C.FIX(fns, e)) = List.exists chkFun fns orelse chkExp e
	    | chkExp (C.SWITCH(v, _, es)) = chkValue v orelse List.exists chkExp es
	    | chkExp (C.BRANCH(P.CMP{kind=P.INT 64, ...}, _, _, _, _)) = true
	    | chkExp (C.BRANCH(P.CMP{kind=P.UINT 64, ...}, _, _, _, _)) = true
	    | chkExp (C.BRANCH(_, vs, _, e1, e2)) =
		chkValues vs orelse chkExp e1 orelse chkExp e2
(* QUESTION: what about RAWSTORE? *)
	    | chkExp (C.SETTER(_, vs, e)) = chkValues vs orelse chkExp e
	    | chkExp (C.LOOKER(_, vs, _, _, e)) = chkValues vs orelse chkExp e
	    | chkExp (C.ARITH(P.IARITH{sz=64, ...}, _, _, _, _)) = true
	    | chkExp (C.ARITH(P.TEST{from=64, ...}, _, _, _, _)) = true
	    | chkExp (C.ARITH(P.TESTU{from=64, ...}, _, _, _, _)) = true
	    | chkExp (C.ARITH(_, vs, _, _, e)) = chkValues vs orelse chkExp e
	    | chkExp (C.PURE(P.PURE_ARITH{kind=P.INT 64, ...}, _, _, _, _)) = true
	    | chkExp (C.PURE(P.PURE_ARITH{kind=P.UINT 64, ...}, _, _, _, _)) = true
	    | chkExp (C.PURE(P.COPY{to=64, ...},  _, _, _, _)) = true
	    | chkExp (C.PURE(P.EXTEND{to=64, ...},  _, _, _, _)) = true
	    | chkExp (C.PURE(P.TRUNC{from=64, ...}, _, _, _, _)) = true
	    | chkExp (C.PURE(P.WRAP(P.INT 64), _, _, _, _)) = true
	    | chkExp (C.PURE(P.UNWRAP(P.INT 64), _, _, _, _)) = true
	    | chkExp (C.PURE(_, vs, _, _, e)) = chkValues vs orelse chkExp e
	    | chkExp (C.RCC(_, _, _, vs, _, e)) = chkValues vs orelse chkExp e
	  and chkFun (_, _, _, tys, e) = List.exists chkTy tys orelse chkExp e
	  in
	    (not Target.is64) andalso (chkFun func)
	  end

  (* we replace occurrences of the 64-bit number type with "pointer to pair" *)
    fun cvtTy (C.NUMt{sz=64, ...}) = pairTy
      | cvtTy ty = ty

    fun elim cfun = let
	  fun value (C.NUM{ival, ty={sz=64, ...}}, k) = let
		val (hi, lo) = split ival
		in
		  to64 (hi, lo, k)
		end
	    | value (v, k) = k v
	  and values (vl, k) = let
		fun f ([], vl') = k (List.rev vl')
		  | f (C.NUM{ival, ty={sz=64, ...}}::vs, vl') = let
		      val (hi, lo) = split ival
		      in
			to64 (hi, lo, fn v => f (vs, v::vl'))
		      end
		  | f (v::vs, vl') = f (vs, v::vl')
		in
		  f (vl, [])
		end
	  fun cexp (C.RECORD (rk, xl, v, e)) = let
		fun f ([], args') = C.RECORD (rk, List.rev args', v, cexp e)
		  | f ((C.NUM{ival, ty={sz=64, ...}}, offp)::args, args') = let
		      val (hi, lo) = split ival
		      in
			to64 (hi, lo, fn v => f (args, (v, offp)::args'))
		      end
		  | f (arg::args, args') = f (args, arg::args')
		in
		  f (xl, [])
		end
	    | cexp (C.SELECT(i, x, v, t, e)) = C.SELECT(i, x, v, cvtTy t, cexp e)
	    | cexp (C.OFFSET(i, v, x, e)) = C.OFFSET(i, v, x, cexp e)
	    | cexp (C.APP(f, xl)) = values (xl, fn xl' => C.APP (f, xl'))
	    | cexp (C.FIX(fl, e)) = C.FIX(List.map function fl, cexp e)
	    | cexp (C.SWITCH(x, v, el)) =
		value (x, fn x' => C.SWITCH(x', v, List.map cexp el))
	    | cexp (C.BRANCH(P.CMP{oper, kind=P.INT 64}, args, _, e1, e2)) =
		values (args, fn args' => (case (oper, args')
		   of (P.GT, [a, b]) => i64Greater(a, b, cexp e1, cexp e2)
		    | (P.GTE, [a, b]) => i64GreaterEq(a, b, cexp e1, cexp e2)
		    | (P.LT, [a, b]) => i64Less(a, b, cexp e1, cexp e2)
		    | (P.LTE, [a, b]) => i64LessEq(a, b, cexp e1, cexp e2)
		    | (P.EQL, [a, b]) => i64Eql(a, b, cexp e1, cexp e2)
		    | (P.NEQ, [a, b]) => i64Eql(a, b, cexp e2, cexp e1)
		    | _ => bug "impossible BRANCH; INT 64"
		  (* end case *)))
	    | cexp (C.BRANCH(P.CMP{oper, kind=P.UINT 64}, args, _, e1, e2)) =
		values (args, fn args' => (case (oper, args')
		   of (P.GT, [a, b]) => w64Greater(a, b, cexp e1, cexp e2)
		    | (P.GTE, [a, b]) => w64GreaterEq(a, b, cexp e1, cexp e2)
		    | (P.LT, [a, b]) => w64Less(a, b, cexp e1, cexp e2)
		    | (P.LTE, [a, b]) => w64LessEq(a, b, cexp e1, cexp e2)
		    | (P.EQL, [a, b]) => w64Eql(a, b, cexp e1, cexp e2)
		    | (P.NEQ, [a, b]) => w64Eql(a, b, cexp e2, cexp e1)
		    | _ => bug "impossible BRANCH; UINT 64"
		  (* end case *)))
	    | cexp (C.BRANCH(rator, args, v, e1, e2)) =
		values (args, fn args' => C.BRANCH(rator, args', v, cexp e1, cexp e2))
	    | cexp (C.SETTER(rator, xl, e)) =
		values (xl, fn xl' => C.SETTER (rator, xl', cexp e))
	    | cexp (C.LOOKER (rator, xl, v, ty, e)) =
		values (xl, fn xl' => C.LOOKER (rator, xl', v, cvtTy ty, cexp e))
	    | cexp (C.ARITH(P.IARITH{oper, sz=64}, args, res, _, e)) =
		values (args, fn args' => (case (oper, args')
		   of (P.IADD, [a, b]) => i64Add(a, b, res, cexp e)
		    | (P.ISUB, [a, b]) => i64Sub(a, b, res, cexp e)
		    | (P.IMUL, [a, b, f]) => mkApply(f, [a, b], res, e)
		    | (P.IDIV, [a, b, f]) => mkApply(f, [a, b], res, e)
		    | (P.IMOD, [a, b, f]) => mkApply(f, [a, b], res, e)
		    | (P.IQUOT, [a, b, f]) => mkApply(f, [a, b], res, e)
		    | (P.IREM , [a, b, f]) => mkApply(f, [a, b], res, e)
		    | (P.INEG, [a]) => i64Neg(a, res, cexp e)
		    | _ => bug "impossible IARITH; sz=64"
		  (* end case *)))
	    | cexp (C.ARITH(P.TEST{from=64, to}, args, res, cty, e)) =
		values (args, fn args' => test64To(to, args', res, cty, cexp e))
	    | cexp (C.ARITH(P.TESTU{from=64, to}, args, res, cty, e)) =
		values (args, fn args' => testu64To(to, args', res, cty, cexp e))
	    | cexp (C.ARITH(rator, args, res, ty, e)) =
		values (args, fn args' => C.ARITH(rator, args', res, ty, cexp e))
	    | cexp (C.PURE(P.PURE_ARITH{oper, kind=P.INT 64}, args, res, _, e)) =
	      (* This case probably cannot happen, but we include it to be safe! *)
		values (args, fn args' => (case (oper, args')
		   of (P.ORB, [a, b]) => w64Orb(a, b, res, cexp e)
		    | (P.XORB, [a, b]) => w64Xorb(a, b, res, cexp e)
		    | (P.ANDB, [a, b]) => w64Andb(a, b, res, cexp e)
		    | (P.NOTB, [a]) => w64Notb(a, res, cexp e)
		    | (P.RSHIFT, [a, b]) => w64RShift(a, b, res, cexp e)
		    | (P.RSHIFTL, [a, b]) => w64RShiftL(a, b, res, cexp e)
		    | (P.LSHIFT, [a, b]) => w64LShift(a, b, res, cexp e)
		    | _ => bug "impossible PURE_ARITH; INT 64"
		  (* end case *)))
	    | cexp (C.PURE(P.PURE_ARITH{oper, kind=P.UINT 64}, args, res, _, e)) =
		values (args, fn args' => (case (oper, args')
		   of (P.ADD, [a, b]) => w64Add(a, b, res, cexp e)
		    | (P.SUB, [a, b]) => w64Sub(a, b, res, cexp e)
		    | (P.MUL, [a, b, f]) => mkApply(f, [a, b], res, e)
		    | (P.QUOT, [a, b, f]) => mkApply(f, [a, b], res, e)
		    | (P.REM , [a, b, f]) => mkApply(f, [a, b], res, e)
		    | (P.NEG, [a]) => i64Neg(a, res, cexp e)
		    | (P.ORB, [a, b]) => w64Orb(a, b, res, cexp e)
		    | (P.XORB, [a, b]) => w64Xorb(a, b, res, cexp e)
		    | (P.ANDB, [a, b]) => w64Andb(a, b, res, cexp e)
		    | (P.NOTB, [a]) => w64Notb(a, res, cexp e)
		    | (P.RSHIFT, [a, b]) => w64RShift(a, b, res, cexp e)
		    | (P.RSHIFTL, [a, b]) => w64RShiftL(a, b, res, cexp e)
		    | (P.LSHIFT, [a, b]) => w64LShift(a, b, res, cexp e)
		    | _ => bug "impossible PURE_ARITH; UINT 64"
		  (* end case *)))
	    | cexp (C.PURE(P.TRUNC{from=64, to}, [a], res, _, e)) =
		value (a, fn x => trunc64To(to, x, res, cexp e))
	    | cexp (C.PURE(P.COPY{from, to=64}, [a], res, _, e)) =
		value (a, fn x => copy64From(from, x, res, cexp e))
	    | cexp (C.PURE(P.EXTEND{from, to=64}, [a], res, _, e)) =
		value (a, fn x => extend64From(from, x, res, cexp e))
	    | cexp (C.PURE(P.WRAP(P.INT 64), [a], res, _, e)) =
		value (a, fn x => wrap64 (x, res, cexp e))
	    | cexp (C.PURE(P.UNWRAP(P.INT 64), [a], res, _, e)) =
		value (a, fn x => unwrap64 (x, res, cexp e))
	    | cexp (C.PURE(rator, args, res, ty, e)) =
		values (args, fn args => C.PURE(rator, args, res, cvtTy ty, cexp e))
	    | cexp (C.RCC(rk, cf, proto, args, res, e)) =
		values (args, fn args => C.RCC(rk, cf, proto, args, res, cexp e))
	(* make an application of the function `f`, where `exp` is the continuation
	 * of the original primop and we assume the result type is a pair of
	 * 32-bit integers.
	 *)
	  and mkApply (f, args, res, exp) =
		values (args, fn args' =>
		  mkApplyWithReturn (f, args', res, pairTy, cexp exp))
	  and function (fk, f, params, tys, body) =
		(fk, f, params, List.map cvtTy tys, cexp body)
	  in
	    if needsRewrite cfun
	      then function cfun
	      else cfun
	  end (* elim *)

  end
