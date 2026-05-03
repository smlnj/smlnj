(* transprim.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 * Translation of primops to PLambda.  The translation adds extra
 * arguments to some primops so that they will be available for
 * the CPS lowering phase.  The extra arguments are as follows:
 *
 *	- for `COPY_INF`, `EXTEND_INF`, `TRUNC_INF`, and `TEST_INF`
 *        the function from `Core` that implements the operation is
 *	  added as a second argument.
 *
 *      - for most 64-bit arithmetic operations on 32-bit targets,
 *        the implementation functions from `Core` are added as a
 *	  second or third argument.  The exceptions are addition,
 *	  subtraction, negation, and the bit-wise operations.
 *)

structure TransPrim : sig

    val trans : {
	    coreAcc : string -> PLambda.lexp,
	    coreExn : string list -> PLambda.lexp option,
	    mkv : unit -> LambdaVar.lvar,
	    mkRaise : PLambda.lexp * Lty.lty -> PLambda.lexp
	  } -> PrimOps.t * Lty.lty * Lty.tyc list -> PLambda.lexp

  end = struct

    structure PO = PrimOps
    structure FP = FPrimOps
    structure CP = CommonOps
    structure InlP = InlineOps
    structure PureP = PureOps
    structure ArithP = ArithOps
    structure CmpP = CompareOps
    structure PL = PLambda
    structure LT = Lty
    structure LD = LtyDef
    structure LB = LtyBasic
    structure Tgt = Target

    fun bug msg = ErrorMsg.impossible("TransPrim: " ^ msg)

    fun warn s = Control.Print.say(concat["*** WARNING: ", s, "\n"])

  (* various useful PLambda types *)
    val lt_tyc = LD.ltc_tyc
    val lt_arw = LD.ltc_parrow
    val lt_tup = LD.ltc_tuple

    val lt_int = LB.ltc_int
    val lt_fixed_int = LB.ltc_num Tgt.fixedIntSz
        (* the largest fixed-precision int type *)
    val lt_bool = LB.ltc_bool
    val lt_unit = LB.ltc_unit

    val lt_ipair = lt_tup [lt_int, lt_int]
    val lt_icmp = lt_arw (lt_ipair, lt_bool)
    val lt_intop1 = lt_arw (lt_int, lt_int)

    val unitLexp = PL.RECORD[]

    val boolsign = BasicTypes.boolsign
    val (trueDcon', falseDcon') = let
	  val lt = LD.ltc_parrow(LB.ltc_unit, LB.ltc_bool)
	  fun mk (Types.DATACON{name, rep, typ,...}) = (name, rep, lt)
	  in
	    (mk BasicTypes.trueDcon, mk BasicTypes.falseDcon)
          end

    val trueLexp = PL.CON(trueDcon', [], unitLexp)
    val falseLexp = PL.CON(falseDcon', [], unitLexp)

    (* make a comparison prim expression *)
    fun pCMP (tst, kind, ty, tycs) = PL.PRIM(FP.CMP{oper=tst, kind=kind}, ty, tycs)

  (* unsigned comparison on tagged integers used for bounds checking *)
    val pLESSU = pCMP(CmpP.LT, PO.UINT Tgt.defaultIntSz, lt_icmp, [])

    val lt_len = LD.ltc_poly([LD.tkc_mono], [lt_arw(LB.ltc_tv 0, lt_int)])
    val lt_upd = let
	  val x = LB.ltc_ref (LB.ltc_tv 0)
          in
	    LD.ltc_poly([LD.tkc_mono], [lt_arw(lt_tup [x, lt_int, LB.ltc_tv 0], LB.ltc_unit)])
          end

  (* get length of sequence *)
    fun lenOp seqtc = PL.PRIM(FP.PRIM CP.LENGTH, lt_len, [seqtc])

  (* inline operations and constants for a given numeric kind *)
    type inline_ops = {
	lt_arg : LT.lty,	(* PLambda type for this kind of number *)
	lt_argpair : LT.lty,	(* PLambda type for pairs of numbers *)
	lt_cmp : LT.lty,	(* PLambda type of comparison function *)
	multiply : PL.lexp,	(* multiplication primitive function *)
	negate : PL.lexp,	(* negation primitive function *)
	less : PL.lexp,		(* less-than primitive function *)
	greater : PL.lexp,	(* greater-than primitive function *)
	equal : PL.lexp,	(* equality primitivefunction *)
	zero : PL.lexp,		(* the value 0 for the given type *)
	negOne : PL.lexp	(* the value -1 for the given type *)
      }

  (* a cache of the inline_ops records *)
    local
      fun mkInlineOps nk = let
	    val (lt_arg, lt_argpair, multiply, negate, zero, negOne) = (case nk
		   of PO.INT sz => let
			val lt_arg = LB.ltc_num sz
	    		val lt_argpair = lt_tup [lt_arg, lt_arg]
			val lt_mul = lt_arw (lt_argpair, lt_arg)
			val lt_neg = lt_arw (lt_arg, lt_arg)
			in (
			  lt_arg, lt_argpair,
			  PL.PRIM(FP.ARITH{oper = ArithP.IMUL, sz = sz}, lt_mul, []),
			  PL.PRIM(FP.ARITH{oper = ArithP.INEG, sz = sz}, lt_neg, []),
			  PL.INT{ival = 0, ty = sz},
			  PL.INT{ival = ~1, ty = sz}
			) end
		    | PO.UINT sz => let
			val lt_arg = LB.ltc_num sz
	    		val lt_argpair = lt_tup [lt_arg, lt_arg]
			val lt_mul = lt_arw (lt_argpair, lt_arg)
			val lt_neg = lt_arw (lt_arg, lt_arg)
			in (
			  lt_arg, lt_argpair,
			  PL.PRIM(FP.PURE{oper = PureP.MUL, kind = nk}, lt_mul, []),
			  PL.PRIM(FP.PURE{oper = PureP.NEG, kind = nk}, lt_neg, []),
			  PL.WORD{ival = 0, ty = sz},
			  PL.WORD{ival = ~1, ty = sz} (* unused *)
			) end
		    | PO.FLOAT sz => let
(* REAL64: type will depend on size *)
			val lt_arg = LB.ltc_real
	    		val lt_argpair = lt_tup [lt_arg, lt_arg]
			val lt_mul = lt_arw (lt_argpair, lt_arg)
			val lt_neg = lt_arw (lt_arg, lt_arg)
			in (
			  lt_arg, lt_argpair,
			  PL.PRIM(FP.PURE{oper = PureP.MUL, kind = nk}, lt_mul, []),
			  PL.PRIM(FP.PURE{oper = PureP.NEG, kind = nk}, lt_neg, []),
			  PL.REAL{rval = RealLit.zero false, ty = sz},
			  PL.REAL{rval = RealLit.m_one, ty = sz} (* unused *)
			) end
		  (* end case *))
	    val lt_cmp = lt_arw (lt_argpair, lt_bool)
	    val less = pCMP(CmpP.LT, nk, lt_cmp, [])
	    val greater = pCMP(CmpP.GT, nk, lt_cmp, [])
	    val equal = pCMP(CmpP.EQL, nk, lt_cmp, [])
	    in {
	      lt_arg = lt_arg, lt_argpair = lt_argpair, lt_cmp = lt_cmp,
	      multiply = multiply, negate = negate,
	      less = less, greater = greater, equal = equal,
	      zero = zero, negOne = negOne
	    } end

    (* equality on number kinds *)
      fun sameNK (PO.INT sz1, PO.INT sz2) = (sz1 = sz2)
	| sameNK (PO.UINT sz1, PO.UINT sz2) = (sz1 = sz2)
	| sameNK (PO.FLOAT sz1, PO.FLOAT sz2) = (sz1 = sz2)
	| sameNK _ = false

    (* hash number kinds *)
      fun hashNK (PO.INT sz) = Word.fromInt sz
	| hashNK (PO.UINT sz) = Word.fromInt sz + 0w1
	| hashNK (PO.FLOAT sz) = Word.fromInt sz + 0w3

    (* hash tables keyed by number kinds *)
      structure NKTbl = HashTableFn (
	struct
	  type hash_key = PO.numkind
	  val hashVal= hashNK
	  val sameKey = sameNK
	end)

      val tbl : inline_ops NKTbl.hash_table = NKTbl.mkTable (16, Fail "num-kind table")
      val find = NKTbl.find tbl

      in
      fun inlops nk = (case find nk
	     of NONE => let
		  val ops = mkInlineOps nk
		  in
		    NKTbl.insert tbl (nk, ops);
		    ops
		  end
	      | SOME ops => ops
	    (* end case *))
      end (* local*)

  (* inline operators for numeric types *)

    fun baselt (PO.UINT sz) = LB.ltc_num sz

  (* type of a shift operation where `k` is the kind of value being shifted *)
    fun shiftTy k = let
	  val elem = baselt k
          in
	    lt_arw(lt_tup [elem, lt_int], elem)
          end

  (* shift primops *)
    fun rshiftOp k = PL.PRIM(FP.PURE{oper=PureP.RSHIFT, kind=k}, shiftTy k, [])
    fun rshiftlOp k = PL.PRIM(FP.PURE{oper=PureP.RSHIFTL, kind=k}, shiftTy k, [])
    fun lshiftOp k = PL.PRIM(FP.PURE{oper=PureP.LSHIFT, kind=k}, shiftTy k, [])

  (* zero literal for given word type *)
    fun lword0 (PO.UINT sz) = PL.WORD{ival = 0, ty = sz}

  (* pick the name of an infinf conversion from "_Core" based on the size; for tagged
   * numbers, we return the boxed conversion for the ML Value-sized numbers; a second
   * conversion to/from the tagged representation will be applied to the result/argument.
   *)
    local
      fun pickName (cvt32, cvt64) sz =
	    if (sz = 64) then cvt64
	    else if (sz = Tgt.mlValueSz) then cvt32
	    else if (sz > Tgt.defaultIntSz)
	      then bug(concat["bogus size ", Int.toString sz, " for intinf conversion"])
	    else if Tgt.is64
	      then cvt64
	      else cvt32
    in
(* TODO: add specialized conversion for default int size *)
    val truncInf = pickName ("truncInf32", "truncInf64")
    val testInf = pickName ("testInf32", "testInf64")
    val copyInf = pickName ("copy32Inf", "copy64Inf")
    val extendInf = pickName ("extend32Inf", "extend64Inf")
    end (* local *)

  (* trans : Primop.primop * Lty.lty * Lty.tyc list
   *
   * Translate Absyn primop to PLambda form using given
   * intrinsic PLambda type and type parameters
   *)
    fun trans { coreAcc, coreExn, mkv, mkRaise } (prim : PO.t, lt, ts) = let
	(* make a function expression *)
	  fun mkFn ty body = let
		val x = mkv()
		in
		  PL.FN(x, ty, body(PL.VAR x))
		end
	(* make a let expression *)
	  fun mkLet rhs body = let
		val x = mkv()
		in
		  PL.LET(x, rhs, body(PL.VAR x))
		end
	(* make an application to two arguments *)
	  fun mkApp2 (rator, a, b) = PL.APP(rator, PL.RECORD[a, b])
	(* if-then-else *)
	  fun mkCOND (a, b, c) = PL.SWITCH(a, boolsign, [
		  (PL.DATAcon(trueDcon', [], mkv()), b),
		  (PL.DATAcon(falseDcon', [], mkv()), c)
		],
		NONE)
	(* inline expand a checked logical shift operation (right or left) *)
	  fun inlineLogicalShift (shiftOp, kind) = let
		val shiftLimit = (case kind
		       of PO.UINT lim => PL.WORD{ival = IntInf.fromInt lim, ty = Tgt.defaultIntSz}
			| _ => bug "unexpected kind in inlineShift"
		      (* end case *))
		val argt = lt_tup [baselt kind, lt_int]
		val cmpShiftAmt = pCMP(CmpP.LTE, PO.UINT Tgt.defaultIntSz, lt_icmp, [])
		in
		  mkFn argt (fn p =>
		    mkLet (PL.SELECT(0, p)) (fn w =>
		      mkLet (PL.SELECT(1, p)) (fn cnt =>
			mkCOND(
			  mkApp2(cmpShiftAmt, shiftLimit, cnt),
			  lword0 kind,
			  mkApp2(shiftOp kind, w, cnt)))))
		end
	(* inline expand an arithmetic-shift-right operation; for this operation, we need
	 * some care to get the sign bit extension correct.  If the size of the value
	 * being shifted is less than the default integer size, then we shift it left first
	 * and then do an arithmetic right shift followed by a logical right shift to
	 * produce the final result.  We use two shifts so that the resulting high bits will
	 * be zeros.
	 *)
	  fun inlineArithmeticShiftRight (kind as PO.UINT sz) = let
		fun lword n = PL.WORD{ival = Int.toLarge n, ty = Tgt.defaultIntSz}
		val shiftLimit = lword sz
		val shiftWidth = lword Tgt.defaultIntSz
		val argt = lt_tup [baselt kind, lt_int]
		val cmpShiftAmt = pCMP(CmpP.LTE, PO.UINT Tgt.defaultIntSz, lt_icmp, [])
		in
		  if (sz < Tgt.defaultIntSz)
		    then let
		      val delta = Tgt.defaultIntSz - sz
		      val delta' = lword delta
		      val wordKind = PO.UINT Tgt.defaultIntSz
		      in
			mkFn (argt) (fn p =>
			  mkLet (PL.SELECT(0, p)) (fn w =>
			  mkLet (PL.SELECT(1, p)) (fn cnt =>
			  mkLet (mkApp2(lshiftOp wordKind, w, delta')) (fn w' =>
			    mkCOND(
			      mkApp2(cmpShiftAmt, shiftLimit, cnt),
			      mkApp2(rshiftlOp wordKind,
			        mkApp2(rshiftOp wordKind, w', shiftWidth),
				delta'),
			      mkApp2(rshiftlOp wordKind,
				mkApp2(rshiftOp wordKind, w', cnt),
				delta'))))))
		      end
		    else mkFn argt (fn p =>
		      mkLet (PL.SELECT(0, p)) (fn w =>
		      mkLet (PL.SELECT(1, p)) (fn cnt =>
			mkCOND(
			  mkApp2(cmpShiftAmt, shiftLimit, cnt),
			  mkApp2(rshiftOp kind, w, shiftWidth),
			  mkApp2(rshiftOp kind, w, cnt)))))
		end
	(* bounds check for vector/array access *)
	  fun boundsChk (ix, seq, seqtc, t) body = (
		case coreExn ["Subscript"]
		 of SOME ssexn =>
		      mkCOND(PL.APP(pLESSU, PL.RECORD[ix, PL.APP(lenOp seqtc, seq)]),
			body,
			mkRaise(ssexn, t))
		  | NONE => (
		      warn "no access to exn Subscript for inline subscript";
		      body)
		(* end case *))
	(* inline subscript for vectors and arrays *)
	  fun inlSubscript (subOp, argt, seqtc, t) = let
		val oper = PL.PRIM (FP.PRIM subOp, lt, ts)
		in
		  case coreExn ["Subscript"]
		   of SOME ssexn =>
			mkFn argt (fn p =>
			  mkLet (PL.SELECT(0, p)) (fn a =>
			    mkLet (PL.SELECT(1, p)) (fn i =>
			      boundsChk (i, a, seqtc, t) (mkApp2(oper, a, i)))))
		     | NONE => (
			warn "no access to exn Subscript for inline subscript";
			oper)
		  (* end case *)
		end
	(* division operators with an explicit test for a zero divisor *)
	  fun inldiv (nk, po, lt, ts) = let
		val oper = PL.PRIM (po, lt, ts)
		in
		  case coreExn ["Assembly", "Div"]
		   of SOME divexn => let
			val { lt_arg, lt_argpair, lt_cmp, zero, equal, ... } = inlops nk
			in
			  mkFn lt_argpair (fn z =>
			    mkLet (PL.SELECT(1, z)) (fn y =>
			      mkCOND (
				mkApp2 (equal, y, zero),
				mkRaise (divexn, lt_arg),
				PL.APP(oper, z))))
			end
		   | NONE => (warn "no access to Div exception"; oper)
		end
	(* inline min/max *)
	  fun inlminmax (nk, ismax) = let
		val { lt_argpair, less, greater, lt_cmp, ... } = inlops nk
		val cmpop = if ismax then greater else less
		in
		  mkFn lt_argpair (fn z =>
		    mkLet (PL.SELECT(0, z)) (fn x =>
		      mkLet (PL.SELECT(1, z)) (fn y =>
			mkCOND (
			  mkApp2 (cmpop, x, y),
			  x,
			  case nk
			   of PO.FLOAT _ => let (* testing for NaN *)
				val fequal = pCMP(CmpP.EQL, nk, lt_cmp, [])
				in
				  mkCOND (mkApp2 (fequal, y, y), y, x)
				end
			    | _ => y))))
		end
	(* inline absolute value for integer types *)
	  fun inlabs nk = let
		val { lt_arg, greater, zero, negate, ... } = inlops nk
		in
		  mkFn lt_arg (fn x =>
		    mkCOND (mkApp2 (greater, x, zero), x, PL.APP(negate, x)))
		end
	(* inline Char.chr function *)
	  fun inlChr () = (case coreExn ["Chr"]
		 of SOME chrExn => let
		      val wk = PO.UINT Tgt.defaultIntSz
		      val geu = pCMP(CmpP.GTE, wk, lt_icmp, [])
		      val c256 = PL.INT{ival = 256, ty = Tgt.defaultIntSz}
		      in
			mkFn lt_int (fn i =>
			  mkCOND(mkApp2 (geu, i, c256), mkRaise(chrExn, lt_int), i))
		      end
		  | NONE => (
		      warn "no access to Chr exception";
		      PL.PRIM(FP.PRIM CP.CAST, lt_intop1, []))
		(* end case *))
	(* conversion from fixed int to intinf *)
	  fun inlToInf (opname: string, cvtName: string, primop, primoplt) = let
		val (orig_arg_lt, res_lt) = (
		      case LD.ltd_arrow primoplt handle LD.DeconExn => bug "inlToInfPrec"
		       of (_, [a], [r]) => (a, r)
			| _ => bug (concat[
			      "unexpected type ", LB.lt_print primoplt, " of ", opname
			    ])
		      (* end case *))
		val extra_arg_lt = LD.ltc_parrow(lt_fixed_int, res_lt)
		val new_arg_lt = LD.ltc_tuple [orig_arg_lt, extra_arg_lt]
		val new_lt = LD.ltc_parrow (new_arg_lt, res_lt)
		in
		  mkFn orig_arg_lt (fn x =>
		    mkApp2 (PL.PRIM(FP.PRIM primop, new_lt, []), x, coreAcc cvtName))
		end
	(* conversion from intinf to fixed int *)
	  fun inlFromInf (opname, cvtName, primop, primoplt) = let
		val (orig_arg_lt, res_lt) = (
		      case LD.ltd_arrow primoplt handle LD.DeconExn => bug "inlFromInfPrec"
		       of (_, [a], [r]) => (a, r)
			| _ => bug (concat[
			      "unexpected type ", LB.lt_print primoplt, " of ", opname
			    ])
		      (* end case *))
		val extra_arg_lt = LD.ltc_parrow (orig_arg_lt, lt_fixed_int)
		val new_arg_lt = LD.ltc_tuple [orig_arg_lt, extra_arg_lt]
		val new_lt = LD.ltc_parrow (new_arg_lt, res_lt)
		in
		  mkFn orig_arg_lt (fn x =>
		    mkApp2 (PL.PRIM(FP.PRIM primop, new_lt, []), x, coreAcc cvtName))
		end
	(* useful error message *)
	  fun unexpectedTy () = bug(concat[
		  "unexpected type (", LB.lt_print lt, "; [",
		  String.concatWithMap "," LB.tc_print ts, "]) for ",
		  PO.toString prim
		])
	  in
            case prim
             of PO.INLINE p => (case p
                   of InlP.DIV(k as (PO.INT sz)) =>
                        inldiv (k, FP.ARITH{oper=ArithP.IDIV, sz=sz}, lt, ts)
                    | InlP.MOD(k as (PO.INT sz)) =>
                        inldiv (k, FP.ARITH{oper=ArithP.IMOD, sz=sz}, lt, ts)
                    | InlP.QUOT(k as (PO.INT sz)) =>
                        inldiv (k, FP.ARITH{oper=ArithP.IQUOT, sz=sz}, lt, ts)
                    | InlP.QUOT k =>
                        inldiv (k, FP.PURE{oper=PureP.QUOT, kind=k}, lt, ts)
                    | InlP.REM(k as (PO.INT sz)) =>
                        inldiv (k, FP.ARITH{oper=ArithP.IREM, sz=sz}, lt, ts)
                    | InlP.REM k => inldiv (k, FP.PURE{oper=PureP.REM, kind=k}, lt, ts)
                    | InlP.LSHIFT k => inlineLogicalShift (lshiftOp, k)
                    | InlP.RSHIFT k => inlineArithmeticShiftRight k
                    | InlP.RSHIFTL k => inlineLogicalShift (rshiftlOp, k)
                    | InlP.CNTZ k => raise Fail "TODO: cntZeros"
                    | InlP.CNTO k => raise Fail "TODO: cntOnes"
                    | InlP.CNTLZ k => raise Fail "TODO: cntLeadingZeros"
                    | InlP.CNTLO k => raise Fail "TODO: cntLeadingOnes"
                    | InlP.CNTTZ k => raise Fail "TODO: cntTrailingZeros"
                    | InlP.CNTTO k => raise Fail "TODO: cntTrailingOnes"
                    | InlP.IS_POW2 k => raise Fail "TODO: isPowerOf2"
                    | InlP.CEIL_LOG2 k => raise Fail "TODO: ceilLog2"
                    | InlP.MIN k => inlminmax (k, false)
                    | InlP.MAX k => inlminmax (k, true)
                    | InlP.ABS k => inlabs k
                  (* int to char conversion *)
                    | InlP.CHR => inlChr()
                  (* NOTE: not sure if we need the INLMKARRAY primop anymore, since we have
                   * eliminated the runtime-type specialization of arrays.  It might be useful
                   * for exposing information about array lengths.
                   *)
                    | InlP.MKARRAY => PL.TAPP(coreAcc "mkNormArray", ts)
                    | InlP.SUBSCRIPT => let
                        val (tc1, t1) = (case ts
                               of [z] => (z, lt_tyc z)
                                | _ => unexpectedTy ()
                              (* end case *))
                        val seqtc = LB.tcc_array tc1
                        val argt = lt_tup [lt_tyc seqtc, lt_int]
                        in
                          inlSubscript (CP.SUBSCRIPT, argt, seqtc, t1)
                        end
                    | InlP.SUBSCRIPTV => let
                        val (tc1, t1) = (case ts
                               of [z] => (z, lt_tyc z)
                                | _ => unexpectedTy ()
                              (* end case *))
                        val seqtc = LB.tcc_vector tc1
                        val argt = lt_tup [lt_tyc seqtc, lt_int]
                        in
                          inlSubscript (CP.SUBSCRIPT, argt, seqtc, t1)
                        end
                    | InlP.UPDATE => let
                        val oper = PL.PRIM(FP.PRIM CP.UPDATE, lt, ts)
                        val (tc1, t1) = (case ts
                               of [z] => (z, lt_tyc z)
                                | _ => unexpectedTy ()
                              (* end case *))
                        val seqtc = LB.tcc_array tc1
                        val argt = lt_tup [lt_tyc seqtc, lt_int, t1]
                        in
                          mkFn argt (fn x =>
                            mkLet (PL.SELECT(0, x)) (fn a =>
                              mkLet (PL.SELECT(1, x)) (fn i =>
                                mkLet (PL.SELECT(2, x)) (fn v =>
                                  boundsChk (i, a, seqtc, LB.ltc_unit) (PL.APP(oper, PL.RECORD[a, i, v]))))))
                        end
                    | InlP.NUMSUBSCRIPT kind => let
                        val (tc1, t1, t2) = (case ts
                               of [a, b] => (a, lt_tyc a, lt_tyc b)
                                | _ => unexpectedTy ()
                              (* end case *))
                        val argt = lt_tup [t1, lt_int]
                        in
                          inlSubscript (CP.NUMSUBSCRIPT kind, argt, tc1, t2)
                        end
                    | InlP.NUMSUBSCRIPTV kind => let
                        val (tc1, t1, t2) = (case ts
                               of [a, b] => (a, lt_tyc a, lt_tyc b)
                                | _ => unexpectedTy ()
                              (* end case *))
                        val argt = lt_tup [t1, lt_int]
                        in
                          inlSubscript (CP.NUMSUBSCRIPTV kind, argt, tc1, t2)
                        end
                    | InlP.NUMUPDATE kind => let
                        val oper = PL.PRIM(FP.PRIM(CP.NUMUPDATE kind), lt, ts)
                        val (tc1, t1, t2) = (case ts
                               of [a, b] => (a, lt_tyc a, lt_tyc b)
                                | _ => unexpectedTy ()
                              (* end case *))
                        val argt = lt_tup [t1, lt_int, t2]
                        in
                          mkFn argt (fn x =>
                            mkLet (PL.SELECT(0, x)) (fn a =>
                              mkLet (PL.SELECT(1, x)) (fn i =>
                                mkLet (PL.SELECT(2, x)) (fn v =>
                                  boundsChk (i, a, tc1, LB.ltc_unit)
                                    (PL.APP(oper, PL.RECORD[a, i, v]))))))
                        end
                    | InlP.NOT => mkFn lt_bool (fn x => mkCOND(x, falseLexp, trueLexp))
                    | InlP.COMPOSE => let
                        val (t1, t2, t3) = (case ts
                               of [a,b,c] => (lt_tyc a, lt_tyc b, lt_tyc c)
                                | _ => unexpectedTy ()
                              (* end case *))
                        val argt = lt_tup [lt_arw(t2, t3), lt_arw(t1, t2)]
                        val f = mkv() and g = mkv()
                        in
                          mkFn argt (fn z =>
                            mkLet (PL.SELECT(0, z)) (fn f =>
                              mkLet (PL.SELECT(1, z)) (fn g =>
                                mkFn t1 (fn x => PL.APP(f, PL.APP(g, x))))))
                        end
                    | InlP.BEFORE => let
                        val (t1, t2) = (case ts
                               of [a,b] => (lt_tyc a, lt_tyc b)
                                | _ => unexpectedTy ()
                              (* end case *))
                        val argt = lt_tup [t1, t2]
                        in
                          mkFn argt (fn x => PL.SELECT(0, x))
                        end
                    | InlP.IGNORE => let
                        val argt = (case ts
                               of [a] => lt_tyc a
                                | _ => unexpectedTy ()
                              (* end case *))
                        in
                          mkFn argt (fn _ => unitLexp)
                        end
                    | InlP.IDENTITY => let
                        val argt = (case ts
                               of [a] => lt_tyc a
                                | _ => unexpectedTy ()
                              (* end case *))
                        in
                          mkFn argt (fn v => v)
                        end
                  (* host properties *)
                    | InlP.HOST_WORD_SIZE =>
                        mkFn lt_unit (fn _ => PL.INT{
                            ival = IntInf.fromInt Target.mlValueSz,
                            ty = Target.defaultIntSz
                          })
                    | InlP.HOST_BIG_ENDIAN =>
                        mkFn lt_unit (fn _ =>
                          if Target.bigEndian then trueLexp else falseLexp)
                    | p => bug("bogus inline primop " ^ InlineOps.toString p)
                  (* end case *))
              | PO.ARITH{oper, sz} => PL.PRIM(FP.ARITH{oper=oper, sz=sz}, lt, ts)
              | PO.PURE{oper, kind} => PL.PRIM(FP.PURE{oper=oper, kind=kind}, lt, ts)
              | PO.CMP{oper, kind} => PL.PRIM(FP.CMP{oper=oper, kind=kind}, lt, ts)
              | PO.PRIM p => (case p
                  (* Precision-conversion operations involving IntInf.
                   * These need to be translated specially by providing
                   * a second argument -- the routine from _Core that
                   * does the actual conversion to or from IntInf.
                   *)
                   of CP.TRUNC_INF sz => inlFromInf ("TRUNC_INF", truncInf sz, p, lt)
                    | CP.TEST_INF sz => inlFromInf ("TEST_INF", testInf sz, p, lt)
                    | CP.COPY_INF sz => inlToInf ("COPY", copyInf sz, p, lt)
                    | CP.EXTEND_INF sz => inlToInf ("EXTEND_INF", extendInf sz, p, lt)
                  (* default handling of common primops *)
                    | _ => PL.PRIM(FP.PRIM p, lt, ts)
                  (* end case *))
            (* end case *)
          end (* trans *)

  end (* PrimTrans *)
