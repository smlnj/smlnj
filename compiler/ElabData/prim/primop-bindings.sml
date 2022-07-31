(* primop-bindings.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure PrimopBindings : sig

    val prims : PrimopBind.primop_bind list

  end = struct

    structure T = Types
    structure BT = BasicTypes
    structure P = Primop

  (* type abbreviations *)

    val tup = BT.tupleTy	(* tuple type constructor *)
    val ar = BT.-->		(* function type constructor *)

  (* built in type constructors *)
    fun contTy t = T.CONty(BT.contTycon,[t])
    fun ccontTy t = T.CONty(BT.ccontTycon,[t])
    fun refTy t = T.CONty(BT.refTycon,[t])
    fun arrTy t = T.CONty(BT.arrayTycon,[t])
    fun vecTy t = T.CONty(BT.vectorTycon,[t])

  (* type variables *)
    val tv1 = T.IBOUND 0
    val tv2 = T.IBOUND 1
    val tv3 = T.IBOUND 2

  (* polymorphic types *)
    fun p1 t = T.POLYty {sign=[false], tyfun=T.TYFUN {arity=1, body=t}}
    fun ep1 t = T.POLYty {sign=[true], tyfun=T.TYFUN {arity=1, body=t}}
    fun p2 t = T.POLYty {sign=[false,false], tyfun=T.TYFUN {arity=2, body=t}}
    fun p3 t = T.POLYty {sign=[false,false,false], tyfun=T.TYFUN {arity=3, body=t}}

  (* NOTE: we give the "inline" numeric subscript/update functions polymorphic
   * types then constrain their type in the InlineT structure.  The instantiated
   * type variables are used in the TransPrim structure to specialize the
   * functions.
   *)
    val numSubTy = p2(ar(tup[tv1, BT.intTy], tv2))
    val numUpdTy = p2(ar(tup[tv1, BT.intTy, tv2], BT.unitTy))

  (* address-sized word type *)
    val addrTy = if Target.is64 then BT.word64Ty else BT.word32Ty

  (* size and type of LargeWord.word *)
    val largeWSz = 64
    val largeWTy = BT.word64Ty

  (* word type of the specified size *)
    fun wTy 32 = BT.word32Ty
      | wTy 64 = BT.word64Ty
      | wTy _ = raise Fail "expected 32 or 64"

  (* default sizes *)
    val intSz = Target.defaultIntSz
    val realSz = Target.defaultRealSz

  (* construct the list left-to-right to reduce free-variable pressure *)
    infix :-:
    fun l :-: m = PrimopBind.mk m :: l

  (* operations on monomorphic word/array representations *)
    fun defineMonoSeqOps (tyName, elemTy, elemK, vecTy, arrTy, sz, prims) = let
	  fun subTy seqTy = ar(tup[seqTy, BT.intTy], elemTy)
	  fun updTy seqTy = ar(tup[seqTy, BT.intTy, elemTy], BT.unitTy)
	  fun mkv (name, ty, p) = (concat[tyName, "_vec_", name], ty, p)
	  fun mka (name, ty, p) = (concat[tyName, "_arr_", name], ty, p)
	  in
	    prims :-:
	    mkv("sub", numSubTy, P.INLNUMSUBSCRIPTV elemK) :-:
	    mkv("unsafe_sub", subTy vecTy, P.NUMSUBSCRIPTV elemK) :-:
	    mkv("unsafe_update", updTy vecTy, P.NUMUPDATE elemK) :-:
	    mka("sub", numSubTy, P.INLNUMSUBSCRIPT elemK) :-:
	    mka("update", numUpdTy, P.INLNUMUPDATE elemK) :-:
	    mka("unsafe_sub", subTy arrTy, P.NUMSUBSCRIPT elemK) :-:
	    mka("unsafe_update", updTy arrTy, P.NUMUPDATE elemK)
	  end

  (* add operations for an integer type to the primop list *)
    fun defineIntOps (prefix, ity, sz, prims) = let
	  val nk = P.INT sz
	  val i_i = ar(ity, ity)
	  val ii_i = ar(tup[ity, ity], ity)
	  val ii_b = ar(tup[ity, ity], BT.boolTy)
	  fun mk (name, ty, p) = (prefix ^ name, ty, p)
	  fun mk_ii_i (name, p) = mk(name, ii_i, p)
	  fun iarith_ii_i (name, p) = mk_ii_i(name, P.IARITH{oper=p, sz=sz})
	  fun cmp (name, p) = mk(name, ii_b, P.CMP{oper=p, kind=nk})
	  in
	    prims :-:
	    iarith_ii_i("add", P.IADD) :-:
	    iarith_ii_i("sub", P.ISUB) :-:
	    iarith_ii_i("mul", P.IMUL) :-:
	    mk_ii_i("div", P.INLDIV nk) :-:
	    mk_ii_i("mod", P.INLMOD nk) :-:
	    mk_ii_i("quot", P.INLQUOT nk) :-:
	    mk_ii_i("rem", P.INLREM nk) :-:
	    mk("neg", i_i, P.IARITH{oper=P.INEG, sz=sz}) :-:
	    cmp("lt", P.LT) :-:
	    cmp("le", P.LTE) :-:
	    cmp("gt", P.GT) :-:
	    cmp("ge", P.GTE) :-:
	    cmp("eql", P.EQL) :-:
	    cmp("neq", P.NEQ) :-:
	    mk_ii_i("min", P.INLMIN nk) :-:
	    mk_ii_i("max", P.INLMAX nk) :-:
	    mk("abs", i_i, P.INLABS nk)
	  end

  (* add operations for a word type to the primop list *)
    fun defineWordOps (prefix, wty, sz, prims) = let
	  val nk = P.UINT sz
	  val w_w = ar(wty, wty)
	  val ww_w = ar(tup[wty, wty], wty)
	  val shftTy = ar(tup[wty, BT.wordTy], wty)
	  val ww_b = ar(tup[wty, wty], BT.boolTy)
	  fun mk (name, ty, p) = (prefix ^ name, ty, p)
	  fun mk_ww_w (name, p) = mk(name, ww_w, p)
	  fun arith_ww_w (name, p) = mk_ww_w(name, P.PURE_ARITH{oper=p, kind=nk})
	  fun shift (name, p) = mk(name, shftTy, p)
	  fun cmp (name, p) = mk(name, ww_b, P.CMP{oper=p, kind=nk})
	  in
	    prims :-:
	    arith_ww_w("add", P.ADD) :-:
	    arith_ww_w("sub", P.SUB) :-:
	    arith_ww_w("mul", P.MUL) :-:
	    mk_ww_w("div", P.INLQUOT nk) :-:
	    mk_ww_w("mod", P.INLREM nk) :-:
	    mk("neg", w_w, P.PURE_ARITH{oper=P.NEG, kind=nk}) :-:
	    arith_ww_w("orb", P.ORB) :-:
	    arith_ww_w("xorb", P.XORB) :-:
	    arith_ww_w("andb", P.ANDB) :-:
	    shift("rshift", P.INLRSHIFT nk) :-:
	    shift("rshiftl", P.INLRSHIFTL nk) :-:
	    shift("lshift", P.INLLSHIFT nk) :-:
	    shift("raw_rshift", P.PURE_ARITH{oper=P.RSHIFT, kind=nk}) :-:
	    shift("raw_rshiftl", P.PURE_ARITH{oper=P.RSHIFTL, kind=nk}) :-:
	    shift("raw_lshift", P.PURE_ARITH{oper=P.LSHIFT, kind=nk}) :-:
	    mk("notb", w_w, P.PURE_ARITH{oper=P.NOTB, kind=nk}) :-:
	    cmp("lt", P.LT) :-:
	    cmp("le", P.LTE) :-:
	    cmp("gt", P.GT) :-:
	    cmp("ge", P.GTE) :-:
	    cmp("eql", P.EQL) :-:
	    cmp("neq", P.NEQ) :-:
	    mk_ww_w("min", P.INLMIN nk) :-:
	    mk_ww_w("max", P.INLMAX nk)
	  end

  (* add operations for a real type to the primop list *)
    fun defineRealOps (prefix, rty, sz, prims) = let
	  val nk = P.FLOAT sz
	  val r_r = ar(rty, rty)
	  val rr_r = ar(tup[rty, rty], rty)
	  val rr_b = ar(tup[rty, rty], BT.boolTy)
	  fun mk (name, ty, p) = (prefix ^ name, ty, p)
	  fun mk_rr_r (name, p) = mk(name, rr_r, p)
	  fun arith_rr_r (name, p) = mk_rr_r(name, P.PURE_ARITH{oper=p, kind=nk})
	  fun arith_r_r (name, p) = mk(name, r_r, P.PURE_ARITH{oper=p, kind=nk})
	  fun cmp (name, p) = mk(name, rr_b, P.CMP{oper=p, kind=nk})
	  in
	    prims :-:
	    arith_rr_r("add", P.ADD) :-:
	    arith_rr_r("sub", P.SUB) :-:
	    arith_rr_r("mul", P.MUL) :-:
	    arith_rr_r("div", P.FDIV) :-:
	    arith_r_r("neg", P.NEG) :-:
	    cmp("lt", P.LT) :-:
	    cmp("le", P.LTE) :-:
	    cmp("gt", P.GT) :-:
	    cmp("ge", P.GTE) :-:
	    cmp("eql", P.EQL) :-:
	    cmp("neq", P.NEQ) :-:
	    mk("sgn", ar(rty, BT.boolTy), P.FSGN sz) :-:
	    mk_rr_r("min", P.INLMIN nk) :-:
	    mk_rr_r("max", P.INLMAX nk) :-:
	    arith_r_r("abs", P.FABS) :-:
	    arith_r_r("sqrt", P.FSQRT) :-:
	  (* note that the argument type is 'a to force boxing of the real *)
	    mk("to_bits", p1(ar(tv1, wTy sz)), P.REAL_TO_BITS sz)
	  end

  (* utility functions for conversions *)
    fun sCopy (srcSz, dstSz) = if (srcSz < dstSz)
	    then P.EXTEND(srcSz, dstSz)
	  else if (srcSz = dstSz)
	    then P.COPY(srcSz, dstSz)
	    else P.TRUNC(srcSz, dstSz)
    fun sCopyChk (srcSz, dstSz) = if (srcSz < dstSz)
	    then P.EXTEND(srcSz, dstSz)
	  else if (srcSz = dstSz)
	    then P.COPY(srcSz, dstSz)
	    else P.TEST(srcSz, dstSz)
    fun uCopy (srcSz, dstSz) = if (srcSz <= dstSz)
	  then P.COPY(srcSz, dstSz)
	  else P.TRUNC(srcSz, dstSz)
    fun uCopyChk (srcSz, dstSz) = if (srcSz < dstSz)
	  then P.COPY(srcSz, dstSz)
	  else P.TESTU(srcSz, dstSz)

  (* generate conversion operators for the int and word types of the given
   * size.
   *)
    fun defineCvtOps (iTy, wTy, sz, prims) = let
	  val (iName, wName) = if (sz = intSz)
		then ("int", "word")
		else let val s = Int.toString sz in ("int"^s, "word"^s) end
	  val lgWName = "word" ^ Int.toString largeWSz
	  fun nm (s, from, to) = concat[s, from, "_to_", to]
	  fun iTo ty = ar(iTy, ty)
	  fun wTo ty = ar(wTy, ty)
	  fun iFrom ty = ar(ty, iTy)
	  fun wFrom ty = ar(ty, wTy)
	(* add conversions to/from default types when sz <> default size *)
	  val prims = if (sz = intSz)
		then prims
		else prims :-:
		  (iName ^ "_to_int", iTo BT.intTy, sCopyChk(sz, intSz)) :-:
		  (wName ^ "_to_word", wTo BT.wordTy, uCopy(sz, intSz)) :-:
		  ("int_to_" ^ iName, iFrom BT.intTy, sCopyChk(intSz, sz)) :-:
		  ("word_to_" ^ wName, wFrom BT.wordTy, uCopy(intSz, sz))
	(* add conversions to/from large word type when sz <> large word size *)
	  val prims = if (sz = largeWSz)
		then prims
		else prims :-:
		  (nm("", lgWName, wName), wFrom largeWTy, P.TRUNC(largeWSz, sz)) :-:
		  (nm("unsigned_", wName, lgWName), wTo largeWTy, P.COPY(sz, largeWSz)) :-:
		  (nm("signed_", wName, lgWName), wTo largeWTy, P.EXTEND(sz, largeWSz))
	  in
	    prims :-:
	  (* int type to/from intinf *)
	    (iName ^  "_to_intinf", iTo BT.intinfTy, P.EXTEND_INF sz) :-:
	    ("intinf_to_" ^ iName, iFrom BT.intinfTy, P.TEST_INF sz) :-:
	  (* word type to/from default int type *)
	    ("int_to_" ^ wName, wFrom BT.intTy, sCopy(intSz, sz)) :-:
	    (nm("unsigned_", wName, "int"), wTo BT.intTy, uCopyChk(sz, intSz)) :-:
	    (nm("signed_", wName, "int"), wTo BT.intTy, sCopyChk(sz, intSz)) :-:
	  (* word type to/from int inf *)
	    ("unsigned_" ^ wName ^ "_to_intinf", wTo BT.intinfTy, P.COPY_INF sz) :-:
	    ("signed_" ^ wName ^ "_to_intinf", wTo BT.intinfTy, P.EXTEND_INF sz) :-:
	    ("intinf_to_" ^ wName, wFrom BT.intinfTy, P.TRUNC_INF sz)
	  end

  (* size-independent primops *)
    val prims = [] :-:
	(* continuation operators *)
	  ("callcc", p1(ar(ar(contTy tv1,tv1),tv1)), P.CALLCC) :-:
	  ("throw", p2(ar(contTy tv1,ar(tv1,tv2))), P.THROW) :-:
	  ("capture", p1(ar(ar(ccontTy tv1,tv1),tv1)), P.CAPTURE) :-:
	  ("isolate", p1(ar(ar(tv1,BT.unitTy),contTy tv1)), P.ISOLATE) :-:
	  ("cthrow", p2(ar(ccontTy tv1,ar(tv1,tv2))), P.THROW) :-:
	(* reference operations *)
	  ("!", p1(ar(refTy tv1,tv1)), P.DEREF) :-:
	  (":=", p1(ar(tup[refTy tv1,tv1],BT.unitTy)), P.ASSIGN) :-:
	  ("makeref", p1(ar(tv1,refTy tv1)), P.MAKEREF) :-:
	(* boxity tests *)
	  ("boxed", p1(ar(tv1,BT.boolTy)), P.BOXED) :-:
	  ("unboxed", p1(ar(tv1,BT.boolTy)), P.UNBOXED) :-:
	(* type casts *)
	  ("cast", p2(ar(tv1,tv2)), P.CAST) :-:
	(* polymorphic equality tests *)
	  ("=", ep1(ar(tup[tv1,tv1],BT.boolTy)), P.POLYEQL) :-:
	  ("<>", ep1(ar(tup[tv1,tv1],BT.boolTy)), P.POLYNEQ) :-:
	  ("ptr_eql", p1(ar(tup[tv1,tv1],BT.boolTy)), P.PTREQL) :-:
	  ("ptr_neq", p1(ar(tup[tv1,tv1],BT.boolTy)), P.PTRNEQ) :-:
	(* runtime hooks *)
	  ("getvar", p1(ar(BT.unitTy,tv1)), P.GETVAR) :-:
	  ("setvar", p1(ar(tv1,BT.unitTy)), P.SETVAR) :-:
	  ("mkspecial", p2(ar(tup[BT.intTy,tv1],tv2)), P.MKSPECIAL) :-:
	  ("getspecial", p1(ar(tv1,BT.intTy)), P.GETSPECIAL) :-:
	  ("setspecial", p1(ar(tup[tv1,BT.intTy],BT.unitTy)), P.SETSPECIAL) :-:
	  ("gethdlr", p1(ar(BT.unitTy,contTy tv1)), P.GETHDLR) :-:
	  ("sethdlr", p1(ar(contTy tv1,BT.unitTy)), P.SETHDLR) :-:
	  ("gettag", p1(ar(tv1,BT.intTy)), P.GETTAG) :-:
	  ("objlength", p1(ar(tv1, BT.intTy)), P.OBJLENGTH) :-:
	(* inline basis operations *)
	  ("inl_compose", p3(ar(tup[ar(tv2,tv3),ar(tv1,tv2)],ar(tv1,tv3))), P.INLCOMPOSE) :-:
	  ("inl_before", p2(ar(tup[tv1,tv2],tv1)), P.INLBEFORE) :-:
	  ("inl_ignore", p1(ar(tv1,BT.unitTy)), P.INLIGNORE) :-:
	  ("inl_identity", p1(ar(tv1,tv1)), P.INLIDENTITY) :-:
	  ("inl_not", ar(BT.boolTy, BT.boolTy), P.INLNOT) :-:
	  ("inl_chr", ar(BT.intTy, BT.charTy), P.INLCHR) :-:
	  ("inl_ord", ar(BT.charTy, BT.intTy), P.CAST) :-:
	(* polymorphic array and vector *)
	  ("mkarray", p1(ar(tup[BT.intTy,tv1],arrTy tv1)), P.INLMKARRAY) :-:
	  ("arr_unsafe_sub", p1(ar(tup[arrTy tv1,BT.intTy],tv1)), P.SUBSCRIPT) :-:
	  ("arr_sub", p1(ar(tup[arrTy tv1,BT.intTy],tv1)), P.INLSUBSCRIPT) :-:
	  ("vec_unsafe_sub", p1(ar(tup[vecTy tv1,BT.intTy],tv1)), P.SUBSCRIPTV) :-:
	  ("vec_sub", p1(ar(tup[vecTy tv1,BT.intTy],tv1)), P.INLSUBSCRIPTV) :-:
	  ("arr_unsafe_update", p1(ar(tup[arrTy tv1,BT.intTy,tv1],BT.unitTy)), P.UPDATE) :-:
	  ("arr_update", p1(ar(tup[arrTy tv1,BT.intTy,tv1],BT.unitTy)), P.INLUPDATE) :-:
	  ("arr_unboxed_update",
	    p1(ar(tup[arrTy tv1,BT.intTy,tv1],BT.unitTy)), P.UNBOXEDUPDATE) :-:
	(* generic sequence operations*)
	  ("newArray0", p1(ar(BT.unitTy, tv1)), P.NEW_ARRAY0) :-:
	  ("seq_length", p1(ar(tv1, BT.intTy)), P.LENGTH) :-:
	  ("seq_data", p2(ar(tv1, tv2)), P.GET_SEQ_DATA) :-:
	  ("raw64Sub", p1(ar(tup[tv1, BT.intTy], BT.realTy)), P.SUBSCRIPT_RAW64) :-:
	  ("recordSub", p2(ar(tup[tv1,BT.intTy], tv2)), P.SUBSCRIPT_REC)

  (* operations on word8 arrays/vectors *)
    val prims = defineMonoSeqOps (
	  "word8", BT.word8Ty, P.UINT 8, BT.word8vectorTy, BT.word8arrayTy, 8, prims)

  (* operations on char arrays/vectors *)
    val prims = defineMonoSeqOps (
	  "char", BT.charTy, P.UINT 8, BT.stringTy, BT.chararrayTy, 8, prims)

  (* operations on Real64 arrays *)
    val prims = let
        (* FIXME: these types really should be monomorphic!! *)
	  val subTy = p1(ar(tup[tv1, BT.intTy], BT.realTy))
	  val updTy = p1(ar(tup[tv1, BT.intTy, BT.realTy], BT.unitTy))
	  val elemK = P.FLOAT 64
	  fun mk (name, ty, p) = ("real64_arr_" ^ name, ty, p)
	  in
	    prims :-:
	    mk("sub", numSubTy, P.INLNUMSUBSCRIPT elemK) :-:
	    mk("update", numUpdTy, P.INLNUMUPDATE elemK) :-:
	    mk("unsafe_sub", subTy, P.NUMSUBSCRIPT elemK) :-:
	    mk("unsafe_update", updTy, P.NUMUPDATE elemK)
	  end
(* TODO: once we have real64vectors, we can define those operations too *)

  (* default integer operations *)
    val prims = defineIntOps ("int_", BT.intTy, intSz, prims)

  (* extra operations for the default integer type, which essentially implement
   * word operations on ints (these are used to simplify the Basis Library
   * implementation).
   *)
    val prims = let
	  val nk = P.UINT intSz
	  val i_i = ar(BT.intTy, BT.intTy)
	  val ii_i = ar(tup[BT.intTy, BT.intTy], BT.intTy)
	  val iw_i = ar(tup[BT.intTy, BT.wordTy], BT.intTy)
	  val ii_b = ar(tup[BT.intTy, BT.intTy], BT.boolTy)
	  in
	    prims :-:
	  (* unchecked addition/subtraction *)
	    ("int_unsafe_add", ii_i, P.PURE_ARITH{oper=P.ADD, kind=nk}) :-:
	    ("int_unsafe_sub", ii_i, P.PURE_ARITH{oper=P.SUB, kind=nk}) :-:
	  (* bitwise operations *)
	    ("int_orb", ii_i, P.PURE_ARITH{oper=P.ORB, kind=nk}) :-:
	    ("int_xorb", ii_i, P.PURE_ARITH{oper=P.XORB, kind=nk}) :-:
	    ("int_andb", ii_i, P.PURE_ARITH{oper=P.ANDB, kind=nk}) :-:
	    ("int_raw_rshift", iw_i, P.PURE_ARITH{oper=P.RSHIFT, kind=nk}) :-:
	    ("int_raw_lshift", iw_i, P.PURE_ARITH{oper=P.LSHIFT, kind=nk}) :-:
	    ("int_notb", i_i, P.PURE_ARITH{oper=P.NOTB, kind=nk}) :-:
	    ("int_ltu", ii_b, P.CMP{oper=P.LT, kind=nk}) :-:
	    ("int_geu", ii_b, P.CMP{oper=P.GTE, kind=nk})
	  end

  (* default word operations *)
    val prims = defineWordOps ("word_", BT.wordTy, intSz, prims)

  (* Int32 operations *)
    val prims = defineIntOps ("int32_", BT.int32Ty, 32, prims)

  (* Word8 operations *)
    val prims = defineWordOps ("word8_", BT.word8Ty, 8, prims)

  (* Word32 operations *)
    val prims = defineWordOps ("word32_", BT.word32Ty, 32, prims)

  (* Int64 operations *)
    val prims = defineIntOps ("int64_", BT.int64Ty, 64, prims)

  (* Word64 operations *)
    val prims = defineWordOps ("word64_", BT.word64Ty, 64, prims)

  (* Real64 operations *)
    val prims = defineRealOps ("real64_", BT.realTy, 64, prims)

  (* conversions integers and words *)
    val prims = defineCvtOps (BT.intTy, BT.wordTy, intSz, prims)
    val prims = defineCvtOps (BT.int32Ty, BT.word32Ty, 32, prims)
    val prims = defineCvtOps (BT.int64Ty, BT.word64Ty, 64, prims)

  (* conversions for Word8
   * NOTE: if we had an Int8.int type, then we could use defineCvtOps here!
   *)
    val prims = let
	  val lgWName = "word" ^ Int.toString largeWSz
	  fun wTo ty = ar(BT.word8Ty, ty)
	  fun wFrom ty = ar(ty, BT.word8Ty)
	  in
	    prims :-:
	    (lgWName ^ "_to_word8", wFrom largeWTy, P.TRUNC(largeWSz, 8)) :-:
	    ("unsigned_word8_to_" ^ lgWName, wTo largeWTy, P.COPY(8, largeWSz)) :-:
	    ("signed_word8_to_" ^ lgWName, wTo largeWTy, P.EXTEND(8, largeWSz)) :-:
	  (* word type to/from default int type *)
	    ("int_to_word8", wFrom BT.intTy, sCopy(intSz, 8)) :-:
	    ("unsigned_word8_to_int", wTo BT.intTy, uCopyChk(8, intSz)) :-:
	    ("signed_word8_to_int", wTo BT.intTy, sCopyChk(8, intSz)) :-:
	  (* word type to/from int inf *)
	    ("unsigned_word8_to_intinf", wTo BT.intinfTy, P.COPY_INF 8) :-:
	    ("signed_word8_to_intinf", wTo BT.intinfTy, P.EXTEND_INF 8) :-:
	    ("intinf_to_word8", wFrom BT.intinfTy, P.TRUNC_INF 8)
	  end

  (* some additional conversions that are used in system/smlnj/init/core-intinf.sml
   * system/smlnj/init/core-int64.sml, and system/smlnj/init/core-word64.sml
   *)
    val prims = if Target.is64
	  then prims :-:
	      ("trunc_int64_to_word", ar(BT.int64Ty, BT.wordTy), P.TRUNC(64, intSz)) :-:
	      ("trunc_word64_to_int", ar(BT.word64Ty, BT.intTy), P.TRUNC(64, intSz)) :-:
	      ("copy_int64_to_word64", ar(BT.int64Ty, BT.word64Ty), P.COPY(64, 64)) :-:
	      ("copy_word_to_int64", ar(BT.wordTy, BT.int64Ty), P.COPY(intSz, 64)) :-:
	      ("copy_word64_to_int64", ar(BT.word64Ty, BT.int64Ty), P.COPY(64, 64))
	  else let
	    in
	      prims :-:
	      ("trunc_int32_to_word", ar(BT.int32Ty, BT.wordTy), P.TRUNC(32, intSz)) :-:
	      ("trunc_word32_to_int", ar(BT.word32Ty, BT.intTy), P.TRUNC(32, intSz)) :-:
	      ("copy_int32_to_word32", ar(BT.int32Ty, BT.word32Ty), P.COPY(32, 32)) :-:
	      ("copy_word_to_int32", ar(BT.wordTy, BT.int32Ty), P.COPY(intSz, 32)) :-:
	      ("copy_word32_to_int32", ar(BT.word32Ty, BT.int32Ty), P.COPY(32, 32))
	    end

  (* primops to go between abstract and concrete representation of
   * 64-bit ints and words for 32-bit targets
   *)
    val prims = if Target.is64
	  then prims
	  else let
	    val pw32 = tup[BT.word32Ty, BT.word32Ty]
	    in
	      prims :-:
	      ("int64_to_pair", ar(BT.int64Ty, pw32), P.EXTERN64) :-:
	      ("int64_from_pair", ar(pw32, BT.int64Ty), P.INTERN64) :-:
	      ("word64_to_pair", ar(BT.word64Ty, pw32), P.EXTERN64) :-:
	      ("word64_from_pair", ar(pw32, BT.word64Ty), P.INTERN64)
	    end

(* REAL32: FIXME *)
  (* real/int conversions *)
    val prims = let
	  val r_i = ar(BT.realTy, BT.intTy)
	  fun r2i (name, fl) =
		(name, r_i, P.REAL_TO_INT{floor=fl, from=realSz, to=intSz})
	  fun i2r (name, iTy, iSz) =
		(name, ar(iTy, BT.realTy), P.INT_TO_REAL{from=iSz, to=realSz})
	  in
	    prims :-:
	    r2i("floor_real64_to_int", true) :-:
	    r2i("round_real64_to_int", false) :-:
	    i2r("int_to_real64", BT.intTy, intSz) :-:
(* FIXME: add "word_to_real64" *)
	    (if Target.is64
	      then i2r("int64_to_real64", BT.int64Ty, 64)
	      else i2r("int32_to_real64", BT.int32Ty, 32))
	  end

  (* Char operations *)
    val prims = let
	  val cc_b = ar(tup[BT.charTy, BT.charTy], BT.boolTy)
	  fun cmp (name, p) = ("char_"^name, cc_b, P.CMP{oper=p, kind=P.UINT intSz})
	  in
	    prims :-:
	    cmp("lt", P.LT) :-:
	    cmp("le", P.LTE) :-:
	    cmp("gt", P.GT) :-:
	    cmp("ge", P.GTE) :-:
	    cmp("eql", P.EQL) :-:
	    cmp("neq", P.NEQ)
	  end

  (* primops for the c_pointer type *)
    val prims = let
	  val wName = "word" ^ Int.toString Target.pointerSz
	  in
	    prims :-:
	    ("cptr_to_" ^ wName, ar(BT.pointerTy, addrTy), P.PTR_TO_WORD) :-:
	    (wName ^ "_to_cptr", ar(addrTy, BT.pointerTy), P.WORD_TO_PTR)
	  end

  (* primops for C FFI *)
    val prims = let
	(* representation of pointers to raw values *)
(* TODO: use BT.pointerTy tof the adrTy *)
	  val adrTy = if Target.is64 then BT.word64Ty else BT.word32Ty
	(* offsets into structs/arrays; currently limited to 32 bits, but should
	 * be target specific.
	 *)
	  val offsetTy = BT.word32Ty
	(* The type of the RAW_CCALL primop (as far as the type checker is concerned)
	 * is:
	 *    adr * 'a * 'b -> 'd
	 * where adr is a word type that is the same size as the machine's pointer
	 * type.  The primop cannot be used without having 'a, 'b, and 'c
	 * monomorphically instantiated.  In particular, 'a will be the type of the
	 * ML argument list, 'c will be the type of the result, and 'b
	 * will be a type of a fake arguments.  The idea is that 'b will be
	 * instantiated with some ML type that encodes the type of the actual
	 * C function in order to be able to generate code according to the C
	 * calling convention.
	 * (In other words, 'b will be a completely ad-hoc encoding of a CTypes.c_proto
	 * value in ML types.  The encoding also contains information about
	 * calling conventions and reentrancy.)
	 *)
	  val rccTy = p3(ar(tup[adrTy, tv1, tv2], tv3))
	  fun mk (name, ty, p) = ("raw_" ^ name, ty, p)
	  fun mkLd (name, ty, nk) =
		("raw_load_" ^ name, ar(adrTy, ty), P.RAW_LOAD nk)
	  fun mkSt(name, ty, nk) =
		("raw_store_" ^ name, ar(tup[adrTy, ty], BT.unitTy), P.RAW_STORE nk)
	  fun mkSub (name, ty, nk) =
		("raw_sub_" ^ name, p1(ar(tup[tv1, offsetTy], ty)), P.RAW_LOAD nk)
	  fun mkUpd (name, ty, nk) = (
		  "raw_update_" ^ name,
		  p1(ar(tup[tv1, offsetTy, ty], BT.unitTy)),
		  P.RAW_STORE nk
		)
	  in
	    prims :-:
	    mk("ccall", rccTy, P.RAW_CCALL NONE) :-:
	  (* Support for direct construction of C objects on ML heap.
	   * rawrecord builds a record holding C objects on the heap.
	   * rawselectxxx index on this record.  They are of type:
	   *    'a * Word32.word -> Word32.word
	   * The 'a is to guarantee that the compiler will treat
	   * the record as a ML object, in case it passes thru a gc boundary.
	   * rawupdatexxx writes to the record.
	   *)
	    mk("record", p1(ar(BT.intTy,tv1)), P.RAW_RECORD { align64 = false }) :-:
	    mk("record64", p1(ar(BT.intTy,tv1)), P.RAW_RECORD { align64 = true }) :-:
	  (* load/store raw values *)
	    mkLd("word8", BT.word32Ty, P.UINT 8) :-:
	    mkLd("int8", BT.int32Ty, P.INT 8) :-:
	    mkLd("word16", BT.word32Ty, P.UINT 16) :-:
	    mkLd("int16", BT.int32Ty, P.INT 16) :-:
	    mkLd("word32", BT.word32Ty, P.UINT 32) :-:
	    mkLd("int32", BT.int32Ty, P.INT 32) :-:
	    mkLd("float32", BT.realTy, P.FLOAT 32) :-:
	    mkLd("float64", BT.realTy, P.FLOAT 64) :-:
	    mkSt("word8", BT.word32Ty, P.UINT 8) :-:
	    mkSt("int8", BT.int32Ty, P.INT 8) :-:
	    mkSt("word16", BT.word32Ty, P.UINT 16) :-:
	    mkSt("int16", BT.int32Ty, P.INT 16) :-:
	    mkSt("word32", BT.word32Ty, P.UINT 32) :-:
	    mkSt("int32", BT.int32Ty, P.INT 32) :-:
	    mkSt("float32", BT.realTy, P.FLOAT 32) :-:
	    mkSt("float64", BT.realTy, P.FLOAT 64) :-:
	  (* load/store raw values with offset *)
	    mkSub("word8", BT.word32Ty, P.UINT 8) :-:
	    mkSub("int8", BT.int32Ty, P.INT 8) :-:
	    mkSub("word16", BT.word32Ty, P.UINT 16) :-:
	    mkSub("int16", BT.int32Ty, P.INT 16) :-:
	    mkSub("word32", BT.word32Ty, P.UINT 32) :-:
	    mkSub("int32", BT.int32Ty, P.INT 32) :-:
	    mkSub("float32", BT.realTy, P.FLOAT 32) :-:
	    mkSub("float64", BT.realTy, P.FLOAT 64) :-:
	    mkUpd("word8", BT.word32Ty, P.UINT 8) :-:
	    mkUpd("int8", BT.int32Ty, P.INT 8) :-:
	    mkUpd("word16", BT.word32Ty, P.UINT 16) :-:
	    mkUpd("int16", BT.int32Ty, P.INT 16) :-:
	    mkUpd("word32", BT.word32Ty, P.UINT 32) :-:
	    mkUpd("int32", BT.int32Ty, P.INT 32) :-:
	    mkUpd("float32", BT.realTy, P.FLOAT 32) :-:
	    mkUpd("float64", BT.realTy, P.FLOAT 64)
	  end

(* Debugging *)
(* *)
fun prBind bind = let
      val n = PrimopBind.nameOf bind
      val ty = PrimopBind.typeOf bind
      val p = PrimopBind.defnOf bind
      in
	Control_Print.say(concat[
	    StringCvt.padLeft #" " 30 n, " = ",
	    PrimopUtil.toString p, "\n"
	  ])
      end

val _ = (
	Control_Print.say "********************* Primop Bindings ********************\n";
	Control_Print.say (concat[
	    "* int size = ", Int.toString intSz,
	    "; real size = ", Int.toString realSz,
	    "; large word size = ", Int.toString largeWSz,
	    "\n"
	  ]);
	Control_Print.say "********************\n";
	List.app prBind prims;
	Control_Print.say "********************\n")
(* *)

  end
