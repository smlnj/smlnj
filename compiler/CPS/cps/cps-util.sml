(* cps-util.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure CPSUtil : sig

  (* flip the meaning of a branch *)
    val opp : CPS.P.branch -> CPS.P.branch

    val combinepaths : CPS.accesspath * CPS.accesspath -> CPS.accesspath
    val lenp : CPS.accesspath -> int

    val hasRCC : CPS.cexp -> bool

    val ctyToString : CPS.cty -> string
    val sizeOfTy : CPS.cty -> int   (* size of its representation in bits *)
    val isFloat : CPS.cty -> bool (* is it a floating point type? *)
    val isTagged : CPS.cty -> bool

    val BOGt : CPS.cty

    val ctyc  : Lty.tyc -> CPS.cty
    val ctype : Lty.lty -> CPS.cty

  end = struct

    structure P = CPS.P
    structure PT = PrimTyc

    fun bug s = ErrorMsg.impossible ("CPS:" ^ s)

    local
      fun ioper P.GT  = P.LTE
	| ioper P.LTE = P.GT
	| ioper P.LT  = P.GTE
	| ioper P.GTE = P.LT
	| ioper P.EQL = P.NEQ
	| ioper P.NEQ = P.EQL

      fun foper P.F_EQ  = P.F_ULG
	| foper P.F_ULG = P.F_EQ
	| foper P.F_GT  = P.F_ULE
	| foper P.F_GE  = P.F_ULT
	| foper P.F_LT  = P.F_UGE
	| foper P.F_LE  = P.F_UGT
	| foper P.F_LG  = P.F_UE
	| foper P.F_LEG = P.F_UN
	| foper P.F_UGT = P.F_LE
	| foper P.F_UGE = P.F_LT
	| foper P.F_ULT = P.F_GE
	| foper P.F_ULE = P.F_GT
	| foper P.F_UE  = P.F_LG
	| foper P.F_UN  = P.F_LEG
    in
    fun opp (P.CMP{oper, kind}) = P.CMP{oper=ioper oper, kind=kind}
      | opp (P.FCMP{oper, size}) = P.FCMP{oper=foper oper, size=size}
      | opp (P.FSGN _) = bug "FSGN has no opposite"
      | opp P.BOXED = P.UNBOXED
      | opp P.UNBOXED = P.BOXED
      | opp P.PEQL = P.PNEQ
      | opp P.PNEQ = P.PEQL
      | opp (P.STREQL _) = bug "STREQL has no opposite"
    end (* local *)

    fun hasRCC cexp = (case cexp
	   of CPS.RCC _ => true
	    | CPS.RECORD(_, _, _, e) => hasRCC e
	    | CPS.SELECT(_, _, _, _, e) => hasRCC e
	    | CPS.OFFSET(_, _, _, e) => hasRCC e
	    | CPS.APP _ => false
	    | CPS.FIX(fl, e) =>
		hasRCC e
		orelse List.exists (fn (_, _, _, _, e) => hasRCC e) fl
	    | CPS.SWITCH(_, _, ce) => List.exists hasRCC ce
	    | CPS.BRANCH(_, _, _, c1, c2) => hasRCC c1 orelse hasRCC c2
	    | CPS.SETTER(_, _, e) => hasRCC e
	    | CPS.LOOKER(_, _, _, _, e) => hasRCC e
	    | CPS.ARITH(_, _, _, _, e) => hasRCC e
	    | CPS.PURE(_, _, _, _, e) => hasRCC e
	  (* end case *))

    fun sizeOfTy (CPS.FLTt sz) = sz
      | sizeOfTy (CPS.NUMt{tag=false, sz}) = sz
      | sizeOfTy (CPS.NUMt _) = Target.mlValueSz
      | sizeOfTy (CPS.PTRt _ | CPS.FUNt | CPS.CNTt) = Target.mlValueSz

    fun isFloat (CPS.FLTt _) = true
      | isFloat _ = false

    fun isTagged (CPS.FLTt _) = false
      | isTagged (CPS.NUMt{tag, ...}) = tag
      | isTagged _ = true

    fun ctyToString (CPS.NUMt{sz, tag=true}) =  "[I]"
      | ctyToString (CPS.NUMt{sz, ...}) = concat["[I", Int.toString sz, "]"]
      | ctyToString (CPS.FLTt sz) = concat["[R", Int.toString sz, "]"]
      | ctyToString (CPS.PTRt(CPS.RPT k)) = concat["[PR", Int.toString k, "]"]
      | ctyToString (CPS.PTRt(CPS.FPT k)) = concat["[PF", Int.toString k, "]"]
      | ctyToString (CPS.PTRt CPS.VPT) =  "[PV]"
      | ctyToString (CPS.FUNt) = "[FN]"
      | ctyToString (CPS.CNTt) = "[C]"

    fun combinepaths (p, CPS.OFFp 0) = p
      | combinepaths (p, q) = let
	  fun comb (CPS.OFFp 0) = q
	    | comb (CPS.OFFp i) = (case q
		 of (CPS.OFFp j) => CPS.OFFp(i+j)
		  | (CPS.SELp(j,p)) => CPS.SELp(i+j,p)
		(* end case *))
	    | comb (CPS.SELp(i, p)) = CPS.SELp(i, comb p)
	  in
	    comb p
	  end

    fun lenp (CPS.OFFp _) = 0
      | lenp (CPS.SELp(_,p)) = 1 + lenp p

    val BOGt = CPS.PTRt CPS.VPT  (* bogus pointer type whose length is unknown *)

    local
      structure LT = Lty
      structure LK = LtyKernel
      structure LD = LtyDef
      structure LB = LtyBasic
      val tc_real = LB.tcc_real (* REAL32: this code assumes only one float type *)
      val lt_real = LB.ltc_real
      val ptc_int = PT.ptc_int
    in

    (* REAL32: this code assumes only one float type *)
    fun tcflt tc = if LK.tc_eqv(tc, tc_real) then true else false
    fun ltflt lt = if LK.lt_eqv(lt, lt_real) then true else false

    fun rtyc (f, []) = CPS.RPT 0
      | rtyc (f, ts) = let
	  fun loop (a::r, b, len) =
		if f a then loop(r, b, len+1) else loop(r, false, len+1)
	    | loop ([], b, len) = if b then CPS.FPT len else CPS.RPT len
	  in
	    loop(ts, true, 0)
	  end

    fun ctyc tc = LD.tcw_prim (tc,
	  fn pt => (case PT.numSize pt
	       of SOME 0 => BOGt
		| SOME sz => CPS.NUMt{sz = sz, tag = (sz <= Target.defaultIntSz)}
		| NONE => (case PT.realSize pt
		     of SOME sz => CPS.FLTt sz
		      | NONE => BOGt
		    (* end case *))
	      (* end case *)),
	  fn tc => LD.tcw_tuple (tc,
	      fn ts => CPS.PTRt(rtyc(tcflt, ts)),
	      fn tc =>
		if LD.tcp_arrow tc then CPS.FUNt
		else if LD.tcp_cont tc then CPS.CNTt
		else BOGt))

    fun ctype lt =
	  LD.ltw_tyc(lt, fn tc => ctyc tc,
	      fn lt =>
		LD.ltw_str(lt, fn ts => CPS.PTRt(rtyc(fn _ => false, ts)),
		    fn lt => if LD.ltp_fct lt then CPS.FUNt
			     else if LD.ltp_cont lt then CPS.CNTt
				  else BOGt))
    end (* local *)

  end
