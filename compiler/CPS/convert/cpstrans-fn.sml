(* cpstrans-fn.sml
 *
 * This module implements a CPS -> CPS transformation that ensures that function
 * (and continuation) arguments will fit in the available machine registers.  It
 * also removes occurrences of the Box/Unbox primops and Wrap/Unwraps of tagged
 * integers.
 *
 * QUESTION: this pass is currently applied before CPS optimization, but if we
 *   implemented something like useless-variable elimination, we might be able
 *   to avoid spilling in some cases.
 *
 * TODO: FLINT limits the number of arguments to a function to 9.  This limit
 *   is controlled by the `flatten_limit` variable in `FLINT/kernel/ltykernel.sml`.
 *
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

functor CPSTransFn (MS : MACH_SPEC) : sig

    val translate : CPS.function -> CPS.function

  end = struct

    structure C = CPS
    structure P = C.P
    structure LV = LambdaVar

(* NOTE: this flag is always true! *)
    val unboxedfloat = MS.unboxedFloats

    (* classify the different kinds of arguments by analysing a list of types.  We
     * partition the arguments based on the possible choices for passing them.
     *
     *  KIND            DESCRIPTION           REG     RECORD   MIXED   RAW
     * --------------------------------------------------------------------
     *  uniform         pointers and mixed    GPR       Y       Y/N     N
     *  enum            tagged integers       GPR       Y       Y/Y     Y
     *  raw ints        machine integers      GPR       N       N/Y     Y
     *  floats          floating-point        FPR       N       N/Y     Y
     *)
    fun classifyArgs tys = let
          fun go ([], uArgs, eArgs, rArgs, fArgs) =
                {nUniform = uArgs, nEnum = eArgs, nRawInt = rArgs, nFloat = fArgs}
            | go (cty::ctys, uArgs, eArgs, rArgs, fArgs) = (case cty
                 of C.NUMt{tag=true, ...} =>
                      go (ctys, uArgs, eArgs+1, rArgs, fArgs)
                  | C.NUMt _ =>
                      go (ctys, uArgs, eArgs, rArgs+1, fArgs)
                  | C.PTRt _ =>
                      go (ctys, uArgs+1, eArgs, rArgs, fArgs)
                  | C.FUNt =>
                      go (ctys, uArgs+1, eArgs, rArgs, fArgs)
                  | C.FLTt _ =>
                      go (ctys, uArgs, eArgs, rArgs, fArgs+1)
                  | C.CNTt _ =>
                      go (ctys, uArgs+1, eArgs, rArgs, fArgs)
                (* end case *))
          in
            go (tys, 0, 0, 0, 0)
          end

    (* specification of a calling convention *)
    datatype cc
      = CC_FLAT                 (* all args passed directly *)
      | CC_RECORD of {          (* some args passed as a record *)
            rep : C.record_rep, (* record representation *)
            args : int list,    (* argument-list indices of the arguments that are
                                 * passed directly.
                                 *)
            flds : int list     (* argument-list indices of the arguments that are
                                 * passed in a record.
                                 *)
          }

    (* determine the calling convention.  The strategy is to prioritize GPRs for
     * pointer arguments and then for raw integers.  If we are allocating a mixed
     * or raw record, then we put enums into the raw part.
     *)
    fun callingConv tys = let
          val {nUniform, nEnum, nRawInt, nFloat} = classifyArgs tys
          (* the number of float args that exceed the available regs *)
          val nHeapFP = Int.min(0, MS.numFloatArgRegs - nFloat)
          in
            if (nUniform + nEnum + nRawInt <= MS.numArgRegs) andalso (nHeapFP = 0)
              then CC_FLAT
              else let
                (* some args are heap allocated, so we need an additional uniform
                 * argument for the record pointer.
                 *)
                val nGPR = MS.numArgRegs - 1
                 (* compute the budgets for the various kinds of GP arguments *)
                val (bU, bT, bR, anyRaw) = if (nUniform >= nGPR)
                      then (nGPR, 0, 0, nHeapFP + nRawInt > 0)
                      else let
                        val nGPR = nGPR - nUniform
                        in
                          if (nRawInt > nGPR)
                            then (nUniform, 0, nGPR, true)
                            else (nUniform, Int.min(nEnum, nGPR - nRawInt), nRawInt, nHeapFP > 0)
                        end
val () = if (bU + bT + bR > nGPR) then ErrorMsg.impossible "CPSTrans: invalid budget" else ()
                (* assign arguments to slots given budgets for each kind of variable
                 * the parameters are:
                 *   i          -- the argument index
                 *   ty::tys    -- the argument type list
                 *   bU         -- the remaining budget for uniform args
                 *   bT         -- the remaining budget for tagged integer args
                 *   bR         -- the remaining budget for raw integer args
                 *   bF         -- the remaining budget for floating-point args
                 *   args       -- argument indices of direct arguments
                 *   uFlds      -- argument indices of uniform record arguments
                 *   rFlds      -- argument indices of raw record arguments
                 *)
                fun assign (i, ty::tys, bU, bT, bR, bF, args, uFlds, rFlds) = (case ty
                       of C.NUMt{tag=true, ...} => if (bT > 0)
                              then assign (i+1, tys, bU, bT-1, bR, bF, i::args, uFlds, rFlds)
                            else if anyRaw
                              then assign (i+1, tys, bU, bT, bR, bF, args, uFlds, i::rFlds)
                              else assign (i+1, tys, bU, bT, bR, bF, args, i::uFlds, rFlds)
                        | C.NUMt _ => if (bR > 0)
                            then assign (i+1, tys, bU, bT, bR-1, bF, i::args, uFlds, rFlds)
                            else assign (i+1, tys, bU, bT, bR, bF, args, uFlds, i::rFlds)
                        | C.PTRt _ => assignPtr (i, tys, bU, bT, bR, bF, args, uFlds, rFlds)
                        | C.FUNt => assignPtr (i, tys, bU, bT, bR, bF, args, uFlds, rFlds)
                        | C.FLTt _ => if (bF > 0)
                            then assign (i+1, tys, bU, bT, bR, bF-1, i::args, uFlds, rFlds)
                            else assign (i+1, tys, bU, bT, bR, bF, args, uFlds, i::rFlds)
                        | C.CNTt _ => assignPtr (i, tys, bU, bT, bR, bF, args, uFlds, rFlds)
                      (* end case *))
                  | assign (_, [], _, _, _, _, args, uFlds, rFlds) = CC_RECORD{
                        rep = {ptrLen = length uFlds, rawLen = length rFlds},
                        args = List.rev args,
                        flds = List.revAppend(uFlds, List.rev rFlds)
                      }
                and assignPtr (i, tys, bU, bT, bR, bF, args, uFlds, rFlds) = if (bU > 0)
                      then assign (i+1, tys, bU-1, bT, bR, bF, i::args, uFlds, rFlds)
                      else assign (i+1, tys, bU, bT, bR, bF, args, i::uFlds, rFlds)
                in
                  assignPtr (0, tys, bU, bT, bR, MS.numFloatArgRegs, [], [], [])
                end
          end (* callingConv *)

    (* given a list of arguments and a list of their types, return the rewritten
     * argument list and a wrapper for the application.
     *)
    fun mkArgs (vs : C.value list, tys : C.cty list) = (case callingConv tys
           of CC_FLAT => (vs, Fn.id)
            | CC_RECORD{rep, args, flds} => let
                val argMap = Array.fromList vs
                val rp = LV.mkLvar()
                (* actual argument list; the record pointer `rp` is the last arg *)
                val args' = List.foldr
                      (fn (i, vs) => Array.sub(argMap, i) :: vs)
                      [C.VAR rp]
                      args
                (* the record fields *)
                val flds' = List.foldr
                      (fn (i, vs) => (Array.sub(argMap, i), C.OFFp 0) :: vs)
                      []
                      args
                val rk = (case rep
                       of {ptrLen, rawLen=0} => C.RK_RECORD
                        | {ptrLen=0, rawLen} => C.RK_RAWBLOCK
                        | _ => C.RK_MIXED rep
                      (* end case *))
                in
                  (args', fn e => C.RECORD(rk, flds', rp, e))
                end
          (* end case *))

    (* given a list of parameters and a list of their types, return the rewritten
     * parameter list, the corresponding list of types, and a wrapper for the
     * function's body.
     *)
    fun mkParams (xs : LV.lvar list, tys : C.cty list) = (case callingConv tys
           of CC_FLAT => (xs, tys, Fn.id)
            | CC_RECORD{rep, args, flds} => let
                val paramMap = Array.fromList(ListPair.zipEq(xs, tys))
                val rp = LV.mkLvar()
                (* actual paramter list; the record pointer `rp` is the last param *)
                val (xs', tys') = List.foldr
                      (fn (i, (xs, tys)) => let
                          val (x, ty) = Array.sub(paramMap, i)
                          in (x::xs, ty::tys) end)
                      ([rp], [C.PTRt(C.RPT rep)])
                      args
                fun hdr e = let
                      fun wrap (_, []) = e
                        | wrap (i, idx::idxs) = let
                            val (x, ty) = Array.sub(paramMap, idx)
                            in
                              C.SELECT(i, C.VAR rp, x, ty, wrap (i+1, idxs))
                            end
                      in
                        wrap (0, flds)
                      end
                in
                  (xs', tys', hdr)
                end
          (* end case *))

    (* the main function: rewrite a CPS function *)
    fun translate func = let
        (* variable substitution table *)
	  val substM : C.value LV.Tbl.hash_table = LV.Tbl.mkTable(32, Fail "subst map")
	  val addvl = LV.Tbl.insert substM
          val findvl = LV.Tbl.find substM
	  fun mapvl x = (case findvl x of SOME v => v | _ => C.VAR x)
        (* variable to type hash*)
	  val ctyM : C.cty LV.Tbl.hash_table = LV.Tbl.mkTable(32, Fail "CType map")
	  val addty = LV.Tbl.insert ctyM
	  val getty = LV.Tbl.lookup ctyM
          val findty = LV.Tbl.find ctyM
	  fun grabty (C.VAR x) = (case findty x of SOME t => t | _ => C.ptrTy)
	    | grabty (C.NUM{ty, ...}) = C.NUMt ty
	    | grabty (C.REAL{ty, ...}) = C.FLTt ty
	    | grabty _ = C.ptrTy
	  fun rewrite ce = (case ce
		 of C.RECORD(k, vl, w, ce) => C.RECORD(k, map rectrans vl, w, rewrite ce)
		  | C.SELECT(i, v, w, t, ce) => let
		      val _ = addty(w, t)
		      val v' = vtrans v
		      val ce' = rewrite ce
		      in
			C.SELECT(i, v', w, getty w, ce')
		      end
		  | C.OFFSET(i, v, w, ce) => C.OFFSET(i, vtrans v, w, rewrite ce)
		  | C.APP(v, vl) => let
                      val (nvl, hdr) = mkArgs (List.map vtrans vl, List.map grabty vl)
                      in
		        hdr (C.APP (vtrans v, nvl))
		      end
		  | C.FIX(l, ce) => C.FIX(map rewriteFun l, rewrite ce)
		  | C.SWITCH(v, c, l) => C.SWITCH(vtrans v,c,map rewrite l)
		  | C.LOOKER(p, vl, w, t, ce) => let
		      val _ = addty(w, t)
		      val vl' = map vtrans vl
		      val ce' = rewrite ce
		      in
			C.LOOKER(p, vl', w, getty w, ce')
		      end
		  | C.SETTER(p, vl, ce) => C.SETTER(p, map vtrans vl, rewrite ce)
		  | C.ARITH(p, vl, w, t, cd) => (
		      addty(w, t);
		      C.ARITH(p, map vtrans vl, w, t, rewrite ce))
		  | C.RCC(k, l, p, vl, wtl, ce) => (
		      List.app addty wtl;
		      C.RCC(k, l, p, map vtrans vl, wtl, rewrite ce))
		  | C.PURE(P.BOX, [u], w, t, ce) => (addvl(w,vtrans u); rewrite ce)
		  | C.PURE(P.UNBOX, [u], w, t, ce) => (
		      case u of C.VAR z => addty(z, t) | _ => ();
		      addvl(w, vtrans u); rewrite ce)
		  | C.PURE(p as P.WRAP(P.INT sz), [u], w, t, ce) =>
		      if (sz <= Target.defaultIntSz)
			then (  (* remove wrapping of tagged ints *)
			  addvl(w, vtrans u);
			  rewrite ce)
			else (
			  addty(w,t);
			  C.PURE(p, [vtrans u], w, t, rewrite ce))
		  | C.PURE(p as P.UNWRAP(P.INT sz), [u], w, t, ce) =>
		      if (sz <= Target.defaultIntSz)
			then (  (* remove unwrapping of tagged ints *)
			  addvl(w,vtrans u);
			  rewrite ce)
			else (
			  addty(w,t);
			  C.PURE(p, [vtrans u], w, t, rewrite ce))
		  | C.PURE(p as P.WRAP(P.FLOAT _), [u], w, t, ce) =>
		      if unboxedfloat
			then (addty(w,t); C.PURE(p, [vtrans u], w, t, rewrite ce))
			else (addvl(w,vtrans u); rewrite ce)
		  | C.PURE(p as P.UNWRAP(P.FLOAT _), [u], w, t, ce) =>
		      if unboxedfloat
			then (addty(w,t); C.PURE(p, [vtrans u], w, t, rewrite ce))
			else (addvl(w,vtrans u); rewrite ce)
		  | C.PURE(P.GETCON,[u], w, t, cd) => (
		      addty (w, t);
		      C.SELECT(0,vtrans u,w,t,rewrite ce))
		  | C.PURE(P.GETEXN,[u], w, t, cd) => (
		      addty (w, t);
		      C.SELECT(0,vtrans u,w,t,rewrite ce))
		  | C.PURE(p,vl, w, t, cd) => let
		      val _ = addty(w,t)
		      val vl' = map vtrans vl
		      val ce' = rewrite ce
		      in
			C.PURE(p, vl', w, getty w, ce')
		      end
		  | C.BRANCH(p,vl,c,e1,e2) =>
		      C.BRANCH(p, map vtrans vl, c, rewrite e1, rewrite e2)
		(* end case *))

	  and rewriteFun (fk, v, xs, ctys, ce) = let
		val _ = ListPair.app addty (xs, ctys)
		val ce' = rewrite ce
                val (xs', ctys', fhdr) = mkParams (xs, ctys)
		in
		  (fk, v, xs', ctys', fhdr ce')
		end

	  and rectrans (v, acp) = (vtrans v, acp)

	  and vtrans (C.VAR v) = (mapvl v)
            | vtrans u = u

          in
	    rewriteFun func
	  end (* translate *)

  end (* CPSTransFn *)
