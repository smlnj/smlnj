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
     *  COUNTER         DESCRIPTION           REG     RECORD   MIXED   RAW
     * --------------------------------------------------------------------
     *  uArgs           pointers              GPR       Y       Y/N     N
     *  tArgs           tagged integers       GPR       Y       Y/Y     Y
     *  rArgs           machine integers      GPR       N       N/Y     Y
     *  fArgs           floating-point        FPR       N       N/Y     Y
     *)
    fun classifyArgs tys = let
          fun go ([], uArgs, tArgs, rArgs, fArgs) =
                {nUniform = uArgs, nTagged = tArgs, nRawInt = rArgs, nFloat = fArgs}
            | go (cty::ctys, uArgs, tArgs, rArgs, fArgs) = (case cty
                 of C.NUMt{tag=true, ...} =>
                      go (ctys, uArgs, tArgs+1, rArgs, fArgs)
                  | C.NUMt _ =>
                      go (ctys, uArgs, tArgs, rArgs+1, fArgs)
                  | C.ENUMt =>
                      go (ctys, uArgs, tArgs+1, rArgs, fArgs)
                  | C.PTRt _ =>
                      go (ctys, uArgs+1, tArgs, rArgs, fArgs)
                  | C.FUNt =>
                      go (ctys, uArgs+1, tArgs, rArgs, fArgs)
                  | C.FLTt _ =>
                      go (ctys, uArgs, tArgs, rArgs, fArgs+1)
                  | C.CNTt _ =>
                      go (ctys, uArgs+1, tArgs, rArgs, fArgs)
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
          val {nUniform, nTagged, nRawInt, nFloat} = classifyArgs tys
(*DEBUG*)val nArgs = nUniform + nTagged + nRawInt + nFloat
          (* the number of float args that exceed the available regs *)
          val nHeapFP = Int.min(0, MS.numFloatArgRegs - nFloat)
          in
            if (nUniform + nTagged + nRawInt <= MS.numArgRegs) andalso (nHeapFP = 0)
              then CC_FLAT
              else let
                (* some args are heap allocated, so we need an additional uniform
                 * argument for the record pointer.
                 *)
                val nGPR = MS.numArgRegs - 1
                (* compute the budgets for the various kinds of GP arguments. The
                 * flag `anyRaw` is true when we are going to use heap storage for
                 * raw values (ints or floats).
                 *)
                val (bU, bT, bR, anyRaw) = if (nUniform >= nGPR)
                      then (nGPR, 0, 0, nHeapFP + nRawInt > 0)
                      else let
                        val nGPR = nGPR - nUniform
                        in
                          if (nRawInt > nGPR)
                            then (nUniform, 0, nGPR, true)
                            else (nUniform, Int.min(nTagged, nGPR - nRawInt), nRawInt, nHeapFP > 0)
                        end
val () = if (bU + bT + bR > nGPR) then ErrorMsg.impossible "CPSTrans: invalid budget" else ()
                val bF = Int.min (MS.numFloatArgRegs, nFloat)
                (* assign arguments to slots given budgets for each kind of variable
                 * the parameters are:
                 *   i          -- the argument index
                 *   ty::tys    -- the argument type list
                 *   bU         -- the remaining budget for uniform args
                 *   bT         -- the remaining budget for tagged-integer args
                 *   bR         -- the remaining budget for raw-integer args
                 *   bF         -- the remaining budget for floating-point args
                 *   args       -- argument indices of direct arguments
                 *   uFlds      -- argument indices of uniform record arguments
                 *   rFlds      -- argument indices of raw record arguments
                 *)
                fun assign (i, ty::tys, bU, bT, bR, bF, args, uFlds, rFlds) = (case ty
                       of C.NUMt{tag=true, ...} =>
                            assignTagged (i, tys, bU, bT, bR, bF, args, uFlds, rFlds)
                        | C.NUMt _ => if (bR > 0)
                            then assign (i+1, tys, bU, bT, bR-1, bF, i::args, uFlds, rFlds)
                            else assign (i+1, tys, bU, bT, bR, bF, args, uFlds, i::rFlds)
                        | C.ENUMt =>
                            assignTagged (i, tys, bU, bT, bR, bF, args, uFlds, rFlds)
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
                (* assign a tagged int/enum argument.  If the budget (`bT`) has been
                 * exceeded, we assign the argument to raw storage (if present).
                 *)
                and assignTagged (i, tys, bU, bT, bR, bF, args, uFlds, rFlds) =
                      if (bT > 0)
                        then assign (i+1, tys, bU, bT-1, bR, bF, i::args, uFlds, rFlds)
                      else if anyRaw
                        then assign (i+1, tys, bU, bT, bR, bF, args, uFlds, i::rFlds)
                        else assign (i+1, tys, bU, bT, bR, bF, args, i::uFlds, rFlds)
                (* assign a pointer argument *)
                and assignPtr (i, tys, bU, bT, bR, bF, args, uFlds, rFlds) = if (bU > 0)
                      then assign (i+1, tys, bU-1, bT, bR, bF, i::args, uFlds, rFlds)
                      else assign (i+1, tys, bU, bT, bR, bF, args, i::uFlds, rFlds)
                in
                  assign (0, tys, bU, bT, bR, bF, [], [], [])
                end
          end (* callingConv *)

    (* given a list of arguments and a list of their types, return the rewritten
     * argument list and a wrapper for the application.
     *)
    fun mkArgs (vs : C.value list, tys : C.cty list) = (case callingConv tys
           of CC_FLAT => (vs, Fn.id)
            | CC_RECORD{rep, args, flds} => let
                val argMap = Vector.fromList vs
(*DEBUG*)
val () = (
    print(concat[
        "# mkArgs: (",
        String.concatWithMap ","
          (fn (v, ty) => concat[PPCps.value2str v, ":", CPSUtil.ctyToString ty])
          (ListPair.zip (vs, tys)),
        ")\n"
      ]);
    print(concat[
        "## cc = RECORD<", Int.toString(#ptrLen rep), ":", Int.toString(#rawLen rep),
        ">{args = [", String.concatWithMap "," Int.toString args,
        "], flds = [", String.concatWithMap "," Int.toString flds, "]}\n"
      ]))
(*DEBUG*)
                val rp = LV.mkLvar()
(*
fun sub (argMap, i) = Vector.sub(argMap, i)
handle Subscript => let
val i2s = Int.toString
val {nUniform, nTagged, nRawInt, nFloat} = classifyArgs tys
in
print(concat["## argMap[", i2s i, "] out of bounds\n"]);
print(concat["## vs = [",
String.concatWithMap ","
  (fn (v, ty) => concat[PPCps.value2str v, ":", CPSUtil.ctyToString ty])
  (ListPair.zip (vs, tys)), "]\n"]);
print(concat["## nUniform = ", i2s nUniform, ", nTagged = ", i2s nTagged, ", nRawInt = ",
i2s nRawInt, ", nFloat = ", i2s nFloat, ", #regs = ", i2s MS.numArgRegs, "\n"]);
print(concat["## cc = RECORD{args = [", String.concatWithMap "," i2s args,
"], flds = [", String.concatWithMap "," i2s flds, "]}\n"]);
raise Subscript
end
*)
                (* actual argument list; the record pointer `rp` is the last arg *)
                val args' = List.foldr
                      (fn (i, vs) => Vector.sub(argMap, i) :: vs)
                      [C.VAR rp]
                      args
                (* the record fields *)
                val flds' = List.foldr
                      (fn (i, vs) => (Vector.sub(argMap, i), C.OFFp 0) :: vs)
                      []
                      flds
                val rk = (case rep
                       of {ptrLen, rawLen=0} => C.RK_RECORD
                        | {ptrLen=0, rawLen} => C.RK_RAWBLOCK
                        | _ => C.RK_MIXED rep
                      (* end case *))
                in
                  (args', fn e => C.RECORD(rk, flds', rp, e))
                end
          (* end case *))
handle ex => (print "## exception in mkArgs\n"; raise ex)

    (* given a list of parameters and a list of their types, return the rewritten
     * parameter list, the corresponding list of types, and a wrapper for the
     * function's body.
     *)
    fun mkParams (xs : LV.lvar list, tys : C.cty list) = (case callingConv tys
           of CC_FLAT => (xs, tys, Fn.id)
            | CC_RECORD{rep, args, flds} => let
                val paramMap = Vector.fromList(ListPair.zipEq(xs, tys))
(*DEBUG*)
val () = (
    print(concat[
        "# mkParams: (",
        String.concatWithMap ","
          (fn (x, ty) => concat[LV.lvarName x, ":", CPSUtil.ctyToString ty])
          (ListPair.zip (xs, tys)),
        ")\n"
      ]);
    print(concat[
        "## cc = RECORD<", Int.toString(#ptrLen rep), ":", Int.toString(#rawLen rep),
        ">{args = [", String.concatWithMap "," Int.toString args,
        "], flds = [", String.concatWithMap "," Int.toString flds, "]}\n"
      ]))
(*DEBUG*)
                val rp = LV.mkLvar()
                (* actual paramter list; the record pointer `rp` is the last param *)
                val (xs', tys') = List.foldr
                      (fn (i, (xs, tys)) => let
                          val (x, ty) = Vector.sub(paramMap, i)
                          in (x::xs, ty::tys) end)
                      ([rp], [C.PTRt(C.RPT rep)])
                      args
                (* header code for extracting the extra paramters *)
                fun hdr e = let
                      fun wrap (_, []) = e
                        | wrap (i, idx::idxs) = let
                            val (x, ty) = Vector.sub(paramMap, idx)
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
handle ex => (print "## exception in mkParams\n"; raise ex)

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
          (* if a parameter-type list has continuation types, then we may need to
           * change them to reflect changes in their calling conventions.
           *)
          fun rewriteTys tys = if List.exists (fn (C.CNTt _) => true | _ => false) tys
                then let
                  fun rewrite (cty as C.CNTt tys) = (case callingConv tys
                         of CC_FLAT => cty
                          | CC_RECORD{rep, args, flds} => let
                              (* project out the types of the arguments that are passed
                               * as "registers".  The correctness of this code relies
                               * on the fact that the `args` list is in increasing
                               * order.
                               *)
                              fun proj (_, _, [], tys') = rev(C.PTRt(C.RPT rep) :: tys')
                                | proj (i, ty::tys, ix::ixs, tys') =
                                    if (i = ix)
                                      then proj(i+1, tys, ixs, ty::tys')
                                    else if (i < ix)
                                      then proj(i+1, tys, ix::ixs, tys')
                                      else raise Fail "impossible"
                                | proj _ = raise Fail "arity mismatch"
                              in
                                C.CNTt(proj(0, tys, args, []))
                              end
                        (* end case *))
                    | rewrite cty = cty
                  in
                    List.map rewrite tys
                  end
                else tys
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
		        hdr (C.APP(vtrans v, nvl))
		      end
		  | C.FIX(l, ce) => C.FIX(map rewriteFun l, rewrite ce)
		  | C.SWITCH(v, c, l) => C.SWITCH(vtrans v, c, map rewrite l)
		  | C.LOOKER(p, vl, w, t, ce) => let
		      val _ = addty(w, t)
		      val vl' = map vtrans vl
		      in
			C.LOOKER(p, vl', w, getty w, rewrite ce)
		      end
		  | C.SETTER(p, vl, ce) => C.SETTER(p, map vtrans vl, rewrite ce)
		  | C.ARITH(p, vl, w, t, ce) => (
		      addty(w, t);
		      C.ARITH(p, map vtrans vl, w, t, rewrite ce))
		  | C.RCC(k, l, p, vl, wtl, ce) => (
		      List.app addty wtl;
		      C.RCC(k, l, p, map vtrans vl, wtl, rewrite ce))
		  | C.PURE(P.BOX, [u], w, t, ce) => (addvl(w, vtrans u); rewrite ce)
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
			then (addty(w ,t); C.PURE(p, [vtrans u], w, t, rewrite ce))
			else (addvl(w, vtrans u); rewrite ce)
		  | C.PURE(P.GETCON, [u], w, t, ce) => (
		      addty (w, t);
		      C.SELECT(0,vtrans u, w, t, rewrite ce))
		  | C.PURE(P.GETEXN, [u], w, t, ce) => (
		      addty (w, t);
		      C.SELECT(0, vtrans u, w, t, rewrite ce))
		  | C.PURE(p, vl, w, t, ce) => let
		      val _ = addty(w, t)
		      val vl' = map vtrans vl
		      in
			C.PURE(p, vl', w, getty w, rewrite ce)
		      end
		  | C.BRANCH(p, vl, c, e1, e2) =>
		      C.BRANCH(p, map vtrans vl, c, rewrite e1, rewrite e2)
		(* end case *))

	  and rewriteFun (fk, v, xs, ctys, ce) = let
                val ctys = rewriteTys ctys
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
