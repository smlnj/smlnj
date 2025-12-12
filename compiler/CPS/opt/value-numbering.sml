(* value-numbering.sml
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *
 * Redundant-computation elimination using value numbering.
 *)

structure ValueNumbering : sig

    val transform : {
            function : CPS.function,
            click : string -> unit
          } -> CPS.function

  end = struct

    structure LV = LambdaVar
    structure C = CPS
    structure P = CPS.P
    structure Ctl = Control.CG

    datatype avail_value
      = RECORD of C.record_kind * (C.value * C.accesspath) list
      | SELECT of int * C.value
      | ARITH of P.arith * C.value list
      | PURE of P.pure * C.value list

(* QUESTION: experiments suggest that redundant `SWITCH` expressions never actually
 * happen (e.g., there are none in the 112k lines of SML code in the compiler).
 *)
    datatype branch
      = SWITCH of C.value * int
      | BRANCH of P.branch * C.value list * bool

    fun bug s = ErrorMsg.impossible ("ValueNumbering: " ^ s)

    (***** operator comparisons *****)

    local
      fun cmpCode (c1 : int, c2) = if (c1 < c2) then LESS
            else if (c1 > c2) then GREATER
            else EQUAL

      fun cmpList cmp = let
            fun cmpL ([], []) = EQUAL
              | cmpL ([], _) = LESS
              | cmpL (_, []) = GREATER
              | cmpL (x::xs, y::ys) = (case cmp(x, y)
                   of EQUAL => cmpL(xs, ys)
                    | order => order
                  (* end case *))
            in
              cmpL
            end

      (* composing comparisons *)
      fun ?=> (EQUAL, f) = f()
        | ?=> (order, _) = order

      infix ?=>
    in
    fun cmpNumKind (P.INT n1, P.INT n2) = cmpCode(n1, n2)
      | cmpNumKind (P.INT _, _) = LESS
      | cmpNumKind (_, P.INT _) = GREATER
      | cmpNumKind (P.UINT n1, P.UINT n2) = cmpCode(n1, n2)
      | cmpNumKind (P.UINT _, _) = LESS
      | cmpNumKind (_, P.UINT _) = GREATER
      | cmpNumKind (P.FLOAT n1, P.FLOAT n2) = cmpCode(n1, n2)

    fun cmpRecordKind (rk1, rk2) = let
          fun toCode C.RK_VECTOR = 0
            | toCode C.RK_RECORD = 1
            | toCode C.RK_ESCAPE = 2
            | toCode C.RK_CONT = 3
            | toCode C.RK_FCONT = 4
            | toCode C.RK_KNOWN = 5
            | toCode C.RK_RAW64BLOCK = 6
            | toCode C.RK_RAWBLOCK = 7
          in
            cmpCode (toCode rk1, toCode rk2)
          end

    fun cmpArithOp (op1, op2) = let
          fun toCode P.IADD = 0
            | toCode P.ISUB = 1
            | toCode P.IMUL = 2
            | toCode P.IDIV = 3
            | toCode P.IMOD = 4
            | toCode P.IQUOT = 5
            | toCode P.IREM = 6
            | toCode P.INEG = 7
          in
            cmpCode(toCode op1, toCode op2)
          end

    fun cmpArith (P.IARITH{oper=op1, sz=n1}, P.IARITH{oper=op2, sz=n2}) =
          cmpArithOp(op1, op2) ?=> (fn () => cmpCode(n1, n2))
      | cmpArith (P.IARITH _, _) = LESS
      | cmpArith (_, P.IARITH _) = GREATER
      | cmpArith (P.TEST{from=f1, to=t1}, P.TEST{from=f2, to=t2}) =
          cmpCode(f1, f2) ?=> (fn () => cmpCode(t1, t2))
      | cmpArith (P.TEST _, _) = LESS
      | cmpArith (_, P.TEST _) = GREATER
      | cmpArith (P.TESTU{from=f1, to=t1}, P.TESTU{from=f2, to=t2}) =
          cmpCode(f1, f2) ?=> (fn () => cmpCode(t1, t2))
      | cmpArith (P.TESTU _, _) = LESS
      | cmpArith (_, P.TESTU _) = GREATER
      | cmpArith (P.TEST_INF sz1, P.TEST_INF sz2) =
          cmpCode(sz1, sz2)
      | cmpArith (P.TEST_INF _, _) = LESS
      | cmpArith (_, P.TEST_INF _) = GREATER
      | cmpArith (
          P.REAL_TO_INT{floor=b1, from=f1, to=t1},
          P.REAL_TO_INT{floor=b2, from=f2, to=t2}
        ) = if (b1 = b2)
            then cmpCode(f1, f2) ?=> (fn () => cmpCode(t1, t2))
            else if b2 then LESS
            else GREATER

    fun cmpPureOp (op1, op2) = let
          fun toCode P.ADD = 0
            | toCode P.SUB = 1
            | toCode P.MUL = 2
            | toCode P.QUOT = 3
            | toCode P.REM = 4
            | toCode P.NEG = 5
            | toCode P.LSHIFT = 6
            | toCode P.RSHIFT = 7
            | toCode P.RSHIFTL = 8
            | toCode P.ORB = 9
            | toCode P.XORB = 10
            | toCode P.ANDB = 11
            | toCode P.NOTB = 12
(*
            | toCode P.CNTPOP = 13
            | toCode P.CNTLZ  = 14
            | toCode P.CNTTZ = 15
            | toCode P.ROTL = 16
            | toCode P.ROTR = 17
            | toCode P.FDIV = 18
            | toCode P.FABS = 19
            | toCode P.FSQRT = 20
*)
            | toCode P.FDIV = 13
            | toCode P.FABS = 14
            | toCode P.FSQRT = 15
          in
            cmpCode(toCode op1, toCode op2)
          end

    fun cmpPure (P.PURE_ARITH{oper=op1, kind=k1}, P.PURE_ARITH{oper=op2, kind=k2}) =
          cmpPureOp(op1, op2) ?=> (fn () => cmpNumKind(k1, k2))
      | cmpPure (P.PURE_ARITH _, _) = LESS
      | cmpPure (_, P.PURE_ARITH _) = GREATER
      | cmpPure (P.PURE_NUMSUBSCRIPT{kind=k1}, P.PURE_NUMSUBSCRIPT{kind=k2}) =
          cmpNumKind(k1, k2)
      | cmpPure (P.PURE_NUMSUBSCRIPT _, _) = LESS
      | cmpPure (_, P.PURE_NUMSUBSCRIPT _) = GREATER
      | cmpPure (P.LENGTH, P.LENGTH) = EQUAL
      | cmpPure (P.LENGTH, _) = LESS
      | cmpPure (_, P.LENGTH) = GREATER
      | cmpPure (P.OBJLENGTH, P.OBJLENGTH) = EQUAL
      | cmpPure (P.OBJLENGTH, _) = LESS
      | cmpPure (_, P.OBJLENGTH) = GREATER
      | cmpPure (P.MAKEREF, P.MAKEREF) = EQUAL
      | cmpPure (P.MAKEREF, _) = LESS
      | cmpPure (_, P.MAKEREF) = GREATER
      | cmpPure (P.COPY{from=f1, to=t1}, P.COPY{from=f2, to=t2}) =
          cmpCode(f1, f2) ?=> (fn () => cmpCode(t1, t2))
      | cmpPure (P.COPY _, _) = LESS
      | cmpPure (_, P.COPY _) = GREATER
      | cmpPure (P.EXTEND{from=f1, to=t1}, P.EXTEND{from=f2, to=t2}) =
          cmpCode(f1, f2) ?=> (fn () => cmpCode(t1, t2))
      | cmpPure (P.EXTEND _, _) = LESS
      | cmpPure (_, P.EXTEND _) = GREATER
      | cmpPure (P.TRUNC{from=f1, to=t1}, P.TRUNC{from=f2, to=t2}) =
          cmpCode(f1, f2) ?=> (fn () => cmpCode(t1, t2))
      | cmpPure (P.TRUNC _, _) = LESS
      | cmpPure (_, P.TRUNC _) = GREATER
      | cmpPure (P.COPY_INF n1, P.COPY_INF n2) =
          cmpCode(n1, n2)
      | cmpPure (P.COPY_INF _, _) = LESS
      | cmpPure (_, P.COPY_INF _) = GREATER
      | cmpPure (P.EXTEND_INF n1, P.EXTEND_INF n2) =
          cmpCode(n1, n2)
      | cmpPure (P.EXTEND_INF _, _) = LESS
      | cmpPure (_, P.EXTEND_INF _) = GREATER
      | cmpPure (P.TRUNC_INF n1, P.TRUNC_INF n2) =
          cmpCode(n1, n2)
      | cmpPure (P.TRUNC_INF _, _) = LESS
      | cmpPure (_, P.TRUNC_INF _) = GREATER
      | cmpPure (P.INT_TO_REAL{from=f1, to=t1}, P.INT_TO_REAL{from=f2, to=t2}) =
          cmpCode(f1, f2) ?=> (fn () => cmpCode(t1, t2))
      | cmpPure (P.INT_TO_REAL _, _) = LESS
      | cmpPure (_, P.INT_TO_REAL _) = GREATER
      | cmpPure (P.BITS_TO_REAL sz1, P.BITS_TO_REAL sz2) = cmpCode(sz1, sz2)
      | cmpPure (P.BITS_TO_REAL _, _) = LESS
      | cmpPure (_, P.BITS_TO_REAL _) = GREATER
      | cmpPure (P.REAL_TO_BITS sz1, P.REAL_TO_BITS sz2) = cmpCode(sz1, sz2)
      | cmpPure (P.REAL_TO_BITS _, _) = LESS
      | cmpPure (_, P.REAL_TO_BITS _) = GREATER
      | cmpPure (P.SUBSCRIPTV, P.SUBSCRIPTV) = EQUAL
      | cmpPure (P.SUBSCRIPTV, _) = LESS
      | cmpPure (_, P.SUBSCRIPTV) = GREATER
      | cmpPure (P.GETTAG, P.GETTAG) = EQUAL
      | cmpPure (P.GETTAG, _) = LESS
      | cmpPure (_, P.GETTAG) = GREATER
      | cmpPure (P.MKSPECIAL, P.MKSPECIAL) = EQUAL
      | cmpPure (P.MKSPECIAL, _) = LESS
      | cmpPure (_, P.MKSPECIAL) = GREATER
      | cmpPure (P.CAST, P.CAST) = EQUAL
      | cmpPure (P.CAST, _) = LESS
      | cmpPure (_, P.CAST) = GREATER
      | cmpPure (P.GETCON, P.GETCON) = EQUAL
      | cmpPure (P.GETCON, _) = LESS
      | cmpPure (_, P.GETCON) = GREATER
      | cmpPure (P.GETEXN, P.GETEXN) = EQUAL
      | cmpPure (P.GETEXN, _) = LESS
      | cmpPure (_, P.GETEXN) = GREATER
      | cmpPure (P.BOX, P.BOX) = EQUAL
      | cmpPure (P.BOX, _) = LESS
      | cmpPure (_, P.BOX) = GREATER
      | cmpPure (P.UNBOX, P.UNBOX) = EQUAL
      | cmpPure (P.UNBOX, _) = LESS
      | cmpPure (_, P.UNBOX) = GREATER
      | cmpPure (P.WRAP k1, P.WRAP k2) =
          cmpNumKind(k1, k2)
      | cmpPure (P.WRAP _, _) = LESS
      | cmpPure (_, P.WRAP _) = GREATER
      | cmpPure (P.UNWRAP k1, P.UNWRAP k2) =
          cmpNumKind(k1, k2)
      | cmpPure (P.UNWRAP _, _) = LESS
      | cmpPure (_, P.UNWRAP _) = GREATER
      | cmpPure (P.GETSEQDATA, P.GETSEQDATA) = EQUAL
      | cmpPure (P.GETSEQDATA, _) = LESS
      | cmpPure (_, P.GETSEQDATA) = GREATER
      | cmpPure (P.RECSUBSCRIPT, P.RECSUBSCRIPT) = EQUAL
      | cmpPure (P.RECSUBSCRIPT, _) = LESS
      | cmpPure (_, P.RECSUBSCRIPT) = GREATER
      | cmpPure (P.RAW64SUBSCRIPT, P.RAW64SUBSCRIPT) = EQUAL
      | cmpPure (P.RAW64SUBSCRIPT, _) = LESS
      | cmpPure (_, P.RAW64SUBSCRIPT) = GREATER
      | cmpPure (P.NEWARRAY0, P.NEWARRAY0) = EQUAL
      | cmpPure (P.NEWARRAY0, _) = LESS
      | cmpPure (_, P.NEWARRAY0) = GREATER
      | cmpPure (P.RAWRECORD NONE, P.RAWRECORD NONE) = EQUAL
      | cmpPure (P.RAWRECORD NONE, P.RAWRECORD _) = LESS
      | cmpPure (P.RAWRECORD _, P.RAWRECORD NONE) = GREATER
      | cmpPure (P.RAWRECORD(SOME rk1), P.RAWRECORD(SOME rk2)) =
          cmpRecordKind (rk1, rk2)

    (* ordering for CPS.value type *)
    fun cmpValue (C.VAR x1, C.VAR x2) = LV.compare(x1, x2)
      | cmpValue (C.VAR _, _) = LESS
      | cmpValue (_, C.VAR _) = GREATER
      | cmpValue (C.NUM{ival=i1, ty=ty1}, C.NUM{ival=i2, ty=ty2}) = let
(* QUESTION: can we ignore the tag field, since it is implied by the size? *)
          fun cmpTy ({sz=sz1, tag=t1}, {sz=sz2, tag=t2}) =
                if (t1 = t2) then cmpCode (sz1, sz2)
                else if t1 then GREATER
                else LESS
          in
            IntInf.compare(i1, i2) ?=> (fn () => cmpTy(ty1, ty2))
          end
      | cmpValue (C.NUM _, _) = LESS
      | cmpValue (_, C.NUM _) = GREATER
      | cmpValue (C.REAL{rval=r1, ty=n1}, C.REAL{rval=r2, ty=n2}) =
          RealLit.compare(r1, r2) ?=> (fn () => cmpCode(n1, n2))
      | cmpValue (C.REAL _, _) = LESS
      | cmpValue (_, C.REAL _) = GREATER
      | cmpValue (C.STRING s1, C.STRING s2) = String.compare(s1, s2)
      | cmpValue (C.STRING _, _) = LESS
      | cmpValue (_, C.STRING _) = GREATER
      | cmpValue _ = bug "unexpected VOID or LABEL"

    (* ordering for lists of CPS.values *)
    val cmpValues = cmpList cmpValue

    fun sameValue (C.VAR x1, C.VAR x2) = (x1 = x2)
      | sameValue (C.LABEL l1, C.LABEL l2) = (l1 = l2)
      | sameValue (C.NUM{ival=i1, ty=ty1}, C.NUM{ival=i2, ty=ty2}) =
(* QUESTION: can we ignore the tag, since it is implied by the size? *)
          (#sz ty1 = #sz ty2) andalso (i1 = i2) andalso (#tag ty1 = #tag ty2)
      | sameValue (C.REAL{rval=r1, ty=n1}, C.REAL{rval=r2, ty=n2}) =
          (n1 = n2) andalso RealLit.same(r1, r2)
      | sameValue (C.STRING s1, C.STRING s2) = (s1 = s2)
      | sameValue _ = false

    (* ordering for avail_value type *)
    fun cmpAvailVal (RECORD(rk1, flds1), RECORD(rk2, flds2)) = let
          fun cmpAccess (C.OFFp n1, C.OFFp n2) = cmpCode(n1, n2)
            | cmpAccess (C.OFFp _, _) = LESS
            | cmpAccess (_, C.OFFp _) = GREATER
            | cmpAccess (C.SELp(n1, ap1), C.SELp(n2, ap2)) =
                cmpCode(n1, n2) ?=> (fn () => cmpAccess(ap1, ap2))
          fun cmpField ((v1, ap1), (v2, ap2)) =
                cmpValue(v1, v2) ?=> (fn () => cmpAccess(ap1, ap2))
          in
            cmpRecordKind(rk1, rk2) ?=> (fn () => cmpList cmpField (flds1, flds2))
          end
      | cmpAvailVal (RECORD _, _) = LESS
      | cmpAvailVal (_, RECORD _) = GREATER
      | cmpAvailVal(SELECT(i1, v1), SELECT(i2, v2)) =
          cmpCode(i1, i2) ?=> (fn () => cmpValue(v1, v2))
      | cmpAvailVal (SELECT _, _) = LESS
      | cmpAvailVal (_, SELECT _) = GREATER
      | cmpAvailVal(ARITH(op1, vs1), ARITH(op2, vs2)) =
          cmpArith(op1, op2) ?=> (fn () => cmpValues(vs1, vs2))
      | cmpAvailVal (ARITH _, _) = LESS
      | cmpAvailVal (_, ARITH _) = GREATER
      | cmpAvailVal(PURE(op1, vs1), PURE(op2, vs2)) =
          cmpPure(op1, op2) ?=> (fn () => cmpValues(vs1, vs2))

    (* search the branch history for a matching switch; if found, return `SOME n`
     * where `n` is the index of the switch case.
     *)
    fun matchSwitch (bh, C.VAR x) = let
          fun match [] = NONE
            | match (SWITCH(C.VAR y, n)::bh) = if LV.same(x, y)
                then SOME n
                else match bh
            | match (_::bh) = match bh
          in
            match bh
          end
      | matchSwitch _ = NONE

    (* equality on branches *)
    fun sameBranch (op1, op2) = let
          fun sameNumKind (P.INT n1, P.INT n2) = (n1 = n2)
            | sameNumKind (P.UINT n1, P.UINT n2) = (n1 = n2)
            | sameNumKind (P.FLOAT n1, P.FLOAT n2) = (n1 = n2)
            | sameNumKind _ = false
          in
            case (op1, op2)
             of (P.CMP{oper=cmp1, kind=k1}, P.CMP{oper=cmp2, kind=k2}) =>
                  (cmp1 = cmp2) andalso sameNumKind(k1, k2)
              | (P.FCMP{oper=cmp1, size=n1}, P.FCMP{oper=cmp2, size=n2}) =>
                  (cmp1 = cmp2) andalso (n1 = n2)
              | (P.FSGN n1, P.FSGN n2) => (n1 = n2)
              | (P.BOXED, P.BOXED) => true
              | (P.UNBOXED, P.UNBOXED) => true
              | (P.PEQL, P.PEQL) => true
              | (P.PNEQ, P.PNEQ) => true
              | (P.STREQL s1, P.STREQL s2) => (s1 = s2)
              | _ => false
            (* end case *)
          end

    (* search the branch history for a matching branch; if found, return `SOME b`
     * where `b` is the direction of the branch.
     *)
    fun matchBranch (bh, tst, vs) = let
(* TODO: generalize this test for the negation of the branch using `CPSUtil.opp` *)
          fun match [] = NONE
            | match (BRANCH(tst', vs', b)::bh) =
                if sameBranch(tst, tst')
                andalso ListPair.allEq sameValue (vs, vs')
                  then SOME b
                  else match bh
            | match (_::bh) = match bh
          in
            match bh
          end

    end (* local *)

    (* statistics *)
    val cntRedundantRecord = Stats.newCounter[]
    val cntRedundantSelect = Stats.newCounter[]
    val cntRedundantArith = Stats.newCounter[]
    val cntRedundantPure = Stats.newCounter[]
    val cntRedundantSwitch = Stats.newCounter[]
    val cntRedundantBranch = Stats.newCounter[]

    val _ = List.app Stats.registerStat [
            Stats.newStat("VN: elim record", [cntRedundantRecord]),
            Stats.newStat("VN: elim select", [cntRedundantSelect]),
            Stats.newStat("VN: elim arith", [cntRedundantArith]),
            Stats.newStat("VN: elim pure", [cntRedundantPure]),
            Stats.newStat("VN: redundant expressions", [
                cntRedundantRecord, cntRedundantSelect,
                cntRedundantArith, cntRedundantPure
              ]),
            Stats.newStat("VN: elim switch", [cntRedundantSwitch]),
            Stats.newStat("VN: elim branch", [cntRedundantBranch]),
            Stats.newStat("VN: redundant control flow", [
                cntRedundantSwitch, cntRedundantBranch
              ])
          ]

    (* renaming map *)
    structure VMap = LV.Map
    type rename_map = LV.lvar LV.Map.map
    fun rename (rn, x) = (case LV.Map.find (rn, x)
           of SOME y => y
            | NONE => x
          (* end case *))
    fun rnValue rn (C.VAR x) = C.VAR(rename(rn, x))
      | rnValue rn v = v
    fun rnField rn (v, off) = (rnValue rn v, off)

    (* available-value map *)
    structure AVMap = RedBlackMapFn (
      struct
        type ord_key = avail_value
        val compare = cmpAvailVal
      end)
    type avail_map = LV.lvar AVMap.map

    fun transform {function, click} = let
          fun tick (s, cntr) = (click s; Stats.incCounter cntr)
          (* controls *)
          val condElim = !Ctl.vnCondElim
          val recordElim = !Ctl.vnRecordElim
          val selectElim = !Ctl.vnSelectElim
          val primElim = !Ctl.vnPrimElim
          (* conditional lookup of available expressions *)
          fun findAvail (false, _, _) = NONE
            | findAvail (true, avail, av) = AVMap.find(avail, av)
          (* transform a CPS expression. The parameters are:
           *   rn     -- a renaming map for lvars
           *   avail  -- a map from available values to their "numbers" (lvars)
           *   bh     -- the branch-history stack
           *   cexp   -- the expression being transformed
           *)
          fun xform (rn : rename_map, avail : avail_map, bh, cexp) = (case cexp
                 of C.RECORD(rk, flds, x, k) => let
                      val flds' = List.map (rnField rn) flds
                      val av = RECORD(rk, flds')
                      in
                        case findAvail(recordElim, avail, av)
                         of SOME y => (
                              tick ("RedundantRecord", cntRedundantRecord);
                              xform (VMap.insert(rn, x, y), avail, bh, k))
                          | NONE => C.RECORD(rk, flds', x,
                              xform (rn, AVMap.insert(avail, av, x), bh, k))
                        (* end case *)
                      end
                  | C.SELECT(n, v, x, cty, k) => let
                      val v' = rnValue rn v
                      val av = SELECT(n, v')
                      in
                        case findAvail(selectElim, avail, av)
                         of SOME y => (
                              tick ("RedundantSelect", cntRedundantSelect);
                              xform (VMap.insert(rn, x, y), avail, bh, k))
                          | NONE => C.SELECT(n, v', x, cty,
                              xform (rn, AVMap.insert(avail, av, x), bh, k))
                        (* end case *)
                      end
                  | C.OFFSET _ => bug "unexpected OFFSET"
                  | C.APP(f, args) => C.APP(rnValue rn f, List.map (rnValue rn) args)
                  | C.FIX(fbs, k) => C.FIX(
                      List.map (xformFun (rn, avail, bh)) fbs,
                      xform (rn, avail, bh, k))
                  | C.SWITCH(arg, id, cases) => let
                      val arg' = rnValue rn arg
                      fun continue () = C.SWITCH(arg', id,
                            List.mapi
                              (fn (i, ce) => xform (rn, avail, SWITCH(arg', i)::bh, ce))
                                cases)
                      in
                        if condElim
                          then (case matchSwitch (bh, arg')
                             of SOME n => (
                                  tick ("RedundantSwitch", cntRedundantSwitch);
                                  xform (rn, avail, bh, List.nth(cases, n)))
                              | NONE => continue ()
                            (* end case *))
                          else continue ()
                      end
                  | C.BRANCH(tst, args, id, trueK, falseK) => let
                      val args' = List.map (rnValue rn) args
                      fun continue () = C.BRANCH(tst, args', id,
                            xform (rn, avail, BRANCH(tst, args', true)::bh, trueK),
                            xform (rn, avail, BRANCH(tst, args', false)::bh, falseK))
                      in
                        if condElim
                          then (case matchBranch (bh, tst, args')
                             of SOME true => (
                                  tick ("RedundantBranch", cntRedundantBranch);
                                  xform (rn, avail, bh, trueK))
                              | SOME false => (
                                  tick ("RedundantBranch", cntRedundantBranch);
                                  xform (rn, avail, bh, falseK))
                              | NONE => continue ()
                            (* end case *))
                          else continue ()
                      end
                  | C.SETTER(rator, args, k) =>
                      C.SETTER(rator, List.map (rnValue rn) args,
                        xform (rn, avail, bh, k))
                  | C.LOOKER(rator, args, x, cty, k) =>
                      C.LOOKER(rator, List.map (rnValue rn) args, x, cty,
                        xform (rn, avail, bh, k))
                  | C.ARITH(rator, args, x, cty, k) => let
                      val args' = List.map (rnValue rn) args
                      val av = ARITH(rator, args')
                      in
                        case findAvail(primElim, avail, av)
                         of SOME y => (
                              tick ("RedundantArith", cntRedundantArith);
                              xform (VMap.insert(rn, x, y), avail, bh, k))
                          | NONE => C.ARITH(rator, args', x, cty,
                              xform (rn, AVMap.insert(avail, av, x), bh, k))
                        (* end case *)
                      end
                  | C.PURE(rator, args, x, cty, k) => let
                      val args' = List.map (rnValue rn) args
                      (* for allocations of mutable memory *)
                      fun notPure () =
                            C.PURE(rator, args', x, cty, xform (rn, avail, bh, k))
                      in
                        case rator
                         of P.MAKEREF => notPure ()
                          | P.NEWARRAY0 => notPure ()
                          | P.MKSPECIAL => notPure ()
                          | P.RAWRECORD _ => notPure ()
                          | _ => let
                              val av = PURE(rator, args')
                              in
                                case findAvail(primElim, avail, av)
                                 of SOME y => (
                                      tick ("RedundantPure", cntRedundantPure);
                                      xform (VMap.insert(rn, x, y), avail, bh, k))
                                  | NONE => C.PURE(rator, args', x, cty,
                                      xform (rn, AVMap.insert(avail, av, x), bh, k))
                                (* end case *)
                              end
                        (* end case *)
                      end
                  | C.RCC(re, cc, ty, args, tys, k) =>
                      C.RCC(re, cc, ty, List.map (rnValue rn) args, tys,
                        xform (rn, avail, bh, k))
                (* end case *))
          and xformFun (rn, avail, bh) (fk, f, xs, ctys, body) =
                (fk, f, xs, ctys, xform(rn, avail, bh, body))
          in
            xformFun (VMap.empty, AVMap.empty, []) function
          end

  end
