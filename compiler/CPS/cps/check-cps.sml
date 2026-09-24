(* check-cps.sml
 *
 * Invariant checking for the CPS IR
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *
 * TODO:
 *      check types and arity of primops
 *)

structure CheckCPS : sig

    val check : string * CPS.function -> unit

  end = struct

    structure C = CPS
    structure P = C.P
    structure LV = LambdaVar
    structure PP = PPCps

    datatype cty = datatype C.cty
    datatype pkind = datatype C.pkind
    datatype value = datatype C.value

    datatype binding
      = Fix of C.function
      | Param
      | Other
      | Label                           (* SWITCH/BRANCH identitier *)
      | Unbound

    datatype context = C of {
        env : (binding * cty) LV.Map.map,       (* lexically-scoped environment *)
        outer : LV.lvar,                        (* the current function *)
        info : info                             (* additional information *)
      }

    and info = I of {
        prefix : string,                (* the message prefix passed to `check` *)
        vars : LV.HSet.set,             (* set of all bound variables *)
        arity : int LV.Tbl.hash_table,  (* map non-fix-bound functions to their arity *)
        nErrors : int ref               (* number of errors detected *)
      }

    val say = Control.Print.say

    val v2s = PP.value2str
    val lv2s = LV.lvarName

    fun app2str (f, args) = String.concat[
            f, "(", String.concatWithMap "," v2s args, ")"
          ]

    fun new (prefix, outer) = C{
            env = LV.Map.empty,
            outer = outer,
            info = I{
                prefix = prefix,
                vars = LV.HSet.mkEmpty 32,
                arity = LV.Tbl.mkTable (32, Fail "arity tbl"),
                nErrors = ref 0
              }
          }

    fun error (C{outer, info=I{prefix, nErrors, ...}, ...}, msg) = let
          val n = !nErrors
          in
            if (n = 0)
              then say(concat["# Error(s) detected when checking ", prefix, "\n"])
              else ();
            nErrors := n+1;
            say (concat("## [" :: lv2s outer :: "] " :: msg @ ["\n"]))
          end

    fun bind (cxt as C{env, outer, info as I{vars, ...}}, x, b, cty) = (
          if LV.HSet.member(vars, x)
            then error (cxt, ["duplicate binding of '", lv2s x, "'"])
            else ();
          LV.HSet.add(vars, x);
          C{ env = LV.Map.insert (env, x, (b, cty)), outer = outer, info = info })

    fun bindParams (cxt, xs, ctys) =
          ListPair.foldlEq
            (fn (x, cty, cxt) => bind(cxt, x, Param, cty))
            cxt (xs, ctys)

    fun getArity (C{info=I{arity, ...}, ...}, f) = LV.Tbl.find arity f

    fun setArity (C{info=I{arity, ...}, ...}, f, n) = LV.Tbl.insert arity (f, n)

    fun enterScope (C{env, info, ...}, f) = C{env=env, outer=f, info=info}

    fun lookup (C{env, ...}, x) = (case LV.Map.find(env, x)
           of SOME info => info
            | NONE => (Unbound, C.ptrTy)
          (* end case *))

    fun isBound (C{env, ...}, x) = LV.Map.inDomain(env, x)

    fun anyErrors (C{info=I{nErrors, ...}, ...}) = (!nErrors > 0)

    fun nameOf (cxt, VAR x) = x
      | nameOf (cxt, LABEL x) = raise Fail "unexpected LABEL"
      | nameOf (cxt, v) = (
          error (cxt, ["expected VAR or LABEL, but found ", PP.value2str v]);
          LV.mkLvar())

    (* the type of a record *)
    fun recordTy (C.RK_RECORD, elems) = C.rPtrTy(List.length elems)
      | recordTy (C.RK_MIXED rep, _) = PTRt(RPT rep)
      | recordTy (C.RK_RAWBLOCK, elems) = C.fPtrTy(List.length elems)
      | recordTy _ = C.ptrTy

    fun typeOfValue (cxt, C.VAR x) = #2 (lookup(cxt, x))
      | typeOfValue (cxt, LABEL _) = raise Fail "unexpected LABEL"
      | typeOfValue (cxt, NUM{ty, ...}) = NUMt ty
      | typeOfValue (cxt, ENUM _) = ENUMt
      | typeOfValue (cxt, REAL{ty, ...}) = FLTt ty
      | typeOfValue (cxt, STRING _) = C.ptrTy
      | typeOfValue (cxt, VOID) = raise Fail "unexpected VOID"

    (* compare types for compatability *)
    fun compatTy (NUMt nty1, NUMt nty2) = (#sz nty1 = #sz nty2)
      | compatTy (ENUMt, ENUMt) = true
        (* tagged ints and enums are compatable *)
      | compatTy (ENUMt, NUMt{tag=true, ...}) = true
      | compatTy (NUMt{tag=true, ...}, ENUMt) = true
        (* unknown pointers are compatable with other pointers and enums *)
      | compatTy (PTRt VPT, ENUMt) = true
      | compatTy (ENUMt, PTRt VPT) = true
      | compatTy (PTRt VPT, PTRt _) = true
      | compatTy (PTRt _, PTRt VPT) = true
        (* record pointers must match *)
      | compatTy (PTRt(RPT rep1), PTRt(RPT rep2)) =
          (#ptrLen rep1 = #ptrLen rep2)
          andalso (#rawLen rep1 = #rawLen rep2)
      | compatTy (FLTt sz1, FLTt sz2) = (sz1 = sz2)
        (* functions are compatible with unknown pointers *)
      | compatTy (FUNt, FUNt) = true
      | compatTy (PTRt VPT, FUNt) = true
      | compatTy (FUNt, PTRt VPT) = true
        (* continuations are compatible with unknown pointers *)
      | compatTy (CNTt tys1, CNTt tys2) =
          ListPair.allEq compatTy (tys2, tys2)
      | compatTy (CNTt _, PTRt VPT) = true
      | compatTy (PTRt VPT, CNTt _) = true
      | compatTy _ = false

    fun check (prefix, func as (_, f, _, _, _)) = let
          val cxt = new (prefix, f)
          in
            (* since the top-level function should never be called, we
             * do not enter it into the environment
             *)
            checkFun cxt func;
            if (anyErrors cxt)
              then (
                say "*********************************************** \n";
                PP.printcps0 func;
                say "*********************************************** \n";
                ErrorMsg.impossible "invalid CPS")
              else ()
          end

    (* check a function; we assume that the function binding has already
     * been entered into the environment.
     *)
    and checkFun cxt (func as (fk, f, params, tys, body)) = let
          val cxt = bindParams (enterScope (cxt, f), params, tys)
          in
            if (List.length params <> List.length tys)
              then error (cxt, ["parameter/type list mismatch"])
              else ();
            checkExp (cxt, body)
          end

    and checkExp (cxt, cexp) = (case cexp
           of C.RECORD(rk, elems, x, ce) => (
                checkArgs (
                  cxt,
                  fn () => concat[
                      lv2s x, " = ", (case rk of C.RK_VECTOR => "#{" | _ => "{"),
                      String.concatWithMap "," PP.vpathToString elems, "}"
                    ],
                  List.map #1 elems);
                checkExp (bind(cxt, x, Other, recordTy(rk, elems)), ce))
            | C.SELECT(i, v, x, cty, ce) => (
                checkArgs (
                  cxt,
                  fn () => concat[lv2s x, " = #", Int.toString i, " ", v2s v],
                  [v]);
                checkExp (bind(cxt, x, Other, cty), ce))
            | C.OFFSET(i, v, x, ce) => raise Fail "unexpected OFFSET"
            | C.APP(f, args) => (
                checkArgs (
                  cxt,
                  fn () => app2str (v2s f, args),
                  args);
                case #1 (lookup (cxt, nameOf(cxt, f)))
                 of Fix(_, _, params, tys, _) => checkApp (cxt, f, tys, args)
                  | Unbound => error (cxt, ["'", v2s f, "' is unbound"])
                  | _ => (case getArity (cxt, nameOf(cxt, f))
                       of SOME n => checkArity (cxt, f, n, List.length args)
                        | NONE => setArity (cxt, nameOf(cxt, f), List.length args)
                      (* end case *))
                (* end case *))
            | C.FIX(fns, ce) => let
                fun bindFn (func as (_, f, _, _, _), cxt) =
                      bind(cxt, f, Fix func, C.ptrTy)
                val cxt = List.foldl bindFn cxt fns
                in
                  List.app (checkFun cxt) fns;
                  checkExp (cxt, ce)
                end
            | C.SWITCH(v, id, cases) => let
                val cxt' = bind(cxt, id, Label, C.ptrTy)
                in
                  ignore (checkArg (cxt, fn () => "switch " ^ v2s v, v));
                  List.app (fn ce => checkExp (cxt', ce)) cases
                end
            | C.BRANCH(tst, args, id, ce1, ce2) => let
                val cxt' = bind(cxt, id, Label, C.ptrTy)
                in
                  checkArgs (
                    cxt,
                    fn () => concat["if ", app2str (PP.branchToString tst, args)],
                    args);
                  checkExp (cxt', ce1);
                  checkExp (cxt', ce2)
                end
            | C.SETTER(p, args, ce) => (
                checkArgs (
                  cxt,
                  fn () => concat[app2str (PP.setterToString p, args)],
                  args);
                checkExp (cxt, ce))
            | C.LOOKER(p, args, x, cty, ce) => (
                checkArgs (
                  cxt,
                  fn () => concat[lv2s x, " = ", app2str (PP.lookerToString p, args)],
                  args);
                checkExp (bind(cxt, x, Other, cty), ce))
            | C.ARITH(p, args, x, cty, ce) => (
                checkArgs (
                  cxt,
                  fn () => concat[lv2s x, " = ", app2str (PP.arithToString p, args)],
                  args);
                checkExp (bind(cxt, x, Other, cty), ce))
            | C.PURE(p, args, x, cty, ce) => (
                checkArgs (
                  cxt,
                  fn () => concat[lv2s x, " = ", app2str (PP.pureToString p, args)],
                  args);
                checkExp (bind(cxt, x, Other, cty), ce))
            | C.RCC(reentrant, cc, proto, args, results, ce) => let
(* TODO: check C function args before binding results *)
                val cxt = List.foldl
                      (fn ((x, cty), cxt) => bind(cxt, x, Other, cty))
                        cxt results
                in
                  checkExp (cxt, ce)
                end
          (* end case *))

    (* check a value; returns true if it is an unbound variable *)
    and checkArg (cxt, exp, v) = let
          fun chk x = if isBound (cxt, x)
                then false
                else (
                  error (cxt, ["'", lv2s x, "' is unbound in `", exp(), "`"]);
                  true)
          in
            case v
             of VAR x => chk x
              | LABEL x => chk x
              | _ => false
            (* end case *)
          end

    and checkArgs (cxt, exp, args) =
          List.app (fn arg => ignore (checkArg (cxt, exp, arg))) args

    and checkArity (cxt, f, nParams, nArgs) =
          if (nParams <> nArgs)
            then error(cxt, ["parameter/argument arity mismatch for '", v2s f, "'"])
            else ()

    (* check the arity and types of a known function application *)
    and checkApp (cxt, f, paramTys, args) = let
          fun chk ([], []) = ()
            | chk ([], _) = error(cxt, [
                  "too many arguments in application of '", v2s f, "'"
                ])
            | chk (_, []) = error(cxt, [
                  "too few arguments in application of '", v2s f, "'"
                ])
            | chk (cty::ctyr, arg::argr) = let
                val argTy = typeOfValue (cxt, arg)
                in
                  if checkArg (cxt, fn () => concat["application of '", v2s f, "'"], arg)
                    then () (* unbound argument, so don't check the types *)
                  else if compatTy(cty, argTy)
                    then ()
                    else error(cxt, [
                        "type mismatch in call to '", v2s f, "'; expected type ",
                        CPSUtil.ctyToString cty, " for argument ",
                        PPCps.value2str arg, " : ", CPSUtil.ctyToString argTy
                      ]);
                  chk (ctyr, argr)
                end
          in
            chk (paramTys, args)
          end

  end
