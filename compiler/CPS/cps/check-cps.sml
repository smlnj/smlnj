(* check-cps.sml
 *
 * Invariant checking for the CPS IR
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *
 * TODO:
 *      check types
 *      check arity for primops
 *)

structure CheckCPS : sig

    val check : string * CPS.function -> unit

  end = struct

    structure P = CPS.P
    structure LV = LambdaVar
    structure PP = PPCps

    datatype cty = datatype CPS.cty
    datatype value = datatype CPS.value

    datatype binding
      = Fix of CPS.function
      | Param
      | Other
      | Label                           (* SWITCH/BRANCH identitier *)
      | Unbound

    datatype context = C of {
        env : binding LV.Map.map,       (* lexically-scoped environment *)
        outer : LV.lvar,                (* the current function *)
        info : info                     (* additional information *)
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

    fun bind (cxt as C{env, outer, info as I{vars, ...}}, x, b) = (
          if LV.HSet.member(vars, x)
            then error (cxt, ["duplicate binding of '", lv2s x, "'"])
            else ();
          LV.HSet.add(vars, x);
          C{ env = LV.Map.insert (env, x, b), outer = outer, info = info })

    fun bindParams (cxt, xs) =
          List.foldl (fn (x, cxt) => bind(cxt, x, Param)) cxt xs

    fun getArity (C{info=I{arity, ...}, ...}, f) = LV.Tbl.find arity f

    fun setArity (C{info=I{arity, ...}, ...}, f, n) = LV.Tbl.insert arity (f, n)

    fun enterScope (C{env, info, ...}, f) = C{env=env, outer=f, info=info}

    fun lookup (C{env, ...}, x) = (case LV.Map.find(env, x)
           of SOME b => b
            | NONE => Unbound
          (* end case *))

    fun isBound (C{env, ...}, x) = LV.Map.inDomain(env, x)

    fun anyErrors (C{info=I{nErrors, ...}, ...}) = (!nErrors > 0)

    fun nameOf (cxt, VAR x) = x
      | nameOf (cxt, LABEL x) = x
      | nameOf (cxt, v) = (
          error (cxt, ["expected VAR or LABEL, but found ", PP.value2str v]);
          LV.mkLvar())

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
          val cxt = bindParams (enterScope (cxt, f), params)
          in
            if (List.length params <> List.length tys)
              then error (cxt, ["parameter/type list mismatch"])
              else ();
            checkExp (cxt, body)
          end

    and checkExp (cxt, cexp) = (case cexp
           of CPS.RECORD(rk, elems, x, ce) => (
                checkArgs (
                  cxt,
                  fn () => concat[
                      lv2s x, " = ", (case rk of CPS.RK_VECTOR => "#{" | _ => "{"),
                      String.concatWithMap "," PP.vpathToString elems, "}"
                    ],
                  List.map #1 elems);
                checkExp (bind(cxt, x, Other), ce))
            | CPS.SELECT(i, v, x, cty, ce) => (
                checkArgs (
                  cxt,
                  fn () => concat[lv2s x, " = #", Int.toString i, " ", v2s v],
                  [v]);
                checkExp (bind(cxt, x, Other), ce))
            | CPS.OFFSET(i, v, x, ce) => raise Fail "unexpected OFFSET"
            | CPS.APP(f, args) => (
                checkArgs (
                  cxt,
                  fn () => app2str (v2s f, args),
                  args);
                case lookup (cxt, nameOf(cxt, f))
                 of Fix(_, _, params, _, _) =>
                      checkArity (cxt, f, List.length params, List.length args)
                  | Unbound => error (cxt, ["'", v2s f, "' is unbound"])
                  | _ => (case getArity (cxt, nameOf(cxt, f))
                       of SOME n => checkArity (cxt, f, n, List.length args)
                        | NONE => setArity (cxt, nameOf(cxt, f), List.length args)
                      (* end case *))
                (* end case *))
            | CPS.FIX(fns, ce) => let
                fun bindFn (func as (_, f, _, _, _), cxt) = bind(cxt, f, Fix func)
                val cxt = List.foldl bindFn cxt fns
                in
                  List.app (checkFun cxt) fns;
                  checkExp (cxt, ce)
                end
            | CPS.SWITCH(v, id, cases) => let
                val cxt' = bind(cxt, id, Label)
                in
                  checkArg (cxt, fn () => "switch " ^ v2s v, v);
                  List.app (fn ce => checkExp (cxt', ce)) cases
                end
            | CPS.BRANCH(tst, args, id, ce1, ce2) => let
                val cxt' = bind(cxt, id, Label)
                in
                  checkArgs (
                    cxt,
                    fn () => concat["if ", app2str (PP.branchToString tst, args)],
                    args);
                  checkExp (cxt', ce1);
                  checkExp (cxt', ce2)
                end
            | CPS.SETTER(p, args, ce) => (
                checkArgs (
                  cxt,
                  fn () => concat[app2str (PP.setterToString p, args)],
                  args);
                checkExp (cxt, ce))
            | CPS.LOOKER(p, args, x, cty, ce) => (
                checkArgs (
                  cxt,
                  fn () => concat[lv2s x, " = ", app2str (PP.lookerToString p, args)],
                  args);
                checkExp (bind(cxt, x, Other), ce))
            | CPS.ARITH(p, args, x, cty, ce) => (
                checkArgs (
                  cxt,
                  fn () => concat[lv2s x, " = ", app2str (PP.arithToString p, args)],
                  args);
                checkExp (bind(cxt, x, Other), ce))
            | CPS.PURE(p, args, x, cty, ce) => (
                checkArgs (
                  cxt,
                  fn () => concat[lv2s x, " = ", app2str (PP.pureToString p, args)],
                  args);
                checkExp (bind(cxt, x, Other), ce))
            | CPS.RCC(reentrant, cc, proto, args, results, ce) => let
(* TODO: check C function args before binding results *)
                val cxt = List.foldl
                      (fn ((x, cty), cxt) => bind(cxt, x, Other))
                        cxt results
                in
                  checkExp (cxt, ce)
                end
          (* end case *))

    and checkArg (cxt, exp, v) = let
          fun chk x = if isBound (cxt, x)
                then ()
                else error (cxt, ["'", lv2s x, "' is unbound in `", exp(), "`"])
          in
            case v
             of VAR x => chk x
              | LABEL x => chk x
              | _ => ()
            (* end case *)
          end

    and checkArgs (cxt, exp, args) =
          List.app (fn arg => checkArg (cxt, exp, arg)) args

    and checkArity (cxt, f, nParams, nArgs) =
          if (nParams <> nArgs)
            then error(cxt, ["parameter/argument arity mismatch for '", v2s f, "'"])
            else ()

  end
