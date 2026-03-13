(* normalize-cfg.sml
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 * Normalize a CFG compilation unit by alpha-converting the lambda
 * variables to canonical names (i.e., based on order of appearence).
 *)

structure NormalizeCFG : sig

    val normalize : CFG.comp_unit -> CFG.comp_unit

  end = struct

    structure C = CFG
    structure LV = LambdaVar
    structure Tbl = LV.Tbl

    fun normalize {srcFile, entry, fns} = let
          val renameTbl = Tbl.mkTable (256, Fail "renameTbl")
          val insert = Tbl.insert renameTbl
          val find = Tbl.find renameTbl
          val nextId = ref 1
          fun rn lv = (case find lv
                 of NONE => let
                      val id = !nextId
                      val newLV = LV.fromId id
                      in
                        nextId := id + 1;
                        insert (lv, newLV);
                        newLV
                      end
                  | SOME newLV => newLV
                (* end case *))
          fun cvtExp (C.VAR{name}) = C.VAR{name = rn name}
            | cvtExp (C.LABEL{name}) = C.LABEL{name = rn name}
            | cvtExp (e as C.NUM _) = e
	    | cvtExp (C.LOOKER{oper, args}) = C.LOOKER{oper = oper, args = cvtExps args}
	    | cvtExp (C.PURE{oper, args}) = C.PURE{oper = oper, args = cvtExps args}
	    | cvtExp (C.SELECT{idx, arg}) = C.SELECT{idx = idx, arg = cvtExp arg}
          and cvtExps args = List.map cvtExp args
          fun cvtParam {name, ty} = {name = rn name, ty = ty}
          fun cvtStm (C.LET(e, param, stm)) =
                C.LET(cvtExp e, cvtParam param, cvtStm stm)
            | cvtStm (C.ALLOC(a, args, x, stm)) =
                C.ALLOC(a, cvtExps args, rn x, cvtStm stm)
            | cvtStm (C.APPLY(f, args, tys)) =
                C.APPLY(cvtExp f, cvtExps args, tys)
            | cvtStm (C.THROW(k, args, tys)) =
                C.THROW(cvtExp k, cvtExps args, tys)
            | cvtStm (C.GOTO(l, args)) =
                C.GOTO(rn l, cvtExps args)
            | cvtStm (C.SWITCH(arg, stms)) =
                C.SWITCH(cvtExp arg, List.map cvtStm stms)
            | cvtStm (C.BRANCH(br, args, p, s1, s2)) =
                C.BRANCH(br, cvtExps args, p, cvtStm s1, cvtStm s2)
            | cvtStm (C.ARITH(oper, args, param, stm)) =
                C.ARITH(oper, cvtExps args, cvtParam param, cvtStm stm)
            | cvtStm (C.SETTER(oper, args, stm)) =
                C.SETTER(oper, cvtExps args, cvtStm stm)
            | cvtStm (C.CALLGC(args, xs, stm)) =
                C.CALLGC(cvtExps args, List.map rn xs, cvtStm stm)
            | cvtStm (C.RCC{reentrant, linkage, proto, args, results, live, k}) =
                C.RCC{
                    reentrant = reentrant, linkage = linkage, proto = proto,
                    args = cvtExps args,
                    results = List.map cvtParam results,
                    live = List.map cvtParam live,
                    k = cvtStm k
                  }
          fun cvtFrag (C.Frag{kind, lab, params, body}) = C.Frag{
                  kind = kind, lab = rn lab,
                  params = List.map cvtParam params,
                  body = cvtStm body
                }
          fun cvtCluster (C.Cluster{attrs, frags}) = C.Cluster{
                  attrs = attrs,
                  frags = List.map cvtFrag frags
                }
          in
            {srcFile = srcFile, entry = cvtCluster entry, fns = List.map cvtCluster fns}
          end

  end
