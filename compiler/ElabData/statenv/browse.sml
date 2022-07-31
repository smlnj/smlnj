(* browse.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *)
structure BrowseStatEnv : sig
    datatype bind_info =
	NoEnv
      | Env of { look : Symbol.symbol -> bind_info,
		 symbols : unit -> Symbol.symbol list }

    val browse : StaticEnv.staticEnv -> Symbol.symbol -> bind_info

    val catalog : StaticEnv.staticEnv -> Symbol.symbol list

end = struct

    fun bug m = ErrorMsg.impossible ("BrowseStatEnv: " ^ m)

    structure M = Modules
    structure MU = ModuleUtil
    structure B = Bindings
    structure S = Symbol
    structure SE = StaticEnv

    datatype bind_info =
	NoEnv
      | Env of { look : Symbol.symbol -> bind_info,
		 symbols : unit -> Symbol.symbol list }

    fun lookElems elements sym =
	(case MU.getSpec(elements,sym)
          of M.STRspec{sign,...} => sigenv sign
           | M.FCTspec{sign,...} => fsgenv sign
           | _ => NoEnv)
	handle MU.Unbound _ => NoEnv

    and sigenv (s as M.SIG {elements,...}) =
	Env {look = lookElems elements,
             symbols = (fn () => MU.getSigSymbols s)}
      | sigenv _ = NoEnv

    (*
     * The following is a hack to make the browse function consistent
     * with the changes made on ast during the elaboration of ast into absyn.
     * Syntactic changes made on ast by the elaborator should be propagated
     * to this function so that CM can do the correct job. I personally think 
     * that syntactic changes on curried functors and insertions of
     * resultId (<resultStr>) symbols should be done on Ast directly, before the
     * elaboration --- this way, we don't have to write the following ugly
     * sigenvSp function. 
     *)
    and sigenvSp (M.SIG {elements=[(sym,M.STRspec{sign,...})],...}) =
	if S.name sym = "<resultStr>" then sigenv sign
	else bug "unexpected case <resultStr> in sigenvSp"
      | sigenvSp (M.SIG {elements=[(sym,M.FCTspec{sign,...})],...}) =
	if S.name sym = "<functor>" then fsgenv sign
	else bug "unexpected case <functor> in sigenvSp"
      | sigenvSp _ = bug "unexpected case in signenvSp"

    and fsgenv (M.FSIG{bodysig,...}) = sigenvSp bodysig
      | fsgenv _ = NoEnv

    fun strenv(M.STR { sign, ... }) = sigenv sign
      | strenv _ = NoEnv

    fun fctenv(M.FCT { sign, ... }) = fsgenv sign
      | fctenv _ = NoEnv

    fun browse env sym =
	(case SE.look(env,sym)
	  of B.SIGbind b => sigenv b
           | B.STRbind b => strenv b
           | B.FSGbind b => fsgenv b
           | B.FCTbind b => fctenv b
           | _ => NoEnv)
	handle SE.Unbound => NoEnv

    fun catalog se = map #1 (StaticEnv.sort se)
end
