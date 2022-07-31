(* expandtycon.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *)
signature EXPAND_TYCON =
sig
  type sigContext = Modules.elements list
  val expandTycon : Types.tycon * sigContext * EntityEnv.entityEnv -> Types.tycon
  val debugging : bool ref
end

structure ExpandTycon : EXPAND_TYCON =
struct

local (* imported structures *)
  structure T = Types
  structure TU = TypesUtil
  structure EP = EntPath
  structure M = Modules
  structure MU = ModuleUtil
in

(* debugging hooks *)
val say = Control_Print.say
val debugging = ref false
fun debugmsg (msg: string) =
      if !debugging then (say msg; say "\n") else ()
fun bug s = ErrorMsg.impossible ("ExpandTycon: " ^ s)

type sigContext = M.elements list

exception OUTER

(* ignoring FCTspec - won't find any types there *)
fun lookEntVar(ev,(_,s as (M.TYCspec{entVar,...} |
                           M.STRspec{entVar,...}))::rest) =
      if EP.eqEntVar(ev,entVar) then SOME s else lookEntVar(ev,rest)
  | lookEntVar(ev,_::rest) = lookEntVar(ev,rest)
  | lookEntVar(ev,nil) = NONE

fun findContext(ev,context as elements0::outer) =
      (case lookEntVar(ev, elements0)
	 of SOME(M.STRspec{sign as M.SIG {elements,...},...}) =>
	    elements :: context
	  | NONE => findContext(ev,outer)
	  | _ => bug "findContext - bad element")
  | findContext(ev,nil) = raise OUTER

fun expandTycon(tycon,context,entEnv) =
    let fun expandTycVar(ev,context as elements::outer) : T.tycon =
	      (case lookEntVar(ev, elements)
		 of SOME(M.TYCspec{info=M.RegTycSpec{spec,...},...}) =>
		     (case spec
			of T.GENtyc _ => spec
			 | T.DEFtyc{stamp,strict,path,tyfun} =>
			     T.DEFtyc{stamp=stamp,strict=strict,path=path,
				      tyfun=expandTyfun(tyfun,context)}
			 | _ => bug "expandTycon 2")
		  | NONE => (* try outer context *)
		     expandTycVar(ev,outer)
		  | _ => bug "expandTycon 1")
	  | expandTycVar(ev,nil) = raise OUTER

	and expandTyc context = 
	     fn (tyc as T.PATHtyc{entPath,...}) =>
	         (expandPath(entPath,context)
		  handle OUTER => (* path outside current signature context *)
		    MU.transTycon entEnv tyc)
	      | tyc => tyc

	and expandTyfun(T.TYFUN{arity,body},context) = 
	     T.TYFUN{arity=arity,
		     body=TU.mapTypeFull (expandTyc context) body}

	and expandPath(ep, context) =
	    (case ep
	       of nil => bug "expandPath 1"
		| ev :: nil =>  (* tycon! *)
		   expandTycVar(ev,context)
		| ev :: rest => (* substructure! *)
		   expandPath(rest,findContext(ev, context)))

     in expandTyc context tycon
    end

end (* local *)
end (* structure ExpandTycon *)
