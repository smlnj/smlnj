(* Copyright 1996 by AT&T Bell Laboratories *)
(* entityenv.sml *)

structure EntityEnv : ENTITY_ENV =
struct

local
  structure EP = EntPath
  structure ED = EntPath.EvDict
  structure ST = Stamps
  structure M = Modules
  structure T = Types
in

val say = Control_Print.say
val debugging = ElabDataControl.eedebugging (* ref false *)
fun debugmsg (msg: string) =
      if !debugging then (say msg; say "\n") else ()
fun bug msg = ErrorMsg.impossible("EntityEnv: "^msg)

type entVar = EP.entVar
type entPath = EP.entPath
type entityEnv = M.entityEnv

exception Unbound

val empty = M.NILeenv

fun mark(_,e as M.MARKeenv _) = e
  | mark(_,e as M.NILeenv) = e
  | mark(_,e as M.ERReenv) = e
  | mark(mkStamp,env) =
    M.MARKeenv { stamp = mkStamp(), env = env, stub = NONE }

fun bind(v, e, M.BINDeenv(d, env)) = M.BINDeenv(ED.insert(d, v, e), env)
  | bind(v, e, env) = M.BINDeenv(ED.insert(ED.empty, v, e), env)

fun atop(_, M.ERReenv) = M.ERReenv
  | atop(M.ERReenv, _) = M.ERReenv
  | atop(e1, M.NILeenv) = e1
  | atop(M.MARKeenv { env, ...}, e2) = atop (env, e2)
  | atop(M.BINDeenv(d,e1),e2) = M.BINDeenv(d,atop(e1,e2))
  | atop(M.NILeenv, e2) = e2

fun atopSp(_, M.ERReenv) = M.ERReenv
  | atopSp(M.ERReenv, _) = M.ERReenv
  | atopSp(e1, M.NILeenv) = e1
  | atopSp(M.MARKeenv { env, ... }, e2) = atopSp (env, e2)
  | atopSp(M.BINDeenv(d,e1),e2) = atopMerge(d,atop(e1,e2))
  | atopSp(M.NILeenv, e2) = e2

and atopMerge(d, M.NILeenv) = M.BINDeenv(d, M.NILeenv)
  | atopMerge(d, M.BINDeenv(d', e)) = M.BINDeenv (ED.unionWith #1 (d,d'),e)
  | atopMerge(d, M.MARKeenv { env, ... }) = atopMerge (d, env)
  | atopMerge (d, M.ERReenv) = M.ERReenv

fun toList (M.MARKeenv { env, ... }) = toList env
  | toList (M.BINDeenv(d, ee)) = (*ED.fold((op ::), toList ee, d)*)
     ED.foldri (fn (key, value, base) => (key,value)::base) (toList ee) d
  | toList M.NILeenv = nil
  | toList M.ERReenv = nil

fun look(env,v) =
    let fun scan(M.MARKeenv { env, ... }) = scan env
	  | scan(M.BINDeenv(d, rest)) = 
              (case ED.find(d, v)
                of SOME e => e
                 | NONE => scan rest)
(*
	      if EP.eqEntVar(v,v')
	      then (debugmsg("$EE.look: found " ^ EP.entVarToString v); e)
	      else (debugmsg("$EE.look: looking for " ^ EP.entVarToString v ^
			     " saw " ^ EP.entVarToString v');
		    scan rest)
*)
	  | scan M.ERReenv = M.ERRORent
	  | scan M.NILeenv = 
	      (debugmsg ("$EE.look: didn't find "^EP.entVarToString v);
	       raise Unbound)
     in scan env
    end

fun lookStrEnt(entEnv,entVar) = 
    case look(entEnv,entVar)
     of M.STRent ent => ent
      | M.ERRORent => M.bogusStrEntity
      | _ => bug "lookStrEnt"

fun lookTycEnt(entEnv,entVar) = 
    case look(entEnv,entVar)
     of M.TYCent ent => ent
      | M.ERRORent => Types.ERRORtyc
      | _ => bug "lookTycEnt"

fun lookFctEnt(entEnv,entVar) = 
    case look(entEnv,entVar)
     of M.FCTent ent => ent
      | M.ERRORent => M.bogusFctEntity
      | _ => bug "lookFctEnt"

fun lookEP(entEnv,[]) = bug "lookEP.1"
  | lookEP(entEnv,[v]) = look(entEnv,v)
  | lookEP(entEnv,ep as (v::rest)) =
     (case look(entEnv,v)
	of M.STRent { entities, ... } => lookEP (entities,rest)
	 | M.ERRORent => M.ERRORent
	 | ent =>
	     (say "lookEnt.1: expected STRent\n";
	      say "found entity: ";
	      case ent
		of M.TYCent _ => say "TYCent\n"
		 | M.FCTent _ => say "FCTent\n"
		 | _ => say "ERRORent\n";
	      say "entpath: "; say (EP.entPathToString(ep)); say "\n";
	      bug "lookEnt.2"))

fun lookTycEP(entEnv,entPath) = 
    case lookEP(entEnv,entPath)
     of M.TYCent tycon => tycon
      | M.ERRORent => T.ERRORtyc
      | _ => bug "lookTycEP: wrong entity"

fun lookStrEP(entEnv,entPath) = 
    case lookEP(entEnv,entPath)
     of M.STRent rlzn => rlzn
      | M.ERRORent => M.bogusStrEntity
      | _ => bug "lookStrEP: wrong entity"

fun lookFctEP(entEnv,entPath) = 
    case lookEP(entEnv,entPath)
     of M.FCTent rlzn => rlzn
      | M.ERRORent => M.bogusFctEntity
      | _ => bug "lookFctEP: wrong entity"

end (* local *)
end (* structure EntityEnv *)

