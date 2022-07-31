(* environ.sml
 *
 * COPYRIGHT (c) 2021 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Environment: ENVIRONMENT =
struct

local structure A = Access
      structure S  = Symbol
      structure M  = Modules
      structure V = Variable
      structure T = Types
      structure MU = ModuleUtil
      structure B  = Bindings
      structure SE = StaticEnv
      structure DE = DynamicEnv
      structure PP = PrettyPrint
in

type symbol = S.symbol
type staticEnv = SE.staticEnv
type dynenv  = DE.env

type environment = { static: staticEnv, dynamic: dynenv }

fun bug msg = ErrorMsg.impossible("Environment: "^msg)

fun staticPart (e: environment) = #static e
fun dynamicPart (e: environment) = #dynamic e

fun mkenv (e as { static, dynamic }) = e

val emptyEnv = {static   = SE.empty,
		dynamic  = DE.empty}

fun layerEnv({static, dynamic}, {static=sta, dynamic=dy}) =
      {static =  SE.atop (static, sta), dynamic = DE.atop (dynamic, dy)}

val layerStatic = SE.atop

fun consolidateEnv ({ static, dynamic }) =
      {static = SE.consolidate static, dynamic = DE.consolidate dynamic}

val consolidateStatic = SE.consolidate

fun root (A.EXTERN pid) = SOME pid
  | root (A.PATH(p,i)) = root p
  | root _ = NONE

(* getting the stamp from a binding *)
fun stampOf(B.VALbind (V.VALvar {access=a, ...})) = root a
  | stampOf(B.CONbind (T.DATACON {rep=A.EXN a, ...})) = root a
  | stampOf(B.STRbind (M.STR { access, ... })) = root access
  | stampOf(B.FCTbind (M.FCT { access, ... })) = root access
  | stampOf _ = NONE

(* functions to collect stale dynamic pids for unbinding in concatEnv *)

(*
 * stalePids: takes a new environment and a base environment to which
 * it is to be added and returns a list of pids that are unreachable
 * when the new environment is added to the base environment
 *
 * what we do instead:
 *  - count the number of occurences for each pid in baseEnv bindings
 *    that is going to be shadowed by deltaEnv
 *  - count the total number of total occurences for each such
 *    pids in baseEnv
 *  - the ones where the counts coincide are stale
 *
 * This code is ok, because deltaEnv is the output of `export'.  `export'
 * calls consolidateStatic, therefore we don't have duplicate bindings
 * of the same symbol.
 *)
fun stalePids (deltaEnv, baseEnv) =
  let

      (* any rebindings? *)
      val anyrebound = ref false

      (* counting map *)
      val countM = ref (PersMap.empty: int ref PersMap.map)
      fun look s = PersMap.find (!countM, s)

      (* initialize the counter map: for each new binding with stamp
       * check if the same symbol was bound in the old env and enter
       * the old stamp into the map *)
      fun initOne s =
        case look s
         of NONE => countM := PersMap.insert (!countM, s, ref (~1))
          | SOME r => r := (!r) - 1

      fun initC (sy, _) =
	  (case stampOf (SE.look (baseEnv, sy))
	     of NONE => ()
	      | SOME s => (initOne s; anyrebound := true))
	  handle SE.Unbound => ()
      (* increment counter for a given stamp *)
      fun incr NONE = ()
	| incr (SOME s) =
 	   case look s
             of NONE => ()
 	      | SOME r => r := (!r) + 1

      fun incC (_, b) = incr (stampOf b)
      (* select the 0s *)
      fun selZero ((s, ref 0), zeros) = s :: zeros
	| selZero (_, zeros) = zeros
   in
      SE.app initC deltaEnv;		(* init counter map *)
      if !anyrebound then let		(* shortcut if no rebindings *)
	  (* count the pids *)
	  val _ = SE.app incC baseEnv
	  (* pick out the stale ones *)
	  val stalepids = foldl selZero [] (PersMap.listItemsi (!countM))
      in
	  stalepids
      end
      else []
  end

fun concatEnv ({ static = newstat, dynamic = newdyn }, { static = oldstat, dynamic = olddyn }) =
    let val hidden_pids = stalePids (newstat, oldstat)
	val slimdyn = DE.remove (hidden_pids, olddyn)
    in {
      static=SE.consolidateLazy(SE.atop(newstat, oldstat)),
      dynamic=DE.atop(newdyn, slimdyn)
    } end

fun getbindings(static: staticEnv, symbols: S.symbol list) :
        (S.symbol * B.binding) list =
  let fun loop([], bindings) = bindings
        | loop(s::rest, bindings) =
            let val bindings' = (s,SE.look(static,s)) :: bindings
				  handle SE.Unbound => bindings
	     in loop (rest, bindings')
            end
   in loop(symbols,[])
  end

fun copystat([], senv) = senv
  | copystat((s,b)::l, senv) = copystat(l,SE.bind(s, b, senv))

(*
fun filterStaticEnv(static: staticEnv, symbols: S.symbol list) : staticEnv =
      copystat(getbindings(static, symbols), SE.empty)
*)

local
    fun copydyn (bindings, dynamic) = let
	fun loop ([], denv) = denv
	  | loop ((_, b) :: l, denv) =
	    (case stampOf b
	      of NONE => loop (l, denv)
	       | SOME pid =>
		     let val dy = valOf (DE.look dynamic pid)
			 val denv = DE.bind (pid, dy, denv)
		     in loop (l, denv)
		     end)
    in
	loop (bindings, DE.empty)
    end
in
    fun filterEnv({static, dynamic}: environment, symbols) =
	let val sbindings = getbindings (static, symbols)
	    val senv = copystat(sbindings, SE.empty)
	    val denv = copydyn(sbindings, dynamic)
	in {static =senv, dynamic = denv}
	end

    fun trimEnv { static, dynamic} = let
	val syms = BrowseStatEnv.catalog static
	val dynamic = copydyn (getbindings (static, syms), dynamic)
    in
	{ static = static, dynamic = dynamic }
    end
end

fun describe static (s: symbol) : unit =
      PP.with_default_pp
	  (fn ppstrm =>
	    (PP.openHVBox ppstrm (PP.Rel 0);
	      PPModules.ppBinding ppstrm static
	        (s, SE.look(static,s), !Control.Print.printDepth);
	      PP.newline ppstrm;
	     PP.closeBox ppstrm))
      handle SE.Unbound => print (S.name s ^ " not found\n")

val primEnv = PrimEnv.primEnv

end (* local *)
end (* structure Environment *)
