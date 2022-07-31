(* COPYRIGHT (c) 1996 Bell Laboratories. *)
(* lookup.sml *)

structure Lookup : LOOKUP =
struct

local structure SP = SymPath
      structure CVP = ConvertPaths
      structure M = Modules
      structure MU = ModuleUtil
      structure T = Types
      structure TU = TypesUtil
      structure A = Access
      structure V = Variable
      structure AS = Absyn
      structure AU = AbsynUtil
      structure B = Bindings
      structure SE = StaticEnv
      structure EM = ErrorMsg
      structure S = Symbol
in

(* error reporting *)

fun bug msg = EM.impossible ("Lookup: " ^ msg)

fun unboundError(unboundSym, symPathOp, errFn, errorVal) =
    let val nameSpace = S.nameSpaceToString(S.nameSpace unboundSym)
	val pathMsg =
	    case symPathOp
	      of NONE => nil
	       | SOME spath =>
		   if SP.length spath > 1
		   then [" in path ", SP.toString spath]
		   else nil
	val symName = S.name unboundSym
	val errorMsg = concat ("unbound " :: nameSpace :: ": " :: symName :: pathMsg)
     in errFn EM.COMPLAIN errorMsg EM.nullErrorBody;
        errorVal
    end

fun otherError(msg, err) = err EM.COMPLAIN msg EM.nullErrorBody


(* lookup symbol *)

(*** look for a fixity binding ***)
fun lookFix (env, sym) : Fixity.fixity =
    (case SE.look (env, sym)
       of B.FIXbind fixity => fixity
	| _ => bug "lookFIX")
    handle SE.Unbound => Fixity.NONfix

(*** look for a signature ***)
fun lookSig (env, sym, err) : M.Signature =
    (case SE.look (env, sym)
       of B.SIGbind sign => sign
        | _ => bug "lookSIG")
    handle SE.Unbound =>
      unboundError (sym, NONE, err, M.ERRORsig)

(*** look for a functor signature ***)
fun lookFsig (env, sym, err) : M.fctSig =
    (case SE.look (env, sym)
       of B.FSGbind fs => fs
        | _ => bug "lookFSIG")
    handle SE.Unbound =>
      unboundError (sym, NONE, err, M.ERRORfsig)

(* bindingToAtomId : B.binding -> AS.value *)
fun bindingToAtomId (B.VALbind v) = AS.VAR v
  | bindingToAtomId (B.CONbind c) = AS.CON c
  | bindingToAtomId _ = AS.ERRORid

(*** look for a variable or a constructor bound to a symbol ***)
fun lookIdSym (env, sym, err) : AS.value =
    bindingToAtomId (SE.look (env, sym))
    handle SE.Unbound =>
      unboundError (sym, NONE, err, AS.ERRORid)

(*** look for a variable or a constructor bound to a symbol ***)
fun lookIdSymOp (env, sym) : AS.value option =
    SOME (bindingToAtomId (SE.look (env, sym)))
    handle SE.Unbound => NONE


(*** lookup path ****)

(*
 * lookGen: generic lookup function for identifiers which may occur in:
 *   1. environments
 *   2. actual structure environments
 *   3. signature parsing environments
 *)
fun lookGen(env, spath, outBind, getPath, errFn, errorVal) =
    case spath
      of SP.SPATH [id] =>
           (outBind (SE.look(env,id))
	    handle SE.Unbound =>
	      (unboundError(id, SOME spath, errFn, errorVal)))
      | SP.SPATH(first::rest) =>
	((case SE.look(env,first)
	   of B.STRbind str =>
	      (getPath(str,SP.SPATH rest,spath)
	       handle MU.Unbound sym =>
		 (unboundError(sym, SOME spath, errFn, errorVal)))
	    | _ =>  bug "lookGen1")
	 handle SE.Unbound => (unboundError(first, SOME spath, errFn, errorVal)))
      | SP.SPATH [] => bug "lookGen:SP.SPATH[]"

(*** look for a variable or a constructor (complete path) ***)
fun lookIdPath (env, path, err) : AS.value =
    lookGen (env, path, bindingToAtomId, MU.getValPath, err, AS.ERRORid)

(*** look for a structure ***)
fun lookStr (env,path,err) : M.Structure =
  let fun outStr(B.STRbind str) = str
        | outStr _ = bug "outStr"
   in lookGen (env, path, outStr, MU.getStrPath, err, M.ERRORstr)
  end

(*** look for a strDef; used in elabsig.sml ***)
fun lookStrDef (env,path,err) : M.strDef =
  let fun outStrDef (B.STRbind s) =
	  (case s
	     of M.STRSIG{sign,entPath} => M.VARstrDef(sign,entPath)
              | sv => M.CONSTstrDef sv)
        | outStrDef _ = bug "outStrDef"
   in lookGen (env, path, outStrDef, MU.getStrDef, err, M.CONSTstrDef M.ERRORstr)
  end

(*** look for a functor ***)
fun lookFct (env,path,err) : M.Functor =
  let fun outFct(B.FCTbind fct) = fct
        | outFct _ = bug "outFct"
   in lookGen (env, path, outFct, MU.getFctPath, err, M.ERRORfct)
  end

(*** look for a type constructor ***)
fun lookTyc (env,path,err) : T.tycon =
  let fun outTyc(B.TYCbind tycon) = tycon
        | outTyc _ = bug "outTyc"
   in lookGen (env, path, outTyc, MU.getTycPath, err, T.ERRORtyc)
  end

(*** tycon lookup with arity checking ***)
fun lookArTyc (env, path, arity, err) =
      (case lookTyc (env,path,err)
        of T.ERRORtyc => T.ERRORtyc
         | tycon =>
	     if TU.tyconArity(tycon) <> arity
 	     then (otherError("type constructor " ^
		      (SP.toString(CVP.invertIPath(valOf(TU.tycPath tycon)))) ^
		      " given " ^ (Int.toString arity) ^ " arguments, wants "
		      ^ (Int.toString (TU.tyconArity tycon)), err);
		   T.ERRORtyc)
	     else tycon)

(*** looking for an exception ***)
fun lookExn (env, path, err) : T.datacon =
      (case lookIdPath (env,path,err)
        of AS.CON(dcon as T.DATACON{rep=(A.EXN _), ...}) => dcon
         | AS.CON _ =>
             (otherError("found data constructor instead of exception", err);
              AU.bogusEXN)
         | AS.VAR _ =>
             (otherError("found variable instead of exception", err);
              AU.bogusEXN))

end (* local *)
end (* structure Lookup *)
