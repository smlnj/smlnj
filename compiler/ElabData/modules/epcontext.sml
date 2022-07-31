(* Copyright 1996 by AT&T Bell Laboratories *)
(* epcontext.sml *)

signature ENT_PATH_CONTEXT =
sig

  type context

  val initContext : context
  val isEmpty : context -> bool
  val enterOpen : context * EntPath.entVar option -> context
  val enterClosed : context -> context
  val lookTycPath : context * ModuleId.tycId -> EntPath.entPath option
  val lookStrPath : context * ModuleId.strId -> EntPath.entPath option
  val lookFctPath : context * ModuleId.fctId -> EntPath.entPath option
  val bindTycPath : context * ModuleId.tycId * EntPath.entVar -> unit
  val bindStrPath : context * ModuleId.strId * EntPath.entVar -> unit
  val bindFctPath : context * ModuleId.fctId * EntPath.entVar -> unit
  val bindTycLongPath : context * ModuleId.tycId * EntPath.entPath -> unit
  val bindStrLongPath : context * ModuleId.strId * EntPath.entPath -> unit
  val bindFctLongPath : context * ModuleId.fctId * EntPath.entPath -> unit

end  (* signature ENT_PATH_CONTEXT *)


structure EntPathContext :> ENT_PATH_CONTEXT =
struct

local structure ST = Stamps
      structure EP = EntPath
      structure MI = ModuleId
in

type pathmap = EP.rEntPath MI.umap

(* 
 * A structure body (struct decls end) is "closed" if 
 *    it is a functor body structure
 * The idea is that the elements of a closed structure are not
 * directly referenced from outside the structure, so the pathEnv
 * local to the closed structure can be discarded after the structure
 * body is elaborated.
 *)

(* pathmap maps stamps to full entPaths relative to current functor context *)
(* each "closed" structure body pushes a new layer *)
datatype context
  = EMPTY
  | LAYER of {locals: pathmap ref, 
              lookContext: EP.entPath,
              bindContext: EP.rEntPath,
              outer: context}

val initContext : context = EMPTY

fun isEmpty(EMPTY : context) = true
  | isEmpty _ = false

(* 
 * called on entering a closed structure scope, whose elements will not
 * be accessed from outside (hence the null bindContext) 
 *)
fun enterClosed epc = 
  LAYER {locals=ref(MI.emptyUmap), lookContext=EP.epnil,
         bindContext=EP.repnil, outer=epc}

(*
 * called on entering an open structure scope (claim: this is always an
 * unconstrained structure decl body), where ev is the entVar of the
 * structure being elaborated.
 *)
fun enterOpen (EMPTY, _) = EMPTY
  | enterOpen (epc, NONE) = epc
  | enterOpen (LAYER{locals,lookContext,bindContext,outer}, SOME ev) = 
      LAYER{locals=locals, lookContext=lookContext@[ev],
            bindContext=EP.repcons (ev, bindContext), outer=outer}

(* relative(path,ctx) - subtract common prefix of path and ctx from path *)
fun relative([],_) = []
  | relative(ep,[]) = ep
  | relative(p as (x::rest),y::rest') = 
      if EP.eqEntVar(x,y) then relative(rest,rest') else p

fun lookPath find (EMPTY, _) = NONE
  | lookPath find (LAYER { locals, lookContext, bindContext, outer }, id) =
    (case find (!locals, id) of
	 NONE => lookPath find (outer, id)
       | SOME rp => SOME (relative (EP.rep2ep rp, lookContext)))

val lookTycPath = lookPath MI.uLookTyc
val lookStrPath = lookPath MI.uLookStr
val lookFctPath = lookPath MI.uLookFct

(* probe(ctx,s) checks whether a stamp has already be bound before *)
fun probe find (EMPTY, s) = false
  | probe find (LAYER{locals, outer, ...}, s) = 
      (case find(!locals, s) of
	   NONE => probe find (outer, s)
         | _ => true)

fun bindPath (find, insert) (EMPTY, _, _) = ()
  | bindPath (find, insert) (xx as LAYER { locals, bindContext, ... }, s, ev) =
    if probe find (xx, s) then ()
    else (locals := insert (!locals, s, EP.repcons (ev, bindContext)))

val bindTycPath = bindPath (MI.uLookTyc, MI.uInsertTyc)
val bindStrPath = bindPath (MI.uLookStr, MI.uInsertStr)
val bindFctPath = bindPath (MI.uLookFct, MI.uInsertFct)

fun bindLongPath (find, insert) (EMPTY, _, _) = ()
  | bindLongPath (find, insert)
		 (xx as LAYER { locals, bindContext, ... }, s, ep) =
    if probe find (xx, s) then ()
    else (locals := insert (!locals, s, EP.ep2rep (ep, bindContext)))

val bindTycLongPath = bindLongPath (MI.uLookTyc, MI.uInsertTyc)
val bindStrLongPath = bindLongPath (MI.uLookStr, MI.uInsertStr)
val bindFctLongPath = bindLongPath (MI.uLookFct, MI.uInsertFct)

end (* local *)
end (* structure EntPathContext *)
