(* Copyright 1996 by AT&T Bell Laboratories *)
(* include.sml *)

signature INCLUDE =
sig

  val elabInclude:
     Modules.Signature * StaticEnv.staticEnv * Modules.elements
     * int * SourceMap.region * ElabUtil.compInfo
     -> StaticEnv.staticEnv * Modules.elements
        * Modules.sharespec list (* type sharing *)
        * Modules.sharespec list (* structure sharing *)
        * int  (* slots *) * bool (* fctflag *)

  val debugging : bool ref

end (* signature INCLUDE *)


structure Include: INCLUDE =
struct

local structure EM = ErrorMsg
      structure IP = InvPath
      structure A = Access
      structure TU = TypesUtil
      structure M = Modules
      structure MU = ModuleUtil
      structure B = Bindings
      structure EU = ElabUtil
      structure EP = EntPath
      structure S = Symbol
      structure SE = StaticEnv
      open Modules Types Variable
in

fun bug msg = EM.impossible ("Include: " ^ msg)
val debugging = ref false
val say = Control_Print.say
fun debugmsg (msg: string) = if (!debugging) then (say msg; say "\n") else ()

fun addElement (elem,elements) = elem::elements

fun substElem(new as (name,spec),(old as (name',_))::rest) =
      if S.eq(name,name') then new::rest
      else old::substElem(new,rest)
  | substElem(_,nil) = bug "substElem"

datatype tyc_compat = KEEP_OLD | REPLACE | INCOMPATIBLE

fun compatible(newtyc,oldtyc) =
  if TU.tyconArity newtyc <> TU.tyconArity oldtyc then INCOMPATIBLE
  else case (newtyc,oldtyc) of
	   (GENtyc { kind, ... }, GENtyc { kind = kind', ... }) =>
	   (case (kind, kind') of
		(FORMAL, FORMAL) => KEEP_OLD
	      | (_, FORMAL) => REPLACE
	      | _ => INCOMPATIBLE)
	 | _ => INCOMPATIBLE

fun specified(symbol,elements) =
      List.exists (fn (n,_) => S.eq(symbol,n)) elements

(*** elaborating IncludeSpec in signatures ***)
(* BUG! currently doesn't deal with general sigexp case (e.g. sigid where ...) *)
fun elabInclude(SIG {stamp, elements=newElements,
		     properties, typsharing, strsharing,
		     name, closed, fctflag, stub},
                oldEnv, oldElements, oldSlots,
                region, compInfo as {mkStamp,error,...} : EU.compInfo) =
let val err = error region

(*
 * When including a list of specs into the current signature; some tycon's
 * entVars might be adjusted, this would force all the types in the specs
 * being adjusted also. This adjustment is implemented using this tycmap
 * table.
 *)
exception TycMap

val tycMap : (EP.entVar * tycon) list ref = ref []

fun addMap z = (tycMap := (z::(!tycMap)))
fun getMap z = (!tycMap)

fun lookTycMap(ev,[]) = raise TycMap
  | lookTycMap(ev,(ev',tyc)::rest) =
      if EP.eqEntVar(ev,ev') then tyc else lookTycMap(ev,rest)

(*
 * adjustType does not get inside each DEFtyc's body because we
 * assume that the body has been adjusted already.
 *)
fun adjustType(ty,[]) = ty
  | adjustType(ty,tycmap) =
      let fun newtyc (tyc as PATHtyc{entPath=[ev],...}) =
                (lookTycMap(ev,tycmap) handle TycMap => tyc)
            | newtyc tyc = tyc
       in TU.mapTypeFull newtyc ty
      end

(*
 * The adjustTyc function is only called at each type specification site.
 *
 * The stamp for DEFtyc is changed; fortunately, this is OK because
 * all other references to this DEFtyc is via PATHtyc.
 *)
fun adjustTyc(tycon,[]) = tycon
  | adjustTyc(tycon,tycmap) =
      (case tycon
        of DEFtyc{stamp=s, tyfun=TYFUN{arity,body}, strict, path} =>
             DEFtyc{tyfun=TYFUN{arity=arity,body=adjustType(body,tycmap)},
                    stamp=mkStamp(), strict=strict, path=path}
         | GENtyc _ => tycon
         | PATHtyc{entPath=[ev],...} =>
             (lookTycMap(ev,tycmap) handle TycMap => tycon)
         | _ => bug "adjustTyc")

(*
 * Changing the stamp of an ANONYMOUS signature may cause unnecessary
 * signature maching operations.
 *)
and adjustSig(sign,[]) = sign
  | adjustSig(sign as SIG {stamp, name, closed, fctflag,
			   elements, properties,
			   (* boundeps, lambdaty, *)
			   typsharing, strsharing, stub},
	      tycmap) =
    if closed then sign
    else SIG{stamp = mkStamp(),
	     name=name, closed=false, fctflag=fctflag,
	     properties = PropList.newHolder (),
             (* boundeps=ref NONE, *)
	     (* lambdaty=ref NONE, *)
	     elements=adjustElems(elements,tycmap),
	     typsharing=typsharing,
	     strsharing=strsharing,
	     stub = NONE}
  | adjustSig _ = bug "adjustSig"

and adjustFsig(sign as FSIG{kind,paramsig,bodysig,paramvar,paramsym},tycmap) =
      let val paramsig' = adjustSig(paramsig,tycmap)
          val bodysig' = adjustSig(bodysig,tycmap)
       in FSIG{kind=kind,paramsig=paramsig',bodysig=bodysig',
               paramvar=paramvar,paramsym=paramsym}
      end
  | adjustFsig _ = bug "adjustFsig"

and adjustElems(elements,tycmap) = map (adjustElem tycmap) elements

and adjustElem tycmap (sym,spec) =
      let val nspec =
            case spec
             of TYCspec{entVar=ev,info=RegTycSpec{spec=tycon, repl=r, scope=s}} =>
                  TYCspec{entVar=ev,
                          info=RegTycSpec{spec=adjustTyc(tycon,tycmap),
                                          repl=r, scope=s}}
              | STRspec{sign, entVar=ev, def=d, slot=s} =>
                  STRspec{sign=adjustSig(sign,tycmap),entVar=ev,def=d,slot=s}
		  (* BUG: def component may need adjustment? *)
              | FCTspec{sign, entVar=ev, slot=s} =>
                  FCTspec{sign=adjustFsig(sign,tycmap),entVar=ev,slot=s}
              | VALspec{spec=typ, slot=s} =>
                  VALspec{spec=adjustType(typ,tycmap), slot=s}
              | CONspec{spec=DATACON{rep,name,typ,const,sign,lazyp},slot=s} =>
                  CONspec{spec=DATACON{rep=rep,name=name,const=const,lazyp=lazyp,
                                       typ=adjustType(typ,tycmap),sign=sign},
                          slot=s}
              | _ => bug "adjustElem"
       in (sym, nspec)
      end

fun addElem((name,nspec: M.spec), env, elems, slot) =
  case nspec
   of TYCspec{entVar=ev, info=RegTycSpec{spec=tc, repl=r, scope=s}} =>
      (let val (oev,otc,or,os) =
	       case MU.getSpec(elems,name) of
		   TYCspec{entVar,info=RegTycSpec{spec,repl,scope}} =>
                      (entVar,spec,repl,scope)
		 | _ => bug "addElem:TYCspec"
         in case compatible(tc,otc)
             of KEEP_OLD =>
                  let val ntc = PATHtyc{arity=TU.tyconArity otc,
					entPath=[oev], path=IP.IPATH[name]}
                      val _ = addMap(ev,ntc)
                   in (env, elems, slot)
                  end
              | REPLACE =>
                  let val ntc = adjustTyc(tc, getMap())
                      val nspec' = TYCspec{entVar=oev,
                                           info=RegTycSpec{spec=ntc,repl=or,scope=s}}
                                          (*???*)
                      val elems' = substElem((name,nspec'),elems)

                      val ntc = PATHtyc{arity=TU.tyconArity ntc,
					entPath=[oev], path=IP.IPATH[name]}
                      val _ = addMap(ev,ntc)

                   in (env, elems', slot)
                  end
              | INCOMPATIBLE =>
                  (err EM.COMPLAIN ("duplicate specifications for type "
                                    ^ S.name name ^ " caused by include")
                   EM.nullErrorBody;
                   (env, elems, slot))
        end handle MU.Unbound _ => (* new tycon *)
              (let val ntyc = PATHtyc{arity=TU.tyconArity tc, entPath=[ev],
				      path=IP.IPATH[name]}
                   val env' = SE.bind(name, B.TYCbind ntyc, env)

                   val spec' = TYCspec{entVar=ev,
                                       info=RegTycSpec{spec=adjustTyc(tc, getMap()),
                                                       repl=r,scope=s}}
                   val elems' = addElement((name,spec'), elems)
                in (env', elems', slot)
               end))

   | STRspec{sign, entVar, def, ...} =>
       (if specified(name,elems)
        then (err EM.COMPLAIN ("duplicate specifications for structure "
                               ^ S.name name ^ " caused by include")
              EM.nullErrorBody;
              (env, elems, slot))
        else (* new specification - ok *)
          let val newsign = adjustSig(sign,getMap())
              val newspec = STRspec{sign=newsign,slot=slot,entVar=entVar,def=def}
              val nstr = STRSIG{sign=newsign, entPath=[entVar]}
              val env' = SE.bind(name, B.STRbind nstr, env)
              val elems' = addElement((name,newspec), elems)
           in (env', elems', slot+1)
          end)

   | FCTspec{sign,entVar, ...} =>
       (if specified(name,elems)
        then (err EM.COMPLAIN ("duplicate specifications for functor "
                               ^ S.name name ^ " caused by include")
              EM.nullErrorBody;
              (env, elems, slot))
        else (* new specification - ok *)
          let val newsign = adjustFsig(sign,getMap())
              val newspec = FCTspec{sign=newsign,slot=slot,entVar=entVar}
              val elems' = addElement((name,newspec), elems)
           in (env, elems', slot+1)
          end)

   | VALspec{spec=typ, ...} =>
       (if specified(name,elems)
        then (err EM.COMPLAIN ("duplicate value specifications for "
                               ^ S.name name ^ " caused by include")
              EM.nullErrorBody;
              (env, elems, slot))
        else (* new specification - ok *)
          let val newtyp = adjustType(typ,getMap())
              val newspec = VALspec{spec=newtyp,slot=slot}
              val elems' = addElement((name,newspec), elems)
           in (env, elems', slot+1)
          end)

   | CONspec{spec=DATACON{rep,name,typ,const,sign,lazyp},...} =>
       (if specified(name,elems)
        then (err EM.COMPLAIN ("duplicate constructor specifications for "
                               ^ S.name name ^ " caused by include")
              EM.nullErrorBody;
              (env, elems, slot))
        else (* new specification - ok *)
          let val newtyp = adjustType(typ,getMap())
              val ndcon = DATACON {rep=rep, name=name, typ=newtyp,
                                   const=const, sign=sign, lazyp=lazyp}
              val (slotOp, slot') =
                case rep
                 of A.EXN _ => (SOME slot, slot+1)
                  | _ => (NONE, slot)

              val newspec = CONspec {spec=ndcon,slot=slotOp}
              val elems' = addElement((name,newspec), elems)
           in (env, elems', slot')
          end)
   | _ => bug "addElem"

fun addElems(nil, env, elems, slot) = (env, elems, slot)
  | addElems(e::nelems, env, elems, slot) =
      let (*** should use s to search for e in nelems if
                elements is represented as a real env. ***)
          val (env', elems', slot') =
                addElem(e, env, elems, slot)
       in addElems(nelems, env', elems', slot')
      end

val (env', elems', slots') =
      addElems(newElements, oldEnv, oldElements, oldSlots)

 in (env',elems', typsharing, strsharing, slots', fctflag)

end  (* end of case #1 for function elabInclude *)

  | elabInclude(ERRORsig, env, elems, slots, region, compInfo) =
      (env, elems, [], [], slots, false)

end (* local *)
end (* structure Include *)
