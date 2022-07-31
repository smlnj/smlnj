(* Copyright 1996 by AT&T Bell Laboratories *)
(* evalent.sml *)

signature EVALENTITY =
sig

  val evalApp : Modules.fctEntity * Modules.strEntity
                * DebIndex.depth * EntPathContext.context
                * InvPath.path * ElabUtil.compInfo
                -> Modules.strEntity

end (* signature EVALENTITY *)

structure EvalEntity : EVALENTITY =
struct

local (* structure DI = DebIndex *)
      structure SS = SpecialSymbols
      structure EP = EntPath
      structure IP = InvPath
      structure S = SourceMap
      structure T = Types
      structure TU = TypesUtil
      structure EE = EntityEnv
      structure EPC = EntPathContext
      structure EU = ElabUtil
      structure MI = ModuleId
      structure MU = ModuleUtil
      structure I = Instantiate
      open Modules
in

(* debugging *)
val say = Control_Print.say
val debugging = ElabDataControl.eedebugging
fun debugmsg (msg: string) =
    if !debugging then (say msg; say "\n") else ()

open ElabDebug

val debugPrint = (fn x => debugPrint debugging x)  (* Value Restriction *)
fun bug msg = ErrorMsg.impossible ("EvalEntity: " ^ msg);

(* DBM: should the following three "special symbols" be added to SpecialSymbols? *)
val anonFctSym = Symbol.fctSymbol "AnonFct"
val paramSym = Symbol.strSymbol "<FsigParamInst>"
val anonStrSym = Symbol.strSymbol "<AnonStr>"

val resultId = SS.resultId
val returnId = SS.returnId

val defaultError =
    ErrorMsg.errorNoFile(ErrorMsg.defaultConsumer(),ref false) (0,0)

fun evalTyc (entv, tycExp, entEnv, epc, rpath,
             compInfo as {mkStamp,...}: EU.compInfo) =
      case tycExp
       of CONSTtyc tycon => tycon
        | FORMtyc (T.GENtyc { kind, arity, eq, path, ... }) =>
	  (case kind of
	       T.DATATYPE{index=0, stamps, freetycs, family, root=NONE, stripped} =>
               let val viztyc = MU.transTycon entEnv
                   val nstamps = Vector.map (fn _ => mkStamp()) stamps
                   val nst = Vector.sub(nstamps,0)
                   val nfreetycs = map viztyc freetycs
                   val _ = EPC.bindTycPath (epc, nst, entv)
               in
		   T.GENtyc{stamp=nst, arity=arity, eq=eq,
                            kind=T.DATATYPE{index=0, stamps=nstamps,
					    root=NONE,
					    freetycs=nfreetycs,
					    family=family,
					    stripped=stripped},
                            path=IP.append(rpath,path), stub=NONE}
               end
             | T.DATATYPE{index=i, root=SOME rtev, stripped, ...} =>
               let val (nstamps, nfreetycs, nfamily, nstripped) =
                       case EE.lookTycEnt(entEnv, rtev)
			of T.GENtyc { kind = T.DATATYPE{stamps,freetycs,family,stripped,...}, ... } =>
			   (stamps, freetycs, family, stripped)
			 | _ => bug "unexpected case in evalTyc-FMGENtyc (2)"
                   val nst = Vector.sub(nstamps,i)
                   val _ = EPC.bindTycPath (epc, nst, entv)
               in
		   T.GENtyc{stamp=nst, arity=arity,
                            kind=T.DATATYPE{index=i, stamps=nstamps,
					    root=NONE,
					    freetycs=nfreetycs,
					    family=nfamily,
					    stripped=nstripped},
                            path=IP.append(rpath,path),
			    eq=eq, stub=NONE}
               end
	     | _ => bug "unexpected GENtyc in evalTyc")
        | FORMtyc (T.DEFtyc{stamp,tyfun=T.TYFUN{arity, body},strict,path}) =>
          let val newstamp = mkStamp()
	      (* tycId=stamp (this should perhaps be more abstract some day) *)
	      val _ = EPC.bindTycPath (epc, newstamp, entv)
	      val newbody = MU.transType entEnv body
	      val newstrict = TU.calcStrictness(arity, newbody)
	  in
	      T.DEFtyc{stamp = newstamp,
		       tyfun=T.TYFUN{arity=arity, body=newbody},
		       strict=newstrict, path=IP.append(rpath,path)}
          end
        | VARtyc entPath =>
	    (debugmsg (">>evalTyc[VARtyc]: "^EP.entPathToString entPath);
	     EE.lookTycEP(entEnv,entPath))
        | _ => bug "unexpected tycExp in evalTyc"

and evalStr(strExp, depth, epc, entsv, entEnv, rpath,
            compInfo as {mkStamp,...}: EU.compInfo) =
     (debugmsg ("[Inside EvalStr ......");
      case strExp
       of VARstr entPath =>
	    (debugmsg (">>evalStr[VARstr]: "^EP.entPathToString entPath);
	     (EE.lookStrEP(entEnv,entPath), entEnv))

        | CONSTstr strEnt => (strEnt, entEnv)

        | STRUCTURE {stamp, entDec} =>
            let val epc = EPC.enterOpen(epc, entsv)
                val stp = evalStp(stamp, depth, epc, entEnv, compInfo)
                val env = evalDec(entDec, depth, epc, entEnv, rpath, compInfo)
	    in
		({stamp = stp, entities=env,
		  properties = PropList.newHolder (),
		  (*lambdaty=ref NONE,*)
		  rpath = rpath, stub = NONE},
		 entEnv)
            end

        | APPLY (fctExp, strExp) =>
	    let val (fctRlzn, entEnv1) =
                  evalFct(fctExp, depth, epc, entEnv, compInfo)
	        val (argRlzn, entEnv2) =
                  evalStr(strExp, depth, epc, entsv, entEnv1,
                          IP.empty, compInfo)
                val epc = EPC.enterOpen(epc, entsv)
             in (evalApp(fctRlzn, argRlzn, depth, epc, rpath, compInfo),
                 entEnv2)
            end

        | LETstr (entDec, strExp) =>
            let val entEnv1 = evalDec(entDec, depth, epc,
                                      entEnv, rpath, compInfo)
                val (strEnt, entEnv2) =
                  evalStr(strExp, depth, epc, entsv, entEnv1,
                          rpath, compInfo)

 	     in (strEnt, entEnv2)
            end

        | ABSstr (sign, strExp) =>
	    let val (srcRlzn, entEnv1) =
                  evalStr(strExp, depth, epc, entsv, entEnv, rpath, compInfo)
                val {rlzn=rlzn, abstycs=abstycs, tyceps=tyceps} =
                  I.instAbstr{sign=sign, entEnv=entEnv, rlzn=srcRlzn,
                              rpath=rpath,
                              region=S.nullRegion, compInfo=compInfo}

                (* because the abstraction creates a bunch of new stamps,
                   we have to bind them to the epcontext.
                 *)
                val epc = EPC.enterOpen(epc, entsv)
                fun bind (T.GENtyc gt, ep) =
		    EPC.bindTycLongPath (epc, MI.tycId gt, ep)
                  | bind _ = ()
                val _ = ListPair.app bind (abstycs, tyceps)
	     in (rlzn, entEnv1)
	    end

        | CONSTRAINstr {boundvar,raw,coercion} =>
            (* propagage the context rpath into the raw uncoerced structure *)
            let val (rawEnt, entEnv1) =
                  evalStr(raw, depth, epc, SOME boundvar,
                          entEnv, rpath, compInfo)
                val entEnv2 = EE.bind(boundvar, STRent rawEnt, entEnv1)
            (*  val entEnv' = EE.bind(boundvar, STRent rawEnt, entEnv) *)
                val (strEnt, entEnv3) =
 	          evalStr(coercion, depth, epc, entsv,
                          entEnv2, IP.empty, compInfo)

             in (strEnt, entEnv3)
            end

        | FORMstr _ => bug "unexpected FORMstr in evalStr")


and evalFct (fctExp, depth, epc, entEnv,
             compInfo as {mkStamp,...}: EU.compInfo) =
      case fctExp
       of VARfct entPath =>
	    (debugmsg (">>evalFct[VARfct]: "^EP.entPathToString entPath);
	     (EE.lookFctEP(entEnv,entPath), entEnv))

        | CONSTfct fctEntity => (fctEntity, entEnv)

        | LAMBDA{param, body} =>
            let val clos = CLOSURE{param=param, body=body, env=entEnv}
	     in ({stamp = mkStamp (),
		  closure=clos,
		  properties = PropList.newHolder (),
		  (*lambdaty=ref NONE,*)
  		  tycpath=NONE,
		  rpath=IP.IPATH[anonFctSym],
		  stub=NONE},
		 entEnv)
            end

        | LAMBDA_TP{param, body, sign as FSIG{paramsig, bodysig, ...}} =>
            let val clos = CLOSURE{param=param, body=body, env=entEnv}
                val tps =
                  let val rpath' = IP.IPATH [paramSym]
                      val {rlzn=paramEnt, tycpaths=paramTps} =
                        I.instParam{sign=paramsig, entEnv=entEnv,
                                    rpath=rpath', tdepth=depth,
                                    region=S.nullRegion, compInfo=compInfo}
                      val entEnv' =
                        EE.mark(mkStamp, EE.bind(param, STRent paramEnt,
                                                 entEnv))
                      val (bodyRlzn,_) =
                        evalStr(body, DebIndex.next depth, epc, NONE,
                                entEnv', IP.empty, compInfo)
                      val bodyTps =
                        I.getTycPaths{sign=bodysig, rlzn=bodyRlzn,
                                      entEnv=entEnv', compInfo=compInfo}
                   in T.TP_FCT(paramTps, bodyTps)
                  end

             in ({stamp = mkStamp(),
		  closure=clos,
		  properties = PropList.newHolder (),
		  (* lambdaty=ref NONE, *)
		  tycpath=SOME tps, rpath=IP.IPATH[anonFctSym],
		  stub = NONE},
		 entEnv)
            end

        | LETfct (entDec, fctExp) =>
            let val entEnv1 = evalDec(entDec, depth, epc,
                                      entEnv, IP.empty, compInfo)
                val (fctEnt, entEnv2) =
                  evalFct(fctExp, depth, epc, entEnv1, compInfo)
             in (fctEnt, entEnv2)
            end

        | _ => bug "unexpected cases in evalFct"

and evalApp(fctRlzn : Modules.fctEntity, argRlzn, depth, epc, rpath,
            compInfo as {mkStamp, ...} : EU.compInfo) =
      let val {closure=CLOSURE{param, body, env}, tycpath, ...} = fctRlzn
	  val nenv = EE.mark(mkStamp, EE.bind(param, STRent argRlzn, env))
          val  _ = debugmsg ("[Inside EvalAPP] ......")
       in case (body, tycpath)
           of (FORMstr(FSIG{paramsig, bodysig, ...}), SOME tp) =>
               let val argTps = I.getTycPaths{sign=paramsig, rlzn=argRlzn,
                                              entEnv=env, compInfo=compInfo}
                   val resTp = T.TP_APP(tp, argTps)

                   (** failing to add the stamps into the epcontext is
                       a potential bug here. Will fix this in the
                       future.  ZHONG **)

                   val {rlzn=rlzn, abstycs=abstycs, tyceps=tyceps} =
                     I.instFmBody {sign=bodysig, entEnv=nenv, tycpath=resTp,
                                   rpath=rpath, region=S.nullRegion,
                                   compInfo=compInfo}

                   fun h (T.GENtyc gt, ep) =
                       EPC.bindTycLongPath (epc, MI.tycId gt, ep)
                     | h _ = ()
                   val _ = ListPair.app h (abstycs, tyceps)
                in rlzn
               end
            | _ =>
               let val (strEnt, deltaEE)
                     = evalStr(body, depth, epc, NONE, nenv, rpath, compInfo)
                   (* invariant: deltaEE should always be same as nenv
                      if the body of an functor is always a BaseStr. Notice
                      functor body is constructed either in the source
                      programs (ml.grm) or in the elabmod.sml when dealing
                      with curried functor applications.
                    *)
                in strEnt
               end
      end

and evalDec(dec, depth, epc, entEnv, rpath,
            compInfo as {mkStamp,...}: EU.compInfo) =
     (debugmsg ("[Inside EvalDec ......");
      case dec
       of TYCdec (entVar, tycExp) =>
            let val tycEnt =
                  evalTyc(entVar, tycExp, entEnv, epc, rpath, compInfo)
	     in EE.bind(entVar, TYCent tycEnt, entEnv)
            end
        | STRdec (entVar, strExp, sym) =>
            let val rpath' =
		    if Symbol.eq(sym, returnId)
		       orelse Symbol.eq(sym, resultId)
		    then rpath
		    else IP.extend(rpath,sym)
		val (strEnt, entEnv1) =
                  evalStr(strExp, depth, epc, SOME entVar,
                          entEnv, rpath', compInfo)
             in EE.bind(entVar, STRent strEnt, entEnv1)
            end

        | FCTdec (entVar, fctExp) =>
            let val (fctEnt, entEnv1) =
                  evalFct(fctExp, depth, epc, entEnv, compInfo)
             in EE.bind(entVar, FCTent fctEnt, entEnv1)
            end
        | SEQdec decs =>
            let fun h (dec, entEnv0) =
                  evalDec(dec, depth, epc, entEnv0, rpath, compInfo)
             in EE.mark(mkStamp, foldl h entEnv decs)
            end
        (*
         * The following may be wrong, but since ASSERTION! the bound symbols
         * are all distinct,it would not appear to cause any harm.
         *)
        | LOCALdec (localDec, bodyDec) =>
            let val entEnv1 = evalDec(localDec, depth, epc,
                                      entEnv, IP.empty, compInfo)
             in evalDec(bodyDec, depth, epc, entEnv1, rpath, compInfo)
            end

        | _  => entEnv)

and evalStp (stpExp, depth, epc, entEnv,
             compInfo as {mkStamp,...}: EU.compInfo) =
      case stpExp
       of (* CONST stamp     => stamp
        | *) NEW             => mkStamp()
        | GETSTAMP strExp => #stamp (#1 (evalStr(strExp, depth, epc, NONE,
						 entEnv, IP.empty, compInfo)))

(*
val evalApp = Stats.doPhase(Stats.makePhase "Compiler 044 x-evalApp") evalApp
*)

end (* toplevel local *)
end (* structure EvalEntity *)
