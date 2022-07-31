(* tprof.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature TPROF =
sig
    (* The first (curried) argument is a function that should return true
     * if the operator (specified via inlining info) can return multiple
     * times.  In practical terms, this means call/cc. *)
  val instrumDec :
      (PrimopId.prim_id -> bool) ->
      (StaticEnv.staticEnv * Absyn.dec CompInfo.compInfo)
      -> Absyn.dec -> Absyn.dec

end (* signature TPROF *)

structure TProf : TPROF =
struct

local structure SP = SymPath
      structure V = Variable
      structure M = Modules
      structure B = Bindings
      structure A = Access
      (* structure II' = InlInfo *)
      (* structure P = PrimOp *)
      structure S = Symbol
      open Absyn Variable Types

      val TUPLEexp = AbsynUtil.TUPLEexp
      val TUPLEpat = AbsynUtil.TUPLEpat

      structure T = Types
      structure BT = BasicTypes
      val intTy = BT.intTy
      val unitTy = BT.unitTy
      val tupleTy = BT.tupleTy
      val refTycon = BT.refTycon
      val arrayTycon = BT.arrayTycon
      val --> = BT.-->
      infix -->
in


fun bug s = ErrorMsg.impossible ("Prof: "^s)

val anonSym = S.varSymbol "anon"
val intreftype = CONty(refTycon, [intTy])

fun poly1 ty =
  POLYty{sign=[false], tyfun=TYFUN{arity=1, body=ty}}

(* We get this stuff via CoreAccess now.  This way we don't need
 * to know the actual primops.... *)
(*
val updateop =
  let val t = poly1(tupleTy[CONty(arrayTycon,[IBOUND 0]),
  			    intTy, IBOUND 0] --> unitTy)
   in VALvar{path=SP.SPATH[S.varSymbol "unboxedupdate"], typ=ref t,
             access=A.nullAcc,
	     info=II'.mkPrimInfo(P.UNBOXEDUPDATE, t)}
  end

val assignop =
  let val t = poly1(tupleTy[CONty(refTycon,[IBOUND 0]), intTy, IBOUND 0]
		    --> unitTy)

   in VALvar{path=SP.SPATH[S.varSymbol ":="], typ=ref t,
             access=A.nullAcc,
	     info=II'.mkPrimInfo(P.ASSIGN, t)}
  end

val subop =
  let val t = poly1(tupleTy[CONty(arrayTycon,[IBOUND 0]), intTy] --> IBOUND 0)
   in VALvar{path=SP.SPATH[S.varSymbol "subscript"], typ=ref t,
             access=A.nullAcc,
	     info=II'.mkPrimInfo(P.SUBSCRIPT, t)}
  end

val derefop =
  let val t = poly1(CONty(refTycon,[IBOUND 0]) --> IBOUND 0)
   in VALvar{path=SP.SPATH [S.varSymbol "!"], typ=ref t,
             access=A.nullAcc, info=II'.mkPrimInfo(P.DEREF, t)}
  end

val addop =
  let val t = (tupleTy[intTy,intTy] --> intTy)
   in VALvar{path=SP.SPATH[S.varSymbol "iadd"], typ=ref t,
             access=A.nullAcc, info=II'.mkPrimInfo(P.IADD, t)}
  end
*)

fun tmpvar(str,ty,mkv) =
    let val sym = S.varSymbol str
     in VALvar{access=A.namedAcc(sym, mkv), prim=PrimopId.NonPrim,
               path=SP.SPATH[sym], btvs = ref [], typ=ref ty}
    end

fun varexp(v as VALvar{typ=ref ty,path,...}) =
    (case TypesUtil.headReduceType ty
      of POLYty _ =>
	 bug ("poly["^SP.toString path^"] in Prof")
       | ty' => VARexp(ref v, [])) (* VARexp(ref v, SOME ty') *)
  | varexp _ = bug "090924 in prof"

fun clean (path as name::names) = if S.eq(name,anonSym) then names else path
  | clean x = x

fun intLiteral n = NUMexp("<lit>", {ival = IntInf.fromInt n, ty = intTy})

fun instrumDec' mayReturnMoreThanOnce (env, compInfo) absyn =
 let fun getVar name = CoreAccess.getVar env [name]
     val updateop = getVar "unboxedupdate"
     val assignop = getVar "assign"
     val subop = getVar "subscript"
     val derefop = getVar "deref"
     val addop = getVar "iadd"

     val mkv = #mkLvar (compInfo: Absyn.dec CompInfo.compInfo)
     val countarrayvar = tmpvar("countarray", CONty(arrayTycon,[intTy]),mkv)
     val countarray = varexp countarrayvar

     val basevar = tmpvar("base", intTy, mkv)
     val baseexp = varexp basevar

     val currentvar = tmpvar("profCurrent",CONty(refTycon,[intTy]), mkv)
     val currentexp = varexp currentvar

     val register = getVar "profile_register"

     local
	 val ty = case register of
		      VALvar { typ = ref ty, ... } => ty
		    | _ => bug "298374 in prof"
     in
         val profDerefTy =
	     case TypesUtil.headReduceType ty of
		 CONty (_, [ty']) => ty'
	       | _ => bug "298342 in prof"
     end

     val entries = ref (nil: string list)
     val entrycount = ref 0
     fun makeEntry(name) = let val i = !entrycount
	                    in entries := "\n" :: name :: !entries;
			       entrycount := i+1;
			       i
			   end

     val intUpdTy = tupleTy[CONty(arrayTycon,[intTy]),intTy,intTy] --> unitTy
     val intSubTy = tupleTy[CONty(arrayTycon,[intTy]),intTy] --> intTy

     fun BUMPCCexp (ccvara : int) =
       let val lvar = tmpvar("indexvar",intTy,mkv)
	in APPexp(VARexp(ref updateop, [ref(INSTANTIATED(intTy))]),
	       TUPLEexp[countarray,
		intLiteral ccvara,
		   APPexp(varexp addop,
		     TUPLEexp[
			APPexp(VARexp(ref subop,[ref(INSTANTIATED(intTy))]),
			       TUPLEexp[countarray, intLiteral ccvara]),
			intLiteral 1])])
       end

     val intAssTy = tupleTy[CONty(refTycon,[intTy]),intTy] --> unitTy

     fun SETCURRENTexp (ccvara : int) =
	 let val lvar = tmpvar("indexvar",intTy, mkv)
	  in LETexp(VALdec[VB{pat=VARpat(lvar),
			      exp=APPexp(varexp addop,
					 TUPLEexp[intLiteral ccvara, baseexp]),
			      typ = T.UNDEFty,
			      tyvars=ref nil,
			      boundtvs=[]}],
		    APPexp(VARexp(ref assignop,[ref(INSTANTIATED(intTy))]),
			   TUPLEexp[currentexp, varexp lvar]))
	 end

     fun instrdec(sp as (names,ccvara), VALdec vbl) =
           let fun getvar(VARpat v) = SOME v
	         | getvar(CONSTRAINTpat(p,_)) = getvar p
   	         | getvar _ = NONE

	       fun instrvb(vb as VB{pat,exp,typ,tyvars,boundtvs}) =
	            (case getvar pat
		      of SOME(VALvar{prim, path=SP.SPATH[n],...}) =>
                          (case prim
                             of PrimopId.NonPrim => vb
                              | _ => VB{pat=pat, tyvars=tyvars, typ = typ,
			                exp=instrexp (n::clean names,
                                                      ccvara) false exp,
  			                boundtvs=boundtvs})
                       | SOME(VALvar{prim, ...}) =>
                          (case prim
                             of PrimopId.NonPrim => vb
                              | _ =>  VB{pat=pat, exp=instrexp sp false exp,
                                         typ=typ, tyvars=tyvars, boundtvs=boundtvs})
		       | _ => VB{pat=pat, exp=instrexp sp false exp,
                                 typ=typ, tyvars=tyvars, boundtvs=boundtvs})

            in VALdec (map instrvb vbl)
           end

       | instrdec(sp as (names,ccvara), VALRECdec rvbl) =
           let fun instrrvb (RVB{var as VALvar{path=SP.SPATH[n],...},
                                 exp,resultty,tyvars}) =
                     RVB{exp=instrexp(n::clean names, ccvara) false exp,
                         var=var, resultty=resultty, tyvars=tyvars}

                 | instrrvb _ = bug "VALRECdec in instrdec"
            in VALRECdec(map instrrvb rvbl)
           end

       | instrdec(sp, ABSTYPEdec {abstycs,withtycs,body}) =
           ABSTYPEdec {abstycs=abstycs,withtycs=withtycs,
                       body=instrdec(sp,body)}

       | instrdec(sp, STRdec strbl) =
           STRdec (map (fn strb => instrstrb(sp,strb)) strbl)

       | instrdec(sp, FCTdec fctbl) =
           FCTdec (map (fn fctb => instrfctb(sp,fctb)) fctbl)

       | instrdec(sp, LOCALdec(localdec,visibledec)) =
           LOCALdec(instrdec (sp,localdec), instrdec (sp,visibledec))

       | instrdec(sp, SEQdec decl) =
           SEQdec (map (fn dec => instrdec(sp,dec)) decl)

       | instrdec(sp, MARKdec(dec,region)) =
           MARKdec(instrdec (sp,dec), region)

       | instrdec(sp, other) = other

     and instrstrexp(names, LETstr(d,body)) =
           LETstr(instrdec((names,0),d), instrstrexp(names,body))

       | instrstrexp(names,MARKstr(body,region)) =
           MARKstr(instrstrexp(names,body),region)

       | instrstrexp(names, x) = x

     and instrstrb ((names,ccvara), STRB{name, str, def}) =
           STRB{str=str, def=instrstrexp(name::names,def), name=name}

     and instrfctexp(names, FCTfct {param, def, argtycs}) =
           FCTfct{param=param, def=instrstrexp(names,def), argtycs=argtycs}

       | instrfctexp(names, LETfct(d,body)) =
           LETfct(instrdec((names,0),d), instrfctexp(names,body))

       | instrfctexp(names,MARKfct(body,region)) =
           MARKfct(instrfctexp(names,body),region)

       | instrfctexp(names, x) = x

     and instrfctb ((names,ccvara), FCTB{name, fct, def}) =
           FCTB{name=name, fct=fct, def=instrfctexp(name::names,def)}

     and instrexp(sp as (names,ccvara)) =
       let fun istail tail =
             let fun iinstr exp = istail false exp
                 fun oinstr exp = istail true exp
                 fun instrrules tr (rules,ty1,ty2) =
		     (map (fn (RULE(p,e)) => RULE(p, tr e)) rules, ty1, ty2)

                 val rec instr:(exp->exp) =
                  fn RECORDexp l =>
                       RECORDexp(map (fn (lab,exp) => (lab,iinstr exp)) l)

                   | VECTORexp(l,t) => VECTORexp((map iinstr l),t)

                   | SEQexp l =>
                       let fun seq [e] = [instr e]
                             | seq (e::r) = (iinstr e)::(seq r)
                             | seq nil = nil
                        in SEQexp (seq l)
                       end

		   | IFexp { test, thenCase, elseCase } =>
		       IFexp { test = iinstr test,
			       thenCase = instr thenCase,
			       elseCase = instr elseCase }

		   | ANDALSOexp (e1, e2) =>
		       ANDALSOexp (iinstr e1, instr e2)
		   | ORELSEexp (e1, e2) =>
		       ORELSEexp (iinstr e1, instr e2)
		   | WHILEexp { test, expr } =>
		       WHILEexp { test = iinstr test, expr = iinstr expr }

                   | exp as APPexp (f,a) =>
                       let fun safe(VARexp(ref(VALvar{prim, ...}), _)) =
                               (case prim
                                 of PrimopId.NonPrim => false
                                  | _ =>
                                     if mayReturnMoreThanOnce prim then false
				     else true)
                             | safe(MARKexp(e,_)) = safe e
                             | safe(CONSTRAINTexp(e,_)) = safe e
                             | safe(SEQexp[e]) = safe e
                             | safe _ = false

                           fun rator_instr a =
                             case a
                              of APPexp(randf,_) =>
                                   if safe randf then iinstr else oinstr
                               | VARexp _ => oinstr
                               | MARKexp(e,_) => rator_instr e
                               | CONSTRAINTexp(e,_) => rator_instr e
                               | SEQexp[e] => rator_instr e
                               | _ => iinstr

                           val f' = rator_instr a f

                        in if tail orelse (safe f)
                           then APPexp (f', oinstr a)
                           else let val ty = Reconstruct.expType exp
                                    val lvar = tmpvar("appvar",ty,mkv)
                                 in LETexp (VALdec[VB{pat=VARpat(lvar),
                                                      exp=APPexp(f', oinstr a),
                                                      typ = T.UNDEFty, tyvars=ref nil,
                                                      boundtvs=[]}],
                                            SEQexp([SETCURRENTexp(ccvara),
                                                    varexp lvar]))
                                end
                       end

                   | CONSTRAINTexp(e,t) => CONSTRAINTexp(instr e, t)

                   | HANDLEexp (e, (l,t1,t2)) =>
                       let fun rule(RULE(p,e)) =
                             RULE(p,SEQexp[SETCURRENTexp ccvara, instr e])
                        in HANDLEexp (instr e, (map rule l,t1,t2))
                       end

                   | RAISEexp(e, t) => RAISEexp(oinstr e, t)

                   | LETexp (d, e) => LETexp (instrdec(sp,d), instr e)

                   | CASEexp (e, l) =>
                       CASEexp(iinstr e, instrrules instr l)

                   | FNexp(l,ty1,ty2) =>
                       let fun dot (a,[z]) = S.name z :: a
                             | dot (a,x::rest) =
                                 dot("." :: S.name x :: a, rest)
                             | dot _ = bug "no path in instrexp"

                           val name =  concat (dot ([], names))
                           val ccvara' = makeEntry(name)
                           val lvar = tmpvar("fnvar",ty1,mkv);

                           val exnMatch = CoreAccess.getCon env ["Match"]

                           val RULE(_,special) = List.last l
                        in FNexp ([RULE(VARpat(lvar),
                                        SEQexp ([BUMPCCexp(ccvara'),
                                           SETCURRENTexp(ccvara'),
                                           CASEexp(varexp lvar,
					     instrrules
					       (instrexp (anonSym::names, ccvara') true)
					       (l,ty1,ty2))])),
                                   RULE(WILDpat,
					RAISEexp(CONexp(exnMatch,[]),
                                                 Reconstruct.expType special))],
                                  ty1,ty2)
                       end
                   | MARKexp(e,region) => MARKexp(instr e, region)
                   | e => e

              in instr
             end
        in istail
       end (* function instrexp *)

     val absyn1 = instrdec(([],0),absyn)

     (*
      * The following break the invariant set in the absyn.sml where
      * the pat in each VB binding should bind single variables !;
      * The following VB only binds monomorphic variables, so it is
      * probably ok for the time being. We definitely should clean it
      * up some time in the future. (ZHONG)
      *)

     val absyn2 =
       LOCALdec(VALdec[VB{pat=TUPLEpat[VARpat basevar,
                                       VARpat countarrayvar,
                                       VARpat currentvar],
                          exp=APPexp(APPexp(VARexp(ref derefop,
                                                   [ref(INSTANTIATED(profDerefTy))]),
                                            varexp register),
                                     STRINGexp(concat(rev(!entries)))),
			  typ = T.UNDEFty,
                          tyvars=ref nil,
                          boundtvs=[]}],
                absyn1)

  in absyn2
 end

fun instrumDec mrmto (env, compInfo) absyn =
      if !SMLofNJ.Internals.ProfControl.profMode
	then instrumDec' mrmto (env, compInfo) absyn
	else absyn

end (* local *)
end (* structure TProf *)
