(* Copyright 1996 by AT&T Bell Laboratories *)
(* elabmod.sml *)

signature ELABMOD =
sig

  (* elaborate module-level declarations *)
  val elabDecl :
         {ast       : Ast.dec,
          statenv   : StaticEnv.staticEnv,
          entEnv    : Modules.entityEnv,
          context   : ElabUtil.context,         (* elab context *)
          level     : bool,                     (* T if top-level decl. *)
          tdepth    : int,
          epContext : EntPathContext.context,
          path      : InvPath.path,
          region    : SourceMap.region,
          compInfo  : ElabUtil.compInfo} -> {absyn     : Absyn.dec,
                                             statenv   : StaticEnv.staticEnv}

  val debugging : bool ref

end (* signature ELABMOD *)


(* functorized to factor out dependencies on FLINT... *)
(* functor ElabModFn (structure SM : SIGMATCH) : ELABMOD = *)
structure ElabMod : ELABMOD =
struct

local structure S  = Symbol
      structure IP = InvPath
      structure SP = SymPath
      structure EP = EntPath
      structure EPC = EntPathContext
      structure EE = EntityEnv
      structure T  = Types
      structure TU = TypesUtil
      structure V  = Variable
      structure M  = Modules
      structure MU = ModuleUtil
      structure MI = ModuleId
      structure L = Lookup
      structure EU = ElabUtil
      structure ET = ElabType
      structure EC = ElabCore
      structure ES = ElabSig
      structure B  = Bindings
      structure LU = Lookup
      structure SM = SigMatch
      structure INS = Instantiate
      structure SE = StaticEnv
      structure EM = ErrorMsg
      structure PP = PrettyPrint
      structure A  = Absyn
      structure DA = Access
      structure DI = DebIndex
      structure PPU = PPUtil
      structure ED = ElabDebug
      structure ST = RedBlackSetFn(type ord_key = S.symbol
                                   val compare = S.compare)
      open Ast Modules
      open SpecialSymbols (* special symbols *)
in

(* debugging *)
val say = Control_Print.say
fun says strs = say (concat strs)
fun newline () = say "\n"
fun saynl str = (say str; newline())
fun saysnl strs = (says strs; newline())

val debugging = ElabControl.emdebugging (* ref false; == Control.Elab.emdebugging *)
fun dbsay msg = if !debugging then say msg else ()
fun dbsaynl msg = if !debugging then saynl msg else ()

val debugmsg = dbsaynl

fun bug msg = ErrorMsg.impossible("ElabMod: "^msg)

val debugPrint = (fn x => ED.debugPrint debugging x)

fun showStr(msg,str,env) =
    ED.withInternals(fn () =>
      debugPrint(msg,
		 (fn pps => fn str =>
		    PPModules.ppStructure pps env (str, 100)),
		 str))

fun showFct(msg,fct,env) =
    ED.withInternals(fn () =>
      debugPrint(msg,
		 (fn pps => fn fct' =>
		    PPModules_DB.ppFunctor pps env (fct', 100)),
		 fct))

fun showDec (msg, dec, env) =
    let fun ppDec ppstrm d =
	    PPAbsyn.ppDec (env, NONE) ppstrm (d, !Control_Print.printDepth)
     in ElabDebug.debugPrint ElabControl.printAbsyn (msg, ppDec, dec)
    end

fun showStrexp (msg, strexp: A.strexp, env) =
    let fun ppStrexp ppstrm sexp =
	    PPAbsyn.ppStrexp (env, NONE) ppstrm (sexp, !Control_Print.printDepth)
     in ElabDebug.debugPrint ElabControl.printAbsyn (msg, ppStrexp, strexp)
    end

fun showStrExpAst (msg, strexp: Ast.strexp, env) =
    let fun ppStrexp ppstrm sexp =
	    PPAst.ppStrExp NONE ppstrm (sexp, !Control_Print.printDepth)
     in ElabDebug.debugPrint ElabControl.printAbsyn (msg, ppStrexp, strexp)
    end

(*
 * Check if an entity declaration is empty in order to avoid the unnecessary
 * recompilation bug reported by Matthias Blume (ZHONG)
 *)
fun nonEmptyEntDec (M.EMPTYdec | M.SEQdec []) = false
  | nonEmptyEntDec _ = true

fun seqEntDec ds =
  let val nds = List.filter nonEmptyEntDec ds
   in case nds of [] => M.EMPTYdec
                | _ => M.SEQdec nds
  end

fun localEntDec(d1, d2) = seqEntDec [d1, d2]

fun stripMarkSigb(MarkSigb(sigb',region'),region) =
      stripMarkSigb(sigb',region')
  | stripMarkSigb x = x

fun stripMarkFsigb(MarkFsigb(fsigb',region'),region) =
      stripMarkFsigb(fsigb',region')
  | stripMarkFsigb x = x

fun stripMarkFctb(MarkFctb(fctb',region'),region) =
      stripMarkFctb(fctb',region')
  | stripMarkFctb x = x

fun stripMarkStrb(MarkStrb(strb',region'),region) =
      stripMarkStrb(strb',region')
  | stripMarkStrb x = x

(* change of context on entering a structure *)
fun inStr (EU.TOP) = EU.INSTR
  | inStr z = z

(*
 * Add modId to entPath mappings for all appropriate elements of a structure
 * that has just been elaborated.  If epc is the empty context (rigid), then
 * this is an expensive no-op, so we test epc first. But, would this be
 * equivalent to context=INFCT _ ?
 *
 * epc is the EntPathContext for the interior of the structure -- i.e.
 * the structure binding's entVar has been added to the bindContext
 *
 * mapPaths is quite heavy weight right now; it can be simplified in
 * several ways, first, all tycon stamps don't have to be remapped,
 * if new tycon stamps are mapped by Instantiate, then each mapPaths
 * only need to deal with structures and functors; even dealing with
 * structures and functors can be distributed into the signature matching
 * or the instantiation process. (ZHONG)
 *)
(*
val mapPathsPhase = (Stats.makePhase "Compiler 033 1-mapPaths")
and mapPaths x = Stats.doPhase mapPathsPhase mapPaths0 x
*)

fun mapPaths(epc, STR { sign, rlzn, ... }, flex) =
    mapEPC(epc, sign, rlzn, flex)
  | mapPaths _ = ()

and mapEPC(epc, sign as SIG { elements, ... }, rlzn: M.strEntity, flex) =
      let val { entities, ... } = rlzn
	  fun doElem(_,TYCspec{entVar=ev, ...}) =
                (*
                 * bind only if tycon is flexible -- have to pass flexibility
                 * tester  -- but wait! what about a rigid structure with a
                 * new signature? Have to record even rigid strs and fcts in
                 * case they have new signatures
                 *)
                (case EE.look(entities,ev)
		   of TYCent tyc =>
		       (case tyc
                          of T.ERRORtyc => ()
			   | _ =>
			      let val stamp = TU.tycStamp tyc
			      in if flex stamp
				 then EPC.bindTycPath(epc, MI.tycId' tyc, ev)
				 else ()
			      end)
	            | ERRORent => ()
		    | _ => bug "mapEPC 1")

            | doElem(_,STRspec{entVar=ev,sign=s,...}) =
                (*
                 * map this structure (unconditionally, because it may
                 * have a different signature)
                 *)
	       (case s  (* don't record ERRORsig -- error tolerance *)
		  of SIG _ =>
		     (case EE.look(entities,ev)
			of STRent nr =>
			    let val i = MU.strId2(s,nr)
			    in case EPC.lookStrPath (epc, i)
				 of SOME _ => ()
				  | _ => (EPC.bindStrPath (epc,i,ev);
					  mapEPC(EPC.enterOpen(epc,SOME ev),
						 s,nr,flex))
			    end
		         | ERRORent => ()
			 | _ => bug "mapEPC 2")
		   | ERRORsig => ())

            | doElem(_,FCTspec{entVar=ev,sign=s,...}) =
                (* map this functor (unconditionally) *)
	       (case s
		  of FSIG _ =>
		     (case EE.look(entities,ev)
			of FCTent nr =>
			    let val i = MU.fctId2(s,nr)
			     in EPC.bindFctPath(epc,i,ev)
			    end
		         | ERRORent => ()
			 | _ => bug "mapEPC 3")
		   | ERRORfsig => ())

            | doElem _ = ()

       in if EPC.isEmpty epc then () else List.app doElem elements
      end

  | mapEPC _ = ()

(*
fun bindReplTyc(EU.INFCT _, epctxt, mkStamp, dtyc) =
     let val ev = mkStamp()
         val tyc_id = MU.tycId dtyc
         val texp =
	     case EPC.lookPath(epContext,tyc_id)
	       of NONE => (debugmsg "tyc not mapped 1"; M.CONSTtyc tyc)
		| SOME entPath => M.VARtyc entPath
      in EPC.bindPath(epctxt, tyc_id, ev);
	 M.TYCdec(ev,texp)
     end
  | bindReplTyc _ = (EE.empty, M.EMPTYdec)
*)


(*
 * ASSERT: order of DEFtycs in tycs respects dependencies, i.e. no
 *         DEFtyc refers to tycons occurring after itself.
 *)
fun bindNewTycs(EU.INFCT _, epctxt, mkStamp, dtycs, wtycs, rpath, err) =
      let fun stripPath path =
	    let val namePath = IP.IPATH[IP.last path]
	        val prefix = IP.lastPrefix path
	        val _ = if IP.equal(rpath,prefix) then ()
		        else err EM.WARN
			     "Harmless compiler bug: bad type path prefix"
			     EM.nullErrorBody
	     in namePath
	    end

	  val vizty = (fn ty => #1(MU.relativizeType epctxt ty))
	  val viztc = (fn tc => #1(MU.relativizeTyc epctxt tc))
           (* this is ok because epContext has state; a bit ugly *)
          val ndtycs =
            (case dtycs
	      of (T.GENtyc { kind, ... } :: _) =>
		 (case kind
		   of T.DATATYPE{index=0,family,freetycs, stamps, root, ...} =>
                      let val rootev = mkStamp()
			  val rtevOp = SOME rootev
			  val nfreetycs = map viztc freetycs
			  val nstamps = Vector.map (fn _ => mkStamp()) stamps

			  fun newdt (dt as T.GENtyc {kind,arity,eq,path,...})=
			      (case kind of
				   T.DATATYPE{index=i,...} =>
				   let val (ev, rtev) =
					   if i=0 then (rootev, NONE)
					   else (mkStamp(), rtevOp)

                                       val nkind =
					   T.DATATYPE{index=i, stamps=nstamps,
						      freetycs=nfreetycs,
						      root=rtev,
						      family=family,
						      stripped=false}
                                       (* the rtev field in DATATYPE indicates
					* how to discover the new stamps when
					* such datatypes get evalent-ed *)

                                       val ndt =
					   T.GENtyc{arity=arity, eq=eq,
						    kind=nkind,
						    path=stripPath path,
						    stamp=
						      Vector.sub(nstamps,i),
						    stub=NONE}

                                       val _ =
					   EPC.bindTycPath(epctxt,
							   MU.tycId dt, ev)
				   in (ev, dt, M.FORMtyc ndt)
				   end
				 | _ => bug "unexpected case in newdtyc (1)")
                            | newdt _ = bug "unexpected case in newdtyc (2)"
                      in map newdt dtycs
                      end
		    | _ => bug "unexpected tycs in bindNewTycs (1)")
	       | [] => []
	       | _ => bug "unexpected tycs in bindNewTycs (2)")

          val nwtycs =
            let fun newtc (tc as T.DEFtyc{stamp, tyfun=T.TYFUN{arity,body},
                                          strict, path}) =
                     let val ev = mkStamp()
                         val _ = EPC.bindTycPath(epctxt, MU.tycId tc, ev)
			 val ntc =
                           T.DEFtyc{stamp=mkStamp(), strict=strict,
				    path=stripPath path,
                                    tyfun=T.TYFUN{arity=arity,
                                                  body=vizty body}}
                      in (ev, tc, M.FORMtyc ntc)
                     end
                  | newtc _ = bug "unexpected case in newwtyc"
             in map newtc wtycs
            end

          fun bind((ev,tc,te)::tcs, entEnv, entDecs) =
                 bind(tcs, EE.bind(ev,M.TYCent(tc),entEnv),
                               M.TYCdec(ev, te)::entDecs)
            | bind(nil, entEnv, entDecs) =
                (EE.mark(mkStamp,entEnv), seqEntDec(rev entDecs))

       in bind(ndtycs@nwtycs, EE.empty, [])
      end

  | bindNewTycs _ = (EE.empty, M.EMPTYdec)


(***************************************************************************
 *                                                                         *
 * extractSig infers the signature for an arbitrary static environment.    *
 *                                                                         *
 * Recompute dynamic accesses after the elaboration of a structure body,   *
 * replacing the original dynamic access by a SLOT and generating a        *
 * thinning that will be used (in translate) to create the structure       *
 * record.                                                                 *
 *                                                                         *
 * Recompute all the dynamic accesses in an environment, suppress doubles  *
 * and allocate slots. Components are ordered so that slot allocation is   *
 * independent from the way elaboration is done.                           *
 *                                                                         *
 * Should use Env.fold or Env.map?                                         *
 *                                                                         *
 ***************************************************************************)
fun extractSig (env, epContext, context,
                compInfo as {mkStamp,...} : EU.compInfo,
	        absDecl) =
  let fun getEpOp (lookfn, modId) =
        case context
          of EU.INFCT _ => lookfn (epContext, modId)
           | _ => NONE

      val relativize =
        case context
	  of EU.INFCT _ => (fn ty => #1(MU.relativizeType epContext ty))
	   | _ => fn x => x

      fun addElems(x, elements) = x::elements

      fun transBind ((sym, binding),
                     (elements, entEnv, entDecl, trans, slotCount, fctflag)) =
        case binding
         of B.VALbind(V.VALvar{typ,path,...}) =>
              let val spec = VALspec{spec=relativize(!typ),
                                     slot=slotCount}
                  val elements' = addElems((sym, spec), elements)
               in (elements', entEnv, entDecl, binding::trans,
                   slotCount+1, fctflag)
              end

          | B.CONbind(dcon as T.DATACON{name,const,lazyp,sign,typ,rep}) =>
              let val typ' = relativize typ
                  val (rep', trans', slotOp, slotCount') =
                    case rep
                     of DA.EXN _ =>
                          (DA.EXN(DA.nullAcc), binding::trans,
                           SOME slotCount, slotCount+1)

                      | _ => (rep, trans, NONE, slotCount)

                  val ndcon = T.DATACON{name=name, const=const, sign=sign,
                                        typ=typ', rep=rep', lazyp=lazyp}

                  val spec = CONspec{spec=ndcon, slot=slotOp}
                  val elements' = addElems((sym, spec), elements)

               in (elements', entEnv, entDecl, trans', slotCount', fctflag)
              end

          | B.STRbind(str as STR { sign, rlzn, ... }) =>
	    let val epOp = getEpOp (EPC.lookStrPath, MU.strId str)
                val (ev, entEnv', entDecl') =
                    case epOp
                     of SOME [x] => (x, entEnv, entDecl)
                      | _ =>
                         (let val x = mkStamp()
                              val ee = EE.bind(x, STRent rlzn, entEnv)
                              val ed =
                                case context
                                 of EU.INFCT _ =>
                                      (let val strExp =
                                             case epOp
                                              of SOME ep => M.VARstr ep
                                               | _ => M.CONSTstr rlzn
                                        in (M.STRdec(x, strExp, sym))::entDecl
                                       end)
                                  | _ => entDecl
                           in (x, ee, ed)
                          end)

                  val spec = STRspec{sign=sign, slot=slotCount, def = NONE,
				     entVar=ev}
                  val elements' = addElems((sym, spec), elements)
                  val fctflag' =
                    (case sign
                      of SIG sg => fctflag orelse #fctflag sg
                       | _ => fctflag)
               in (elements', entEnv', entDecl', binding::trans,
                   slotCount+1, fctflag')
              end

          | B.FCTbind(fct as FCT {sign, rlzn, ...}) =>
	    let val epOp = getEpOp(EPC.lookFctPath, MU.fctId fct)
                val (ev, entEnv', entDecl') =
                    case epOp
                     of SOME [x] => (x, entEnv, entDecl)
                      | _ =>
                         (let val x = mkStamp()
                              val ee = EE.bind(x, FCTent rlzn, entEnv)
                              val ed =
                                case context
                                 of EU.INFCT _ =>
                                      (let val fctExp =
                                             case epOp
                                              of SOME ep => M.VARfct ep
                                               | _ => M.CONSTfct rlzn
                                        in (M.FCTdec(x, fctExp))::entDecl
                                       end)
                                  | _ => entDecl
                           in (x, ee, ed)
                          end)

                  val spec = FCTspec{sign=sign,slot=slotCount,entVar=ev}
                  val elements' = addElems((sym, spec), elements)
               in (elements', entEnv', entDecl', binding::trans,
                   slotCount+1, true)
              end

          | B.TYCbind tyc =>
              let val epOp =
		      case tyc
			of T.ERRORtyc => NONE
			 | _ => getEpOp(EPC.lookTycPath, MU.tycId tyc)
                  val (ev, entEnv', entDecl') =
                    case epOp
                     of SOME [x] => (x, entEnv, entDecl)
                      | _ =>
                         (let val x = mkStamp()
                              val ee = EE.bind(x, TYCent tyc, entEnv)
                              val ed =
                                case context
                                 of EU.INFCT _ =>
                                      (let val tycExp =
                                             case epOp
                                              of SOME ep => M.VARtyc ep
                                               | _ => M.CONSTtyc tyc
                                        in (M.TYCdec(x, tycExp))::entDecl
                                       end)
                                  | _ => entDecl
                           in (x, ee, ed)
                          end)

                  val spec = TYCspec{entVar=ev,
                                     info=InfTycSpec{name=sym,arity=TU.tyconArity tyc}}
                  val elements' = addElems((sym, spec), elements)
                  (*
                   * Use of T.ERRORtyc here is a hack. It relies on the
                   * fact that the inferred signature would never be
                   * instantiated or signature-matched against. One might
                   * wonder what about a functor declaration with no result
                   * signature constraint ? the inferred fctSig would contain
                   * T.ERRORtyc --- fortunately the result-sig in this fctSig
                   * would never be matched against either. (ZHONG)
                   *)

               in (elements', entEnv', entDecl', trans, slotCount, fctflag)
              end

          | _ => (elements, entEnv, entDecl, trans, slotCount, fctflag)

      (* getDeclOrder : absyn -> symbol list
	 getDeclOrder returns the names of all the surface declaractions
	 in decl. We use this function to return the signature elements
         in the same order as the structure element decls. *)
      fun getDeclOrder(decl) =
	  let fun procstrbs([]) = []
		| procstrbs((A.STRB{name,...})::rest) = name::(procstrbs rest)
	      fun procpat(A.VARpat var) = [V.varName var]
		| procpat(A.RECORDpat{fields,...}) =
		    foldl (fn ((_,pat), names) => (procpat pat)@names) []
			  fields
		| procpat(A.APPpat(_,_,pat)) = procpat pat
		| procpat(A.CONSTRAINTpat(pat,_)) = procpat pat
		| procpat(A.LAYEREDpat(pat,pat')) =
		    (procpat pat)@(procpat pat')
		| procpat(A.ORpat(pat,pat')) = (procpat pat)@(procpat pat')
		| procpat(A.VECTORpat(pats,_)) =
		    foldl (fn (pat,names) => (procpat pat)@names) [] pats
		| procpat _ = []
	      fun procvbs [] = []
		| procvbs((A.VB{pat,...})::rest) =
		    (procpat pat)@(procvbs rest)
	      fun proctycs([]) = []
		| proctycs(tyc::rest) = (TU.tycName tyc)::(proctycs rest)
	      fun procdatatycs([]) = []
		| procdatatycs(T.GENtyc{kind=T.DATATYPE dt, path, ...}::rest) =
		    let val {index,family as {members,...},...} = dt
			val {tycname,dcons,...} = Vector.sub(members,index)
			val pathname = InvPath.last path
		    in (map (fn ({name,...}) => name) dcons)@
		       (pathname::procdatatycs rest)
		    end
		| procdatatycs(_) = bug "elabmod: extractSig -- bad datatycs"
	      fun procebs([]) = []
		| procebs((A.EBgen{exn=T.DATACON{name,...},...})::rest) =
		    name::(procebs rest)
		| procebs((A.EBdef{exn=T.DATACON{name,...},...})::rest) =
		    name::(procebs rest)
	      fun procfctbs([]) = []
		| procfctbs(A.FCTB{name,...}::rest) = name::(procfctbs rest)
	      fun procstr(M.STR{sign=M.SIG{elements,...},...}) =
                    MU.getElementsSymbols elements
		| procstr(M.STR{sign=M.ERRORsig,...}) = []
		| procstr(M.STRSIG{sign=M.SIG{elements,...},...}) =
                    MU.getElementsSymbols elements
		| procstr(M.STRSIG{sign=M.ERRORsig,...}) =
		    bug "elabmod: extractSig ERRORsig in STRSIG"
		| procstr(M.ERRORstr) = []
	      fun procrvbs([]) = []
		| procrvbs(A.RVB{var=V.VALvar{path,...},...}::rest) =
		    (SymPath.first path)::(procrvbs rest)
		| procrvbs(_::rest) = bug "elabmod: extractSig -- Bad RVB"
	  in case decl
	      of A.STRdec(strbs) => procstrbs strbs
	       | A.VALdec(vbs) => procvbs vbs
	       | A.VALRECdec(rvbs) => procrvbs rvbs
	       | A.TYPEdec(tycs) => proctycs tycs
	       | A.DATATYPEdec{datatycs,withtycs} =>
		   (rev (proctycs withtycs))@(rev (procdatatycs datatycs))
	       | A.ABSTYPEdec{abstycs,withtycs,body} =>
		   (proctycs abstycs)@(proctycs withtycs)@(getDeclOrder body)
	       | A.EXCEPTIONdec(ebs) => procebs ebs
	       | A.FCTdec(fctbs) => procfctbs fctbs
	       | A.OPENdec(pathstrs) =>
		   foldl (fn (str,names) => (rev (procstr str))@names) []
			 (map #2 pathstrs)
	       | A.LOCALdec(_,dec) => (getDeclOrder dec)
	       | A.SEQdec(decs) =>
		   foldl (fn (dec,names) => (getDeclOrder dec)@names) [] decs
	       | A.MARKdec(dec,_) => getDeclOrder dec
	       | A.FIXdec{ops,...} => ops
	       | A.DOdec _ => []
	       | A.VARSELdec (var, _, _) => [V.varName var]
	       | _ => bug "extractSig: Unexpected dec"
	  end

	(* suppressDuplicates is not strictly necessary for correctness
	   because signature matching will just try to match the duplicate
	   specs to the same type. However, suppressing duplicates will
	   eliminate these extraneous signature match checks.
	   [GK 4/18/07] *)
        fun suppressDuplicates syms =
	    let fun helper([], memset, result) = (memset, result)
		  | helper(s::rest, memset, result) =
		    if ST.member(memset,s)
		    then helper(rest,memset, result)
		    else helper(rest,ST.add(memset,s),s::result)
	    in helper(syms, ST.empty, [])
	    end

	(* Check that the decl names list computed by getDeclOrder is
	   equivalent (up to reordering) to the keys in the static
	   environment. If they are not equal, then getDeclOrder may
	   be missing some decl name. We use the decl names list to
	   order the elements in this extracted/inferred signature.
	   [GK 4/18/07] *)
        val (declnameset, origdeclorder) =
	      suppressDuplicates(getDeclOrder absDecl)

	(* [GK 4/15/07] Consolidate will compact the potentially
	   linear static environment (i.e., BIND(...BIND(...)))
           into a hashtable (IntStrMap) and therefore eliminate
	   any connection between statenv binding order and the
	   structure declaration order. We use getDeclOrder to
           extract the structure decl order and then use
           SE.foldOverElems to compute the elements (specs) in
           the structure decl order on the consolidated list. *)
        val cenv = SE.consolidate env

        val (elements, entEnv, entDecl, trans, _, fctflag) =
          SE.foldOverElems(transBind,(nil, EE.empty, [], [], 0, false),cenv,
			   origdeclorder)
	  handle SE.Unbound => bug "elabmod: extractSig -- SE.foldOverElems \
				    \Unbound symbol in origdeclorder"
     in (rev elements, entEnv, rev entDecl, rev trans, fctflag)
    end (* fun extractSig *)


(****************************************************************************
 * The constrStr function is used to carry out the signature matching
 * on structure declarations with signature constraints. The first argument
 * "transp" is a boolean flag used to indicate whether the signature
 * matching should be done transparently (true) or opaquely (false).
 * (called 3 times within this module).
 ****************************************************************************)
fun constrStr(transp, sign, str, strDec, strExp, evOp, tdepth, entEnv, rpath,
              env, region, compInfo) : A.dec * M.Structure * M.strExp =
  let val _ = debugmsg ">>> constrStr"
      val {resDec=matchedDec, resStr=matchedStr, resExp=matchedExp} =
          SM.matchStr{sign=sign, str=str, strExp=strExp, evOp=evOp,
                      tdepth=tdepth, entEnv=entEnv, statenv=env, rpath=rpath,
                      region=region, compInfo=compInfo}
   in if transp
      then (A.SEQdec[strDec, matchedDec], matchedStr, matchedExp)
      else (* instantiate the signature (opaque match) *)
	   let val STR {rlzn=matchedRlzn, access, prim, ...} = matchedStr
	       val {rlzn=abstractRlzn, ...} =
		   INS.instAbstr {sign=sign, entEnv=entEnv, rlzn=matchedRlzn,
				  rpath=rpath, region=region, compInfo=compInfo}
	       val abstractStr = STR {sign=sign, rlzn=abstractRlzn, access=access, prim=prim}
	       val _ = debugmsg "<<< constrStr[transp=false]"
            in (A.SEQdec[strDec, matchedDec], abstractStr, matchedExp)
	   end
  end (* fun constrStr *)


(*** elabStr: elaborate the raw structure, without signature constraint ***)
(*** several invariants:
      Every structure expression strexp is now elaborated into a quadruple
       (absdec, str, exp, ee) where absdec is the corresponding abstract
      syntax tree, str is the resulting structure, exp is the entity
      expression, and ee is the delta entity environment collected while
      elaborating the current structure expression. The delta EntEnv is
      designed to deal with LetStr and LetFct and to maintain the hidden
      entity environment context.
 *)
fun elabStr
      (strexp: Ast.strexp,
       name: S.symbol option,
       env: SE.staticEnv,
       entEnv: M.entityEnv,
       context: EU.context,
       tdepth : int,
       epContext: EPC.context,
       entsv: EP.entVar option,
       rpath: IP.path,
       region: SourceMap.region,
       compInfo as {mkLvar=mkv, mkStamp, error, ...}: EU.compInfo)
      : A.dec * M.Structure * M.strExp * EE.entityEnv =
let

val sname : string =
    (case name
       of SOME n => S.name n
        | NONE => "<anonymous>")

val depth : int =
    (case context
       of EU.INFCT{depth=d,...} => d
        | _ => DI.top)  (* = 0 *)

val _ = dbsaynl (">>> elabStr: " ^ sname)
val _ = showStrExpAst ("### elabStr: strexp = ", strexp, env)
		   
(* elab: Ast.strexp * staticEnv * entityEnv * region
 *        -> A.dec * M.Structure * M.strExp * EE.entityEnv
 *  subsidiary function for elaborating strexps *)
fun elab (BaseStr decl, env, entEnv, region) =
      let val _ = dbsaynl ">>> elab[BaseStr]"

          (* we enter the epcontext when we get into BaseStr *)
          val epContext' = EPC.enterOpen(epContext,entsv)
          val (absDecl, entDecl, env', entEnv') =
                 elabDecl0(decl, env, entEnv, inStr context, true, tdepth,
                           epContext', rpath, region, compInfo)
          val _ = debugmsg "--elab[BaseStr]: elabDecl0 done"

          val (elements, entEnv'', entDecls, locations, fctflag) =
                extractSig(env', epContext', context, compInfo, absDecl)
          val _ = debugmsg "--elab[BaseStr]: extractSig done"

          val (entEnvLocal, entDecLocal) =
              case context
               of EU.INFCT _ =>
                    (EE.mark(mkStamp,EE.atop(entEnv'',entEnv')),
                     seqEntDec(entDecl::entDecls))
		| _ => (entEnv'', entDecl)

          val strExp = STRUCTURE{stamp=M.NEW, entDec=entDecLocal}

          val resStr =
	      let val symbols = map #1 elements
		  val sign =
		    M.SIG{stamp = mkStamp (),
			  name=NONE, closed=false, fctflag=fctflag,
			  elements=elements,
			  properties = PropList.newHolder (),
			  typsharing=nil, strsharing=nil,
			  stub=NONE}

		  val strRlzn =
		      { stamp = mkStamp(), (* generate structure stamp *)
			entities=EE.mark(mkStamp,EE.atop(entEnvLocal, entEnv)),
			properties = PropList.newHolder (),
			rpath=rpath, stub = NONE}

		  val dacc = DA.namedAcc(tempStrId, mkv)
		  val prim = MU.strPrimElemInBinds locations

	       in M.STR {sign=sign, rlzn=strRlzn, access=dacc,
			 prim=prim}
	      end

          val _ = debugPrint("BaseStr after resStr  - symbols: ", ED.ppSymList,
                             ED.envSymbols env')
          (* absyn *)
          val resDec =
            let val body = A.LETstr(absDecl, A.STRstr(locations))
             in A.STRdec [A.STRB {name=tempStrId, str=resStr, def=body}]
            end

          val _ = debugmsg "<<elab[BaseStr]"

       in (resDec, resStr, strExp, EE.empty)
      end

  | elab (AppStr(spath,args), env, entEnv, region) =
      let val strexp' = LetStr(StrDec[Strb{name=returnId,constraint=NoSig,
					   def=AppStrI(spath,args)}],
			       VarStr([returnId,resultId]))
       in elab(strexp', env, entEnv, region)
      end

  | elab (AppStrI(spath,[(arg,b)]), env, entEnv, region) =
      let val _ = debugmsg ">>elab[AppStr-one]"

          val fct = LU.lookFct(env, SP.SPATH spath, error region)

          val _ = debugmsg "--elab[AppStr-one]: functor lookup done"
          val _ = showFct("--elab[AppStr]: functor ",fct,env)

          val entv = mkStamp()   (* ev for the uncoerced argument *)
          val (argDec, argStr, argExp, argDee) =
	      elabStr(arg, NONE, env, entEnv, context, tdepth, epContext,
		      SOME entv, IP.IPATH[], region, compInfo)

          val _ = debugmsg "--elab[AppStr-one]: elab arg done"
          val _ = showStr ("--elab[AppStr-one]: arg str: ", argStr, env)

       in case (fct,argStr)
	    of (M.ERRORfct, _) =>
		(debugmsg "<<<elab[AppStr-one]: error functor";
		 (A.SEQdec[], M.ERRORstr, M.CONSTstr(M.bogusStrEntity), EE.empty))
	     | (_, M.ERRORstr) =>
		(debugmsg "<<<elab[AppStr-one]: error arg structure";
		 (A.SEQdec[], M.ERRORstr, M.CONSTstr(M.bogusStrEntity), EE.empty))
	     | (M.FCT { rlzn = fctEnt, ... }, M.STR { rlzn = argEnt, ... }) =>
	       let val resDee =
		       EE.mark(mkStamp, EE.bind(entv, M.STRent argEnt, argDee))
		        (* the argument structure should be bound to entv *)
		    val fctExp =
			case EPC.lookFctPath(epContext, MU.fctId fct)
			  of SOME ep => VARfct ep
			   | NONE => CONSTfct fctEnt
		    val {resDec, resStr, resExp} =
			SM.applyFct{fct=fct, fctExp=fctExp, argStr=argStr,
				    argExp=argExp, evOp = SOME entv, tdepth=depth,
				    epc=EPC.enterOpen(epContext,entsv),
				    statenv=env, rpath=rpath, region=region,
				    compInfo=compInfo}
		    val _ = debugmsg "--elab[AppStr-one]: applyFct done"
		    val _ = showStr("--elab[AppStr-one]: result: ",resStr,env)
		    val _ = debugmsg "<<elab[AppStr-one]"
		 in (A.SEQdec [argDec, resDec], resStr, resExp, resDee)
		end
	     | _ => bug "AppStrI:one arg"
      end (* AppStrI - one arg *)

  | elab (AppStrI(spath,arg :: larg), env, entEnv, region) =
      let val _ = debugmsg ">>elab:[AppStr-many]"
          val strexp' = LetStr(StrDec[Strb{name=hiddenId,constraint=NoSig,
                                           def=AppStrI(spath,[arg])}],
                               AppStrI([hiddenId,functorId],larg))
       in elab(strexp', env, entEnv, region)
      end (* AppStrI - multiple args *)

  | elab (AppStrI(spath,[]), env, entEnv, region) =
      bug "elabStr.AppStrI -- empty arg list"

  | elab (v as VarStr path, env, entEnv, region) =
      let val _ = debugmsg ">>elab[VarStr]"
          val str = LU.lookStr(env,SP.SPATH path,error region)

          val _ = showStr("--elab[VarStr]: str: ",str,env)

          val strRlzn =
		case str
		  of STR { rlzn, ... } => rlzn
		   | _ => M.bogusStrEntity  (* error recovery *)
          val resExp =
	        case str
		  of STR _ =>
		      ((* debugmsg "--elab[VarStr]: resExp/STR"; *)
		       case EPC.lookStrPath(epContext,MU.strId str)
			 of NONE => M.CONSTstr strRlzn
			  | SOME ep => M.VARstr ep)
		   | _ => M.CONSTstr M.bogusStrEntity (* error recovery *)

       in debugmsg "<<<elab[VarStr]";
	  (A.SEQdec [], str, resExp, EE.empty)
      end

  | elab (LetStr(decl,str), env, entEnv, region) =
      let val _ = debugmsg ">>elab[LetStr]"
          val (localDecAbsyn, localEntDecl, env', entEnv') =
            elabDecl0(decl, env, entEnv, context, true, tdepth,
                      epContext, rpath, region, compInfo)
	     (* top = true: don't allow nongeneralized type variables
              * in local decls because of bug 905/952.  This is
	      * stricter than necessary.  Could allow top = false
	      * if the body str contains no functors.  To make the
              * condition more precise, have to synthesize a boolean
	      * attribute indicating presence of functors [dbm] *)
                (*
                 * DAVE? what context to use for the local decls?
                 * perhaps should null bindContext as for functor body?
                 * perhaps it doesn't matter because of relativization
                 * and the fact that local entities can't be referred
                 * to from outside.
                 *)
          val _ = debugmsg "--elab[LetStr]: local elabDecl0 done"
          val (bodyDecAbsyn, bodyStr, bodyExp, bodyDee) =
            elab(str, SE.atop(env',env), EE.atop(entEnv',entEnv), region)

          val resDec = A.SEQdec [localDecAbsyn, bodyDecAbsyn]
          val resExp = M.LETstr (localEntDecl, bodyExp)
          val _ = debugmsg "<<<elab[LetStr]: elab body str done"

       in (resDec, bodyStr, resExp, EE.mark(mkStamp,EE.atopSp(bodyDee,entEnv')))
      end

  | elab (ConstrainedStr(strexp,constraint), env, entEnv, region) =
    let val (entsv, evOp, csigOp, transp) =
            let fun elabsig sigexp =
                  ES.elabSig {sigexp=sigexp, nameOp=NONE, env=env,
                              entEnv=entEnv, epContext=epContext,
                              region=region, compInfo=compInfo}

                val (csigOp, transp) =
		    (case constraint
		      of Transparent x => (SOME (elabsig x), true)
		       | Opaque x => (SOME (elabsig x), false)
		       | NoSig => (NONE, true))  (* can this happen? *)

                val (entsv, evOp) =
		    (case constraint
		       of NoSig => (entsv, NONE)
		        | _ => let val nentv = SOME(mkStamp())
			        in (nentv, nentv)
			       end)

             in (entsv, evOp, csigOp, transp)
            end

          (** elaborating the structure body *)
          val (strDecAbsyn, str, exp, deltaEntEnv) =
            elabStr(strexp, NONE, env, entEnv, context, tdepth,
                    epContext, entsv, rpath, region, compInfo)

          val resDee =
	      case constraint
		of NoSig => deltaEntEnv
		 | _ =>
		   (case evOp
		     of SOME tmpev =>
			 let val strEnt =
			       case str of M.STR { rlzn, ... } => rlzn
					 | _ => M.bogusStrEntity
			  in (EE.bind(tmpev, M.STRent strEnt, deltaEntEnv))
			 end
		      | _ => bug "unexpected while elaborating constrained str")

          (** elaborating the signature matching *)
          val (resDec, resStr, resExp) =
              case csigOp
		of NONE =>
		     (if transp then ()
		      else (error region EM.COMPLAIN
			    "missing signature in abstraction declaration"
			    EM.nullErrorBody);
		      (strDecAbsyn, str, exp))
		 | SOME csig =>
                      constrStr(transp, csig, str, strDecAbsyn, exp,
                                evOp, depth, entEnv, rpath,
                                env, region, compInfo)

       in (resDec, resStr, resExp, resDee)
      end

  | elab (MarkStr(strexp',region'),env,entEnv,region) =
      let val (resDec, str, resExp, resDee) =
            elab(strexp', env, entEnv, region')
       in (A.MARKdec(resDec, region'), str, resExp, resDee)
      end

val (resDec, resStr, resExp, resDee) = elab(strexp, env, entEnv, region)
val _ = showStr ("<<<elabStr: resStr = ", resStr, env)

in (resDec, resStr, resExp, resDee)
end (* end of function elabStr *)


(*** elabFct: elaborate the functor, possibly with signature constraint ***)
and elabFct
      (fctexp: Ast.fctexp,
       curried : bool,
       name: S.symbol,
       env: SE.staticEnv,
       entEnv: M.entityEnv,
       context: EU.context,
       tdepth : int,
       epContext: EPC.context,
       rpath: IP.path,
       region: SourceMap.region,
       compInfo as {mkLvar=mkv, mkStamp, error, ...}: EU.compInfo)
      : A.dec * M.Functor * M.fctExp * EE.entityEnv =

let

val depth = (case context of EU.INFCT{depth=d,...} => d
                           | _ => DI.top)
val _ = debugmsg (">>elabFct: " ^ (S.name name))

in

case fctexp
 of VarFct(spath,constraintExpOp) =>
      let val fct = LU.lookFct(env,SP.SPATH spath,error region)
       in case fct
	    of ERRORfct =>
		(A.SEQdec [], fct, CONSTfct(M.bogusFctEntity), EE.empty)
	     | _ =>
		let val uncoercedExp =
		        (case EPC.lookFctPath(epContext, MU.fctId fct)
			   of SOME ep => VARfct ep
			    | NONE =>
			       let val rlzn = case fct of FCT ft => #rlzn ft
							| _ => M.bogusFctEntity
			        in CONSTfct rlzn
			       end)

		 in case constraintExpOp
		      of NoSig => (A.SEQdec [], fct, uncoercedExp, EE.empty)
		       | Transparent astFsig =>
			  let val nameOp = SOME(anonfsigId)
			      val fsig =
				  ES.elabFctSig
				    {fsigexp=astFsig, nameOp=nameOp, env=env,
				     entEnv=entEnv, epContext=epContext,
				     region=region, compInfo=compInfo}

			      val {resDec, resFct, resExp} =
				  SM.matchFct
				    {sign=fsig, fct=fct, fctExp=uncoercedExp,
				     tdepth=depth, entEnv=entEnv,
				     rpath=rpath, statenv=env, region=region,
				     compInfo=compInfo}
			   in (resDec, resFct, resExp, EE.empty)
			  end
		       | Opaque astFsig =>
                          bug "Opaque functor constraint not impl"
		end
      end

  | LetFct(decl,fct) =>
      let val _ = debugmsg ">>elab[LetFct]"
          val (localDecAbsyn, localEntDecl, env', entEnv') =
            elabDecl0(decl, env, entEnv, context, true, tdepth,
	              epContext, rpath, region, compInfo)
           (* top = true: don't allow nongeneralized type variables
              in local decls because of bug 905/952 [dbm] *)
          val _ = debugmsg "--elab[LetFct]: local elabDecl0 done"
          val (bodyDecAbsyn, bodyFct, bodyExp, bodyDee) =
            elabFct(fct, false, name, SE.atop(env',env), EE.atop(entEnv',entEnv),
                    context, tdepth, epContext, rpath, region, compInfo)

          val resAbsyn = A.SEQdec [localDecAbsyn, bodyDecAbsyn]
          val resExp = M.LETfct(localEntDecl, bodyExp)
          val resDee = EE.mark(mkStamp, EE.atopSp(bodyDee, entEnv'))

       in (resAbsyn, bodyFct, resExp, resDee)
      end

  | AppFct(spath,larg,constraint) =>
      let val fctexp' = LetFct(StrDec[Strb{name=hiddenId,constraint=NoSig,
                                           def=AppStrI(spath,larg)}],
                               VarFct([hiddenId,functorId],constraint))

       in elabFct(fctexp', false, name, env, entEnv, context, tdepth, epContext,
                  rpath, region, compInfo)
      end

  | BaseFct{params=[(paramNameOp,paramSigExp)],body,constraint} =>
      let val _ = debugmsg ">>elabFct[BaseFct]"
	  val body = if curried then body
		     else BaseStr(StrDec[Strb{name=resultId, def=body,
					      constraint=constraint}])
	  val constraint = if curried then constraint else NoSig
          val (flex, depth) =
            case context
             of EU.INFCT {flex=f,depth=d} => (f, d)
              | _ => (*** Entering functor for first time ***)
                 let val base = mkStamp()
                     fun h s = (case Stamps.compare(base,s)
                                 of LESS => true
                                  | _ => false)
                  in (h, DI.top)
                 end
          val paramName = case paramNameOp of NONE => paramId
                                            | SOME n => n

          val paramEntVar = mkStamp()

	  val _ = debugmsg (">>elabFct[BaseFct]: paramEntVar = "^
			    EP.entVarToString paramEntVar)

          val paramSig =
              ES.elabSig {sigexp=paramSigExp, nameOp=NONE, env=env,
                          entEnv=entEnv, epContext=epContext,
                          region=region, compInfo=compInfo}
          val _ = debugmsg "--elabFct[BaseFct]: paramSig defined"

	  val _ = case paramSig
		    of ERRORsig => raise EM.Error
		        (* bail out -- not attempting to recover *)
		     | _ => ()

	  (* now know that paramSig is defined *)
          (* this creates new stamps, but we don't bother to update the
             epcontext, we do it later through mapPaths *)
          val {rlzn=paramRlzn, tycpaths=paramTps} =
                INS.instParam
                  {sign=paramSig, entEnv=entEnv, region=region,
		   tdepth=depth, rpath=IP.IPATH(case paramNameOp
						 of NONE => []
						  | _ => [paramName]),
		   compInfo=compInfo}

          val paramStr =
              let val paramDacc = DA.namedAcc(paramName, mkv)
               in M.STR{sign=paramSig, rlzn=paramRlzn,
                        access=paramDacc, prim=[]}
              end

          val _ = debugmsg "--elabFct[BaseFct]: param instantiated"
          val _ = showStr("--elabFct[BaseFct]: paramStr: ",paramStr,env)

          val entEnv' =
               EE.mark(mkStamp,EE.bind(paramEntVar,M.STRent paramRlzn,entEnv))
          val _ = debugmsg "--elabFct[BaseFct]: param EE.bind"

	  val _ = debugmsg "elabmod before env'\n"
          val env' =
              case paramNameOp
                of NONE => MU.openStructure(env,paramStr)
                 | SOME _ => SE.bind(paramName, B.STRbind paramStr, env)
          val _ = debugmsg "--elabFct[BaseFct]: param bound/opened"

          val epContext' = EPC.enterClosed epContext

          (* fill in pathEnv with paths for elements of paramStr *)
          val _ = mapPaths(EPC.enterOpen(epContext', SOME paramEntVar),paramStr,flex)
          val _ = EPC.bindStrPath(epContext',MU.strId paramStr,paramEntVar)
          val _ = debugmsg "--elabFct[BaseFct]: epContext initialized"

          (* must elaborate result signature before the body is elaborated
	     so that epContext' is not changed *)
          val (entsv, csigOp, csigTrans) =
	      let fun doSig x =
		      ES.elabSig{sigexp=x, nameOp=NONE, env=env',
				 entEnv=entEnv', epContext=epContext',
				 region=region, compInfo=compInfo}
	       in case constraint
		   of NoSig => (NONE, NONE, true)
		    | Transparent x => (SOME(mkStamp()), SOME (doSig x), true)
		    | Opaque x =>      (SOME(mkStamp()), SOME (doSig x), false)
	      end

          val _ = debugmsg "--elabFct[BaseFct]: result signature elaborated"

          (* adjust the EU.context value; the depth refers to the number
             of enclosing functor abstractions. (ZHONG) *)
          val depth' = DI.next depth
          val context' = EU.INFCT{flex=flex, depth=depth'}

          (* bodyDee was discarded here; however, it was not discarded when
             functor is applied. *)
          val (bodyDecAbsyn, bodyStr, bodyExp, bodyDee) =
              elabStr(body, NONE, env', entEnv', context', tdepth, epContext', entsv,
                      IP.IPATH [], region, compInfo)
          val _ = debugmsg "--elabFct[BaseFct]: body elaborated"
          val _ = showStr("--elabFct[BaseFct]: bodyStr: ",bodyStr,env)

          (* constrain by result signature, either transparent or opaque *)
          val (bodyDecAbsyn', bodyStr', bodyExp') =
              case csigOp
                of NONE => (bodyDecAbsyn, bodyStr, bodyExp)
                 | SOME csig =>
		   constrStr(csigTrans, csig, bodyStr, bodyDecAbsyn, bodyExp,
			     entsv, depth', entEnv', IP.IPATH[], env',
			     region, compInfo)

          val _ = debugmsg "--elabFct[BaseFct]: body constrained"

          val fctExp = M.LAMBDA{param=paramEntVar,body=bodyExp'}

          val resFct =
	      case bodyStr'
		of ERRORstr => ERRORfct (* result sig match failed *)
		 | STR { sign, ... } =>
		   let val fctSig =
			   M.FSIG{kind=NONE, paramsig=paramSig,
				  bodysig=sign, paramvar=paramEntVar,
				  paramsym=paramNameOp}

                       val rlzn = { stamp = mkStamp(),
				    closure=M.CLOSURE{param=paramEntVar,
						      body=bodyExp',
						      env=entEnv},
				    tycpath=NONE,
				    properties = PropList.newHolder (),
				    rpath=rpath,
				    stub = NONE}
                          (* Closure: Using the old entity environment !! *)

                       val dacc = DA.namedAcc(name, mkv)

		    in M.FCT{sign=fctSig, rlzn=rlzn, access=dacc, prim=[]}
		   end
		 | STRSIG _ => bug "elabFct"

          val _ = debugmsg "--elabFct[BaseFct]: resFct defined"

          val resDec =
            let val x = A.FCTfct{param=paramStr, argtycs=paramTps,
                                 def=A.LETstr(bodyDecAbsyn',A.VARstr bodyStr')}
             in A.FCTdec [A.FCTB {name=name, fct=resFct, def=x}]
            end

          val _ = debugmsg "<<elabFct[BaseFct]"
          val _ = showStr("--elabFct[BaseFct]: paramStr: ",paramStr,env)

       in (resDec, resFct, fctExp, EE.empty)
      end

  | BaseFct{params=param :: lparam,body,constraint} =>
      let val fctexp' =
            BaseFct{params=[param],
		    body=BaseStr(
                           FctDec[Fctb{name=functorId,
                                       def=BaseFct{params=lparam, body=body,
                                                  constraint=constraint}}]),
		    constraint=NoSig}

       in elabFct(fctexp', true, name, env, entEnv, context, tdepth, epContext,
                  rpath, region, compInfo)
      end

  | BaseFct{params=[],...} => bug "elabFct"

  | MarkFct(fctexp',region') =>
      elabFct(fctexp', curried, name, env, entEnv, context, tdepth, epContext,
              rpath, region', compInfo)

end (* end of function elabFct *)


(*** elabStrbs: elaborate structure bindings, with signature constraint ***)
and elabStrbs(strbs: Ast.strb list,
              transp: bool,
              env0: SE.staticEnv,
              entEnv0: M.entityEnv,
              context: EU.context,
              tdepth : int,
              epContext: EPC.context,
              rpath: IP.path,
              region: SourceMap.region,
              compInfo as {mkStamp,mkLvar=mkv,error,...}: EU.compInfo)
            : A.dec * M.entityDec * SE.staticEnv * entityEnv =
let

val depth = (case context
	      of EU.INFCT{depth=d, ...} => d
               | _ => DI.top)

val _ = debugmsg ">>elabStrbs"

fun loop([], decls, entDecls, env, entEnv) =
      let val _ = debugmsg "<<elabStrbs"
          val resDec = A.STRdec (rev decls)
          val entDec = case entDecls of [] => M.EMPTYdec
                                      | _ => seqEntDec(rev entDecls)
       in (resDec, entDec, env, entEnv)
      end

  | loop(strb::rest, decls, entDecls, env, entEnv) =
      let val _ = debugmsg ">>elabStrbs"
          val (name, constraint, def, region') =
              case stripMarkStrb(strb,region)
                of (Strb{name=n,constraint=c,def=d},r) => (n, c, d, r)
                 | _ => bug "non structure bindings in elabStrbs"
          val _ = debugmsg("--elabStrbs: structure "^S.name name)

          val baseEntVar = mkStamp ()
          (* make up an entity variable for the current str declaration
           * entsv is the context for evaluating the right-handside
           * of a structure declaration *)
          val strbEntVar = mkStamp()   (* we don't always have to do this ?? *)

	  fun elabConstraint x =
	      ES.elabSig {sigexp=x, nameOp=NONE, env=env0,
			  entEnv=entEnv0, epContext=epContext,
			  region=region, compInfo=compInfo}

          val (csigOp, transp) =
	      (case constraint
		of Transparent x => (SOME(elabConstraint x), transp)
		 | Opaque x =>
		   (case elabConstraint x
		     of ERRORsig => (SOME(ERRORsig), transp)
		      | s => (SOME s, false))
		 | NoSig => (NONE, transp))

	  (* the temporary anonymous structure *)
	  val (strbEntVar, evOp) =
	      (case csigOp
		of (NONE | SOME(ERRORsig)) => (baseEntVar, NONE)
		 | _ => let val newEntv = mkStamp()
			 in (newEntv, SOME newEntv)
			end)

          (* strbEntVar = baseEntVar and evOp = NONE unless there is a constraint
           * signature that elaborates successfully, in which case a new
           * entvar is created for strbEntVar and also used for evOp. *)

          (** elaborating the structure body *)
          val (strDecAbsyn, str, exp, deltaEntEnv) =
              elabStr(def, SOME name, env0, entEnv0, context, tdepth, epContext,
                      SOME strbEntVar, IP.extend(rpath,name), region', compInfo)

          val _ = showStr ("--elabStrbs: elabStr returns str =", str, env)

	  (* check for partially applied curried functor *)
          (* [DBM,2020.4.24]. This check is buggy (bug 246), because the structure
           * can be the indirect (via returnId) result of a AppStrI functor
           * application, but that functor application may have "failed"
	   * because the functor is/should be ERRORfct. Perhaps it will
           * suffice to check that str.resultId is a valid structure (since
           * it is the "actual" functor application result).*)
	  val str = if S.eq(name,returnId) then
	            (* str should be functor application wrapper structure
		     * with single structure component "resultStr" *)
                       if (case str
                             of ERRORstr => true
                              | _ => (case MU.getStrSymbols str
                                        of [sym] => S.eq(sym,resultId)
                                         | _ => false))
                       then str
                       else (error region' EM.COMPLAIN
                             ("structure " ^ S.name(IP.last rpath) ^
                              " defined by partially applied functor")
                             EM.nullErrorBody;
                             ERRORstr)
		    else str

          val _ = debugmsg "--elabStrbs: elabStr done"
(*
	  val _ = showStr("unconstrained structure: ",str,env)
*)
          (** elaborating the signature matching: notice that we did
              introduce stamps during the abstraction matching, but
              these stamps are always visible, thus will always be
              caught by the post sig-matching "mapPaths" function call. *)
          val (resDec, resStr, resExp) =
              case csigOp
	       of (NONE | SOME(ERRORsig)) =>
		    (if transp then ()
		     else (error region' EM.COMPLAIN
			   "missing signature in abstraction declaration"
			    EM.nullErrorBody);
		     (strDecAbsyn, str, exp))
		| SOME csig =>
                    constrStr(transp, csig, str, strDecAbsyn, exp,
                              evOp, depth, entEnv0, IP.IPATH[name],
                              StaticEnv.atop(env,env0), region, compInfo)

          val deltaEntEnv =
              case (evOp, csigOp)
               of (NONE, (NONE | SOME(ERRORsig))) => deltaEntEnv
                | (SOME ev, SOME _) =>
                    (case str
                      of M.STR { rlzn, ... } =>
                          EE.bind(ev, M.STRent rlzn, deltaEntEnv)
                       | _ =>
                          EE.bind(ev, M.STRent M.bogusStrEntity, deltaEntEnv))
                | _ => bug "unexpected case in elabStrbs: deltaEntEnv"

          val _ = debugmsg "--elabStrbs: constrain done"

          val _ = showStr ("--elabStrbs: resStr: ", resStr, env)
          (*
           * WARNING: bindStr modifies the access field of resStr; this
           * may create structures with the same modIds but different dynamic
           * accesses --- BUT, we assume that before or during the pickling,
           * both the dynamic access and the [inl_info: obsolete] will be updated
           * completely and replaced with proper persistent accesses (ZHONG)
           *)
          (* [KM ???] What is the purpose of changing the dynamic access? *)
          val (bindStr, strEnt) =
            case resStr
             of STR { rlzn, sign, access, prim } =>
                (STR{rlzn = rlzn, sign = sign,
		     access = DA.namedAcc(name, mkv), prim = prim},
                 M.STRent rlzn)
              | _ => (resStr, M.STRent M.bogusStrEntity)

(*          val _ = showStr("--elabStrbs: bindStr: ",bindStr,env) -- redundant *)

          val sb = A.STRB{name = name, str = bindStr,
                          def = A.LETstr(resDec, A.VARstr resStr)}
          val decls' = sb :: decls

          val (entEnv', entDecls') =
	      (case context
		of EU.INFCT {flex,...} =>
		     (let val entEnv1 = EE.atopSp(deltaEntEnv, entEnv)
			  val entEnv2 = EE.bind(baseEntVar, strEnt, entEnv1)
			  val entEnv3 = EE.mark(mkStamp, entEnv2)

			  val _ = debugmsg "--elabStrbs: about to mapPaths bindStr"
			  (*
			   * We are remapping entPaths for elements of
			   * the new structure unconditionally, even if
			   * there is no signature constraint and the
			   * defining strexp is a BaseStr (DBM).
			   *)
			  val _ = mapPaths(EPC.enterOpen(epContext, SOME baseEntVar),
					   bindStr, flex)
			  val _ = debugmsg "--elabStrbs: mapPaths bindStr done"
			  val _ = (case bindStr
				    of STR { sign, rlzn, ... } =>
				       EPC.bindStrPath(epContext,
						       MU.strId2(sign,rlzn), baseEntVar)
				     | _ => ())
		      in  (entEnv3, ((M.STRdec(baseEntVar, resExp, name))::entDecls))
		      end)
		  | _ => (entEnv, entDecls))

          val env' = SE.bind(name, B.STRbind bindStr, env)

       in loop(rest, decls', entDecls', env', entEnv')
      end   (* function loop (strb::rest) *)

 in loop(strbs, [], [], SE.empty, EE.empty)
      handle EE.Unbound =>
	     (debugmsg "$elabStrbs0(StrDec): EE.Unbound";
              raise EE.Unbound)

end (* function elabStrbs *)


(*** elabDecl0: elaborate an arbitrary module-level declaration ***)
and elabDecl0
      (decl: Ast.dec,
       env0: SE.staticEnv,
       entEnv0: M.entityEnv,
       context: EU.context,
       top: bool,
       tdepth: int,
       epContext: EPC.context,
       rpath: IP.path,
       region: SourceMap.region,
       compInfo as {mkStamp,mkLvar=mkv,error,anyErrors,transform,...}
         : EU.compInfo)
      : A.dec * entityDec * SE.staticEnv * entityEnv =

(case decl
  of StrDec strbs =>
       elabStrbs(strbs, true, env0, entEnv0, context, tdepth, epContext,
                 rpath, region, compInfo)

   | OpenDec paths =>
       let val err = error region
           val strs = map (fn s => let val sp = SP.SPATH s
                                    in (sp, LU.lookStr(env0, sp, err))
                                   end) paths
           fun loop([], env) = (A.OPENdec strs, M.EMPTYdec, env, EE.empty)
             | loop((_, s)::r, env) = loop(r, MU.openStructure(env, s))

        in loop(strs, SE.empty)
       end

   | FctDec fctbs =>
       let val _ = debugmsg ">>elabFctbs"

           fun loop([], decls, entDecls, env, entEnv) =
                 let val resDec = A.FCTdec (rev decls)
                     val entDec = case entDecls of [] => M.EMPTYdec
                                                 | _ => seqEntDec(rev entDecls)
                     val _ = debugmsg "<<elabFctbs"
                  in (resDec, entDec, env, entEnv)
                 end

             | loop(fctb::rest, decls, entDecls, env, entEnv) =
                 let val (name, def, region') =
                       case stripMarkFctb(fctb,region)
                        of (Fctb{name=n, def=d}, r) => (n, d, r)
                         | _ => bug "non functor bindings for FctDec fctbs"
                     val _ = debugmsg("--elabDecl0: functor "^S.name name)

                     (* dynamic access is already assigned in elabFct *)
                     val (fctDecAbsyn, fct, fctExp, deltaEntEnv) =
                         elabFct(def, false, name, env0, entEnv0, context, tdepth,
				 epContext, rpath, region', compInfo)

                     (*
                      * WARNING: bindFct modifies the access field of fct;
                      * this may create functors with same modIds but
                      * different dynamic accesses --- BUT, we assume that
                      * before or during the pickling, both the dynamic
                      * access and the [inl_info] will be updated completely
                      * and replaced with proper persistent accesses (ZHONG)
                      *)
                     val (bindFct, fctEnt) =
                       case fct
			 of FCT {rlzn, sign, access, prim} =>
			    (FCT{rlzn = rlzn, sign = sign, prim = prim,
                                 access = DA.namedAcc(name, mkv)},
			     FCTent rlzn)
			  | ERRORfct => (fct, ERRORent)

                     val fb = A.FCTB{name = name, fct = bindFct,
                                     def = A.LETfct(fctDecAbsyn,A.VARfct fct)}
                     val decls' = fb :: decls

                     val (entEnv', entDecls') =
                       case context
                        of EU.INFCT _ =>
                            (let val entVar = mkStamp()
                                 val _ = case bindFct
				           of FCT _ =>
					       EPC.bindFctPath(epContext,
                                                 MU.fctId bindFct, entVar)
					    | ERRORfct => ()
                                 val entEnv1 = EE.atopSp(deltaEntEnv, entEnv)
                                 val entEnv2 = EE.bind(entVar, fctEnt, entEnv1)
                                 val entEnv3 = EE.mark(mkStamp, entEnv2)
                              in (entEnv3, M.FCTdec(entVar,fctExp)::entDecls)
                             end)
                         | _ => (entEnv, entDecls)

                     val env' = SE.bind(name, B.FCTbind bindFct, env)

                  in loop(rest, decls', entDecls', env', entEnv')
                 end

        in loop(fctbs, nil, nil, SE.empty, EE.empty)
           handle EE.Unbound => (debugmsg("$elabDecl0: FctDec");
                                raise EE.Unbound)
       end

   | SigDec sigbs =>
       let val _ = debugmsg ">>elabSigbs"

           fun loop([], sigs, env) =
                 let val _ = debugmsg "<<elabSigbs"
                  in (A.SIGdec (rev sigs), M.EMPTYdec, env, EE.empty)
                 end

             | loop (sigb::rest, sigs, env) =
                 let val (name, def, region') =
                       case stripMarkSigb(sigb,region)
                        of (Sigb{name=n,def=d},r) => (n, d, r)
                         | _ => bug "non signature bindings for SigDec sigbs"
                     val _ = debugmsg("--elabDecl0: signature "^S.name name)

                     val s =
                       ES.elabSig {sigexp=def, nameOp=SOME name, env=env0,
                                   entEnv=entEnv0, epContext=epContext,
                                   region=region', compInfo=compInfo}
		     val _ = (* instantiate to check well-formedness,
                              * but only if there have been no earlier errors *)
			if !ElabControl.instantiateSigs
                           andalso not(!(#anyErrors compInfo))
			then (INS.instParam
			        {sign=s,entEnv=EE.empty,tdepth=DI.top,
				 rpath=InvPath.empty,region=region',
				 compInfo=compInfo};
			      ())
			else ()
                  in loop(rest, s::sigs, SE.bind(name, B.SIGbind s, env))
                 end

        in loop(sigbs, nil, SE.empty)
           handle EE.Unbound => (debugmsg("$elabDecl0: SigDec");
                                raise EE.Unbound)
       end

   | FsigDec fsigbs =>
       let val _ = debugmsg ">>elabFSigbs"

           fun loop([], fsigs, env) =
                 let val _ = debugmsg "<<elabFSigbs"
                  in (A.FSIGdec(rev fsigs), M.EMPTYdec, env, EE.empty)
                 end

             | loop (fsigb::rest, fsigs, env) =
                 let val (name, def, region') =
                       case stripMarkFsigb(fsigb,region)
                         of (Fsigb{name=n,def=d},r) => (n, d, r)
                          | _ => bug "non fctSig bindings for FsigDec fsigbs"
                     val _ = debugmsg("--elabDecl0: fctsig "^S.name name)

                     val s =
                       ES.elabFctSig {fsigexp=def, nameOp=SOME name, env=env0,
                                      entEnv=entEnv0, epContext=epContext,
                                      region=region', compInfo=compInfo}
                  in loop(rest, s::fsigs, SE.bind(name, B.FSGbind s, env))
                 end

        in loop(fsigbs, nil, SE.empty)
           handle EE.Unbound => (debugmsg("$elabDecl0: FsigDec");
                                 raise EE.Unbound)
       end

   | LocalDec(decl_in,decl_out) =>
       let val top_in = EU.hasModules decl_in orelse EU.hasModules decl_out
	       (* if decl_in contains a functor declaration (at
		* any nesting depth, have to suppress ungeneralized
		* type variables to avoid bug 905/952.  Using
		* EU.hasModules is a cheap conservative approximation to
		* checking for the presence of functor declarations, though
		* it excludes some legal SML 96 programs where structures
		* but not functors are present. *)
	   val (absyn_in, entDecl_in, env_in, entEnv_in) =
               elabDecl0(decl_in, env0, entEnv0, context, top_in, tdepth,
			 epContext, rpath, region, compInfo)

           (*** DAVE? what is the right epContext to pass here? ***)
           val env = SE.atop (env_in, env0)
           val entEnv = EE.mark (mkStamp, EE.atop (entEnv_in, entEnv0))
           val (absyn_out, entDecl_out, env_out, entEnv_out) =
               elabDecl0(decl_out, env, entEnv, context, top, tdepth,
			 epContext, rpath, region, compInfo)

           val resAbsyn = A.LOCALdec (absyn_in, absyn_out)

           val (entDec, resEE) =
             case context
              of EU.INFCT _ =>
                   (localEntDec(entDecl_in,entDecl_out),
                    EE.mark(mkStamp,EE.atop(entEnv_out,entEnv_in)))
               | _ => (M.EMPTYdec, EE.empty)

        in (resAbsyn, entDec, env_out, resEE)
       end

   | SeqDec decls =>
       let val _ = debugmsg ">>elabSeqDec"

           fun loop([], asdecls, entdecls, env, entEnv) =
                 let val resAbsyn = A.SEQdec(rev asdecls)
                     val (entDec', entEnv') =
                         case context
			   of EU.INFCT _ =>
                               (seqEntDec(rev entdecls), entEnv)
			    | _ => (M.EMPTYdec, EE.empty)

                     val _ = debugPrint("elabseq - symbols: ", ED.ppSymList,
                                        ED.envSymbols env)
                     val _ = debugmsg "<<elabSeqDec"

                  in (resAbsyn, entDec', env, entEnv')
                 end

             | loop(decl::rest, asdecls, entDecls, env, entEnv) =
                 let val env1 = SE.atop(env,env0)
                     val entEnv1 = EE.mark(mkStamp, EE.atop(entEnv,entEnv0))
                     val (absyn, entDecl, env', entEnv') =
			 elabDecl0(decl, env1, entEnv1, context, top, tdepth,
				   epContext, rpath, region, compInfo)
		     (* Consolidate env in case of large number of bindings
			in a single module *)
		     val env2 = SE.consolidateLazy (SE.atop(env',env))
                  in loop(rest, absyn::asdecls, entDecl::entDecls,
                          env2,
                          EE.mark (mkStamp, EE.atop (entEnv', entEnv)))
                 end

        in loop(decls, nil, nil, SE.empty, EE.empty)
           handle EE.Unbound =>
	     (debugmsg("$elabDecl0: SeqDec");
	      raise EE.Unbound)
       end

   | TypeDec tbs =>
       (*** ASSERT: the tycons declared are all DEFtycs ***)
       (let val (dec, env) =
                ET.elabTYPEdec(tbs,env0,rpath,region,compInfo)
            val tycs = case dec
			 of A.TYPEdec z => z
			  | _ => bug "elabDecl0 for TypeDec"
            val (entEnv, entDec) =
              bindNewTycs(context, epContext, mkStamp, [], tycs, rpath,
			  error region)
         in (dec, entDec, env, entEnv)
        end
        handle EE.Unbound =>
	  (debugmsg("$elabDecl0: TypeDec");
	   raise EE.Unbound))

   | DatatypeDec (x as {datatycs,withtycs}) =>
       let val isFree =
               (case context
                 of EU.INFCT _ =>
                    (fn tyc =>
                        (case EPC.lookTycPath(epContext, MU.tycId tyc)
                          of SOME _ => true
                           | _ => false))
                  | _ => (fn _ => false))

           val (datatycs,withtycs,_,env) =
	       ET.elabDATATYPEdec(x, env0, [], EE.empty, isFree, rpath,
                                  region, compInfo)
	   val (entEnv, entDec) =
	       bindNewTycs(context, epContext, mkStamp,
                           datatycs, withtycs, rpath, error region)
	   val resDec =
               A.DATATYPEdec{datatycs=datatycs,withtycs=withtycs}
        in (resDec, entDec, env, entEnv)
       end

   | DataReplDec(name,syms) =>
       let fun no_datatype () =
	       (error region EM.COMPLAIN
		      "rhs of datatype replication not a datatype"
		      EM.nullErrorBody;
		(A.SEQdec[],M.ERRORdec,SE.empty,EE.empty))
           val tyc = L.lookTyc(env0, SP.SPATH syms, error region)
        in case tyc
	    of T.GENtyc {stamp, arity, eq, path, stub,
			 kind = dt as (T.DATATYPE{stripped=false,...})} =>
	       let val dcons = TU.extractDcons tyc
		   val envDcons =
		       foldl (fn (d as T.DATACON{name,...}, e) =>
				 SE.bind(name, B.CONbind d, e))
 			     SE.empty dcons
		   val env = SE.bind(name, B.TYCbind tyc, envDcons)
		   val ev = mkStamp()
		   val tyc_id = MU.tycId tyc
		   val (ee_dec,ee_env) =
		       case context
			of EU.INFCT _ =>
			   let val texp =
				   case EPC.lookTycPath(epContext,tyc_id)
				    of NONE => M.CONSTtyc tyc
				     | SOME entPath =>
				       M.VARtyc entPath
			    in (M.TYCdec(ev,texp),
			        EE.bind(ev, M.TYCent tyc, EE.empty))
			   end
			 | _ => (M.EMPTYdec,EE.empty)
		   val tyc' = T.GENtyc{stamp=stamp, arity=arity,
				       eq=eq, path=InvPath.extend(InvPath.empty,name),
				       stub=stub, kind=dt}
		   val resDec = A.DATATYPEdec{datatycs=[tyc' (* tyc *)],
					      withtycs=[]}
	        in EPC.bindTycPath(epContext, tyc_id, ev);
		   (resDec, ee_dec, env, ee_env)
	       end
	     | _ => no_datatype ()
       end

   | AbstypeDec x =>
       (let val isFree =
              (case context
                of EU.INFCT _ =>
                     (fn tyc =>
                       (case EPC.lookTycPath(epContext, MU.tycId tyc)
                         of SOME _ => true
                          | _ => false))
                 | _ => (fn _ => false))

            val (decl, env', abstycs, withtycs) =
		case  EC.elabABSTYPEdec(x, env0, context, isFree,
					rpath, region, compInfo) of
		    (d as A.ABSTYPEdec x, e) =>
		    (d, e, #abstycs x, #withtycs x)
		  | _ => bug "elabDecl0:AbstypeDec"
            (*
             * Potential bug: what about other datatype declarations within
             * the body of ABSTYPEdec ? they are local declarations; but
             * they may not be properly dealt with now ! (ZHONG)
             *)

            fun chkError () = !anyErrors
            (* note that transform is applied to decl before type checking *)
            val env1 = SE.atop(env', env0)
            val decl0 = Typecheck.decType(env1, transform decl,
                                          top, error, chkError, region)
	    val decl1 = if !anyErrors then decl0
			else MatchTrans.transMatchDec (decl0, env1, error, region)
            val (entEnv, entDec) =
                bindNewTycs(context, epContext, mkStamp, abstycs, withtycs,
			    rpath, error region)
         in (decl1, entDec, env', entEnv)
        end
        handle EE.Unbound =>
	  (debugmsg("$elabDecl0: AbstypeDec");
	   raise EE.Unbound))

   | MarkDec(decl',region') =>
       elabDecl0(decl', env0, entEnv0, context, top, tdepth,
                 epContext, rpath, region', compInfo)

   | dec =>
       (let val isFree =
             (case context
               of EU.INFCT _ =>
                    (fn tyc =>
                       (case EPC.lookTycPath(epContext, MU.tycId tyc)
                         of SOME _ => true
                          | _ => false))
                | _ => (fn _ => false))

            val (decl0, env') = EC.elabDec(dec, env0, isFree,
                                           rpath, region, compInfo)
                                handle EE.Unbound =>
				  (debugmsg("$EC.elabDec"); raise EE.Unbound)
            val _ = debugmsg (">>elabDecl0.dec[after EC.elabDec: top="
                              ^ (Bool.toString top))
            val decl1 = transform decl0
            val _ = debugmsg ">>elabDecl0.dec [after transform]"
            fun chkError () = !anyErrors
	    val env1 = SE.atop(env',env0)
            val decl2 = Typecheck.decType(env1, decl1,
                                          top, error, chkError, region)
                         handle EE.Unbound => (debugmsg("$decType");
                                               raise EE.Unbound)
	    val decl3 = if !anyErrors then decl2
			else MatchTrans.transMatchDec (decl2, env1, error, region)
            val _ = debugmsg ">>elabDecl0.dec[after decType]"
         in (decl3, M.EMPTYdec, env', EE.empty)
        end handle EE.Unbound =>
		   (debugmsg("$elabDecl0: CoreDec"); raise EE.Unbound))
    (* end case *))
    (* end elabDecl0 *)

(*** the top-level wrapper for the elabDecl0 function ***)
fun elabDecl {ast, statenv, entEnv, context, level, tdepth,
              epContext, path, region, compInfo} =
    let val (resDec, _, senv, _) =
	    elabDecl0(ast, statenv, entEnv, context, level, tdepth,
                      epContext, path, region, compInfo)
     in {absyn=resDec, statenv=senv}
    end

end (* local *)

end (* structure ElabMod *)
