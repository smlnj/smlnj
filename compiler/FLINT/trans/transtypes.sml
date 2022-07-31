(* transtypes.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature TRANSTYPES =
sig
  val genTT  : unit -> {tpsKnd : Types.tycpath -> Lty.tkind,
                        tpsTyc : DebIndex.depth -> Types.tycpath
                                 -> Lty.tyc,
                        toTyc  : DebIndex.depth -> Types.ty -> Lty.tyc,
                        toLty  : DebIndex.depth -> Types.ty -> Lty.lty,
                        strLty : Modules.Structure * DebIndex.depth
                                 * ElabUtil.compInfo -> Lty.lty,
                        fctLty : Modules.Functor * DebIndex.depth
                                 * ElabUtil.compInfo -> Lty.lty}
end (* signature TRANSTYPES *)

structure TransTypes : TRANSTYPES =
  struct

    structure BT = BasicTypes
    structure DA = Access
    structure DI = DebIndex
    structure EE = EntityEnv
    structure EM = ErrorMsg
    structure EPC = EntPathContext
    structure EV = EvalEntity
    structure INS = Instantiate
    structure IP = InvPath
    structure LT = Lty
    structure LK = LtyKernel
    structure LD = LtyDef
    structure LB = LtyBasic
    structure LE = LtyExtern (* == PLambdaType *)
    structure PT = PrimTyc
    structure MU = ModuleUtil
    structure SE = StaticEnv
    structure TU = TypesUtil
    structure PP = PrettyPrint
    open Types Modules ElabDebug

    fun bug msg = ErrorMsg.impossible ("TransTypes: " ^ msg)
    fun say msg = (Control.Print.say msg; Control.Print.flush ())
    fun saynl msg = (Control.Print.say msg; Control.Print.say "\n"; Control.Print.flush ())
    fun saysnl msgs = saynl (concat msgs)

    val debugging = FLINT_Control.tmdebugging
    fun dbsaynl (msg: string) =
	if !debugging then saynl msg else ()
    fun dbsaysnl (msgs: string list) =
	if !debugging then saysnl msgs else ()

    val debugPrint = (fn x => debugPrint debugging x)
    val defaultError =
	  EM.errorNoFile(EM.defaultConsumer(),ref false) SourceMap.nullRegion

    val env = StaticEnv.empty

    fun ppType x =
     ((PP.with_pp (EM.defaultConsumer())
	       (fn ppstrm => (PP.string ppstrm "find: ";
			      PPType.resetPPType();
			      PPType.ppType env ppstrm x)))
      handle _ => say "fail to print anything")

    fun ppTycon x =
	((PP.with_pp (EM.defaultConsumer())
	    (fn ppstrm => (PP.string ppstrm "find: ";
			   PPType.resetPPType();
			   PPType.ppTycon env ppstrm x)))
	handle _ => say "fail to print anything")


    fun ppLtyc ltyc =
	PP.with_default_pp (fn ppstrm => PPLty.ppTyc 20 ppstrm ltyc)


  (****************************************************************************
   *               TRANSLATING ML TYPES INTO FLINT TYPES                      *
   ****************************************************************************)
local
  val recTyContext = ref [~1]
in
  fun enterRecTy (a) = (recTyContext := (a::(!recTyContext)))
  fun exitRecTy () = (recTyContext := tl (!recTyContext))
  fun recTyc (i) =
	let val x = hd(!recTyContext)
	 in if x = 0 then LD.tcc_var (1, i)  (* innermost TFN *)
	    else if x > 0 then LD.tcc_var (2, i)  (* second innermost TFN *)
		 else bug "unexpected RECtyc"
	end
  fun freeTyc (i) =
	let val x = hd (!recTyContext)
	 in if x = 0 then LD.tcc_var (2, i)  (* second innermost TFN *)
	    else if x > 0 then LD.tcc_var (3, i)  (* third innermost TFN *)
		 else bug "unexpected RECtyc"
	end
end (* end of recTyc and freeTyc hack *)

fun tpsKnd (TP_VAR x) = TransTKind.trans(#kind x)
  | tpsKnd _ = bug "unexpected tycpath parameters in tpsKnd"

(* ---------------- begin genTT ---------------- *)
fun genTT () = let

(* tycpathToTyc : int (dbindex) -> T.tycpath -> LE.tyc *)
fun tycpathToTyc tfd tycpath =
  let fun setTyvarIndex (TP_VAR {tdepth, num, ...}, curTfd) =
	  let val dbindex = curTfd - tdepth
	   in if dbindex < 0
	      then bug ("tycpathToTyc: dbindex = " ^ Int.toString dbindex ^ " < 0")
	      else ();
              LD.tcc_var (dbindex, num)
	  end
        | setTyvarIndex (TP_TYC tc, curTfd) = tycTyc(tc, curTfd)
        | setTyvarIndex (TP_SEL (tp, i), curTfd) = LD.tcc_proj(setTyvarIndex(tp, curTfd), i)
        | setTyvarIndex (TP_APP (tp, ps), curTfd) =
              LD.tcc_app(setTyvarIndex(tp, curTfd), map (fn x => setTyvarIndex(x, curTfd)) ps)
        | setTyvarIndex (TP_FCT (ps, ts), curTfd) =
              let val ks = map tpsKnd ps
                  val ts' = map (fn x => setTyvarIndex(x, curTfd + 1)) ts
               in LD.tcc_fn(ks, LD.tcc_seq ts')
              end

   in setTyvarIndex(tycpath, tfd)
  end (* tycpathToTyc *)

(*
and tycTyc x =
  Stats.doPhase(Stats.makePhase "Compiler 043 1-tycTyc") tycTyc0 x
*)

and tycTyc (tc, d) =
  let fun dtsTyc nd ({dcons: dconDesc list, arity=i, ...} : dtmember) =
            let val nnd = if i=0 then nd else DI.next nd
                fun f ({domain=NONE, rep, name}, r) = (LB.tcc_unit)::r
                  | f ({domain=SOME t, rep, name}, r) = (toTyc nnd t)::r

                val _ = enterRecTy i
                val core = LD.tcc_sum(foldr f [] dcons)
                val _ = exitRecTy()

                val resTyc = if i=0 then core
                             else (let val ks = LD.tkc_arg i
                                    in LD.tcc_fn(ks, core)
                                   end)
             in (LD.tkc_int i, resTyc)
            end

      fun dtsFam (freetycs, fam as { members, ... } : dtypeFamily) =
	  case ModulePropLists.dtfLtyc fam of
	      SOME (tc, od) =>
              LB.tc_adj(tc, od, d) (* invariant: tc contains no free variables
				    * so tc_adj should have no effects *)
	    | NONE =>
              let fun ttk (GENtyc { arity, ... }) = LD.tkc_int arity
                    | ttk (DEFtyc{tyfun=TYFUN{arity=i, ...},...}) =
		      LD.tkc_int i
                    | ttk _ = bug "unexpected ttk in dtsFam"
                  val ks = map ttk freetycs
                  val (nd, hdr) =
                      case ks of [] => (d, fn t => t)
                               | _ => (DI.next d, fn t => LD.tcc_fn(ks, t))
                  val mbs = Vector.foldr (op ::) nil members
                  val mtcs = map (dtsTyc (DI.next nd)) mbs
                  val (fks, fts) = ListPair.unzip mtcs
                  val nft = case fts of [x] => x | _ => LD.tcc_seq fts
                  val tc = hdr(LD.tcc_fn(fks, nft))
                  val _ = ModulePropLists.setDtfLtyc (fam, SOME(tc, d))
              in tc
              end

      and g (tycon as GENtyc { arity, kind, ... }) =
	  (case kind
	     of PRIMITIVE => (* translation defined in FLINT/kernel/primtyc.sml *)
		  LD.tcc_prim(PrimTyc.pt_fromtyc tycon)
              | DATATYPE {index, family, freetycs, stamps, ...} =>
		if TU.eqTycon(tycon, BT.refTycon) then LD.tcc_prim (PT.ptc_ref)
		else let val tc = dtsFam (freetycs, family)
			 val n = Vector.length stamps
			 val names = Vector.map
				       (fn ({tycname,...}: dtmember) => Symbol.name tycname)
				       (#members family)
                          (* invariant: n should be the number of family members *)
		     in LD.tcc_fix((n, names, tc, (map g freetycs)), index)
		     end
              | ABSTRACT tc => (g tc)
              | FLEXTYC tp => tycpathToTyc d tp
              | FORMAL => bug "unexpected FORMAL kind in tycTyc-h"
              | TEMP => bug "unexpected TEMP kind in tycTyc-h")
        | g (DEFtyc{tyfun, ...}) = tfTyc(tyfun, d)
        | g (RECtyc i) = recTyc i
        | g (FREEtyc i) = freeTyc i
        | g (RECORDtyc _) = bug "unexpected RECORDtyc in tycTyc-g"
        | g (PATHtyc{arity, path=InvPath.IPATH ss, entPath}) =
              ((* say "*** Warning for compiler writers: PATHtyc ";
               app (fn x => (say (Symbol.name x); say ".")) ss;
               say " in translate: ";
               say (EntPath.entPathToString entPath);
               say "\n"; *)
               if arity > 0 then LD.tcc_fn(LD.tkc_arg arity, LB.tcc_void)
               else LB.tcc_void)
        | g (ERRORtyc) = bug "unexpected tycon in tycTyc-g"

   in (g tc)
  end (* tycTyc *)

and tfTyc (TYFUN{arity=0, body}, d) = toTyc d body
  | tfTyc (TYFUN{arity, body}, d) =
      let val ks = LD.tkc_arg arity
       in LD.tcc_fn(ks, toTyc (DI.next d) body)
      end

(* toTyc : DI.depth -> ty -> LT.tyc *)
and toTyc d t =
  let val tvDict : (tyvar * LT.tyc) list ref = ref []
      fun lookTv tv =
        let val tv_alist = !tvDict
            fun lookup ((a,x)::rest) =
                 if a = tv then x else lookup rest
              | lookup nil =  (* tv not found in tvDict *)
		 let val tyc = trMTyvarKind (!tv)
                  in tvDict := (tv, tyc) :: tv_alist;  (* bind new tyc in tvDict *)
		     tyc
                 end
         in lookup tv_alist
        end

      and trMTyvarKind (INSTANTIATED t) = trTy t
        | trMTyvarKind (LBOUND{depth,index,...}) =
	     (* ASSERT: depth < d *)
	    let val dbindex = d - depth  (* ASSERT: dbindex > 0 *)
	    in if dbindex <= 0
	       then (say (concat["toTyc:trMTyvarKind/LBOUND -- dbindex = ", Int.toString dbindex,
				 " <= 0\n   d = ", Int.toString d, "; depth = ", Int.toString depth,
				 "\n"]);
		     bug "trMTyvarKind: dbindex < 0")
	       else ();
               LD.tcc_var (dbindex, index)
	    end
        | trMTyvarKind (UBOUND _) = LB.tcc_void
            (* dbm: a user-bound type variable that didn't get generalized;
               treat the same as an uninstantiated metatyvar.
	       E.g. val x = ([]: 'a list; 1) *)
        | trMTyvarKind (OPEN _) = LB.tcc_void
            (* dbm: a metatyvar that was neither instantiated nor
	       generalized.  E.g. val x = ([],1); -- the metatyvar
               introduced by the generic instantiation of the type of [] is
               neither instantiated nor generalized (unless at toplevel). *)
        | trMTyvarKind _ = bug "toTyc:trMTyvarKind" (* OVLD should have been resolved *)

      and trTy (VARty tv) = lookTv tv
        | trTy (CONty(RECORDtyc _, [])) = LB.tcc_unit
        | trTy (CONty(RECORDtyc _, tys)) = LD.tcc_tuple (map trTy tys)
        | trTy (CONty(tyc, [])) = tycTyc(tyc, d)
        | trTy (CONty(DEFtyc{tyfun,...}, args)) = trTy (TU.applyTyfun(tyfun,args))
	| trTy (CONty (tc as GENtyc { kind, ... }, ts)) =
	  (case (kind, ts) of
	       (ABSTRACT _, ts) =>
	       LD.tcc_app(tycTyc(tc, d), map trTy ts)
             | (_, [t1, t2]) =>
               if TU.eqTycon(tc, BT.arrowTycon) then LD.tcc_parrow(trTy t1, trTy t2)
               else LD.tcc_app(tycTyc(tc, d), [trTy t1, trTy t2])
	     | _ => LD.tcc_app (tycTyc (tc, d), map trTy ts))
        | trTy (CONty(tyc, ts)) = LD.tcc_app(tycTyc(tyc, d), map trTy ts)
        | trTy (IBOUND i) = LD.tcc_var(DI.innermost, i)
			 (* [KM] IBOUNDs are encountered when toTyc
                          * is called on the body of a POLYty in
                          * toLty (see below). *)
	| trTy (MARKty (t, _)) = trTy t
        | trTy (POLYty _) = bug "unexpected poly-type in toTyc"
        | trTy (UNDEFty) = (* mkVB kluge!!! *) LB.tcc_void
	    (* bug "unexpected undef-type in toTyc" *)
        | trTy (WILDCARDty) = bug "unexpected wildcard-type in toTyc"
   in trTy t
  end

and toLty d (POLYty {tyfun=TYFUN{arity=0, body}, ...}) = toLty d body
  | toLty d (POLYty {tyfun=TYFUN{arity, body},...}) =
      let val ks = LD.tkc_arg arity
       in LD.ltc_poly(ks, [toLty (DI.next d) body])
      end
  | toLty d x = LD.ltc_tyc (toTyc d x)

(****************************************************************************
 *               TRANSLATING ML MODULES INTO FLINT TYPES                    *
 ****************************************************************************)

fun specLty (elements, entEnv, depth, compInfo) =
  let fun g ([], entEnv, ltys) = rev ltys
        | g ((sym, (TYCspec _ ))::rest, entEnv, ltys) =
              g(rest, entEnv, ltys)
        | g ((sym, STRspec {sign, entVar, ...})::rest, entEnv, ltys) =
              let val rlzn = EE.lookStrEnt(entEnv,entVar)
                  val lt = strRlznLty(sign, rlzn, depth, compInfo)
               in g(rest, entEnv, lt::ltys)
              end
        | g ((sym, FCTspec {sign, entVar, ...})::rest, entEnv, ltys) =
              let val rlzn = EE.lookFctEnt(entEnv,entVar)
                  val lt = fctRlznLty(sign, rlzn, depth, compInfo)
               in g(rest, entEnv, lt::ltys)
              end
        | g ((sym, spec)::rest, entEnv, ltys) =
              let val _ = dbsaynl ">>specLtyElt"
                  fun transty ty =
                    ((MU.transType entEnv ty)
                      handle EE.Unbound =>
                         (dbsaynl "$specLty";
                          withInternals(fn () =>
                           debugPrint("entEnv: ",
                                 (fn pps => fn ee =>
                                    PPModules_DB.ppEntityEnv pps SE.empty (ee, 12)),
                                 entEnv));
                          dbsaynl ("$specLty: should have printed entEnv");
                          raise EE.Unbound))

                  fun mapty t = toLty depth (transty t)

               in case spec
                   of VALspec{spec=typ,...} =>
                        g(rest, entEnv, (mapty typ)::ltys)
                    | CONspec{spec=DATACON{rep=DA.EXN _,
                                           typ, ...}, ...} =>
                        let val argt =
                              if BT.isArrowType typ then
                                   #1(LD.ltd_parrow (mapty typ))
                              else LB.ltc_unit
                         in g(rest, entEnv, (LB.ltc_etag argt)::ltys)
                        end
                    | CONspec{spec=DATACON _, ...} =>
                        g(rest, entEnv, ltys)
                    | _ => bug "unexpected spec in specLty"
              end

   in g (elements, entEnv, [])
  end

(*
and signLty (sign, depth, compInfo) =
  let fun h (SIG {kind=SOME _, lambdaty=ref (SOME(lt, od)), ...}) = lt
             (* LB.lt_adj(lt, od, depth) *)
        | h (sign as SIG{kind=SOME _, lambdaty as ref NONE, ...}) =
          (* Invariant: we assum that all Named signatures (kind=SOME _) are
           * defined at top-level, outside any functor definitions. (ZHONG)
           *)
             let val {rlzn=rlzn, tycpaths=tycpaths} =
                   INS.instParam {sign=sign, entEnv=EE.empty, depth=depth,
                                  rpath=InvPath.IPATH[], compInfo=compInfo,
                                  region=SourceMap.nullRegion}
                 val nd = DI.next depth
                 val nlty = strMetaLty(sign, rlzn, nd, compInfo)

                 val ks = map tpsKnd tycpaths
                 val lt = LD.ltc_poly(ks, nlty)
              in lambdaty := SOME (lt, depth); lt
             end
        | h _ = bug "unexpected sign in signLty"
   in h sign
  end
*)
and strMetaLty (sign, rlzn as { entities, ... }: strEntity, depth, compInfo) =
    case (sign, ModulePropLists.strEntityLty rlzn) of
	(_, SOME (lt, od)) => LB.lt_adj(lt, od, depth)
      | (SIG { elements, ... }, NONE) =>
	let val ltys = specLty (elements, entities, depth, compInfo)
            val lt = (* case ltys of [] => LE.ltc_int
                                   | _ => *) LD.ltc_str(ltys)
        in
	    ModulePropLists.setStrEntityLty (rlzn, SOME(lt, depth));
	    lt
        end
      | _ => bug "unexpected sign and rlzn in strMetaLty"

and strRlznLty (sign, rlzn : strEntity, depth, compInfo) =
    case (sign, ModulePropLists.strEntityLty rlzn) of
	(sign, SOME (lt,od)) => LB.lt_adj(lt, od, depth)

(* Note: the code here is designed to improve the "toLty" translation;
   by translating the signature instead of the structure, this can
   potentially save time on strLty. But it can increase the cost of
   other procedures. Thus we turn it off temporarily. (ZHONG)

      | (SIG{kind=SOME _, ...}, {lambdaty, ...}) =>
             let val sgt = signLty(sign, depth, compInfo)
                 (* Invariant: we assum that all Named signatures
                  * (kind=SOME _) are defined at top-level, outside any
                  * functor definitions. (ZHONG)
                  *)
                 val argtycs = INS.getTycPaths{sign=sign, rlzn=rlzn,
                         entEnv=EE.empty, compInfo=compInfo}
                 val lt = LE.lt_inst(sgt, map (tycpathToTyc depth) argtycs)
              in lambdaty := SOME(lt, depth); lt
             end
*)
      | _ => strMetaLty(sign, rlzn, depth, compInfo)

and fctRlznLty (sign, rlzn, depth, compInfo) =
    case (sign, ModulePropLists.fctEntityLty rlzn, rlzn) of
	(sign, SOME (lt, od), _) => LB.lt_adj(lt, od, depth)
      | (FSIG{paramsig, bodysig, ...}, _,
         {closure as CLOSURE{env,...}, ...}) =>
        let val {rlzn=argRlzn, tycpaths=tycpaths} =
                INS.instParam {sign=paramsig, entEnv=env, tdepth=depth,
                               rpath=InvPath.IPATH[], compInfo=compInfo,
                               region=SourceMap.nullRegion}
            val nd = DI.next depth
            val paramLty = strMetaLty(paramsig, argRlzn, nd, compInfo)
            val ks = map tpsKnd tycpaths
            val bodyRlzn =
                EV.evalApp(rlzn, argRlzn, nd, EPC.initContext,
                           IP.empty, compInfo)
            val bodyLty = strRlznLty(bodysig, bodyRlzn, nd, compInfo)

            val lt = LD.ltc_poly(ks, [LD.ltc_fct([paramLty],[bodyLty])])
        in
	    ModulePropLists.setFctEntityLty (rlzn, SOME (lt, depth));
	    lt
        end
      | _ => bug "fctRlznLty"

and strLty (str as STR { sign, rlzn, ... }, depth, compInfo) =
    (case ModulePropLists.strEntityLty rlzn of
	 SOME (lt, od) => LB.lt_adj(lt, od, depth)
       | NONE =>
         let val lt = strRlznLty(sign, rlzn, depth, compInfo)
         in
	     ModulePropLists.setStrEntityLty (rlzn, SOME(lt, depth));
	     lt
         end)
  | strLty _ = bug "unexpected structure in strLty"

and fctLty (fct as FCT { sign, rlzn, ... }, depth, compInfo) =
    (case ModulePropLists.fctEntityLty rlzn of
	 SOME (lt,od) => LB.lt_adj(lt, od, depth)
       | NONE =>
         let val lt = fctRlznLty(sign, rlzn, depth, compInfo)
	 in
	     ModulePropLists.setFctEntityLty (rlzn, SOME(lt,depth));
	     lt
         end)
  | fctLty _ = bug "unexpected functor in fctLty"

(****************************************************************************
 *           A HASH-CONSING VERSION OF THE ABOVE TRANSLATIONS               *
 ****************************************************************************)

(*
structure MIDict = RedBlackMapFn(struct type ord_key = ModuleId.modId
                                     val compare = ModuleId.cmp
                              end)
*)

(*
      val m1 = ref (MIDict.mkDict())   (* modid (tycon) -> LE.tyc *)
      val m2 = ref (MIDict.mkDict())   (* modid (str/fct) -> LE.lty *)

      fun tycTycLook (t as (GENtyc _ | DEFtyc _), d) =
            let tid = MU.tycId t
             in (case MIDict.peek(!m1, tid)
                  of SOME (t', od) => LB.tc_adj(t', od, d)
                   | NONE =>
                       let val x = tycTyc (t, d)
                           val _ = (m1 := TcDict.insert(!m1, tid, (x, d)))
                        in x
                       end)
            end
        | tycTycLook x = tycTyc tycTycLook x

(*
      val toTyc = toTyc tycTycLook
      val toLty = toTyc tycTycLook
*)
      val coreDict = (toTyc, toLty)

      fun strLtyLook (s as STR _, d) =
            let sid = MU.strId s
             in (case MIDict.peek(!m2, sid)
                  of SOME (t', od) => LB.lt_adj(t', od, d)
                   | NONE =>
                       let val x = strLty (coreDict, strLtyLook,
                                           fctLtyLook) (s, d)
                           val _ = (m2 := TcDict.insert(!m2, sid, (x, d)))
                        in x
                       end)
            end
        | strLtyLook x = strLty (coreDict, strLtyLook, fctLtyLook)

      and fctLtyLook (f as FCT _, d) =
            let fid = fctId f
             in (case MIDict.peek(!m2, fid)
                  of SOME (t', od) => LB.lt_adj(t', od, d)
                   | NONE =>
                       let val x = fctLty (tycTycLook, strLtyLook,
                                           fctLtyLook) (s, d)
                           val _ = (m2 := TcDict.insert(!m2, fid, (x, d)))
                        in x
                       end)
            end
        | fctLtyLook x = fctLty (coreDict, strLtyLook, fctLtyLook)
*)

   in {tpsKnd=tpsKnd, tpsTyc=tycpathToTyc,
       toTyc=toTyc, toLty=toLty, strLty=strLty, fctLty=fctLty}
  end (* function genTT *)
(* ---------------- end genTT ---------------- *)

end (* structure TransTypes *)
