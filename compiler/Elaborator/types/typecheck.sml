(* typecheck.sml
 *
 * COPYRIGHT (c) 2021 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature TYPECHECK =
sig

  val decType : StaticEnv.staticEnv * Absyn.dec * bool
		* ErrorMsg.errorFn * (unit -> bool) * SourceMap.region
                -> Absyn.dec
    (* decType (senv, dec, toplev, errFn, anyErrors, region):
         senv: the context static environment
         dec: the declaration to be type checked
         errFn: error reporting function
         anyErrors : unit -> bool  -- have errors been detected
         region: source region of dec
     *)
  val debugging : bool ref

end (* signature TYPECHECK *)

structure Typecheck : TYPECHECK =
struct

local open Types TypesUtil Unify Absyn
	   ErrorMsg PPUtil PPType PPAbsyn

  structure SE = StaticEnv
  structure DI = DebIndex
  structure DA = Access
  structure BT = BasicTypes
  structure TU = TypesUtil
  structure OC = Occurrence
  structure V = Variable
  structure EU = ElabUtil
  structure ED = ElabDebug
  structure PP = PrettyPrint
  structure MC = MatchComp
in

(* debugging *)
val debugging = ElabControl.tcdebugging
val say = Control_Print.say
fun newline () = say "\n"
fun dbsay (msg: string) = if !debugging then say msg else ()
fun dbsaynl (msg: string) = if !debugging then (say msg; newline()) else ()
fun dbsaysnl (msgs: string list) = if !debugging then (say (concat msgs); newline()) else ()
val debugPrint = (fn x => ED.debugPrint debugging x)

fun bug msg = ErrorMsg.impossible("TypeCheck: "^msg)

infix 9 sub
val --> = BT.-->
infix -->

val printDepth = Control_Print.printDepth
val showCulprits = ElabControl.showTypeErrorCulprits

fun refNewDcon(DATACON{name, const, rep, typ, sign, lazyp}) =
    DATACON{name=name, const=const, rep=rep, typ=BT.refPatType, sign=sign, lazyp=lazyp}

fun message(msg,mode: Unify.unifyFail) =
    String.concat[msg," [",Unify.failMessage mode,"]"]

fun mkDummy0 () = BasicTypes.unitTy

(* decType : SE.staticEnv * A.dec * bool * EM.errorFn * region -> A.dec *)
fun decType (env, dec, toplev, err, anyErrors, region) =
let

(* setup for recording and resolving overloaded variables and literals *)
val { pushv = olv_push, pushl = oll_push, resolve = ol_resolve } = Overload.new ()

val ppType = PPType.ppType env
val ppTycon = PPType.ppTycon env
val ppPat = PPAbsyn.ppPat env
val ppExp = PPAbsyn.ppExp(env,NONE)
val ppRule = PPAbsyn.ppRule(env,NONE)
val ppVB = PPAbsyn.ppVB(env,NONE)
val ppRVB = PPAbsyn.ppRVB(env,NONE)
val ppDec = PPAbsyn.ppDec(env, NONE)

fun ppType' ty = (dbsaynl ">>> ppType'";
		  PP.with_default_pp
		      (fn ppstrm => (ppType ppstrm ty));
		  dbsaynl "<<< ppType'")

val ppDec' =
  (fn ppstrm => fn d => PPAbsyn.ppDec (env,NONE) ppstrm (d,!printDepth))

fun ppDecDebug (msg,dec) =
  ED.withInternals(fn () => ED.debugPrint debugging (msg, ppDec', dec))

fun ppTypeDebug (msg,ty) =
  ED.withInternals(fn () => ED.debugPrint debugging (msg, ppType, ty))

fun ppTyvarDebug tv =
  ED.withInternals(fn () => dbsaynl (PPType.tyvarPrintname tv))

fun ppRegion ppstrm ((l,u): SourceMap.region) =
    (PP.string ppstrm (Int.toString l);
     PP.string ppstrm "-";
     PP.string ppstrm (Int.toString u))

fun ppModeErrorMsg ppstrm (mode: Unify.unifyFail) =
    if !showCulprits then
      (case mode
	of TYC(tyc1,tyc2,reg1,reg2) =>
	   (PP.newline ppstrm;
	    PP.string ppstrm "Mode: tycon mismatch"; PP.newline ppstrm;
	    PP.string ppstrm "tycon1: ";
	    ppTycon ppstrm tyc1; PP.newline ppstrm;
	    PP.string ppstrm "from: "; ppRegion ppstrm reg1; PP.newline ppstrm;
	    PP.string ppstrm "tycon2: ";
	    ppTycon ppstrm tyc2; PP.newline ppstrm;
	    PP.string ppstrm "from: "; ppRegion ppstrm reg2)
	 | TYP(ty1,ty2,reg1,reg2) =>
	   (PP.newline ppstrm;
	    PP.string ppstrm "Mode: type mismatch"; PP.newline ppstrm;
	    PP.string ppstrm "type1: ";
	    ppType ppstrm ty1; PP.newline ppstrm;
	    PP.string ppstrm "from: "; ppRegion ppstrm reg1; PP.newline ppstrm;
	    PP.string ppstrm "type2: ";
	    ppType ppstrm ty2; PP.newline ppstrm;
	    PP.string ppstrm "from: "; ppRegion ppstrm reg2)
	  | _ => ())
    else ()

(* setup for recording FLEX tyvars and checking that they are eventually
 * resolved to exact record types. This is to prevent the leakage of
 * unresolved flex record types into the middle end. *)
val flexTyVars : (tyvar * region) list ref = ref nil

fun registerFlex (x as (tv : tyvar, region: region)) =
    flexTyVars := x :: !flexTyVars

fun checkFlex (): unit =
    let fun check1 (tv,r) =
            (case !tv
               of OPEN{kind=FLEX _,...} =>
                  (err region COMPLAIN
			  "unresolved flex record (hidden)"
		       (fn ppstrm =>
			     (PPType.resetPPType();
			      PP.newline ppstrm;
			      PP.string ppstrm "type: ";
			      ppType ppstrm (VARty(tv)))))
                | INSTANTIATED _ => ()
                | _ => bug "checkFlex")
    in if anyErrors () then ()
       else app check1 (!flexTyVars)
    end

(* managing source locations (srcloc = SourceMap.region) *)

val nullRegion = SourceMap.nullRegion

(* tyToLoc : ty -> region *)
(* translating a marked type to its origin srcloc *)
(* We need to worry about immediately nested MARKty's, where a wider
 * region is wrapped immediately around a narrower one. Hence the
 * first rule. *)
fun tyToLoc (MARKty (t as MARKty _, region)) = tyToLoc t
  | tyToLoc (MARKty (ty,region)) = region
  | tyToLoc _ = SourceMap.nullRegion

(* unifyErr : {ty1: T.ty, name1: string, ty2: T.ty, name2: string,
               message: string, region: region,
               kind: PP.ppstream -> ('phrase * int) -> unit, kindname: string, phrase: 'phrase}
              -> bool *)
fun unifyErr{ty1,name1,ty2,name2,message=m,region,kind,kindname,phrase} =
    (unifyTy(ty1, ty2, tyToLoc ty1, tyToLoc ty2); true)
    handle Unify(mode) =>
      (err region COMPLAIN (message(m,mode))
       (fn ppstrm =>
	 (PPType.resetPPType();
	  let val len1 = size name1
	      val len2 = size name2
	      val spaces = "                                   "
	      val pad1 = substring(spaces,0,Int.max(0,len2-len1))
	      val pad2 = substring(spaces,0,Int.max(0,len2-len1))
	      val m = if m = ""
		      then concat[name1, " and ", name2, " do not agree"]
		      else m   (* but name1 and name2 may be "" ! *)
	  in if name1="" then ()
             else (PP.newline ppstrm;
                   PP.string ppstrm (concat[name1, ": ", pad1]);
	           ppType ppstrm ty1);
	     if name2="" then ()
	      else (PP.newline ppstrm;
                    PP.string ppstrm (concat[name2, ": ", pad2]);
		    ppType ppstrm ty2);
	     if kindname="" then ()
	     else (
		PP.newline ppstrm;
		PP.string ppstrm(concat["in ", kindname, ":"]);
		PP.break ppstrm {nsp=1,offset=2};
		kind ppstrm (phrase,!printDepth));
             ppModeErrorMsg ppstrm mode
	 end));
       false)  (* false will never be returned, because of the call of err *)

val _ = dbsaynl (">>decType: toplev = " ^ Bool.toString toplev)
val _ = ppDecDebug(">>decType: dec = ",dec)

(* generalizeTy : V.var         -- (bound) VALvar whose type is being generalized
                * tyvar list    -- user bound tyvars
		* occ           -- occurrence position constroling generalization
		* bool          -- supresses generalization (partially?) as required
		                   by "value" and "irrefutable" restrictions
                * region        -- region of the variable occurrence?
                -> tyvar list   -- the generalized type metavariables *)
fun generalizeTy(V.VALvar{typ,path,btvs,...}, userbound: tyvar list,
		 occ: OC.occ, generalize: bool, region) : tyvar list =
    let val _ = dbsaynl (">>>generalizeTy: "^SymPath.toString path)
	val _ = dbsaynl ("userbound: ")
	val _ = List.app ppTyvarDebug userbound
        val _ = dbsaynl ("generalize: "^Bool.toString generalize)
        val lambdaDepth = OC.lamdepth occ
	val topLevel = OC.toplevel occ
        val _ = dbsaynl ("lamdepth occ: " ^ Int.toString lambdaDepth)
        val _ = dbsaynl ("toplevel occ: " ^ Bool.toString topLevel)

	val failure = ref false
	val mkDummy = if topLevel
	              then TypesUtil.dummyTyGen()
		      else mkDummy0 (* shouldn't be called *)

	val index = ref 0  (* counts number of type variables bound *)
	fun next () = !index before (index := !index + 1)
	val sign = ref([]: Types.polysign)
	fun localUbound tv = List.exists (fn tv' => eqTyvar(tv,tv')) userbound

	(* menv: a reference to an association list environment mapping
	 *   generalized tyvars to the corresponding IBOUND type.
	 * ASSERT: there are no duplicate tyvars in domain of menv (a tyvar
         *   will not be bound twice).
         * ASSERT: the second element of an alist pair is an IBOUND type *)
	val menv = ref([]: (tyvar * ty) list)
	(* lookup : tyvar -> ty option *)
	fun lookup tv =
	    let fun find [] = NONE
		  | find((tv',ty)::rest) =
		    if eqTyvar(tv,tv') then SOME ty
		    else find rest
	     in find(!menv)
	    end
	fun bind (tv: tyvar, ty: ty) = menv := (tv,ty) :: !menv

        (* gen : ty -> ty *)
	fun gen(ty) =
	    case ty
	      of VARty(ref(INSTANTIATED ty)) => gen ty
	       | VARty(tv as ref(OPEN{depth,eq,kind})) =>
		  (case kind
		     of FLEX[(lab,_)] =>
                         if (depth > lambdaDepth andalso
                             (generalize orelse topLevel))
                            orelse (topLevel andalso depth = 0)
                         then
			   (err region COMPLAIN (String.concat
			     ["unresolved flex record\n\
			      \   (can't tell what fields there are besides #",
			      Symbol.name lab, ")"])
			     nullErrorBody;
                            tv := INSTANTIATED WILDCARDty;
			    WILDCARDty)
                         else ty
		      | FLEX _ =>
                         if (depth > lambdaDepth andalso
                             (generalize orelse topLevel))
                            orelse (topLevel andalso (depth=0))
                         then
  			   (err region COMPLAIN
			        "unresolved flex record (need to know the \
			        \names of ALL the fields\n in this context)"
			    (fn ppstrm =>
			       (PPType.resetPPType();
				PP.newline ppstrm;
				PP.string ppstrm "type: ";
				ppType ppstrm ty));
                            tv := INSTANTIATED WILDCARDty;
			    WILDCARDty)
                         else ty
		      | META =>
			  if depth > lambdaDepth
			  then if generalize then
				   (case lookup tv
				     of NONE =>
					let val new = IBOUND(next())
					 in sign := eq :: !sign;
				            bind(tv,new);
					    new
					end
				     | SOME ty => ty)
			       else (if topLevel
				     then let val new = mkDummy()
					   in failure := true;
                                              tv := INSTANTIATED new;
					      new
					  end
				     else (if !ElabControl.valueRestrictionLocalWarn
					   then err region WARN
				             ("type variable not generalized\
                                              \ in local decl (value restriction): "
                                              ^ (tyvarPrintname tv))
				             nullErrorBody
					   else ();
					   (* reset depth to prevent later
					      incorrect generalization inside
					      a lambda expression. See typechecking
					      test 5.sml *)
					   tv := OPEN{depth = lambdaDepth,
						      eq = eq, kind = kind};
					   ty))
			  else if topLevel andalso depth = 0
			   (* ASSERT: failed generalization at depth 0.
			      see bug 1066. *)
			  then (case lookup tv
				 of NONE =>
				      let val new = mkDummy()
				       in failure := true;
					  tv := INSTANTIATED new;
					  new
				      end
				 | SOME ty => ty)
			  else ty) (* raise SHARE *)
	      | VARty(tv as ref (UBOUND {name,depth,eq})) =>
		 (dbsaynl ("UBOUND:" ^Symbol.name name);
		  if localUbound tv
		  then (dbsaynl "is local";
		       if depth > lambdaDepth andalso generalize
		       then (dbsaynl "is generalized";
			     case lookup tv
			       of NONE =>
				    let val new = IBOUND(next())
				     in tv := OPEN {depth=depth, eq=eq, kind=META};
					sign := eq :: !sign;
					bind (tv,new);
					new
				    end
			       | SOME ty => ty)
		       else (err region COMPLAIN
			     ("explicit type variable cannot be \
			       \generalized at its binding \
			       \declaration: " ^
			       (tyvarPrintname tv))
			      nullErrorBody;
			     tv := INSTANTIATED WILDCARDty;
			     WILDCARDty))
		  else (dbsaynl "is not local"; ty))
	      | VARty(ref(OVLDV _ | OVLDI _ | OVLDW _)) => ty
	      | CONty(tyc,args) => CONty(tyc, map gen args) (* could use hareMap *)
	      | WILDCARDty => WILDCARDty
	      | MARKty(ty, region) =>
	      	  let val () = ppTypeDebug (">> Markty", ty)
		   in gen ty
		  end
	      | _ => bug "generalizeTy -- bad arg"
        (* end fun gen *)

	val _ = ppTypeDebug (">>> gen: before: ", !typ)
	val generalizedTy = gen(!typ)
	val _ = ppTypeDebug ("<<< gen: after: ", generalizedTy)

        val generalizedTyvars = map #1 (rev(!menv))

     in if !failure andalso !ElabControl.valueRestrictionTopWarn
	then err region WARN
	        "type vars not generalized because of\n\
                 \   value restriction are instantiated to dummy types (X1,X2,...)"
		nullErrorBody
        else ();
	dbsaynl ("generalizeTy returning: # generalized tyvars = "
		  ^ Int.toString (length generalizedTyvars));
	typ := (if !index > 0 then  (* some tyvars were generalized *)
                   POLYty{sign = rev (!sign),
		          tyfun = TYFUN {arity = !index, body = generalizedTy}}
               else generalizedTy);
	btvs := generalizedTyvars; (* these have been replaced by IBOUNDs in typ *)
	generalizedTyvars  (* return the generalized tyvars *)
    end

  | generalizeTy _ = bug "generalizeTy - arg not a VALvar"


(* generalizePat : pat * tyvar list * occ * bool * region -> tyvar list *)
(* (if generalize is true?) collects the type metavariables that were generalized
 * when generalizing all the variables occuring in the pattern.
 * Note: val x::y = nil::nil. ==> x: 'a list, y: 'a list list, with 'a or its generalized
 * metatyvar equivalent (say, mtv) in common:
 *    btvs(x) = btvs(y) = boundtvs("x::y") = {mtv}?
 * So there can be overlaps between the type variables generalized for different pattern
 * variables (or, as in this case, they can be equal). *)
fun generalizePat(pat: pat, userbound: tyvar list, occ: OC.occ, generalize: bool, region) =
    let val tvs : tyvar list ref = ref []
	fun union ([],tvs) = tvs
	  | union (tv::rest,tvs) = if List.exists (fn tv' => (tv = tv')) tvs then union(rest,tvs)
				   else tv :: (union(rest,tvs))
        fun gen (VARpat var) =
	      tvs := union (generalizeTy (var,userbound,occ,generalize,region), !tvs)
	      (* can these two sets of metatyvars overlap?  Example? [DBM:2020:7:25]*)
	  | gen (RECORDpat {fields,...}) = app (gen o #2) fields
	  | gen (APPpat (_,_,arg)) = gen arg
	  | gen (CONSTRAINTpat (pat,_)) = gen pat
	  | gen (LAYEREDpat (varPat,pat)) = (gen varPat; gen pat)
          | gen (MARKpat (pat,reg)) = gen pat
	  | gen _ = ()
     in gen pat;
	!tvs
    end

fun applyType(ratorTy: ty, randTy: ty) : ty =
  let val resultType = mkMETAty()
   in unifyTy(ratorTy, (randTy --> resultType), tyToLoc ratorTy, tyToLoc randTy);
      resultType
  end

fun stripMarksVar (MARKpat(p as VARpat _, reg)) = p
  | stripMarksVar (MARKpat(p,reg)) = stripMarksVar p
  | stripMarksVar (CONSTRAINTpat (p, ty)) =
      CONSTRAINTpat(stripMarksVar p, ty)
  | stripMarksVar p = p

(* patType : pat * int * region -> pat * ty *)
(* essentially just copies the pattern structure, except variable patterns, which
 * may be updated (UNDEFty replaced by (bounded?) meta tyvar. Could possibly return
 * the original pattern instead.  Could pass occ instead of int for depth, but then
 * would have to apply lamdepth to occ on each recursive call. *)
fun patType(pat: pat, depth, region) : pat * ty =
    case pat
      of WILDpat => (pat, mkMETAtyBounded depth)
       | MARKpat (p,region') => patType (p, depth, region')
       | VARpat (V.VALvar{typ as ref UNDEFty,...}) =>
	   (typ := mkMETAtyBounded depth; (pat, MARKty(!typ, region)))
	   (* multiple occurrence due to or-pat, but all occurrences share the same type *)
       | VARpat (V.VALvar{typ, ...}) => (pat, MARKty(!typ, region))
       | NUMpat (src, {ival, ty}) => (pat, oll_push(ival, src, ty, err region))
       | STRINGpat _ => (pat, MARKty(BT.stringTy, region))
       | CHARpat _ => (pat, MARKty(BT.charTy, region))
       | RECORDpat {fields,flex,typ} =>
	   (* fields assumed already sorted by label *)
	   let fun fieldType(lab,pat) =
                 let val (npat,nty) = patType (pat,depth,region)
                  in ((lab,npat), (lab,nty))
                 end
               val (fields',labtys) = mapUnZip fieldType fields
               val newpat = RECORDpat {fields=fields',flex=flex,typ=typ}
	    in if flex
	       then let val tv = mkTyvar (mkFLEX(labtys,depth))
                        val ty = VARty tv
		     in registerFlex(tv,region);
                        typ := ty; (newpat,ty)
		    end
	       else (newpat, MARKty (BT.recordTy labtys, region))
	   end
       | VECTORpat (pats,_) =>
          (let val (npats,ntys) =
                     mapUnZip (fn pat => patType(pat,depth,region)) pats
               val nty =
	       foldr (fn (a,b) => (unifyTy(a, b, tyToLoc a, tyToLoc b); b))
		     (mkMETAtyBounded depth) ntys
            in (VECTORpat(npats,nty),
	    	MARKty(CONty(BT.vectorTycon,[nty]), region))
           end handle Unify(mode) => (
	     err region COMPLAIN
		 (message("vector pattern type failure",mode)) nullErrorBody;
	     (pat,WILDCARDty)))
       | ORpat (p1, p2) =>
           let val (p1, ty1) = patType(p1, depth, region)
  	       val (p2, ty2) = patType(p2, depth, region)
	    in unifyErr{ty1=ty1,ty2=ty2,name1="expected",name2="found",
			message="or-patterns do not agree",region=region,
			kind=ppPat,kindname="pattern",phrase=pat};
	       (ORpat(p1, p2), MARKty(ty1, region))
	   end
       | CONpat (dcon as DATACON{typ,...}, _) =>
           let val (ty, insts) = instantiatePoly typ
               (* the following unification is used to set the correct depth information
                * for the type variables in ty. (ZHONG)  It cannot fail.
                *)
               val nty = mkMETAtyBounded depth  (* DBM: why "Bounded"? *)
               val _ = unifyTy(nty, ty, nullRegion, nullRegion)
		(* propagates depth into ty.  Why?  Example where this
                 * prevents generalization of an affected type variable? *)
            in (CONpat(dcon, insts), MARKty(ty, region))
           end
       | APPpat (dcon as DATACON{typ,rep,...}, _, argPat) =>
	   let val (typedArgPat, argTy) = patType (argPat, depth, region)
               val (ty1, ndcon) = case rep
                                    of DA.REF => (BT.refPatType, refNewDcon dcon)
                                     | _ => (typ,dcon)
               val (ty2,insts) = instantiatePoly ty1
               val npat = APPpat (ndcon, insts, typedArgPat)
            in (npat, MARKty (applyType (ty2, argTy), region))
	       handle Unify(mode) =>
		(err region COMPLAIN
                  (message("constructor and argument do not agree in pattern",mode))
		  (fn ppstrm =>
		   (PPType.resetPPType();
		    PP.newline ppstrm;
		    PP.string ppstrm "constructor: ";
		    ppType ppstrm typ; PP.newline ppstrm;
		    PP.string ppstrm "argument:    ";
		    ppType ppstrm argTy; PP.newline ppstrm;
		    PP.string ppstrm "in pattern:"; PP.break ppstrm {nsp=1,offset=2};
		    ppPat ppstrm (pat,!printDepth)));
		 (pat,WILDCARDty))
	   end
       | CONSTRAINTpat (pat', constraintTy) =>
	   let val (typedPat, patTy) = patType(pat',depth,region)
	    in if unifyErr{ty1=patTy, name1="pattern", ty2=constraintTy, name2="constraint",
			   message="pattern and constraint do not agree",
			   region=region, kind=ppPat, kindname="pattern", phrase=pat}
		then (CONSTRAINTpat (typedPat, MARKty(constraintTy, region)),
		      (MARKty(constraintTy, region)))
		else (pat, WILDCARDty)
	   end
       | LAYEREDpat (vpat, basePat) =>
	   (case stripMarksVar vpat
              of VARpat(V.VALvar{typ,...}) =>
		 let val (npat,patTy) = patType(basePat,depth,region)
		     val _ = (typ := patTy)
		  in (LAYEREDpat(vpat,npat),MARKty(patTy, region))
		 end
	       | (cpat as CONSTRAINTpat(VARpat(V.VALvar{typ,...}),ty)) =>
		 let val (npat,patTy) = patType (basePat,depth,region)
		  in if unifyErr{ty1=patTy, name1="pattern", ty2=ty, name2="constraint",
				 message="pattern and constraint do not agree",
				 region=region, kind=ppPat, kindname="pattern", phrase=pat}
		     then (typ := ty; (LAYEREDpat(cpat,npat),MARKty(ty, region)))
		     else (pat,WILDCARDty)
		 end)
       | p => bug "patType -- unexpected pattern"

(* expType : exp * OC.occ * region -> exp * ty *)
fun expType(exp: exp, occ: OC.occ, region) : exp * ty =
let fun boolUnifyErr { ty, name, message } =
	unifyErr { ty1 = ty, name1 = name, ty2 = BT.boolTy, name2 = "",
		   message = message, region = region, kind = ppExp,
		   kindname = "expression", phrase = exp }
    fun boolshortcut (con, what, e1, e2) =
	let val (e1', t1) = expType (e1, occ, region)
	    val (e2', t2) = expType (e2, occ, region)
	    val m = String.concat ["operand of ", what, " is not of type bool"]
	in
	    if boolUnifyErr { ty = t1, name = "operand", message = m }
	    andalso boolUnifyErr { ty = t2, name = "operand", message = m }
	    then (con (e1', e2'), MARKty(BT.boolTy, region))
	    else (exp, WILDCARDty)
	end
in
     case exp
      of VARexp(r as ref(v as V.VALvar{typ, ...}), _) =>
	   let val (ty, insts) = instantiatePoly(!typ)
	    in (VARexp(r, insts), MARKty(ty, region))
	   end
       | VARexp(varref as ref(V.OVLDvar _),_) =>
 	   (exp, olv_push (varref, region, err region))
       | VARexp(r as ref ERRORvar, _) => (exp, WILDCARDty)
       | CONexp(dcon as DATACON{typ,...},_) =>
           let val (ty,insts) = instantiatePoly typ
            in (CONexp(dcon, insts), MARKty(ty, region))
           end
       | NUMexp(src, {ival, ty}) => (exp, oll_push(ival, src, ty, err region))
(* REAL32: overload real literals *)
       | REALexp _ => (exp, MARKty(BT.realTy, region))
       | STRINGexp _ => (exp, MARKty(BT.stringTy, region))
       | CHARexp _ => (exp, MARKty(BT.charTy, region))
       | RECORDexp fields =>
           let fun h(l,exp') =
                    let val (nexp,nty) = expType (exp', occ, region)
                     in ((l,nexp),(l,nty))
                    end
               fun extract(LABEL{name,...},t) = (name,t)
               val (fields',tfields) = mapUnZip h fields
               val rty = map extract (sortFields tfields)
            in (RECORDexp fields', MARKty (BT.recordTy rty, region))
           end
       | RSELECTexp (exp, index) =>
           (* the index for RSELECTexp is 0-based, so need to increment to get
            * corresponding number label *)
           let val (nexp, nty) = expType(exp, occ, region)
               val res = mkMETAty ()
	       val label = Symbol.labSymbol(Int.toString (index+1))
               val labtys = [(label, res)]
               val tv = mkTyvar(mkFLEX(labtys,infinity))
               val pt = VARty tv
               val _ = registerFlex(tv,region)
            in (unifyTy(pt, nty, region, tyToLoc nty);
		(RSELECTexp(nexp,index), MARKty(res, region)))
               handle Unify(mode) =>
                 (err region COMPLAIN
                  (message("selecting a non-existing field from a record",mode))
                  (fn ppstrm =>
                   (PPType.resetPPType();
                    PP.newline ppstrm;
                    PP.string ppstrm "the field name: ";
                    ppSym ppstrm label;
                    PP.newline ppstrm;
                    PP.string ppstrm "the record type:    ";
                    ppType ppstrm nty; PP.newline ppstrm;
                    PP.string ppstrm "in expression:";
                    PP.break ppstrm {nsp=1,offset=2};
                    ppExp ppstrm (exp,!printDepth)));
                    (exp, WILDCARDty))
           end
       | VSELECTexp (exp, elemTy, index) => bug "expType:VSELECTexp"

       | VECTORexp(exps,_) =>
          (let val (exps',nty) = mapUnZip (fn e => expType (e, occ, region)) exps
               val vty = foldr (fn (a,b) => (unifyTy(a,b,tyToLoc a, tyToLoc b); b))
			       (mkMETAty()) nty
            in (VECTORexp(exps',vty),
	    	MARKty(CONty(BT.vectorTycon,[vty]), region))
           end handle Unify(mode) =>
	   (err region COMPLAIN
	     (message("vector expression type failure",mode))
             nullErrorBody; (exp,WILDCARDty)))

       | SEQexp exps =>
	   let fun scan nil = (nil, BT.unitTy)
	         | scan [e] =
                     let val (e',ety) = expType (e,occ,region)
                      in ([e'],ety)
                     end
		 | scan (e::rest) =
                     let val (e',_) = expType(e,occ,region)
                         val (el',ety) = scan rest
                      in (e'::el',ety)
                     end
               val (exps',expty) = scan exps
            in (SEQexp exps',MARKty(expty, region))
	   end

       | APPexp(rator, rand) =>
	   let val (rator',ratorTy) = expType (rator, occ, region)
	       val (rand',randTy) = expType (rand, occ, region)
               val exp' = APPexp(rator',rand')
	    in (exp',applyType(ratorTy,MARKty(randTy, region)))
	       handle Unify(mode) =>
	       let val ratorTy = prune ratorTy
		   val reducedRatorTy = headReduceType ratorTy
		in PPType.resetPPType();
		   if BT.isArrowType(reducedRatorTy)
		   then (err region COMPLAIN
			  (message("operator and operand do not agree",mode))
			  (fn ppstrm =>
			   (PP.newline ppstrm;
			    PP.string ppstrm "operator domain: ";
			    ppType ppstrm (BT.domain reducedRatorTy);
			    PP.newline ppstrm;
			    PP.string ppstrm "operand:         ";
			    ppType ppstrm randTy; PP.newline ppstrm;
			    PP.string ppstrm "in expression:";
			    PP.break ppstrm {nsp=1,offset=2};
			    ppExp ppstrm (exp,!printDepth);
			    ppModeErrorMsg ppstrm mode));
			 (exp,WILDCARDty))
		   else (err region COMPLAIN
			  (message("operator is not a function",mode))
			  (fn ppstrm =>
			    (PP.newline ppstrm;
			     PP.string ppstrm "operator: ";
			     ppType ppstrm (ratorTy); PP.newline ppstrm;
			     PP.string ppstrm "in expression:";
			     PP.break ppstrm {nsp=1,offset=2};
			     ppExp ppstrm (exp,!printDepth);
			     ppModeErrorMsg ppstrm mode));
			 (exp,WILDCARDty))
	       end
	   end

       | CONSTRAINTexp(e,ty) =>
	   let val (e',ety) = expType (e, occ, region)
	    in if unifyErr{ty1=ety,name1="expression", ty2=ty, name2="constraint",
			message="expression does not match constraint",
			region=region,kind=ppExp,kindname="expression",
			phrase=exp}
		then (CONSTRAINTexp(e',MARKty(ty, region)),
			MARKty(ty, region))
		else (exp,WILDCARDty)
	   end

       | HANDLEexp(baseExp, (rules, _, _)) =>
	   let val (baseExp', baseTy) = expType (baseExp, occ, region)
	       val (rules', argTy, resTy) = matchType (rules, occ, region)
               val handleExp = HANDLEexp(baseExp', (rules', argTy, resTy))
	    in (unifyTy(argTy-->resTy, BT.exnTy --> baseTy, region, tyToLoc baseTy); (* unifyErr? *)
		(handleExp, MARKty(baseTy, region)))
	       handle Unify(mode) =>
		 (if unifyErr{ty1=prune argTy, name1="handler domain",
			      ty2=BT.exnTy, name2="",
			      message="handler domain is not exn",
			      region=region,kind=ppExp,kindname="expression",
			      phrase=exp}
		     then unifyErr{ty1=baseTy, name1="body",
				   ty2=prune resTy, name2="handler range",
				   message="expression and handler do not agree",
				   region=region,
				   kind=ppExp,kindname="expression",phrase=exp}
		     else false;
		  (exp,WILDCARDty))
	   end

       | RAISEexp (exnExp, _) =>
	   let val (exnExp',ety) = expType (exnExp, occ, region)
               val newty = mkMETAty() (* new type metavariable, will unify with context *)
	    in unifyErr{ty1=ety, name1="raised", ty2=BT.exnTy, name2="",
			message="argument of raise is not an exception",
			region=region,kind=ppExp,kindname="expression",phrase=exp};
	       (RAISEexp (exnExp', newty), MARKty (newty, region))
	   end

       | LETexp (dec,exp) =>
           let val dec' = decType0 (dec, OC.LetDef(occ), region)
               val (exp', expTy) = expType (exp, occ, region)
            in (LETexp (dec', exp'), MARKty (expTy, region))
           end

       | CASEexp (scrutinee,(rules,_,_)) =>
	   let val (scrutinee', scrutTy) = expType (scrutinee, occ, region)
	       val (rules', argTy, resTy) = matchType (rules,occ,region)
               val exp' = CASEexp (scrutinee', (rules', argTy, resTy))
	    in (unifyTy (scrutTy, argTy, region, tyToLoc scrutTy);  (* unifyErr? *)
                (exp', MARKty(resTy, region)))
	       handle Unify(mode) =>
	       (unifyErr{ty1 = argTy, name1 = "rule domain",
			 ty2 = scrutTy, name2 = "object",
			 message = "case object and rules do not agree",
			 region = region, kind = ppExp, kindname = "expression",
			 phrase = exp};
	        (exp,WILDCARDty))
	   end (* this causes case to behave differently from let, i.e.
		  bound variables do not have polymorphic types *)

       | IFexp { test, thenCase, elseCase } =>
	   let val (test', tty) = expType (test, occ, region)
	       val (thenCase', tct) = expType (thenCase, occ, region)
	       val (elseCase', ect) = expType (elseCase, occ, region)
	   in
	       if boolUnifyErr
		      { ty = tty, name = "test expression",
			message="test expression in if is not of type bool" }
	       andalso
	          unifyErr { ty1 = tct, name1 = "then branch",
			     ty2 = ect, name2 = "else branch",
			     message="types of if branches do not agree",
			     region = region, kind = ppExp,
			     kindname = "expression", phrase = exp }
	       then
		   (IFexp { test = test', thenCase = thenCase',
			    elseCase = elseCase' },
		    MARKty(tct, region))
	       else
		   (exp, WILDCARDty)
	   end
       | ANDALSOexp (e1, e2) =>
	   boolshortcut (ANDALSOexp, "andalso", e1, e2)
       | ORELSEexp (e1, e2) =>
	   boolshortcut (ORELSEexp, "orelse", e1, e2)
       | WHILEexp { test, expr } =>
	   let val (test', tty) = expType (test, occ, region)
	       val (expr', _) = expType (expr, occ, region)
	   in
	       if boolUnifyErr { ty = tty, name = "test expression",
				 message = "test expression in while is not of type bool" }
	       then
		   (WHILEexp { test = test', expr = expr' }, MARKty (BT.unitTy, region))
	       else
		   (exp, WILDCARDty)
	   end
       | FNexp(rules,_,_) =>
           let val (rules', argTy, resTy) = matchType (rules, occ, region)
            in (FNexp (rules', argTy, resTy), MARKty(argTy --> resTy, region))
           end
       | MARKexp(e,region) =>
           let val (e',et) = expType (e, occ, region)
            in (MARKexp(e',region),MARKty(et, region))
           end
end

(* ruleType : rule * occ * region -> rule * ty * ty *)
(* the types returned are the lhs (pat) type and the rhs (exp) types *)
and ruleType (RULE(pat,exp), occ, region) =
    let val occ = OC.Abstr occ
	val (pat',patTy) = patType (pat, OC.lamdepth occ, region)
	val (exp',expTy) = expType (exp, occ, region)
     in (RULE(pat',exp'), patTy, expTy)
    end

(* matchType : rule list * occ * region -> rule list * ty * ty *)
and matchType (rules, occ, region) =
    case rules
      of [] => bug "empty rule list in Typecheck.matchType"
       | [rule] =>
	    let val (rule0, lhsTy, rhsTy) = ruleType (rule, occ, region)
	     in ([rule0], lhsTy, rhsTy)
	    end
       | rule::rest =>
	    let val (rule0, lhsTy0, rhsTy0) = ruleType (rule, occ, region)
		fun checkrule rule =
		   let val (rule1, lhsTy1, rhsTy1) = ruleType (rule, occ, region)
		    in unifyErr{ty1 = (lhsTy0 --> rhsTy0), ty2 = (lhsTy1 --> rhsTy1),
				name1 = "earlier rule(s)", name2 = "this rule",
				message = "types of rules do not agree",
				region = region, kind = ppRule, kindname = "rule",
				phrase = rule1};
		       rule1
		   end
	     in (rule0::(map checkrule rest), lhsTy0, rhsTy0)
	    end

and decType0 (decl, occ, region) : dec =
    case decl
      of VALdec vbs =>
	   let val _ = dbsaynl ">>> decType0 VALdec"
	       fun vbType(vb as VB{pat, exp, tyvars=(tv as (ref tyvars)), ...}) =
		   let val (typedPat,patTy) = patType (pat, infinity, region)
		       val (typedExp,expTy) = expType (exp, occ, region)
		       val generalize =
			   (* suppress generalization unless irrefutable pat and value
			    * expression on RHS *)
			   TypesUtil.isValue exp andalso not (TypesUtil.refutable pat)
					(* orelse isVarTy expTy ?? *)
		       val _ = unifyErr{ty1=patTy, ty2=expTy, name1="pattern", name2="expression",
				message="pattern and expression in val dec do not agree",
				region=region,kind=ppVB,kindname="declaration",
				phrase=vb};
		       val boundtvs = generalizePat(typedPat, tyvars, occ, generalize, region)
		       (* doesn't matter whether pat or typedPat is used here, since the
			* pattern structure is identical, and patType updates the var typ
			* fields of pattern variables. *)
		       val vb = VB{pat = typedPat, exp = typedExp,
				   typ = patTy,
				   boundtvs = boundtvs,
				   tyvars = tv}  (* preserve the ref for explicit tyvars *)
		     in debugPrint ("decType0 VALdec VB: ", ppVB, (vb,100));
			dbsaysnl ["   generalize: ", Bool.toString generalize];
			dbsaysnl ["   occ: ", Int.toString (OC.lamdepth occ), ", ",
				  Bool.toString (OC.toplevel occ)];
			dbsaysnl ["   # boundtvs: ", Int.toString (length boundtvs)];
			vb
		    end
	       val result = VALdec(map vbType vbs)
	    in dbsaynl "<<< decType0: VALdec";
	       result
	   end (* VALdec *)

       | VALRECdec rvbs =>
 	   (* -- First go through and type-check all the patterns and
		 result-constraints, unifying with each other and with
		 the specified "result" type (i.e. function type), if any.
              -- Initially we assume that the typ field of the var in
                 each RVB is UNDEFty, though this does not matter since we don't
                 use that type and the typ field  will be assigned the inferred type. *)

	       (* preType : rvb -> (unit -> exp) *)
	   let fun preType(rvb as RVB{var=V.VALvar{typ,...},exp,resultty,tyvars}) =
                   let val domainty = mkMETAty () (* mkMETAtyBounded(OC.lamdepth occ) *) (* lamdepth = infinity *)
		       val rangety = (* mkMETAty () *) mkMETAtyBounded(OC.lamdepth occ + 1) (* lamdepth = infinity *)
		       val funty = domainty --> rangety  (* the inferred type of var *)

		       val _ = typ := funty

		       val _ = (* unify funty with resultty, if any *)
			   case resultty
			    of NONE => true
			     | SOME constraintTy =>  (* should be a function type *)
			       unifyErr{ty1 = funty, name1="",
					ty2 = constraintTy, name2="constraint",
					message="type constraint of val rec dec\
							   \ is not a function type",
					region=region, kind=ppRVB,
					kindname="declaration", phrase=rvb}

                       (* pre: exp * region -> (unit -> exp) *)
		       (* type the defn expression, which will be a FNexp, possibly inside
                        * MARKexp and/or CONSTRAINTexp wrappers *)
		       fun pre (FNexp (rules, _, _), region) =
		             let val inside_occ = OC.Abstr occ
				 val inside_lamdepth = OC.lamdepth inside_occ
			         fun checkRulePat (RULE (pat, exp)) =
				     let val (typedPat, patTy) = patType (pat, inside_lamdepth, region)
				      in case exp
					   of CONSTRAINTexp(exp',constraintTy) =>
					        (typedPat, patTy-->constraintTy, (exp',region))
					    | _ => (typedPat, patTy-->rangety, (exp,region))
				     end

				 fun checkRuleExp (exp,region) =
				     let val (typedExp, expTy) = expType (exp, inside_occ, region)
				      in unifyErr{ty1 = expTy, name1 = "expression",
						  ty2 = rangety, name2 = "result type",
						  message="right-hand-side of function clause\
					            \ does not agree with function result type",
						  region=region, kind=ppRVB,
						  kindname="declaration", phrase=rvb};
					 typedExp
				     end

			       fun unify ruleTy =
				   (unifyErr{ty1 = ruleTy, name1 = "this clause",
					     ty2 = funty, name2 = "previous clauses",
					     message = "parameter or result constraints\
						       \ of clauses do not agree",
					     region=region, kind=ppRVB,
					     kindname="declaration", phrase=rvb};
				    ())

			         (* could be constructed by foldl rather than map? *)
				 val patTyExps = map checkRulePat rules
				 val pats = map #1 patTyExps
				 val ruletys = map #2 patTyExps
				 val exps = map #3 patTyExps

                              in app unify ruletys;  (* unify all the rule types with funty *)
				 fn () => FNexp (ListPair.map RULE (pats, map checkRuleExp exps),
					         prune(domainty), prune(rangety))
			     end
		         | pre (MARKexp (exp, region), _) =
			     let val typeExp = pre (exp, region)
			      in fn () => MARKexp (typeExp (), region)
			     end
                         | pre (CONSTRAINTexp (exp, constraintTy), region) =
			     (unifyErr{ty1 = constraintTy, name1 = "this constraint",
				       ty2 = funty, name2 = "outer constraints",
				       message="type constraints on val rec declaraction disagree",
				       region=region, kind=ppRVB,
				       kindname="declaration", phrase=rvb};
			      let val typeExp = pre (exp, region)
			       in (fn () => CONSTRAINTexp (typeExp (), constraintTy))
			      end)
			| pre _ = bug "typecheck.823"
                    in pre (exp, region)
                   end
	         | preType _ = bug "preType -- arg not RVB"

	      (* Second, go through and type-check the right-hand-side
	         expressions (function bodies) *)
	       fun typeRVB (RVB {var as V.VALvar{typ,...}, resultty, tyvars, ...}, typeRHS) =
		   let val typedRHS = typeRHS ()
		      (* finish typing the rhs fun exp -- this can affect var typ through
                       * unification via the rangety metavariable *)
                    in RVB {var=var, exp=typedRHS, resultty=resultty, tyvars=tyvars}
		   end

               (* this will set the typ and btvs fields of the var, if generalized *)
	       fun genType (RVB{var,tyvars, ...}) =
		   ignore (generalizeTy (var, !tyvars, occ, true, region))

	       val _ = dbsaynl ">>> decType0: VALRECdec"

               val rhsTypings = map preType rvbs
		   (* pre-type the rvbs, unifying with fun arg pats and constraints *)
               val typedRVBs = ListPair.map typeRVB (rvbs, rhsTypings)
		   (* type the rvbs rhs expressions, which can affect var type through unification *)
	       val _ = app genType typedRVBs
                   (* Lastly, apply genType to generalize the var typ *)
	    in dbsaynl "<<< decType0: VALRECdec";
	       VALRECdec typedRVBs
	   end

       | DOdec exp => let
	  val (exp',ety) = expType (exp, occ, region)
	  val _ = unifyErr{
		    ty1=BT.unitTy, ty2=ety, name1="", name2="expression",
		    message="do expression does not have type unit",
		    region=region, kind=ppDec, kindname="declaration",
		    phrase=decl
		  }
	  in
	    DOdec exp'
	  end

       | EXCEPTIONdec ebs =>
	   let fun check(VARty(ref(UBOUND _))) =
		     err region COMPLAIN
		         "type variable in top level exception type"
			 nullErrorBody
		 | check(CONty(_,args)) =
		     app check args
		 | check _ = ()
	       fun ebType(EBgen{etype=SOME ty,...}) = check ty
	         | ebType _ = ()
	       val _ = dbsaynl ">>decType0: EXCEPTIONdec"
            in if OC.lamdepth occ < 1 then app ebType ebs else ();
               decl
	   end

       | LOCALdec (decIn,decOut) =>
	   let val decIn' = decType0 (decIn, OC.LetDef occ, region)
               val decOut' = decType0 (decOut, occ, region)
	       val _ = dbsaynl ">>decType0: LOCALdec"
            in LOCALdec(decIn',decOut')
           end

       | SEQdec decls =>
           SEQdec(map (fn decl => decType0 (decl, occ, region)) decls)

       | ABSTYPEdec {abstycs,withtycs,body} =>
	   let fun makeAbstract(GENtyc { eq, ... }) = eq := ABS
		 | makeAbstract _ = bug "makeAbstract"
	       fun redefineEq(DATATYPEdec{datatycs,...}) =
		   let fun setDATA (GENtyc { eq, ... }) = eq := DATA
			 | setDATA _ = ()
		   in
		       app setDATA datatycs;
		       EqTypes.defineEqProps(datatycs,nil,EntityEnv.empty)
		   end
	         | redefineEq(SEQdec decs) = app redefineEq decs
	         | redefineEq(LOCALdec(din,dout)) =
		    (redefineEq din; redefineEq dout)
	         | redefineEq(MARKdec(dec,_)) = redefineEq dec
	         | redefineEq _ = ()
	       val body'= decType0 (body, occ, region)
	       val _ = dbsaynl ">>decType0: ABSTYPEdec"
	    in app makeAbstract abstycs;
	       redefineEq body';
	       ABSTYPEdec{abstycs=abstycs,withtycs=withtycs,body=body'}
	   end

       | MARKdec (dec,region) => MARKdec (decType0 (dec, occ, region), region)

      (*
       * The next several declarations will never be seen ordinarily;
       * they are for re-typechecking after the instrumentation phase
       * of debugger or profiler.
       *)
       | STRdec strbs => STRdec(map (strbType (occ, region)) strbs)
       | FCTdec fctbs => FCTdec(map (fctbType (occ, region)) fctbs)
       | _ => decl

and fctbType (occ, region) (FCTB{fct,def,name}) =
      let fun fctexpType(FCTfct{param, argtycs, def}) =
  	        FCTfct{param=param, def=strexpType (occ, region) def,
	               argtycs=argtycs}
 	    | fctexpType(LETfct(dec,e)) =
	        LETfct (decType0 (dec, OC.LetDef occ, region), fctexpType e)
	    | fctexpType(MARKfct(f,region)) = MARKfct(fctexpType f,region)
            | fctexpType v = v
       in FCTB{fct=fct,def=fctexpType def,name=name}
      end

and strexpType (occ, region) (se as (APPstr{oper,arg,argtycs})) = se
  | strexpType (occ, region) (LETstr(dec,e)) =
      LETstr (decType0 (dec, OC.LetDef occ, region), strexpType (occ, region) e)
  | strexpType (occ, _) (MARKstr(e,region)) =
      MARKstr(strexpType (occ, region) e, region)
  | strexpType _ v = v

and strbType (occ, region) (STRB{str,def,name}) =
    STRB{str=str, def=strexpType (occ, region) def, name=name}

val _ = dbsaynl ">>decType: calling decType0"
val occ0 = if toplev then OC.Root else (OC.LetDef OC.Root)
val dec' = decType0(dec, occ0, region)
in
    ol_resolve env;
    checkFlex ();
    dbsaynl "<<decType: returning";
    dec'
end (* function decType *)

val decType = Stats.doPhase (Stats.makePhase "Compiler 035 typecheck") decType

end (* local *)
end (* structure Typecheck *)
