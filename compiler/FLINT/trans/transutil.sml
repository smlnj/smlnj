(* transutil.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure TransUtil =
struct

local  (* don't need all these structures *)
  structure LV = LambdaVar
  structure DA = Access
  structure SP = SymPath
  structure V = Variable
  structure T = Types
  structure TU = TypesUtil
  structure PL = PLambda
  open Absyn
in

val debugging = FLINT_Control.trdebugging
fun bug msg = ErrorMsg.impossible("TransUtil: " ^ msg)
val say = Control.Print.say
fun says strs = say (concat strs)
fun newline () = say "\n"
fun saynl str = (say str; newline())
fun saysnl strs = saynl (concat strs)
fun dbsaynl (msg : string) =
    if !debugging then saynl msg else ()
fun dbsaysnl (msgs : string list) =
    if !debugging then saysnl msgs else ()

fun ppType ty =
    ElabDebug.withInternals
     (fn () => ElabDebug.debugPrint debugging
		("type: ", PPType.ppType StaticEnv.empty, ty))

fun ident x = x
val unitLexp = PL.RECORD []

(* pathToName would be a better name for this function *)
fun getNameOp p = if SP.null p then NONE else SOME(SP.last p)

type pid = PersStamps.persstamp
type compInfo = Absyn.dec CompInfo.compInfo

(* foldr' : ('a * 'b -> 'b) -> 'a list -> 'b -> 'b
 *   old-style foldr for cases where it is partially applied *)
fun foldr' f l init = foldr f init l

(** sorting the record fields for record types and record expressions *)
fun elemgtr ((LABEL{number=x,...},_),(LABEL{number=y,...},_)) = (x>y)
fun sorted x = ListMergeSort.sorted elemgtr x
fun sortrec x = ListMergeSort.sort elemgtr x

(** check if an access is external *)
fun extern (DA.EXTERN _) = true
  | extern (DA.PATH(a, _)) = extern a
  | extern _ = false

(** an exception raised if coreEnv is not available *)
exception NoCore

(* instPoly redundant.  Use TypesUtil.applyPoly
(** instPoly : ty * ty list -> ty
 * instPoly(ty,tyargs): the type ty is instantiated with parameters tyargs.
 * Checked innvariant: ts <> nil <==>  t is polymophic (a POLYty) (DBM) *)
fun instPoly(ty: T.ty, tyargs : T.ty list) : T.ty =
    case tyargs
      of nil =>  (* no instantiation parameters *)
         (case ty
            of T.POLYty{tyfun=T.TYFUN{arity,body},...} =>
               if arity = 0 then body
               else (say "instPoly: polytype with no inst parameters\n";
                     ppType ty;
                     ty)
             | _ => ty)
       | _ =>    (* instantiation parameters *)
         (case ty
            of T.POLYty _ => TU.applyPoly(ty, tys)
             | _ => bug "instPoly: non-polytype with inst parameters")
 *)

(* aconvertPat: Absyn.pat * compInfo -> Absyn.pat * V.var list * V.var list
 *   "alpha convert" a pattern with respect to the lvar access values
 *   of the pattern variables. Original variables are replaced by
 *   new ones, with fresh LVAR accesses and new refs for the typ field.
 *   Returns the converted pattern, the list of the original pattern
 *   variables (VALvars), and the list of new variables (VALvars).
 *   Called only once, in mkVB inside mkVBs in Translate.
 *   DBM: why is this function needed? *)

fun aconvertPat (pat, {mkLvar=mkv, ...} : compInfo)
    : Absyn.pat * V.var list * V.var list =
    let val varmap : (V.var * V.var) list ref = ref nil
            (* association list mapping old vars to newl; alpha-conversion substitution *)
        (* ASSERT: all vars in a VARpat will have an LVAR access. *)
        (* ASSERT: pat will not contain MARKpat, because stripPatMarks has been applied *)
	fun mappat (VARpat(oldvar as V.VALvar{access=DA.LVAR(oldlvar),
                                            typ=ref oldtyp,prim,btvs,path})) =
              let fun find ((V.VALvar{access=DA.LVAR(lv),...}, newvar)::rest) =
                        if lv=oldlvar then newvar else find rest
			(* a variable could occur multiple times because
                           repetition in OR patterns *)
                    | find (_::rest) = bug "aconvertPat: bad varmap key"
		    | find nil =
		        let val (newtyp,_) = TypesUtil.instantiatePoly oldtyp
			    val newvar =
                                V.VALvar{access=DA.dupAcc(oldlvar,mkv), prim=prim,
					  typ=ref newtyp, path=path, btvs = btvs}
			 in varmap := (oldvar,newvar)::(!varmap); newvar
			end
	       in VARpat(find(!varmap))
	      end
	  | mappat (VARpat _) = bug "aconvertPat: bad variable"
	  | mappat (RECORDpat{fields,flex,typ}) =
	      RECORDpat{fields=map (fn(l,p)=>(l,mappat p)) fields,
                        flex=flex, typ=typ}
	  | mappat (VECTORpat(pats,t)) = VECTORpat(map mappat pats, t)
	  | mappat (APPpat(d,c,p)) = APPpat(d,c,mappat p)
	  | mappat (ORpat(a,b)) = ORpat(mappat a, mappat b)
	  | mappat (CONSTRAINTpat(p,t)) = CONSTRAINTpat(mappat p, t)
	  | mappat (LAYEREDpat(p,q)) = LAYEREDpat(mappat p, mappat q)
	  | mappat (MARKpat(p,_)) = bug "aconvertPat: MARKpat"
	  | mappat p = p

        val newpat = mappat pat

        val (oldvars,newvars) = ListPair.unzip (!varmap)

     in (newpat,oldvars,newvars)
    end (* aconvertPat *)

(* checkBoundTvsEqual: T.tyvar list * T.tyver list -> unit *)
(* check that the two tyvar lists are equal as tyvar sets (i.e. same elements. We can't depend
 * on an order on the lists, or even that they are the same length. The boundtvs argument is
 * from the boundtvs field of a VB, while the btvs is from the btvs field of a VALvar. *)
fun checkBoundTvsEqual (boundtvs, btvs) =
    let val lengthBoundtvs = length boundtvs
	val lengthBtvs = length btvs
	fun member (tv, tvs) = List.exists (fn tv' => TU.eqTyvar (tv, tv')) tvs
	fun equalTvs (tvs1, tvs2) =  (* assuming tvs1 and tvs2 have same lenght *)
	    let val remainder = List.filter (fn tv => member (tv, tvs1)) tvs2
	     in length remainder = length tvs2
	    end
    in if lengthBoundtvs <> lengthBtvs
       then bug (concat ["checkBoundTvs: boundtvs and btvs have different lengths: ",
			 Int.toString lengthBoundtvs, ", ", Int.toString lengthBtvs])
       else if not (equalTvs (boundtvs, btvs))
       then bug "checkBoundTvs: boundtvs and btvs not equal"
       else () (* print "checkBoundTvsEqual: OK\n" *)
    end

(* checkBoundTvsSubset: T.tyvar list * T.tyver list -> unit *)
(* check that the first tyvar list is a subset (as tyvar sets) of the second. We can't depend
 * on an order on the lists. *)
fun checkBoundTvsSubset (tvs1, tvs2) =
    let fun member tvs tv = List.exists (fn tv' => TU.eqTyvar (tv, tv')) tvs
	fun subsetTvs (tvs1, tvs2) =
	    List.all (member tvs2) tvs1
    in if not (subsetTvs (tvs1, tvs2))
       then bug "checkBoundTvsSubset: FAIL\n"
       else () (* print "checkBoundTvsSubset: OK\n" *)
    end

(* ## obsolete ## -- to be deleted
(* recDecs : Absyn.rvb list -> Absyn.dec *)
(* formerly defined in translate/nonrec.sml. Now moved to transutil.sml, but not used.
 * Checks whether a single "recursive" function declaration (RVB) is not actually recursive,
 * in which case it converts it into a simple VALdec.
 * This was assumed to be performed pre-typechecking.
 * This function is now performed by aconvertLvars below, so it is obsolete.*)
fun recDecs (rvbs as [RVB {var as V.VALvar{access=A.LVAR defLvar,typ,btvs, ...},
                           exp, resultty, tyvars}]) =
     let fun findexp e =
            (case e
              of VARexp (ref(V.VALvar{access=A.LVAR x, ...}), _) =>
                   if x = defLvar then raise IsRec else ()
	       | VARexp _ => ()
               | RECORDexp l => app (fn (lab, x)=>findexp x) l
               | SEQexp l => app findexp l
               | APPexp (a,b) => (findexp a; findexp b)
               | CONSTRAINTexp (x,_) => findexp x
               | HANDLEexp (x, (l, _, _)) =>
		   (findexp x; app (fn RULE (_, x) => findexp x) l)
               | RAISEexp (x, _) => findexp x
               | LETexp (d, x) => (finddec d; findexp x)
               | CASEexp (x, (rules,_,_)) =>
                   (findexp x; app (fn RULE (_, x) => findexp x) rules)
	       | IFexp { test, thenCase, elseCase } =>
		   (findexp test; findexp thenCase; findexp elseCase)
	       | (ANDALSOexp (e1, e2) | ORELSEexp (e1, e2) |
		  WHILEexp { test = e1, expr = e2 }) =>
		    (findexp e1; findexp e2)
               | FNexp (l, _, _) =>  app (fn RULE (_, x) => findexp x) l
               | MARKexp (x, _) => findexp x
	       | VECTORexp (el, _) => app findexp el
	       | (CONexp _ | NUMexp _ | REALexp _ | STRINGexp _ | CHARexp _) => ()
	       | (RSELECTexp _ | VSELECTexp _ | SWITCHexp _ | VSWITCHexp _) => ())

          and finddec d =
            (case d
              of VALdec vbl => app (fn (VB{exp,...}) => findexp exp) vbl
               | VALRECdec rvbl => app (fn(RVB{exp,...})=>findexp exp) rvbl
               | LOCALdec (a,b) => (finddec a; finddec b)
               | SEQdec l => app finddec l
               | ABSTYPEdec {body, ...} => finddec body
               | MARKdec (dec,_) => finddec dec
               | _ => ())

       in (findexp exp;
           VALdec [VB{pat=VARpat var, typ=!typ, tyvars=tyvars, boundtvs=!btvs,
                      exp = case resultty
                             of SOME ty => CONSTRAINTexp(exp,ty)
                              | NONE => exp}])
          handle IsRec => VALRECdec rvbs
      end

  | recDecs rvbs = VALRECdec rvbs
*)

(* aconvertLvars : V.var list * Absyn.exp list -> LV.lvar list * bool *)
(* aconvertLvars (vars, exps): replaces occurrences of vars in exps with new versions with
   fresh lvar access, and returns the list of these fresh lvars, plus a boolean that
   indicates whether any substitutions were performed (i.e. if any of the vars occurred
   in the exps).  Substitution is performed by overwriting the var field of VARexps,
   so the exp arguments are altered in place.
   ASSERT: v in vars is a bound variable, with access = DA.LVAR lv *)
fun aconvertLvars (vars, exps) =
    let val oldLvars = map (fn V.VALvar {access = DA.LVAR lvar,...} => lvar) vars
	val (newVars, newLvars) = ListPair.unzip (map V.replaceLvar vars)
	val occurs = ref false  (* is there an occurrence of a vars in an exp *)
	val lvarMap = ListPair.foldr
		        (fn (oldLvar, newVar, map) => LV.Map.insert (map, oldLvar, newVar))
			LV.Map.empty (oldLvars, newVars)
	fun substExp e =
            (case e
              of VARexp (var as ref(V.VALvar {access = DA.LVAR x, ...}), _) =>
		 (dbsaysnl ["aconvertLvars:old ", V.toString (!var), " ", LV.toString x];
		  case !var
		    of V.VALvar {access = DA.LVAR x, ...} =>
                       (case LV.Map.find (lvarMap,x)
			 of NONE => (dbsaysnl ["aconvertLvars:not bound ", LV.toString x]; ())
			   | SOME newVar =>
			     (dbsaynl ("aconvertLvars:newVar = " ^ V.toString newVar);
			      var := newVar; occurs := true))
		     | _ => (dbsaynl "aconvertLvars:not LVAR"; ()))
	       | VARexp _ => ()
               | RECORDexp l => app (fn (lab, x) => substExp x) l
               | SEQexp l => app substExp l
               | APPexp (a,b) => (substExp a; substExp b)
               | CONSTRAINTexp (x,_) => substExp x
               | HANDLEexp (x, (l, _, _)) =>
		   (substExp x; app (fn RULE (_, x) => substExp x) l)
               | RAISEexp (x, _) => substExp x
               | LETexp (d, x) => (substDec d; substExp x)
               | LETVexp (var, defexp, bodyexp) => (substExp defexp; substExp bodyexp)
               | CASEexp (x, (rules,_,_)) =>
                   (substExp x; app (fn RULE (_, x) => substExp x) rules)
	       | IFexp { test, thenCase, elseCase } =>
		   (substExp test; substExp thenCase; substExp elseCase)
	       | (ANDALSOexp (e1, e2) | ORELSEexp (e1, e2) |
		  WHILEexp { test = e1, expr = e2 }) =>
		    (substExp e1; substExp e2)
               | FNexp (l, _, _) =>  app (fn RULE (_, x) => substExp x) l
               | MARKexp (x, _) => substExp x
	       | VECTORexp (el, _) => app substExp el
	       | SWITCHexp (_, srules, defaultOp) =>
		   (app (fn SRULE(_,_,e) => substExp e) srules;
		    Option.app substExp defaultOp)
	       | VSWITCHexp (_, _, srules, default) =>
		   (app (fn SRULE(_,_,e) => substExp e) srules;
		    substExp default)
	       | _ => ())

          and substDec d =
            (case d
              of VALdec vbl => app (fn (VB{exp,...}) => substExp exp) vbl
               | VALRECdec rvbs => app (fn(RVB{exp,...}) => substExp exp) rvbs
               | LOCALdec (a,b) => (substDec a; substDec b)
               | SEQdec l => app substDec l
               | ABSTYPEdec {body, ...} => substDec body
               | MARKdec (dec,_) => substDec dec
	       | DOdec exp => substExp exp
               | _ => ())

    in app substExp exps;
       (newLvars, !occurs)
    end

end (* local *)
end (* structure TransUtil *)
