(* elabutil.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure ElabUtil : ELABUTIL =
struct

local structure SP = SymPath
      structure LU = Lookup
      structure A = Access
      structure AS = Absyn
      structure AU = AbsynUtil
      structure B  = Bindings
      structure SE = StaticEnv
      structure EE = EntityEnv
      structure T = Types
      structure TU = TypesUtil
      structure TS = TyvarSet
      structure S = Symbol
      structure V = Variable
      structure BT = BasicTypes

      open Symbol Absyn Ast ErrorMsg PrintUtil AstUtil Types BasicTypes
           EqTypes ModuleUtil TypesUtil Variable

in

(* debugging *)
val say = Control_Print.say
val debugging = ref false
fun debugmsg (msg: string) =
    if !debugging then (say msg; say "\n") else ()

fun bug msg = ErrorMsg.impossible("ElabUtil: "^msg)

fun for l f = app f l
fun single x = [x]

val internalSym = SpecialSymbols.internalVarId

(* elaboration context *)

datatype context
  = TOP      (* at top level -- not inside any module, rigid *)
  | INSTR    (* inside a rigid structure, i.e. not inside any functor body *)

  | INFCT of {flex: Stamps.stamp -> bool,  depth: DebIndex.depth}
             (* within functor body *)
  | INSIG    (* within a signature body *)

type compInfo = Absyn.dec CompInfo.compInfo

fun mkVALvar(s, mkv) = V.mkVALvar(s, A.namedAcc(s, mkv))

fun smash f l =
    let fun h(a,(pl,oldl,newl)) =
	  let val (p,old,new) = f a
	   in (p::pl,old@oldl,new@newl)
	  end
     in foldr h (nil,nil,nil) l
    end

local
  fun uniq ((a0 as (a,_,_))::(r as (b,_,_)::_)) =
	if S.eq(a,b) then uniq r else a0::uniq r
    | uniq l = l
  fun gtr ((a,_,_), (b,_,_)) =  let
	val a' = S.name a and b' = S.name b
	val a0 = String.sub(a',0) and b0 = String.sub(b',0)
	in
	  if Char.isDigit a0
	      then if Char.isDigit b0
		then size a' > size b' orelse size a' = size b' andalso a' > b'
		else false
	      else if Char.isDigit b0
		then true
		else (a' > b')
	end
 in fun sort3 x = uniq (ListMergeSort.sort gtr x)
end

(* Access to the match and bind exceptions for match compilation. *)
local
  val matchExnRef : T.datacon option ref = ref NONE
  val bindExnRef : T.datacon option ref = ref NONE
in
  fun initializeMatchBind (env: SE.staticEnv) =
      (* Define exceptions used in match compilation when a match/bind is
       * non-exhaustive.
       * The staticEnv argument is assumed to contain the Core structure, so
       * initializeMatchBind must be called in a context having such a staticEnv
       * available, e.g. elabTop. The Match and Bind exceptions are needed for
       * match compilation; this function is called in matchcomp/matchcomp.sml. *)
      (matchExnRef := SOME (CoreAccess.getExn env ["Match"]);
       bindExnRef := SOME (CoreAccess.getExn env ["Bind"]))

  fun getMatchExn () =
      (case !matchExnRef
         of NONE => bug "uninitialized Match exn"
          |  SOME exn => exn)

  fun getBindExn () =
      (case !bindExnRef
         of NONE => bug "uninitialized Bind exn"
          | SOME exn => exn)

end (* local *)

val EQUALsym = S.varSymbol "="

val anonParamName = S.strSymbol "<AnonParam>"

(* following could go in Absyn *)
val bogusID = S.varSymbol "*bogus*"
val bogusExnID = S.varSymbol "*Bogus*"

val TRUEpat = CONpat(trueDcon,[])
val TRUEexp = CONexp(trueDcon,[])
val FALSEpat = CONpat(falseDcon,[])
val FALSEexp = CONexp(falseDcon,[])

val NILpat = CONpat(nilDcon,[])
val NILexp = CONexp(nilDcon,[])
val CONSpat = fn pat => APPpat(consDcon,[],pat)
val CONSexp = CONexp(consDcon,[])

val unitExp = AbsynUtil.unitExp
val unitPat = RECORDpat{fields = nil, flex = false, typ = ref UNDEFty}
val bogusExp = VARexp(ref(V.mkVALvar(bogusID, A.nullAcc)), [])

(* Verifies that all the elements of a list are unique *)
fun checkUniq (err,message,names) =
    let val names' = ListMergeSort.sort S.symbolGt names
	fun check (x::y::rest) =
	     (if S.eq(x,y)
	      then err COMPLAIN (message ^ ": " ^ S.name x) nullErrorBody
	      else ();
	      check (y::rest))
	  | check _ = ()
     in check names'
    end

(* symbols that are forbidden for use as data or exn constructor names *)
val forbiddenConstructors =
    [EQUALsym, S.varSymbol "it", S.varSymbol "true", S.varSymbol "false",
     S.varSymbol "nil", S.varSymbol "::", S.varSymbol "ref"]

(* checks whether names contains a forbidden constructor name *)
fun checkForbiddenCons symbol =
    List.exists (fn x => S.eq(symbol,x)) forbiddenConstructors

(*
 * Extract all the variables from a pattern
 * NOTE: the "freeOrVars" function in elabcore.sml should probably
 * be merged with this.
 *)
fun bindVARp (patlist,err) =
    let val vl = ref (nil: symbol list)
	val env = ref(SE.empty: SE.staticEnv)
	fun f (VARpat(v as VALvar{path=SP.SPATH[name],...})) =
	       (if S.eq(name, EQUALsym)
		then err WARN "rebinding \"=\" is not allowed" nullErrorBody
		else ();
		env := SE.bind(name,B.VALbind v,!env);
		vl := name :: !vl)
	  | f (RECORDpat{fields,...}) = app(fn(_,pat)=>f pat) fields
	  | f (VECTORpat(pats,_)) = app f pats
	  | f (APPpat(_,_,pat)) = f pat
	  | f (CONSTRAINTpat(pat,_)) = f pat
	  | f (LAYEREDpat(p1,p2)) = (f p1; f p2)
	  | f (ORpat(p1, p2)) = (f p1; bindVARp([p2], err); ())
	  | f (MARKpat(p,_)) = f p
	  | f _ = ()
     in app f patlist;
	checkUniq (err,"duplicate variable in pattern(s)",!vl);
	!env
    end

(* sort the labels in a record the order is redefined to take the usual
   ordering on numbers expressed by strings (tuples) *)

local
   fun sort x =
       ListMergeSort.sort (fn ((a,_),(b,_)) => TypesUtil.gtLabel (a,b)) x
in fun sortRecord(l,err) =
       (checkUniq(err, "duplicate label in record", map #1 l);
        sort l)
end

(* records and tuples *)
fun makeRECORDexp(fields,err) =
    let val fields' = map (fn(id,exp)=> (id,(exp,ref 0))) fields
	fun assign(i,(_,(_,r))::tl) = (r := i; assign(i+1,tl))
	  | assign(_,nil) = ()
	fun f(i,(id,(exp,ref n))::r) = (LABEL{name=id,number=n},exp)::f(i+1,r)
	  | f(_,nil) = nil
     in assign(0, sortRecord(fields',err)); RECORDexp(f(0,fields'))
    end

val TUPLEexp = AbsynUtil.TUPLEexp

val TUPLEpat = AbsynUtil.TUPLEpat

fun varToExp var = VARexp (ref var, [])

(* FUNdec : {var : V.var,  -- name of the (recursive) function
             clauses: {pats: Absyn.pat list, resultty: Types.ty option, exp: Absyn.exp} list,
	     tyvars: Types.tyvar list ref, -- explicit tyvars
	     region: Ast.region} list
            * compInfo  -- for mkLvar
            -> Absyn.dec * StaticEnv.staticEnv *)
(* translates "fun" declarations into "val rec" form *)
fun FUNdec (fundecs, compInfo as {mkLvar=mkv, ...}: compInfo) =
    let fun funToValRec {var, clauses as ({pats,resultty,exp}::_), tyvars, region} =
	    let fun mkArgVar n = mkVALvar (S.varSymbol ("<arg"^Int.toString n^">"), mkv)
		val argVars = List.tabulate (length pats, mkArgVar)
		fun not1(f,[a]) = a
		  | not1(f,l) = f l
		fun clauseToRule {pats,exp,resultty=NONE} =
			      RULE(not1(TUPLEpat,pats), exp)
		  | clauseToRule {pats,exp,resultty=SOME ty} =
			      RULE(not1(TUPLEpat,pats),CONSTRAINTexp(exp,ty))

		fun buildFnExp [var] =
                      FNexp (map clauseToRule clauses, UNDEFty, UNDEFty)
		  | buildFnExp vars =
                      foldr (fn (w,e) => FNexp([RULE(VARpat w, e)], UNDEFty, UNDEFty))
			    (CASEexp(TUPLEexp (map varToExp vars),
				     (map clauseToRule clauses, UNDEFty, UNDEFty)))
			    vars
		val exp0 = buildFnExp argVars
		val exp = if !ElabControl.markabsyn then MARKexp (exp0, region) else exp0
	     in RVB {var=var, exp=exp, resultty=NONE, tyvars=tyvars}
	    end
          | funToValRec _ = bug "FUNdec"
     in VALRECdec (map funToValRec fundecs) (* no function name static environment needed *)
    end

(* pat_id : SP.path * StaticEnv.staticEnv * errorfn * compInfo) -> AS.pat option
 *  A VarPat translates to either a variable pattern or a constructor pattern.
 *  If we are given path of length > 1, then it has to be a constructor pattern;
 *  if the spath is not bound to a data constructor in env, then returns NONE. *)
fun pat_id (spath, env, err, compInfo as {mkLvar=mkv, ...}: compInfo) =
    case spath
      of SymPath.SPATH[id] => (* single symbol path always produces SOME AS.value *)
	   (case LU.lookIdSymOp (env,id)
	      of SOME(AS.CON c) => SOME (CONpat(c,[]))
	       | _ => SOME (VARpat(mkVALvar(id,mkv)))) (* may be bound in an outer scope; OK *)
       | _ =>  (* spath is not a single symbol (cannot be empty), should be bound to
		* a data constructor *)
	   (case LU.lookIdPath (env, spath, err)
	      of AS.CON dcon => SOME (CONpat (dcon,[]))  (* producing a constant datacon pattern *)
	       | _ => (err COMPLAIN
			 ("undefined constructor path in pattern: " ^ SymPath.toString spath)
			 nullErrorBody;
		       NONE))

fun makeRECORDpat(l,flex,err) =
    RECORDpat{fields=sortRecord(l,err), flex=flex, typ=ref UNDEFty}

fun clean_pat err (CONpat(DATACON{const=false,name,...},_)) =
      (err COMPLAIN ("data constructor "^S.name name^
		     " used without argument in pattern")
         nullErrorBody;
       WILDpat)
  | clean_pat err (p as CONpat(DATACON{lazyp=true,...},_)) =
      APPpat(BT.dollarDcon,[],p) (* LAZY *) (* second argument = nil OK? *)
  | clean_pat err (MARKpat(p,region)) = MARKpat(clean_pat err p, region)
  | clean_pat err p = p

(* patToString : pat -> string
 *   -- not exported, used once in makeAppPat error message *)
fun patToString WILDpat = "_"
  | patToString (VARpat(VALvar{path,...})) = SP.toString path
  | patToString (CONpat(DATACON{name,...},_)) = S.name name
  | patToString (NUMpat(src, _)) = src
  | patToString (STRINGpat s) = concat["\"", String.toString s, "\""]
  | patToString (CHARpat c) = concat["#\"", Char.toString c, "\""]
  | patToString (RECORDpat _) = "<record>"
  | patToString (APPpat _) = "<application>"
  | patToString (CONSTRAINTpat _) = "<constraint pattern>"
  | patToString (LAYEREDpat _) = "<layered pattern>"
  | patToString (VECTORpat _) = "<vector pattern>"
  | patToString (ORpat _) = "<or pattern>"
  | patToString (MARKpat _) = "<marked pattern>"
  | patToString _ = "<illegal pattern>"

(* obsolete
fun makeAPPpat err (CONpat(d as DATACON{const=false,lazyp,...},tvs),p) =
      let val p1 = APPpat(d, tvs, p)
       in if lazyp (* LAZY *)
	  then APPpat(BT.dollarDcon, [], p1)
          else p1
      end
  | makeAPPpat err (CONpat(d as DATACON{name,...},_),_) =
      (err COMPLAIN
        ("constant constructor applied to argument in pattern:"
	 ^ S.name name)
         nullErrorBody;
       WILDpat)
  | makeAPPpat err (MARKpat(rator,region),p) =
      MARKpat(makeAPPpat err (rator,p), region)
  | makeAPPpat err (rator,_) =
      (err COMPLAIN (concat["non-constructor applied to argument in pattern: ",
			     patToString rator])
         nullErrorBody;
       WILDpat)
*)

fun makeLAYEREDpat ((x as VARpat _), y, _) = LAYEREDpat(x,y)
  | makeLAYEREDpat ((x as MARKpat(VARpat _, reg)), y, _) = LAYEREDpat(x,y)
  | makeLAYEREDpat (CONSTRAINTpat(x,t), y, err) =
      makeLAYEREDpat(x, CONSTRAINTpat(y,t), err)
  | makeLAYEREDpat (MARKpat(CONSTRAINTpat(x,t),reg), y, err) =
      makeLAYEREDpat(MARKpat(x,reg), CONSTRAINTpat(y,t), err)
  | makeLAYEREDpat (x,y,err) =
      (err COMPLAIN "pattern to left of \"as\" must be variable" nullErrorBody;
       y)

(* fillPat : AS.pat -> AS.pat *)
(* (1) fills out flex record patterns according to the known record type, turning them
 *     into nonflex record patterns.  Using WILDpat for the elided fields.
 * [used to (in translate.sml) use mkRep to adjust representations for exception constructors and
 *     the SUSP pseudo-constructor ] *)
fun fillPat pat =
  let fun fill (pat as RECORDpat {fields, flex=true, typ}) =
            let val fields' = map (fn (l,p) => (l, fill p)) fields
                val labels =
		    (case TU.headReduceType (!typ)
		       of (t as T.CONty(T.RECORDtyc labels, _)) =>
			    (typ := t; labels)
                        | _ => bug "fillPat: unresolved flex record type")
                fun merge (a as ((id,p)::r), lab::s) =
                      if S.eq(id,lab)
		      then (id,p) :: merge(r,s)
                      else (lab,WILDpat) :: merge(a,s)
                  | merge ([], lab::s) = (lab,WILDpat) :: merge([], s)
                  | merge ([], []) = []
                  | merge _ = bug "merge in translate"
             in RECORDpat{fields = merge(fields', labels), flex = false, typ = typ}
            end
        | fill (RECORDpat {fields, flex=false, typ}) =
            RECORDpat{fields = map (fn (lab, p) => (lab, fill p)) fields,
                      typ = typ, flex = false}
	| fill (CONSTRAINTpat (p,_)) = fill p   (* stripping type constraint *)
	| fill (MARKpat (p,_)) = fill p         (* stripping mark *)
        | fill (LAYEREDpat (p,q)) = LAYEREDpat(fill p, fill q)
        | fill (VECTORpat(pats,ty)) = VECTORpat(map fill pats, ty)
        | fill (ORpat(p1, p2)) = ORpat(fill p1, fill p2)
        | fill (APPpat(dcon, tvs, pat)) = APPpat(dcon, tvs, fill pat)
        | fill xp = xp

   in fill pat
  end (* function fillPat *)


(* aconvertPat: Absyn.pat * compInfo -> Absyn.pat * V.var list * V.var list
 *   "alpha convert" a pattern with respect to the lvar access values
 *   of the pattern variables. Original variables are replaced by
 *   new ones, with fresh LVAR accesses and new refs for the "typ" field.
 *   Returns the converted pattern, the list of the original pattern
 *   variables (VALvars), and the list of new variables (VALvars).
 *   Called only once, in mkVB inside mkVBs in Translate.
 *   DBM: why is this function needed? (what bug does it fix?) *)

fun aconvertPat (pat: AS.pat) : Absyn.pat * V.var list * V.var list =
    let val varmap : (V.var * V.var) list ref = ref nil
            (* association list mapping old vars to newl; alpha-conversion substitution *)
        (* ASSERT: all vars in a VARpat will have an LVAR access. *)
        (* ASSERT: pat will not contain MARKpat, because stripPatMarks has been applied *)
	fun mappat (VARpat var) =
              let val lvar = V.varToLvar var
		  fun find ((oldvar, newvar)::rest) =
                        if varToLvar oldvar = lvar then newvar else find rest
		    | find nil =
		        let val (newvar, _) = V.replaceLvar var
			 in varmap := (var, newvar) :: !varmap;
			    newvar
			end
	       in VARpat(find(!varmap))
	      end
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

        val (oldvars,newvars) = ListPair.unzip (rev(!varmap))

     in (newpat,oldvars,newvars)
    end (* aconvertPat *)


(* checkBoundTyvars: check whether the tyvars appearing in a type (used) are
   bound (as parameters in a type declaration) *)
fun checkBoundTyvars(used,bound,err) =
    let val boundset =
              foldr (fn (v,s) => TS.union(TS.singleton v,s,err))
	        TS.empty bound
	fun nasty(ref(INSTANTIATED(VARty v))) = nasty v
	  | nasty(ubound as ref(UBOUND _)) =
	     err COMPLAIN ("unbound type variable in type declaration: " ^
			   (PPType.tyvarPrintname ubound))
		 nullErrorBody
	  | nasty _ = bug "checkBoundTyvars"
     in app nasty (TS.elements(TS.diff(used, boundset, err)))
    end

(* labsym : Absyn.numberedLabel -> Symbol.symbol *)
fun labsym (LABEL{name, ...}) = name

exception IsRec

(* hasModules tests whether there are explicit module declarations in a decl.
 * This is used in elabMod when elaborating LOCALdec as a cheap
 * approximate check of whether a declaration contains any functor
 * declarations. *)
fun hasModules(StrDec _) = true
  | hasModules(FctDec _) = true
  | hasModules(LocalDec(dec_in,dec_out)) =
      hasModules dec_in orelse hasModules dec_out
  | hasModules(SeqDec decs) =
      List.exists hasModules decs
  | hasModules(MarkDec(dec,_)) = hasModules dec
  | hasModules _ = false

end (* top-level local *)
end (* structure ElabUtil *)
