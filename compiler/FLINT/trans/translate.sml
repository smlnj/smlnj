(* translate.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature TRANSLATE =
sig

  (* Invariant: transDec always applies to a top-level absyn declaration *)
  val transDec : { rootdec: Absyn.dec,
		   exportLvars: LambdaVar.lvar list,
                   oldenv: StaticEnv.staticEnv,
                   env: StaticEnv.staticEnv,
		   cproto_conv: string,
		   compInfo: Absyn.dec CompInfo.compInfo }
                 -> {flint: FLINT.prog,
                     imports: (PersStamps.persstamp
                               * ImportTree.importTree) list}

end (* signature TRANSLATE *)

structure Translate : TRANSLATE =
struct

local
  structure B  = Bindings
  structure BT = BasicTypes
  structure DA = Access
  structure DI = DebIndex
  structure EM = ErrorMsg
  structure LV = LambdaVar
  structure V  = Variable
  structure AS = Absyn
  structure AU = AbsynUtil
  structure PL = PLambda
  structure LT = Lty
  structure LD = LtyDef
  structure LB = LtyBasic
  structure LE = LtyExtern  (* == PLambdaType *)
  structure M  = Modules
  structure MC = MatchComp
  structure PO = Primop
  structure PP = PrettyPrint
  structure PU = PPUtil
  structure S  = Symbol
  structure SP = SymPath
  structure LN = LiteralToNum
  structure TT = TransTypes
  structure T = Types
  structure TU = TypesUtil
  structure EU = ElabUtil
  structure Tgt = Target

  structure IIMap = RedBlackMapFn (type ord_key = IntInf.int
				   val compare = IntInf.compare)

  open Absyn PLambda TransUtil
in

(****************************************************************************
 *                   DEBUGGING AND PRETTYPRINTING                           *
 ****************************************************************************)

val debugging = FLINT_Control.trdebugging
fun bug msg = EM.impossible("Translate: " ^ msg)
fun warn msg = EM.warn("Translate: " ^ msg)

val say = Control.Print.say
fun says strs = say (concat strs)
fun newline () = say "\n"
fun saynl str = (say str; newline())
fun saysnl strs = saynl (concat strs)
fun dbsay (msg : string) =
    if !debugging then say msg else ()
fun dbsaynl (msg : string) =
    if !debugging then saynl msg else ()
fun dbsaysnl (msgs : string list) =
    if !debugging then saysnl msgs else ()

val ppDepth = Control.Print.printDepth

val with_pp = PP.with_default_pp

fun ppPat pat =
    PP.with_default_pp
      (fn ppstrm => PPAbsyn.ppPat StaticEnv.empty ppstrm (pat, (!ppDepth)))

fun ppExp exp =
    PP.with_default_pp
      (fn ppstrm => PPAbsyn.ppExp (StaticEnv.empty,NONE) ppstrm (exp, (!ppDepth)))

fun ppDec dec =
    PP.with_default_pp
      (fn ppstrm => PPAbsyn.ppDec (StaticEnv.empty,NONE) ppstrm (dec, (!ppDepth)))

fun ppType msg ty =
    PP.with_default_pp
	(fn ppstrm => (PP.string ppstrm (msg^": "); PPType.ppType StaticEnv.empty ppstrm ty))

fun ppLexp lexp =
    PP.with_default_pp
      (fn ppstrm => PPLexp.ppLexp (!ppDepth) ppstrm lexp)

fun ppTycArgs tycs =
    PP.with_default_pp
      (fn ppstrm =>
	  PPUtil.ppBracketedSequence ("[", "]", (PPLty.ppTyc 50)) ppstrm tycs)

	
(****************************************************************************
 *                   TRANSLATING NUMBER LITERALS                            *
 ****************************************************************************)

(* listUnion : T.tyvar list * T.tyvar list -> T.tyvar list *)
(* utility function used in transRVBs -- append without duplicates *)
fun listUnion (l : T.tyvar list, m : T.tyvar list) =
    let val l' = List.filter (fn tv => (not (List.exists (fn tv' => tv' = tv) m))) l
     in l' @ m
    end

(* mkTyargs : T.tyvar list * T.tyvar list * 'a * (int -> 'a) -> 'a list *)
(* map each tyvar, tv, in fromBbtvs to some form of "type" derived from the _position_, k,
 * of that tyvar in the toBtvs list (maker (k,tv)), or to default if tv is not in toBtvs.
 * The length of the result is therefore the same as the length of fromBtvs. *)
fun mkTyargs (fromBtvs, toBtvs, default, maker) =
    let fun search (tv: Types.tyvar, k, []) = default
	  | search (tv, k, tv'::r) = if TU.eqTyvar(tv, tv') then maker(k,tv) else search (tv, k+1, r)
	fun lookup tv = search (tv, 0, toBtvs)
     in map lookup fromBtvs
    end

(* transNum : Types.ty IntConst.t * T.ty -> int IntConst.t *)
(* Translates a front-end numeric literal (Types.ty IntConst.t) into a FLINT-style
 * numeric literal representation (int IntCons.t).
 * QUESTION: perhaps we should preserve the size, in the case of
 * word8, for better jump tables?  Also, chars are represented as default ints. *)
fun transNum ({ival, ty}: T.ty IntConst.t) : con =
    let fun mkWORD sz = WORDcon{ival = ival, ty = sz}  (* FLINT-style literal *)
	fun mkINT sz  = INTcon{ival = ival, ty = sz}   (* FLINT-style literal *)
	val defaultIntSz = (* 63 *) Target.defaultIntSz
     in if TU.equalType(ty, BT.intTy)
	  then mkINT defaultIntSz
	else if TU.equalType(ty, BT.int32Ty)
	  then mkINT 32
	else if TU.equalType(ty, BT.int64Ty)
	  then mkINT 64
	else if TU.equalType(ty, BT.intinfTy)
	  then mkINT 0
	else if TU.equalType(ty, BT.wordTy)
	  then mkWORD defaultIntSz
	else if TU.equalType(ty, BT.word8Ty)
	  then mkWORD defaultIntSz  (* or:  mkWORD 8 (if we want accurate char size) *)
	else if TU.equalType(ty, BT.word32Ty)
	  then mkWORD 32
        else if TU.equalType(ty, BT.word64Ty)
          then mkWORD 64
	else bug "transNum"
    end

(* selectTyArgs : tyvar list * tyvar list -> LT.tyc list  -- not currently used *)
(* pattvs are the generalized tyvars of the pattern, vartvs are those for a variable
 * in the pattern *)
fun selectTyArgs (pattvs, vartvs) =
    let val vartvsArity = length vartvs
	val indices = List.tabulate(vartvsArity, (fn x => x))
	(* 0-based indexes into btvs, the bound type variable
	 * sequence for _this_ bvar *)
	val tvToIndex = ListPair.zip(vartvs,indices)
	fun lookup (tv: Types.tyvar, nil) = NONE
	  | lookup (tv, (tv',k)::r) = if tv = tv' then SOME k else lookup (tv,r)
	val targs = map (fn tv => case lookup (tv, tvToIndex)
				   of NONE => LB.tcc_void
				    | SOME k => LD.tcc_var(1,k))
			pattvs
    in targs
    end

(****************************************************************************
 *                          MAIN FUNCTION                                   *
 *                                                                          *
 *  val transDec : Absyn.dec * Access.lvar list                             *
 *                 * StaticEnv.staticEnv * CompBasic.compInfo               *
 *                 -> {flint: FLINT.prog,                                   *
 *                     imports: (PersStamps.persstamp                       *
 *                               * ImportTree.importTree) list}             *
 ****************************************************************************)

fun transDec
	{ rootdec, exportLvars, oldenv, env, cproto_conv,
	 compInfo as {errorMatch,error,...}: Absyn.dec CompInfo.compInfo } =
let

(* lvar generator taken from compInfo *)
val mkvN = #mkLvar compInfo
fun mkv () = mkvN NONE

(** generate the set of ML-to-FLINT type translation functions *)
val {tpsKnd, tpsTyc, toTyc, toLty, strLty, fctLty} =
    TT.genTT()
fun toTcLt d = (toTyc d, toLty d)

(* toDconLty : DebIndex.depth -> Types.ty -> lty *)
(** translating the typ field in DATACON into lty; constant datacons
    will take ltc_unit as the argument *)
fun toDconLty d ty =
    (case ty
       of T.POLYty{sign, tyfun=T.TYFUN{arity, body}} =>
            if BT.isArrowType body then toLty d ty
            else toLty d (T.POLYty{sign=sign,
				    tyfun=T.TYFUN{arity=arity,
						   body=BT.-->(BT.unitTy, body)}})
	| _ => if BT.isArrowType ty then toLty d ty
               else toLty d (BT.-->(BT.unitTy, ty)))

(* CON' : Plambda.dataconstr * PlambdType.tyc list * lexp -> lexp *) 
(* version of CON with special translation of ref and susp pseudo datacons *)
fun CON' ((_, DA.REF, lt), ts, e) = APP (PRIM (PO.MAKEREF, lt, ts), e)
  | CON' ((_, DA.SUSP (SOME(DA.LVAR d, _)), lt), ts, e) =
      let val v   = mkv ()
          val fe = FN (v, LD.ltc_tuple [], e)
       in APP(TAPP (VAR d, ts), fe)
      end
  | CON' x = CON x

fun patToConsig (APPpat(dcon,_,_)) = TU.dataconSign dcon
  | patToConsig (CONpat(dcon,_)) = TU.dataconSign dcon
  | patToConsig _ = DA.CNIL

(*
 * The following code implements the exception tracking and
 * errormsg reporting.
 *)

local
  val region = ref(0,0)
  val markexn = PRIM(PO.MARKEXN,
		  LD.ltc_parrow(LD.ltc_tuple [LB.ltc_exn, LB.ltc_string],
				LB.ltc_exn), [])
in

fun withRegion loc f x =
  let val r = !region
   in (region := loc; f x before region:=r)
      handle e => (region := r; raise e)
  end

fun mkRaise(x, lt) =
  let val e = if !Control.trackExn
              then APP(markexn, RECORD[x, STRING(errorMatch(!region))])
              else x
   in RAISE(e, lt)
  end

fun complain msg = error (!region) msg
fun repErr msg = complain EM.COMPLAIN msg EM.nullErrorBody
fun repWarn msg = complain EM.WARN msg EM.nullErrorBody
fun repPolyEq () =
    if !Control.polyEqWarn then complain EM.WARN "calling polyEqual" EM.nullErrorBody
    else ()

end (* markexn-local *)

(***************************************************************************
 *          SHARING AND LIFTING OF STRUCTURE IMPORTS AND ACCESSES          *
 ***************************************************************************)

(* _Dependent_ or _secondary_ lvars are defined in terms of a base lvar via
 * an access path (int list). These will be bound to a nested SELECT expression
 * rooted at the base lvar. "Variables" with access of the form, e.g.,
 * PATH(PATH(LVAR lv, i1),i2) are translated to the lexp 
 * SELECT(i2, SELECT(i1, VAR lv)).  A given base lvar may have several
 * dependent lvars defined by paths based on that lvar. These are registered
 * in a "dependentLvarsTable" lvar hash table. Each dependent variable is
 * uniquely determined by the base lvar and the access path. *)

exception DEP_LVAR_TABLE
type key = int  (* hash of an accesspath : int list *)
type accesspath = int list
type depLvar = (key * accesspath * LV.lvar)
(* "Dependent lvars and their accesspaths off the base lvar mapping to the depLvar,
 * where the key is the hash of the accesspath *)

(** dependentLvarsTable: lvar --> depLvar list 
    or lvar -> (<hashkey(accesspath)>: int * <accesspath> : int list * lvar) list *)
val dependentLvarsTable : depLvar list LambdaVar.Tbl.hash_table =
    LambdaVar.Tbl.mkTable (32, DEP_LVAR_TABLE)

(* hashkey : int list -> key *)
(* hash of an accesspath (an int list) *)
fun hashkey ints = foldr (fn (x,y) => ((x * 10 + y) mod 1019)) 0 ints

(* buildHeader : lvar -> (lexp -> lexp) *)
(* creates a wrapper function that wraps a nested sequence of let declarations
 * around a base lvar (the argument), where the definiens of each let is nested SELECTS for
 * the accesspath in each depLvar item starting from VAR lvar for the baseLvar. *)
fun buildHeader baseLvar =
  let val depLvars = LambdaVar.Tbl.lookup dependentLvarsTable baseLvar
      fun wrapHeader((_, accesspath, accessLvar), hdr) =
             let val accessExp = foldl (fn (k,e) => SELECT(k,e)) (VAR baseLvar) accesspath
	      in fn e => hdr (LET (accessLvar, accessExp, e))
	     end
   in foldr wrapHeader ident depLvars   (* ident = TransUtil.ident = identity fn *)
  end handle DEP_LVAR_TABLE => ident   (* if lvar not in dependentLvarsTable? *)

fun apToString (accesspath: int list) =
    PrintUtil.listToString ("(", ",", ")") Int.toString accesspath

fun nameOpToString (symbolOp: S.symbol option) : string =
    case symbolOp
      of NONE => ""
       | SOME s => S.name s

datatype pidInfo = ANON of (int * pidInfo) list
                 | NAMED of LV.lvar * LT.lty * (int * pidInfo) list

(* mkPidInfo : lty * int list * symbol option -> pidInfo * lvar *)
fun mkPidInfo (lty, l, nameOp) =
    let val lvar = mkvN nameOp
	fun mkpi [] = NAMED(lvar, lty, [])
          | mkpi (a::r) = ANON [(a, mkpi r)]
     in (mkpi l, lvar)
    end

(* mergePidInfo : pidInfo * lty * int list * symbol option -> pidInfo * lvar *)
fun mergePidInfo (pi, lty, l, nameOp) =
  let fun merge (pi as NAMED(v,_,_), []) = (pi, v)
        | merge (ANON xl, [])  =
            let val lvar = mkvN nameOp
             in (NAMED(lvar, lty, xl), lvar)
            end
        | merge (z, a::r) =
            let val (xl, mknode) =
                    case z of ANON c => (c, ANON)
                            | NAMED (v,tt,c) => (c, fn x => NAMED(v,tt,x))

                fun dump ((np, v), z, y) =
                      let val nz = (a, np) :: z
                       in (mknode((rev y) @ nz), v)
                      end

                fun look ([], y) = dump(mkPidInfo(lty, r, nameOp), [], y)
                  | look (u as ((x as (i,pi))::z), y) =
		      (case Int.compare (i, a)
		         of LESS => look(z, x::y)
                          | EQUAL => dump (merge (pi, r), z, y)
                          | GREATER => dump (mkPidInfo (lty, r, nameOp), u, y))

             in look(xl, [])
            end
   in merge(pi, l)
  end (* mergePidInfo *)

(** a map that stores information about external references *)
val persmap = ref (PersMap.empty : pidInfo PersMap.map)

(* mkPid : pid * lty * int list * symbol option -> lvar *)
fun mkPid (pid, lty, l, nameOp) =
    case PersMap.find (!persmap, pid)
      of NONE =>
	  let val (pinfo, var) = mkPidInfo (lty, l, nameOp)
	   in persmap := PersMap.insert(!persmap, pid, pinfo);
	      var
	  end
       | SOME pinfo =>
	  let val (newPinfo, var) = mergePidInfo (pinfo, lty, l, nameOp)
	      fun rmv (key, map) = (* clear the old pinfo for pid *)
		  let val (newMap, _) = PersMap.remove(map, key)
		  in newMap
		  end handle e => map
	   in persmap := PersMap.insert(rmv (pid, !persmap), pid, newPinfo);
	      var
	  end

val iimap = ref (IIMap.empty : LV.lvar IIMap.map)

(* getII : IntInf.int -> lvar *)
(* uses iimap to map IntInf.int to lvars, creating new mappings is necessary *)
fun getII n =
    case IIMap.find (!iimap, n)
      of SOME lvar => lvar
       | NONE =>
	 let val lvar = mkv ()
	  in iimap := IIMap.insert (!iimap, n, lvar);
	     lvar
	 end

(* accessLvar : lvar * accesspath * symbol option -> lvar *)
(* returns an dependent lvar to be bound to lvar via accesspath selection, and
 * if it is new, registers it in dependentLvarsTable.
 * Called only (once) in transAccess.
 * ASSERT: not (null accesspath) *)
fun accessLvar (baseLvar, accesspath, nameOp) =
      let val _ = dbsaysnl [">>> accessLvar ", LV.toString baseLvar, apToString accesspath, nameOpToString nameOp]
	  val depLvars = LambdaVar.Tbl.lookup dependentLvarsTable baseLvar
	  	         handle DEP_LVAR_TABLE => []
          val key = hashkey accesspath  (* hash of accesspath *)
          fun look [] =
                let val dependentLvar = mkvN nameOp  (* add new dependent lvar *)
                in LambdaVar.Tbl.insert dependentLvarsTable
		     (baseLvar, (key, accesspath, dependentLvar) :: depLvars);
		   dbsaysnl ["### accessLvar[new]: ", LV.toString dependentLvar];
		   dependentLvar
                end
            | look ((key', accesspath', dependentLvar) :: rest) =
                if (key' = key) andalso (accesspath' = accesspath)
	        then (dbsaysnl ["### accessLvar[old]", LV.toString dependentLvar];
		      dependentLvar)
	        else look rest
       in look depLvars
      end

(* transAccess : A.access * lty * S.symbol option -> lvar *)
(* translating an access with type into an lvar, which is registered
 * in dependentLvarsTable if local (rooted at LVAR), or in persmap if external,
 * (rooted at EXTERN). This returns an lvar rather than a VAR because it
 * is used for both variables (including var, str, fct) and constructors. *)
fun transAccess (access, lty, nameOp) =
  let fun unwrapAccess (DA.PATH(a,i), accesspath) = unwrapAccess (a, i::accesspath)
        | unwrapAccess (DA.LVAR lvar, nil) = lvar
        | unwrapAccess (DA.LVAR lvar, accesspath) = accessLvar (lvar, accesspath, nameOp)
        | unwrapAccess (DA.EXTERN pid, accesspath) = mkPid (pid, lty, accesspath, nameOp)
        | unwrapAccess _ = bug "transAccess: bad access"
   in unwrapAccess (access, [])
  end
(*
(* transAccessLocal : A.access * S.symbol option -> lvar *)
(* translating a "local" (LVAR or PATH rooted at an LVAR) access into a
 * VAR lexp, and registering it in dependentLvarsTable if accesspath is not null. *)
fun transAccessLocal (access, nameOp) =
  let fun register (DA.PATH(a,i), accesspath) = register (a, i::accesspath)
        | register (DA.LVAR lvar, accesspath) = accessLvar(lvar, accesspath, nameOp)
        | register _ = bug "transAccessLocal: bad access"
   in register (access, [])
  end

(* transAccess: DA.access * lty * S.symbol option -> lvar *)
(* translating an access into a VAR lexp, using transAccessTyped if
 * the access is external, or transAccessLocal if it is local *)
fun transAccess (access, lty, nameOp) =
    if extern access    (* TransUtil.extern, check if access is EXTERN-based *)
    then transAccessTyped (access, lty, nameOp)
    else transAccessLocal (access, nameOp)
*)
(*
 * These two functions (coreExn, coreAcc) are major gross hacks. The NoCore exception
 * would be raised when compiling boot/dummy.sml, boot/assembly.sig, and
 * boot/core.sml; the assumption is that the result of coreExn and coreAcc
 * would never be used when compiling these three files. A good way to
 * clean up this is to put all the core constructors and primitives into
 * the primitive environment. (ZHONG)
 *
 * NOTE: the CoreAccess structure (ElabData/stateenv/coreacc.sml) also
 * defines a NoCore exception, but does not export it.  Does it make
 * sense to combine these things?
 *)
exception NoCore

(* coreExn : S.symbol list -> lexp option *)
(* Accessing _constructors_ via the Core structure.
 * Used in TansPrim.trans to access Subscript, Assembly.Div, and Char
 * exception constructors. *)
fun coreExn ids =
    (case CoreAccess.getCon' (fn () => raise NoCore) oldenv ids
      of T.DATACON { name, rep as DA.EXN _, typ, ... } =>
	   let val lty = toDconLty DI.top typ
	       val newrep = mkRep(rep, lty, name)
	       val _ = dbsaynl ">>coreExn in translate.sml: "
              (* val _ = PPLexp.printLexp (CON'((name, nrep, nt), [], unitLexp))
	         val _ = print "\n" *)
            in SOME (CON'((name, newrep, lty), [], unitLexp))
           end
       | _ => bug "coreExn in translate"
      (* end case *))
    handle NoCore => NONE

(* coreAcc : symbol -> lexp *)
(* Accessing variables via the Core structure, and localizing access via
 * transAccess. *)
and coreAcc id =
    (case CoreAccess.getVar' (fn () => raise NoCore) oldenv [id]
       of V.VALvar { access, typ, path, ... } =>
	    VAR (transAccess (access, toLty DI.top (!typ), getNameOp path))
        | _ => bug "coreAcc in translate"
    (* end case *))
    handle NoCore =>
	(warn(concat["no Core access for '", id, "'\n"]);
	 INT{ival = 0, ty = Tgt.defaultIntSz})

(* mkRep : DA.conrep * lty * S.symbol -> DA.conrep *)
(* "localize" the conrep's access, for exception or SUSP constructors *)
and mkRep (rep, lty, name) =
    (case rep
       of (DA.EXN access) =>
             let (* val _ = saysnl ["mkRep:EXN: ", S.name name, " ", DA.prAcc access] *)
		 val (argt, _) = LD.ltd_parrow lty
              in DA.EXN (DA.LVAR (transAccess (access, LB.ltc_etag argt, SOME name)))
             end
        | (DA.SUSP NONE) =>  (* a hack to support "delay-force" primitives *)
             (case (coreAcc "delay", coreAcc "force")
               of (VAR x, VAR y) => DA.SUSP(SOME (DA.LVAR x, DA.LVAR y))
                | _ => bug "unexpected case on mkRep SUSP 1")
        | (DA.SUSP (SOME _)) => bug "unexpected case on mkRep SUSP 2"
        | _ => rep)

(** The runtime polymorphic equality and string equality dictionary. *)
val eqDict =
  let val strEqRef : lexp option ref = ref NONE
      val polyEqRef : lexp option ref = ref NONE
      val intInfEqRef : lexp option ref = ref NONE

      fun getStrEq () =
        (case (!strEqRef)
          of SOME e => e
           | NONE => (let val e = coreAcc "stringequal"
                       in strEqRef := (SOME e); e
                      end))

      fun getIntInfEq () =		(* same as polyeq, but silent *)
	  case !intInfEqRef of
	      SOME e => e
	    | NONE => let val e =
			      TAPP (coreAcc "polyequal",
				    [toTyc DI.top BT.intinfTy])
		      in
			  intInfEqRef := SOME e; e
		      end

      fun getPolyEq () =
        (repPolyEq();
	 case (!polyEqRef)
          of SOME e => e
           | NONE => (let val e = coreAcc "polyequal"
                       in polyEqRef := (SOME e); e
                      end))
   in {getStrEq=getStrEq, getIntInfEq=getIntInfEq, getPolyEq=getPolyEq}
  end

val eqGen = PEqual.equal (eqDict, env)

val boolsign = BT.boolsign
val (trueDcon', falseDcon') =
  let val lt = LD.ltc_parrow(LB.ltc_unit, LB.ltc_bool)
      fun h (T.DATACON{name,rep,typ,...}) = (name, rep, lt)
   in (h BT.trueDcon, h BT.falseDcon)
  end

val trueLexp = CON(trueDcon', [], unitLexp)
val falseLexp = CON(falseDcon', [], unitLexp)

fun COND(a,b,c) =
  SWITCH(a,boolsign, [(DATAcon(trueDcon', [], mkv()),b),
                      (DATAcon(falseDcon', [], mkv()),c)], NONE)

fun composeNOT (eq, t) =
  let val v = mkv()
      val argt = LD.ltc_tuple [t, t]
   in FN(v, argt, COND(APP(eq, VAR v), falseLexp, trueLexp))
  end

val lt_unit = LB.ltc_unit
val lt_u_u = LD.ltc_parrow (lt_unit, lt_unit)

(* translation of prim ops *)
val transPrim =
    TransPrim.trans {coreAcc = coreAcc, coreExn = coreExn, mkv = mkv,
		     mkRaise = mkRaise}

(* genintinfswitch : var * (con * exp) list * lexp -> lexp *)
(* generates PLambda.lexp code for a case over an IntInf.int value. *)
(* where does this belong?  At what level should it be coded?  To Absyn? *)
(* This was moved from the old trans/matchcomp.sml.
 * con is an IntCON of type IntInf.int (: Types.ty IntConst.t).
 * translate.sml has to recognize this special form of a shallow Case
 * and invoke this function to handle it. *)
fun genintinfswitch (subject: lexp, cases, default) =
    let (* build a chain of equality tests for checking large pattern values *)
	val sv = mkv ()  (* lvar bound to the subject *)
	fun build [] = default
	  | build ((n, e) :: r) =
	      COND (APP (#getIntInfEq eqDict (), RECORD [VAR sv, VAR (getII n)]),
		    e, build r)
	(* make a small int constant pattern *)
	fun mkSmall n = INTcon{ival = IntInf.fromInt n, ty = Tgt.defaultIntSz}
	(* split pattern values into small values and large values;
	 * small values can be handled directly using SWITCH *)
	fun split ([], s, l) = (rev s, rev l)
	  | split ((n, e) :: r, sm, lg) =
	      (case LN.lowVal n
		 of SOME l => split (r, (mkSmall l, e) :: sm, lg)
		  | NONE => split (r, sm, (n, e) :: lg)
	      (* end case *))
	fun gen () =
	      (case split (cases, [], [])
		 of ([], largeints) => build largeints
		  | (smallints, largeints) =>
		      let val iv = mkv ()
		       in LET (sv, subject,
			    LET (iv, APP (coreAcc "infLowValue", VAR sv),
			      SWITCH (VAR iv, DA.CNIL, smallints, SOME (build largeints))))
		      end
	      (* end case *))
       in gen ()
      end

(* similar special cases for REF and SUSP constructors in patterns.  mkExp0(SWITCH _)
 * rule below. *)


(***************************************************************************
 *                                                                         *
 * Translating various bindings into lambda expressions:                   *
 *                                                                         *
 *   val mkVar : V.var * DI.depth -> L.lexp                                *
 *   val mkVE : V.var * T.ty list * DI.depth -> L.lexp                     *
 *   val mkCE : T.datacon * T.ty list * L.lexp option * DI.depth -> L.lexp *
 *   val mkStr : M.Structure * DI.depth -> L.lexp                          *
 *   val mkFct : M.Functor * DI.depth -> L.lexp                            *
 *   val mkBnd : DI.depth -> B.binding -> L.lexp                           *
 *                                                                         *
 ***************************************************************************)
(* [KM???] mkVar is calling transAccess, which just drops the prim!!! *)
fun mkVar (V.VALvar{access, typ, path, ...}, d) =
      VAR (transAccess(access, toLty d (!typ), getNameOp path))
  | mkVar _ = bug "unexpected vars in mkVar"

(* mkVE : V.var * T.ty list * depth -> lexp
 * This translates a variable, which may be bound to a primop.
 * In the case of a primop variable, this function reconstructs the
 * type parameters of instantiation of the intrinsic primop type relative
 * to the variable occurrence type *)
fun mkVE (e as V.VALvar { typ, prim = PrimopId.Prim p, ... }, tys, d) =
      let val occurenceTy = TU.applyPoly(!typ, tys)
              (* compute the occurrence type of the variable *)
          val primop = PrimopBind.defnOf p
          val intrinsicType = PrimopBind.typeOf p
	  val _ = dbsaynl ">>mkVE: before matchInstTypes"
	  val intrinsicParams =
              (* compute intrinsic instantiation params of intrinsicType *)
              case (TU.matchInstTypes(true, d, occurenceTy, intrinsicType)
                      : (T.tyvar list * T.tyvar list) option )
                of SOME(_, tvs) =>
		   (if !debugging then
                      complain EM.WARN
                        "mkVE ->matchInstTypes -> pruneTyvar"
                        (fn ppstrm =>
                          (PP.string ppstrm
                            ("tvs length: " ^ Int.toString (length tvs));
                           PP.newline ppstrm;
                           PPVal.ppDebugVar
                            (fn x => "") ppstrm env e;
                           if (length tvs) = 1
                           then PPType.ppType env ppstrm (T.VARty (hd tvs))
                           else ()))
                    else ();
                    map TU.pruneTyvar tvs)
                 | NONE =>
                   (ElabDebug.withInternals (fn () => (complain EM.COMPLAIN
                      "mkVE:primop intrinsic type doesn't match occurrence type"
                      (fn ppstrm =>
                          (PP.string ppstrm "VALvar: ";
                           PPVal.ppVar ppstrm e;
                           PP.newline ppstrm;
                           PP.string ppstrm "occtypes: ";
                           PPType.ppType env ppstrm occurenceTy;
                           PP.newline ppstrm;
                           PP.string ppstrm "intrinsicType: ";
                           PPType.ppType env ppstrm intrinsicType;
                           PP.newline ppstrm;
                           PP.string ppstrm "instpoly occ: ";
                           PPType.ppType env ppstrm
                             (#1 (TU.instantiatePoly occurenceTy));
                           PP.newline ppstrm;
                           PP.string ppstrm "instpoly intrinsicType: ";
                           PPType.ppType env ppstrm
                             (#1 (TU.instantiatePoly intrinsicType))));
                    bug "mkVE -- NONE")))
	  val _ = dbsaynl "<<mkVE: after matchInstTypes"
       in case (primop, intrinsicParams)
            of (PO.POLYEQL, [t]) => eqGen(intrinsicType, t, toTcLt d)
             | (PO.POLYNEQ, [t]) =>
               composeNOT(eqGen(intrinsicType, t, toTcLt d), toLty d t)
             | (PO.RAW_CCALL NONE, [a, b, c]) =>
               let val i = SOME (CProto.decode cproto_conv
                                   { fun_ty = a, encoding = b })
                           handle CProto.BadEncoding => NONE
               in PRIM (PO.RAW_CCALL i, toLty d intrinsicType,
                        map (toTyc d) intrinsicParams)
               end
             | _ => (** where do these intrinsicType originate?
			A: PrimopBindings *)
		    transPrim(primop, (toLty d intrinsicType),
                              map (toTyc d) intrinsicParams)
      end
  | mkVE (var as V.VALvar{typ, prim = PrimopId.NonPrim, path, access, ...}, tys, d) =
    (* non primop variable *)
      (if !debugging
       then (say "### mkVE nonprimop: ";
             saynl (SymPath.toString path);
	     say "   access = "; saynl (DA.prAcc access);
             ppType "  typ: " (!typ); newline();
             saysnl ["  |tys| = ", Int.toString(length tys)];
             say "   tys = ";
	     app (ppType "   tys = ") tys; newline())
       else ();
       case tys
        of [] => (dbsaysnl ["### mkVE[no poly]: ", V.toString var, " ", DA.prAcc(V.varAccess var)];
		  mkVar (var, d))
         | _ => (dbsaysnl ["### mkVE[poly]: ", V.toString var, " ", DA.prAcc(V.varAccess var)];
		 TAPP(mkVar(var, d), map (toTyc d) tys)))
                 (* dbm: when does this second case occur? (e.g. mctests/mlr/t5.sml) *)
  | mkVE _ = bug "non VALvar passed to mkVE"

(* mkCE : T.datacon * T.tyvar list * lexp option * DB.depth -> lexp *)
(* Translation of constructor constant and contructor application expressions.
 * This should deal with rep translation for Match and Bind exceptions,
 * among others(?). *)
fun mkCE (T.DATACON{const, rep, name, typ, ...}, tyvars, argOp, d) =
    let val lty = toDconLty d typ
	val localRep = mkRep(rep, lty, name)
	val dataconstr = (name, localRep, lty)
	val tycs = map (toTyc d o T.VARty) tyvars
     in if const then CON' (dataconstr, tycs, unitLexp)
	else (case argOp
	       of SOME le => CON'(dataconstr, tycs, le)
		| NONE =>
		   let val (argLty, _) = LD.ltd_parrow(LE.lt_pinst(lty, tycs))
		       val paramLvar = mkv()
		    in FN(paramLvar, argLty, CON'(dataconstr, tycs, VAR paramLvar))
		   end)
    end

fun mkStr (s as M.STR { access, prim, ... }, d) =
      VAR(transAccess(access, strLty(s, d, compInfo), NONE))
  | mkStr _ = bug "unexpected structures in mkStr"

fun mkFct (f as M.FCT { access, prim, ... }, d) =
      PL.VAR(transAccess(access, fctLty(f, d, compInfo), NONE))
  | mkFct _ = bug "unexpected functors in mkFct"

fun mkBnd d =
  let fun transBind (B.VALbind v) = mkVar(v, d)
        | transBind (B.STRbind s) = mkStr(s, d)
        | transBind (B.FCTbind f) = mkFct(f, d)
        | transBind (B.CONbind (T.DATACON {rep=(DA.EXN access), name, typ, ...})) =
          let val nt = toDconLty d typ
              val (argt,_) = LD.ltd_parrow nt
          in VAR (transAccess (access, LB.ltc_etag argt, SOME name))
          end
        | transBind _ = bug "unexpected bindings in transBind"
   in transBind
  end


(******************************************************************************
 *                                                                            *
 * Translating core absyn declarations into lambda expressions:               *
 *                                                                            *
 *    val transVBs  : Absyn.vb list * depth -> PLambda.lexp -> PLambda.lexp   *
 *    val transRVBs : Absyn.rvb list * depth -> PLambda.lexp -> PLambda.lexp  *
 *    val transEBs  : Absyn.eb list * depth -> PLambda.lexp -> PLambda.lexp      *
 *                                                                            *
 * transVBs(vbs,d) produces a function taking a "body" or "scope" lexp.       *
 * Top-level variable bindings are handled as specified at the end of         *
 * the main translate function, transDec.                                  *
 ******************************************************************************)

(* setBoundTyvars : depth * Types.tyvar list * (unit -> 'a) -> 'a *)
(* Temporarily set the tvkind of polymorphically bound tyvars to LBOUNDs with
 * the "current" TFN depth while performing a translation. *)
fun setBoundTyvars (TFNdepth, boundtvs, transfn) =
    let val savedtvkinds = map ! boundtvs
        (* save original contents of boundtvs (tvkinds)for restoration
         * after the invocation of transfn *)
	fun setLBOUND (i, tv) =
	    (tv := T.LBOUND {depth=TFNdepth, eq=TU.tyvarIsEq tv, index=i})
	val _ = List.appi setLBOUND boundtvs
        (* assign LBOUNDs to the boundtvs to mark them as type
         * parameter variables _locally_ during translation of exp *)		    
	val result = transfn ()
	(* perform and encapsulated translation with the new boundtvs context*)
	val _ = ListPair.app (op :=) (boundtvs, savedtvkinds)
        (* restore tyvar contents to state before the translation *)
    in result
    end

(* transPolyExp : Absyn.exp * depth * Types.tyvar list -> PLambda.lexp  (old mkPE)
 * Translate an expression with (potential) type parameters (boundtvs).
 * The boundtvs are temporarily set to LBOUNDs with appropriate depth and index
 * during a call of mkExp, then restored to their previous values. This is done
 * in case this call of transPolyExp occurs dynamically within an "outer" call
 * of transPolyExp that sets its own boundtvs which may overlap with
 * those of this call.
 * -- exp : Absyn.exp, the expression to be translated with tmv abstracted
 *      The expression is assumed to be the rhs of a simple var binding (val v = exp)
 * -- depth : int, the current depth of TFN abstractions (0-based)
 * -- boundtvs : tyvar list (3rd arg), the generalized tmvs of the expression
 * QUESTION: Can this happen?  If so, what is an example? 
 * CONJECTURE: a metatyvar can only be generalized at one declaration level, though
 * it may be generalized in parallel multiple times in a single declaration (e.g. RVB).
 * So scopes of a single metatyvar will not be "nested". If so, is the "restoration" of
 * the tvkinds of "bound tmvs" not necessary? Needs to be justified. *)
fun transPolyExp (exp, TFNdepth, []) = (* mkExp(exp, TFNdepth) *)
    let val _ = dbsaynl ">>> transPolyExp.1"
	val result = mkExp(exp, TFNdepth)
     in dbsaynl "<<< transPolyExp.1"; result
    end
    (* exp is not polymorphic, hence no type abstraction around exp and TFNdepth
     * is not incremented *)
  | transPolyExp (exp, TFNdepth, boundtvs) =
    (* The LBOUND equality property should not matter at this point
     * because typechecking and signature matching are already completed [GK 2/24/08]. *)
      let val _ = dbsaynl ">>> transPolyExp.2"
	  val TFNdepth' = TFNdepth + 1  (* TFNdepth incremented for translation of exp *)
	  val bodyLexp = setBoundTyvars (TFNdepth, boundtvs, (fn () => mkExp (exp, TFNdepth')))
          val result =  TFN(LD.tkc_arg(length(boundtvs)), bodyLexp)
      in dbsaynl "<<< transPolyExp.2"; result
      end

(* transVBs : Absyn.vb list * depth -> PLambda.lexp -> PLambda.lexp *)
(* implicit _body_: lexp parameter, representing the scope of the vb declarations.
 * Compound patterns will have been eliminated by match compilation in transMatch,
 * so the only cases here should be simple VBs with VARpat or WILDpat patterns.
 * NOTE: there should be a special VB form produced by bindCompile, instead of having
 *  to examine exp for the special case produced by matchComp. *)
and transVBs (vbs, d) =
  let val _ = dbsaynl ">>> transVBs"
      fun transVB (VB{pat, exp, boundtvs,...}, body) =  (* tyvars field of VB not relevant *)
	  let val _ = if !debugging
		      then (say ">>> transVB: "; ppPat pat; newline())
		      else ()
	      val pat = AU.stripPatMarks pat  (* strip out MARKpat's *)
	      val result =
		  (case pat
		    of (VARpat bvar | CONSTRAINTpat(VARpat bvar, _)) =>
		       (* Simple variable pattern. No special case needed for primops [dbm: 5/1/07] *)
		       let val (bvarLvar, bvarBtvs) =  (* extract lvar (access) and btvs from bvar *)
			       (case bvar
				  of V.VALvar{access=DA.LVAR lvar, btvs, ...} => (lvar, !btvs)
				   | _ =>  bug "mkVB 1")
			in LET(bvarLvar, transPolyExp(exp, d, boundtvs), body)
		       end
		     | _ => (ppPat pat; bug "transVB -- unexpected compound or wild pat"))
		       (* can't happen -- after match compilation, all VBs bind a simple
			* variable, or a wildcard (?) *)
 	   in (dbsaynl "<<< transVB"; result)
	  end (* fun transVB *)
      val result = foldr' transVB vbs
   in dbsaynl "<<< transVBs"; result
      (* missing fold(r) argument is the _body_; the return type is lexp -> lexp *)
  end (* transVBs *)

(* transRVBs : rvb list * int -> lexp -> lexp *)
(* mkRVB is rewritten to achieve the effect of the former ElabUtil.wrapRECdec, but post type
   checking. 

       rvbs ==>

       local 
         val fns = let rvbs in (f0, f1, ...)  
       in
         val f0' = fns.0      -- TFN((_), SELECT(0, TAPP(fns, f0_tyargs))), 
	 val f1' = fns.1
	 ...
       end

   1. extract btvs (btvsLists) from the defined variables (vars).
   2. Form their "union" to get list of bound tyvars (jointBtvs) for the main variable (fns).
      There may be overlaps between the btvs lists for the defined functions (f0, f1, ...), so
      this is not just the concatenation. Does order matter?
   3. translate the declaration of fns, abstracted wrt jointBtvs using transPolyExp, but the
      type abstraction is for the entire defn of fns. This produces an lexp of the
      form TFN(_, FIX rvb* IN fn-var-tuple).
   3. Construct type argument lists (TV & VOID) for the component functions f0', ,,, using tyargs
   4  Construct lexp for the wrapped rvb declaration.

  Special Case: single function val recs (recursive or not).  E.g.

   fun f x = x

   val rec f = fn x => x

  Translates to?

   val fr = let val rec f = fn x => x in f    (fr : All 'a . 'a -> 'a)
    val f' = fr
*)
and transRVBs (nil, _) = bug "transRVBs - no rbv"
  | transRVBs ([RVB{var as V.VALvar{access=DA.LVAR defLvar, typ, btvs,...}, exp,...}], d) =
    (* single function defn, binding defLvar, no FIX needed if not recursive (occurs = false) *)
    let val _ = dbsaynl ">>> transRVBs"
	val ([newDefLvar], occurs) = aconvertLvars ([var], [exp])
     in if occurs
	then (* recursive case -- produce FIX *)
	    let val fixLvar = mkv ()  (* fresh lvar to be bound to the FIX lexp -- not used!!! *)
		val boundTvs = !btvs
		val numBoundTvs = length boundTvs  (* ??? *)
		val poly = numBoundTvs > 0
		fun mkTyArgs (nil, i, tyargs) = rev tyargs
		  | mkTyArgs (tv::tvs, i, tyargs) = 
		    mkTyArgs (tvs, i+1, T.VARty(ref(T.LBOUND{depth = d, eq = TU.tyvarIsEq tv, index = i}))::tyargs)
		val polyTyArgs = mkTyArgs (boundTvs, 0, nil)    (* null boundTvs => null polyTyArgs *)
		val monoType = TU.applyPoly (!typ, polyTyArgs)  (* de-polymorphize !typ, instantiating with LBOUNDS *)
		val lty = toLty (if poly then d+1 else d) monoType
		    (* was d+1; changed to d to fix utils/t1.sml bug. WRONG PLACE! *)
		val lexp =  (* translate exp without type abstraction, but with LBOUND instantiated boundTvs *)
		    if poly
		    then setBoundTyvars(d, boundTvs, (fn () => mkExp (exp, d+1)))
		    else mkExp (exp, d)
		val fixLexp = FIX([newDefLvar], [lty], [lexp], VAR newDefLvar)
		val rvbLexp =
		    if poly
		    then TFN(LD.tkc_arg numBoundTvs, fixLexp)      (* <=== BUG! *)
		    else fixLexp
	    in dbsaynl "<<< transRVBs[rec]"; (fn body => LET (defLvar, rvbLexp, body))
	    end
        else (* non-recursive case -- translate as though it were a VALdec *)
	   let val result = (fn body => LET(defLvar, transPolyExp(exp, d, !btvs), body))
	    in dbsaynl "<<< transRVBs[nonrec]"; result
	   end
    end
  | transRVBs (rvbs, d) = (* general case with multiple recursive function definitions *)
    (* depend on fact that btvs values have not been nilled-out by signature matching!!! *)
    let fun collect (RVB{var as V.VALvar{access=DA.LVAR lvar, typ, btvs, ...}, exp, ...}::rest,
		     vars, lvars, typs, btvss, exps) =
	      collect (rest, var::vars, lvar::lvars, !typ::typs, !btvs::btvss, exp::exps)
	  | collect (nil, vars, lvars, typs, btvss, exps) =
	      (rev vars, rev lvars, rev typs, rev btvss, rev exps)
	  | collect _ = bug "transRVB:collect -- bad RVB"
        val (vars, lvars, typs, btvss, exps) = collect (rvbs, nil, nil, nil, nil, nil)
	val _ = dbsaysnl ["transRVBs:oldlvars = ", PrintUtil.listToString ("(", ",", ")") LV.toString lvars]
	val (newLvars, _) = aconvertLvars (vars, exps)  (* not checking rhs occurrences of vars *)
	val _ = dbsaysnl ["transRVBs:newlvars = ", PrintUtil.listToString ("(", ",", ")") LV.toString newLvars]
        val boundTvs = foldr listUnion [] btvss         (* ordered "union" of btvs lists (duplicates merged) *)
	val numBoundTvs = length boundTvs
	val isPoly = numBoundTvs > 0
	val voidTycArgs = List.tabulate (numBoundTvs, (fn _ => LB.tcc_void))
	val fnsLvar = mkv ()  (* fresh lvar to be bound to the tuple of val rec defined functions *)
	val fntupleLexp = RECORD (map VAR newLvars)

        (* argTys : T.tyvar list -> T.type list *)
	fun argTys btvs =
	    mkTyargs (btvs, boundTvs, T.UNDEFty,
		      (fn (k,tv) => T.VARty(ref(T.LBOUND{depth=d, index=k, eq=TU.tyvarIsEq tv}))))
				  
        (* argLtcs : T.tyvar list -> LD.tyc list *)
        fun argLtcs btvs =
	    mkTyargs (boundTvs, btvs, LB.tcc_void, (fn (k,_) => LD.tcc_var(1,k)))

	val rvbLexp =
	    let val lexps =
		    if isPoly
		    then setBoundTyvars (d, boundTvs, (fn () => map (fn e => mkExp (e, d+1)) exps))
		    else map (fn e => mkExp (e, d)) exps
		      (* no polymorphism _within_ FIX, so no TFN is introduced *)
		val ltys = ListPair.map
			     (fn (ty,btvs) =>
				 toLty (if isPoly then d+1 else d)
				       (TU.applyPoly (ty, argTys btvs)))
			     (typs, btvss)
	        val fixLexp = FIX(newLvars, ltys, lexps, fntupleLexp)
	     in if isPoly
		then TFN (LD.tkc_arg numBoundTvs, fixLexp) 
		else fixLexp
	    end

	fun buildDec ([], _, _, body) = body
	  | buildDec (lvar::lvars, btvs::btvss, i, body) =
	      let val defn = (case (boundTvs, btvs)
			       of (nil,_) => SELECT (i, VAR fnsLvar)  (* "fnsLvar" not polymorphic *)
			        | (_,nil) => SELECT (i, TAPP(VAR fnsLvar, voidTycArgs))  (* lvar not polymorphic *)
				| _ => TFN (LD.tkc_arg (length btvs), (* fnsLvar and lvar both polymorphic *)
					    SELECT (i, TAPP(VAR fnsLvar, argLtcs btvs))))
	       in buildDec (lvars, btvss, i+1, LET (lvar, defn, body))
	      end
     in (fn body => LET (fnsLvar, rvbLexp, buildDec (lvars, btvss, 0, body)))
    end

and transEBs (ebs, d) =
  let fun transExn (EBgen {exn=T.DATACON{rep=DA.EXN(DA.LVAR v), typ, name, ...}, ...}, b) =
              let val nt = toDconLty d typ
                  val (argt, _) = LD.ltd_parrow nt
		  val lexp = STRING(Symbol.name name)
               in LET(v, ETAG(lexp, argt), b)
              end
        | transExn (EBdef {exn=T.DATACON{rep=DA.EXN(DA.LVAR v), typ, name, ...},
                    edef=T.DATACON{rep=DA.EXN(acc), ...}}, b) =
              let val nt = toDconLty d typ
                  val (argt, _) = LD.ltd_parrow nt
               in LET(v, VAR (transAccess(acc, LB.ltc_etag argt, SOME name)), b)
              end
        | transExn _ = bug "unexpected exn bindings in transEBs"

   in foldr' transExn ebs
  end

(* transVARSELdec : V.var * V.var * int -> lexp -> lexp *)
and transVARSELdec (pvar, ptupleVar, i) (body: lexp) =
    (* i is selection index: pvar = RSELECT(ptupleVar, i) *)
       let val _ = dbsaynl ">>> transVARSELdec"
	   val (pvarLvar, pvarBtvs) =  (* extract lvar (access) and btvs from bvar *)
	       (case pvar
		  of V.VALvar{access=DA.LVAR lvar, btvs, ...} => (lvar, !btvs)
		   | _ =>  bug "mkVB 1")
	in case ptupleVar
	     of V.VALvar {access=DA.LVAR ptupleLvar, btvs,...} =>
		  let val ptupleVarBtvs = !btvs  (* ASSERT: ptupleVarBtvs contains pvarBtvs (as set) *)
		      val _ = dbsaysnl
				["transVS: ptupleVar.name = ", S.name(V.varName ptupleVar),
				 ", ptupleLvar = ", LV.lvarName ptupleLvar]
		      val _ = checkBoundTvsSubset (pvarBtvs, ptupleVarBtvs)
		      val defnLexp =
			    (case (ptupleVarBtvs, pvarBtvs)
			      of (nil, _) => SELECT(i,VAR ptupleLvar)  (* => null pvarBtvs, so no polymorphism *)
			       | (_, nil) => (* ptupleVarBtvs non-null, pvarBtvs null; dummy instantiate ptupleVar *)
				 let val argTvs = List.tabulate (length ptupleVarBtvs, (fn _ => LB.tcc_void))
				  in SELECT (i, TAPP (VAR ptupleLvar, argTvs))
				 end
			       | _ => (* both ptupleVarBtvs and pvarBtvs non null *)
				 let val pvarArity = length pvarBtvs
				     val argTvs = mkTyargs (ptupleVarBtvs, pvarBtvs,
							    LB.tcc_void, (fn (k,_) => LD.tcc_var(1,k)))
				     val _ = if !debugging then
						(says
						 ["transVS: pvar = ", S.name(V.varName pvar),
						  "; pvarLvar = ", LV.lvarName pvarLvar,
						  "\n |pvarBtvs| = ", Int.toString(length(pvarBtvs)),
						  ", |ptupleVarBtvs| =", Int.toString(length(ptupleVarBtvs)),
						  "\n argTvs = "];
		                                 ppTycArgs argTvs;
						 newline ())
					     else ()
				  in TFN(LD.tkc_arg (length pvarBtvs),  (* tuple of M (mono) kinds *)
					 SELECT(i, TAPP(VAR ptupleLvar, argTvs)))
				 end)
		   in dbsaynl "<<< transVARSELdec";
		      LET(pvarLvar, defnLexp, body)
		  end
	     | _ => bug "transVARSELdec: bad ptupleVar"
       end (* transVARSELdec *)

(***************************************************************************
 *                                                                         *
 * Translating module exprs and decls into lambda expressions:             *
 *                                                                         *
 *    val mkStrexp : Absyn.strexp * depth -> PLambda.lexp                   *
 *    val mkFctexp : Absyn.fctexp * depth -> PLambda.lexp                   *
 *    val mkStrbs  : Absyn.strb list * depth -> PLambda.lexp -> PLambda.lexp *
 *    val mkFctbs  : Absyn.fctb list * depth -> PLambda.lexp -> PLambda.lexp *
 *                                                                         *
 ***************************************************************************)
and mkStrexp (se, d) =
  let fun transStrexp (VARstr s) = mkStr(s, d)
        | transStrexp (STRstr bs) = SRECORD (map (mkBnd d) bs)
        | transStrexp (APPstr {oper, arg, argtycs}) =
              let val e1 = mkFct(oper, d)
                  val tycs = map (tpsTyc d) argtycs
                  val e2 = mkStr(arg, d)
               in APP(TAPP(e1, tycs), e2)
              end
        | transStrexp (LETstr (dec, b)) = mkDec (dec, d) (transStrexp b)
        | transStrexp (MARKstr (b, reg)) = withRegion reg transStrexp b
   in transStrexp se
  end

and mkFctexp (fe, d) =
  let fun transFctexp (VARfct f) = mkFct(f, d)
        | transFctexp (FCTfct {param as M.STR { access, ... }, argtycs, def }) =
	  (case access of
	       DA.LVAR v =>
               let val knds = map tpsKnd argtycs
                   val nd = DI.next d  (* reflecting type abstraction *)
                   val body = mkStrexp (def, nd)
                   val hdr = buildHeader v
               (* binding of all v's components *)
               in
		   TFN(knds, FN(v, strLty(param, nd, compInfo), hdr body))
               end
	     | _ => bug "mkFctexp: unexpected access")
        | transFctexp (LETfct (dec, b)) = mkDec (dec, d) (transFctexp b)
        | transFctexp (MARKfct (b, reg)) = withRegion reg transFctexp b
        | transFctexp _ = bug "unexpected functor expressions in mkFctexp"
   in transFctexp fe
  end

(* new mkStrbs
and mkStrbs (sbs, d) =
  let fun transSTRB (STRB{name, str=M.STR { access, sign, rlzn, prim }, def}, b) =
	  (case access
	     of DA.LVAR v =>  (* binding of all v's components *)
                  let val hdr = buildHeader v
                   in LET(v, mkStrexp(def, d), hdr b)
                  end
	      | _ => bug "mkStrbs: unexpected access")
        | transSTRB _ = bug "unexpected structure bindings in mkStrbs *"
  in foldr' transSTRB sbs
  end
*)

and mkStrbs (sbs, d) =
  let fun transSTRB (STRB {str, def, ... }, body) =
	  (case str
	    of M.STR {access, ...} =>
	       (case access
		 of DA.LVAR v =>  (* binding of all v's components *)
                    let val hdr = buildHeader v
                    in LET(v, mkStrexp(def, d), hdr body)
                    end
		  | _ => bug "mkStrbs: unexpected access")
	       | M.STRSIG _ => bug "mkStrbs: str=STRSIG"
	       | M.ERRORstr => bug "mkStrbs: str=ERRORstr")
  in foldr' transSTRB sbs
  end

(* old version saved --
and mkStrbs (sbs, d) =
  let fun transSTRB (STRB{str=M.STR { access, ... }, def, ... }, b) =
	  (case access
	     of DA.LVAR v =>  (* binding of all v's components *)
                  let val hdr = buildHeader v
                   in LET(v, mkStrexp(def, d), hdr b)
                  end
	      | _ => bug "mkStrbs: unexpected access")
        | transSTRB (STRB{str=M.STRSIG _, ...}, b) =
	    bug "mkStrbs: str=STRSIG"
        | transSTRB (STRB{str=M.ERRORstr, ...}, b) =
	    bug "mkStrbs: str=ERRORstr"
(*        | transSTRB _ = bug "unexpected structure bindings in mkStrbs" *)
  in foldr' transSTRB sbs
  end
*)
      
and mkFctbs (fbs, d) =
  let fun transFCTB (FCTB{fct=M.FCT { access, ... }, def, ... }, b) =
	  (case access
	     of DA.LVAR v =>
		let val hdr = buildHeader v
                 in LET(v, mkFctexp(def, d), hdr b)
		end
	      | _ => bug "mkFctbs: unexpected access")
        | transFCTB _ = bug "unexpected functor bindings in mkFctbs"
  in foldr' transFCTB fbs
  end


(***************************************************************************
 * Translating absyn decls and exprs into lambda expression:               *
 *                                                                         *
 *    val mkDec : A.dec * DI.depth -> PLambda.lexp -> PLambda.lexp         *
 *    val mkExp : A.exp * DI.depth -> PLambda.lexp                         *
 *                                                                         *
 ***************************************************************************)
and mkDec (dec, d) =
(* "let dec in body": mkDec produces a function to be applied to the translation of body,
 *  to translate the entire let expression *)
    let val _ = if !debugging
		then (saynl ">>> mkDec";
		      ppDec dec; newline())
		else ()
	(* mkDec0 : AS.dec -> (lexp -> lexp) *)
	fun mkDec0 (VALdec vbs) = transVBs(vbs, d)
	  | mkDec0 (VALRECdec rvbs) = transRVBs(rvbs, d)
	  | mkDec0 (DOdec exp) = (fn body => LET(mkv(), mkExp(exp, d), body))
	  | mkDec0 (ABSTYPEdec{body,...}) =
	    ((* print "mkDec:ABSTYPEdec:body = ";
	     ppDec body; *)
	     mkDec0 body)
	  | mkDec0 (EXCEPTIONdec ebs) = transEBs(ebs, d)
	  | mkDec0 (STRdec sbs) = mkStrbs(sbs, d)
	  | mkDec0 (FCTdec fbs) = mkFctbs(fbs, d)
	  | mkDec0 (LOCALdec(localdec, bodydec)) =
	      let val locf = mkDec0 localdec
		  val bodyf = mkDec0 bodydec
	       in locf o bodyf
	      end
	  | mkDec0 (SEQdec ds) =  foldr (op o) ident (map mkDec0 ds)
	  | mkDec0 (MARKdec(x, reg)) =
	      let val f = withRegion reg mkDec0 x
	       in fn y => withRegion reg f y
	      end
	  | mkDec0 (OPENdec xs) =
	      let (* special hack to make the import tree simpler *)
		  fun mkopen (_, s as M.STR { access, ... }) =
			if extern access
			then ignore (transAccess (access, strLty(s, d, compInfo), NONE))
			else ()
		    | mkopen _ = ()
	       in app mkopen xs; ident
	      end
	  | mkDec0 (VARSELdec args) = transVARSELdec args
	  | mkDec0 _ = ident
	val result = mkDec0 dec
     in dbsaynl "<<< mkDec"; result
    end

and mkExp (exp, d) =
  let val tTyc = toTyc d
      val tLty = toLty d

      (* mkDcon : Types.datacon -> Plambda.dataconstr *)
      fun mkDcon (T.DATACON {name, rep, typ, ...}) =
	  let val lty = toDconLty d typ
	   in (name, mkRep (rep, lty, name), lty)
	  end

(* patToCon : AS.pat * -> Plambda.con
 * maps a "shallow" pattern to a Plambda.con *)
(* ### REWRITE AND MERGE THESE TWO COMMENTS -- mostly obsolete ### *)
(* How does the fresh lvar introduced (old translate.sml) in the CONpat and APPpat
 * cases get connected to the rhs expression of rules? Is a lambda-abstraction over
 * the new lvar wrapped around the corresponding rhs?  Or is a Let-binding
 * of the lvar to the argument of the constuctor (unit for constant constructors)
 * introduced somewhere (or implicit in the semantics of SWITCH)?  We have already
 * introduced svars that represent the argument to non-constant constructors. *)
    (* The pattern argument is a "shallow" pattern, i.e. a constant or a
     * datacon applied to a variable.
     * How does the fresh lvar introduced in the CONpat case get
     * It doesn't matter since there is nothing for it to bind to in
     * a destructed CONpat.
     * In the old match compiler, fresh lvars created for CONpat and APPpat
     * are mapped by an "environment" from a path identifying the pattern point.
     * Here we get the lvar from the var (VALvar) in the shallow pattern for
     * APPpat and VECLENpat. This variable is already refered to in the corresponding
     * rule rhs. *)
(* patToCon has been moved inside function mkExp to give it access to the tTyc type
 * translation function. *)

(*
      (* patToCon : AS.pat * -> Plambda.con *)
      (* this function was brought inside mkExp to give it access to tTyc above *)
      fun patToCon pat =
	  (case pat
	     of CONpat (datacon, tvs) =>
		  let val dummyLvar = mkv()
			(* fresh lvar to be bound to nonexistent datacon "argument". This
			 * var bindings should be a separate var option component in SWITCH
			 * cases. *)
		      val nts = map (tTyc o T.VARty) tvs
		   in DATAcon (mkDcon datacon, nts, dummyLvar)
		  end
	      | APPpat (datacon, tvs, VARpat(V.VALvar{access=DA.LVAR lvar,...})) =>
		  let val nts = map (tTyc o T.VARty) tvs
		   in DATAcon (mkDcon datacon, nts, lvar)
		  end
	      | NUMpat (_, lit) => transNum lit
	      | STRINGpat s => STRINGcon s
	      | CHARpat c => bug "patToCon: CHARpat found (should be NUMpat)" 
	      | pat => (ppPat pat;
			bug "patToCon: unexpected pattern")
	    (* end case *))
*)

    (* conToCon : AS.con * mvar option -> PLambda.con
     *  translates Absyn.con to PLambda.con incorporating a variable naming
     *  the destruct of a datacon-headed value (even if the datacon is a constant!)
     *  -- mvarOp will be SOME mvar if the con is a non-constant datacon or VLENcon,
     *  NONE for constant datacon. The mvar, if present, was produced by
     *  Generate.generate..genDectree.
     * NOTE: conToCon does nothing with the dcvarOp in the vector length case where
     *  the con is INTcon. *)
    fun conToCon (con: AS.con, mvarOp: V.var option) =  (* uses toLty and toTyc *)
            (* mkDataconstr : T.datacon -> PL.dataconstr
	     *  uses toLty arg of generate; used only in conToCon *)
	let fun mkDataconstr (T.DATACON {name, rep, typ, ...}: T.datacon) : PL.dataconstr =
	    let val lty = (* translation of the datacon type *)
		    (case typ
		       of T.POLYty{sign, tyfun=T.TYFUN{arity, body}} =>
			  if BT.isArrowType body then tLty typ
			  else tLty (T.POLYty{sign=sign,
						tyfun=T.TYFUN{arity=arity,
							       body=BT.-->(BT.unitTy, body)}})
			| _ => if BT.isArrowType typ then tLty typ
			       else tLty (BT.--> (BT.unitTy, typ)))
	     in (name, mkRep (rep, lty, name), lty)
	    end
	 in case con
	      of AS.DATAcon (datacon, tyvars) =>
		  let val tycvars = map (tTyc o T.VARty) tyvars
		      val lvar = getOpt (Option.map V.varToLvar mvarOp, LV.mkLvar())
			  (* get argument mvar from mvarOp = SOME mvar when the datacon is not
			     a constant, otherwise when datacon is a constant, mvarOp = NONE, and
			     we generate a new, but redundant, mvar that is required to construct
			     a PL.DATAcon. Probably don't need to pass argument mvar via DATAcon. *)
		   in PL.DATAcon (mkDataconstr datacon, tycvars, lvar)
		  end
	       | AS.INTcon i => PL.INTcon i
	       | AS.WORDcon w => PL.WORDcon w
	       | AS.STRINGcon s => PL.STRINGcon s
	       | AS.VLENcon(i, t) => bug "conToCon: VLENcon"
	end


(*      and transRULE (RULE(pat, rhs)) = (patToCon pat, mkExp0 rhs) -- replaced by ... *)

      (* transSRULE : srule -> PL.con * PL.lexp *)
      (* translate the rhs expression of an SRULE and translate AS.con into
       * PL.con, incorporating dcvarOp=SOME v into DATAcon using conToCon. *)
      and transSRULE (SRULE(con, dcvarOp, rhs)) = (conToCon (con, dcvarOp), mkExp0 rhs)

      and mkExp0 (VARexp (ref v, ts)) =
            (dbsaysnl [">>> mkExp0[VARexp]: ", V.toString v];
	     mkVE(v, map T.VARty ts, d))
        | mkExp0 (CONexp (dc, ts)) =
	   let val _ = dbsaynl ">>> mkExp0[CONexp]: "
	       val result = mkCE(dc, ts, NONE, d)
	       val _ = if !debugging then ppLexp result else ()
	    in result
	   end
        | mkExp0 (APPexp (CONexp(dc, ts), e2)) =
	   let val _ = dbsaynl ">>> mkExp[APPexp(CONexp _)]: "
	       val result = mkCE(dc, ts, SOME(mkExp0 e2), d)
	       val _ = if !debugging then ppLexp result else ()
	    in result
	   end
        | mkExp0 (NUMexp(src, {ival, ty})) = (
	    dbsaynl ">>> mkExp[NUMexp]";
	    if TU.equalType (ty, BT.intTy) then INT{ival = ival, ty = Tgt.defaultIntSz}
	    else if TU.equalType (ty, BT.int32Ty) then INT{ival = ival, ty = 32}
	    else if TU.equalType (ty, BT.int64Ty) then INT{ival = ival, ty = 64}
	    else if TU.equalType (ty, BT.intinfTy) then VAR (getII ival)
	    else if TU.equalType (ty, BT.wordTy) then WORD{ival = ival, ty = Tgt.defaultIntSz}
	  (* NOTE: 8-bit word is promoted to default tagged word representation *)
	    else if TU.equalType (ty, BT.word8Ty) then WORD{ival = ival, ty = Tgt.defaultIntSz}
	    else if TU.equalType (ty, BT.word32Ty) then WORD{ival = ival, ty = 32}
	    else if TU.equalType (ty, BT.word64Ty) then WORD{ival = ival, ty = 64}
	    else (ppType "### NUMexp: " ty; bug "translate NUMexp"))
(* REAL32: handle 32-bit reals *)
        | mkExp0 (REALexp(_, {rval, ty})) = REAL{rval = rval, ty = Tgt.defaultRealSz}
        | mkExp0 (STRINGexp s) = STRING s
(* QUESTION: do we want to map characters to word8? *)
(** NOTE: the following won't work for cross compiling to multi-byte characters **)
        | mkExp0 (CHARexp c) = INT{ival = IntInf.fromInt (Char.ord c),
				   ty = Tgt.defaultIntSz}
        | mkExp0 (RECORDexp []) = unitLexp
        | mkExp0 (RECORDexp xs) =
            if sorted xs then RECORD (map (fn (_,e) => mkExp0 e) xs)
            else let val vars = map (fn (l,e) => (l,(mkExp0 e, mkv()))) xs
                     fun bind ((_,(e,v)),x) = LET(v,e,x)
                     val bexp = map (fn (_,(_,v)) => VAR v) (sortrec vars)
                  in foldr bind (RECORD bexp) vars
                 end

        | mkExp0 (RSELECTexp (exp,i)) =  (* record selection, no polymorphism *)
	    SELECT(i, mkExp0 exp)

        | mkExp0 (VECTORexp ([], ty)) =
             TAPP(coreAcc "vector0", [tTyc ty])
        | mkExp0 (VECTORexp (xs, ty)) =
             let val tc = tTyc ty
                 val vars = map (fn e => (mkExp0 e, mkv())) xs
                 fun bind ((e,v),x) = LET(v, e, x)
                 val bexp = map (fn (_,v) => VAR v) vars
              in foldr bind (VECTOR (bexp, tc)) vars
             end

        | mkExp0 (VSELECTexp (exp, elemTy, index)) =
	    let val tc = tTyc elemTy
		val lt_sub =
                    let val vecTyc = LB.ltc_vector (LB.ltc_tv 0)
                    in LD.ltc_poly([LD.tkc_mono],
				   [LD.ltc_parrow(LD.ltc_tuple [vecTyc, LB.ltc_int],
						  LB.ltc_tv 0)])
                    end
		val indexLexp = INT{ival = IntInf.fromInt index, ty = Target.defaultIntSz}
	     in APP(PRIM(PO.SUBSCRIPTV, lt_sub, [tc]),
		    RECORD[ mkExp0 exp, indexLexp ])
            end

        | mkExp0 (SEQexp [e]) = mkExp0 e
        | mkExp0 (SEQexp (e::r)) = LET(mkv(), mkExp0 e, mkExp0 (SEQexp r))

        | mkExp0 (APPexp (e1, e2)) = APP(mkExp0 e1, mkExp0 e2)
        | mkExp0 (MARKexp (e, reg)) = withRegion reg mkExp0 e
        | mkExp0 (CONSTRAINTexp (e,_)) = mkExp0 e

        | mkExp0 (RAISEexp (e, ty)) = mkRaise(mkExp0 e, tLty ty)

        | mkExp0 (HANDLEexp (baseExp, (rules, lhsTy, rhsTy))) =
	    (case rules
	       of [RULE(VARpat exnvar, handlerExp)] =>
		    (case V.varAccess exnvar
		       of DA.LVAR exnlvar =>
			    HANDLE (mkExp0 baseExp, FN(exnlvar, tLty lhsTy, mkExp0 handlerExp))
			| _ => bug "mkExp0:HANDLEexp:exnvar:access")
		| _ => bug "mkExp0:HANDLEexp")

        | mkExp0 (FNexp (rules, argty, resty)) =
	     (* argty is the type of the patterns (lhs) of the rules, produced
	      *  by the type checker for FNexp;
              * resty is the result type of the function. The function may have
	      * a polymorphic type, but argty and resty should not be polymorphic
	      * (i.e. should not be POLYty). *)
	  let val _ = dbsaynl ">>> mkExp0[FNexp]: "
	      val _ = if !debugging then (ppType "### mkExp0[FNexp]" argty; newline()) else ()
	      val result = 
             (case rules
	        of [RULE(pat, rhs)] =>  (* expect single rule, produced by match compilation *)
		     (case pat
		       of VARpat matchVar =>
			  (case V.varAccess matchVar
		            of DA.LVAR paramLvar =>
			         FN (paramLvar, tLty argty, mkExp0 rhs)
			     | _ => bug "mkExp0:FNexp:matchVar.access not LVAR")
			| _ => (ppPat pat; bug "mkExp0:FNexp:non-variable pattern"))
		 | r1::r2::_ => bug "mkExp0:FNexp:multiple rules"
		 | _ => bug "mkExp0:FNexp:WTF")
	   in dbsaynl "<<< mkExp0[FNexp]";
	      result
	  end

        (* For SWITCHexp, we translate to SWITCH lexp except for special cases of
         * REF and SUSP scrutinees.
         * NOTE: translate should never see an Absyn.CASEexp, FUNexp, or handler match.
	 * All CASEexp's, etc. are translated by match compilation after type checking.
	 * Source matches (FNexp, CASEexp, HANDLEexp) are translated by the MC
         * into a combination of SWITCHexp, VSWITCHexp and LETexp.
         * The SWITCHes are "shallow" case expressions with AS.con (constructor/constants)
	 * as discriminators in its srules or "cases". An srule has the form
	 * SRULE (con, deconvar option, rhsexp) where con can be a constant
	 * (INTcon, WORDcon, STRINGcon or constant DATAcon), in which case the
	 * deconvar option is NONE (no deconstruction), or a nonconstant
	 * DATAcon(datacon,_), in which case the deconvar option is SOME var,
         * where the var gets bound to the decon[datacon] of the datatype value.
         * The rhs expression in this deconl[datacon] case will use the decon variable
         * to refer to the constructor argument (the decon of the switch subject wrt this
	 * datacon).
	 * The special single (pseudo-) constructor pattern cases involving
         * the "ref" and "susp" constructors, and switching on
         * intinf constants are treated as special cases. For all other cases,
         * immediately builds a PLambda.SWITCH.
	 * The defaultOp arg will be SOME when the srule cons are not
         * exhaustive and the underlying OR node had default rules (some values matching
	 * via variables or wildcards).
	 * NOTE: the scrutinee expression is either a VARexp(ref(var),[]) where var has
	 *  LVAR access (it is a match compiler administrative variable introduced to denote
	 *  intermediate values during matching), or such a var expression wrapped in
	 *  RSELECTexp and VSELECTexp selections.
	 * ASSERT: length srules > 0 *)
        | mkExp0 (SWITCHexp (scrutinee: exp, srules, defaultOp)) =
	     (* non-degenerate case, multiple rules *)
             let val scrutineeLexp = mkExp0 scrutinee (* was V.varToLvar scrutinee *)
		 val SRULE(con, _, _) :: _ = srules  (* get con of first rule *)
		 val consig = AU.conToSign con
		 val trules as ((con1, lexp1) :: _) = map transSRULE srules
		     (* was "rev srules", why? Possible source of bug 290. *)
		 val defaultOp' = Option.map mkExp0 defaultOp
             in case con1
		 of DATAcon((_, DA.REF, lt), ts, lvar) =>
		      (* ref pseudo-constructor, single, hence unique rule *)
		      LET(lvar,
			  APP (PRIM (Primop.DEREF, LE.lt_swap lt, ts), scrutineeLexp),
			  lexp1)
		  | DATAcon((_, DA.SUSP(SOME(_, DA.LVAR f)), lt), ts, lvar) =>
		      (* susp pseudo-constructor, single, hence unique rule *)
		      let val localLvar = mkv()
		      in LET(lvar,
			     LET(localLvar, TAPP(VAR f, ts), APP(VAR localLvar, scrutineeLexp)),
			     lexp1)
		      end
		  | INTcon{ty=0, ...} => (* IntInf.int constant *)
		      let fun strip (INTcon{ty=0, ival}, lexp) = (ival, lexp)
			    | strip _ = bug "genswitch - INTINFcon"
		      in genintinfswitch (scrutineeLexp, map strip trules,
					  Option.valOf defaultOp')
		      end
		  | _ => SWITCH(scrutineeLexp, consig, trules, defaultOp')
             end

        (* VSWITCHexp:
	 *  srules for vector switches are of the form SRULE(INTcon n, NONE, exp) *)  
	| mkExp0 (VSWITCHexp (scrutinee: exp, elemType, srules, default)) = 
	    let val scrutineeLexp = mkExp0 scrutinee  (* V.varToLvar scrutinee *)
		val lengthLvar = mkv()  (* fresh lvar to be bound to the length of the vector *)
(*                val scrutTy = V.varType scrutinee
		(* val _ = (ppType "Translate.mkExp0[VSWITCH]: scrutTy = " scrutTy; newline()) *)
		val elemType = TU.vectorElemTy scrutTy
                               handle exn =>
				      (ppType "Translate.mkExp0[VSWITCHexp]" scrutTy; newline();
				       raise exn)
*)
		val elemtyc = tTyc elemType
		val lt_len = LD.ltc_poly([LD.tkc_mono],
					 [LD.ltc_parrow(LB.ltc_tv 0, LB.ltc_int)])
		val vectortyc = LB.tcc_vector elemtyc
	     in LET(lengthLvar,
		    APP(PRIM(PO.LENGTH, lt_len, [vectortyc]), scrutineeLexp),
		    SWITCH(VAR lengthLvar, DA.CNIL, map transSRULE srules, SOME(mkExp0 default)))
	    end

	| mkExp0 (LETVexp (var, definiens, body)) = 
	    let val _ = dbsaysnl [">>> mkExp0[LETVexp]: ", V.toString var]
		val result  = LET(V.varToLvar var, mkExp0 definiens, mkExp0 body)
	     in dbsaynl "<<< mkExp0[LETVexp]";
	        result
	    end

	| mkExp0 (IFexp { test, thenCase, elseCase }) =
	    COND (mkExp0 test, mkExp0 thenCase, mkExp0 elseCase)

	| mkExp0 (ANDALSOexp (e1, e2)) =
	    COND (mkExp0 e1, mkExp0 e2, falseLexp)

	| mkExp0 (ORELSEexp (e1, e2)) =
	    COND (mkExp0 e1, trueLexp, mkExp0 e2)

	| mkExp0 (WHILEexp { test, expr }) =
	    let val fv = mkv ()
		val body =
		    FN (mkv (), lt_unit,
			COND (mkExp0 test,
			      LET (mkv (), mkExp0 expr, APP (VAR fv, unitLexp)),
			      unitLexp))
	    in
		FIX ([fv], [lt_u_u], [body], APP (VAR fv, unitLexp))
	    end

        | mkExp0 (LETexp (dec, body)) =
	    let val _ = dbsay ">>> mkExp0[LETexp]"
		val decfn = mkDec (dec, d)
		val body = mkExp0 body
		val result = decfn body
	     in dbsay "<<< mkExp0[LETexp]";
	        result
	    end

        | mkExp0 e =
            EM.impossibleWithBody "untranslateable expression:\n  "
              (fn ppstrm => (PPAbsyn.ppExp (env,NONE) ppstrm (e, !ppDepth)))

   in mkExp0 exp
  end

(* DBM: are tranIntInf and wrapII still relevant? Relation with genintinfswitch? *)
and transIntInf d s =
    (* This is a temporary solution.  Since IntInf literals
     * are created using a core function call, there is
     * no indication within the program that we are really
     * dealing with a constant value that -- in principle --
     * could be subject to such things as constant folding. *)
    let val consexp = CONexp (BT.consDcon, [ref (T.INSTANTIATED BT.wordTy)])
	fun build [] = CONexp (BT.nilDcon, [ref (T.INSTANTIATED BT.wordTy)])
	  | build (d :: ds) = let
	      val i = Word.toIntX d
	      in
		APPexp (consexp, EU.TUPLEexp [
		    NUMexp("<lit>", {ival = IntInf.fromInt i, ty = BT.wordTy}),
		    build ds
		  ])
	      end
	fun mkSmallFn s =
	      coreAcc(if LN.isNegative s then "makeSmallNegInf" else "makeSmallPosInf")
	fun mkFn s =
	      coreAcc(if LN.isNegative s then "makeNegInf" else "makePosInf")
	fun small w =
	      APP (mkSmallFn s,
		mkExp (
		  NUMexp("<lit>", {ival = IntInf.fromInt (Word.toIntX w), ty = BT.wordTy}),
		  d))
     in case LN.repDigits s
          of [] => small 0w0
	   | [w] => small w
	   | ws => APP (mkFn s, mkExp (build ws, d))
    end

(* Wrap bindings for IntInf.int literals around body. *)
fun wrapII body = let
    fun one (n, v, b) = LET (v, transIntInf DI.top n, b)
in
    IIMap.foldli one body (!iimap)
end

(* wrapPidInfo: lexp * (pid * pidInfo) list -> lexp * importTree *)
fun wrapPidInfo (body, pidinfos) =
  let val imports =
        let fun p2itree (ANON xl) =
                  ImportTree.ITNODE (map (fn (i,z) => (i, p2itree z)) xl)
              | p2itree (NAMED _) = ImportTree.ITNODE []
         in map (fn (p, pi) => (p, p2itree pi)) pidinfos
        end

(*
      val _ = let val _ = say "\n ****************** \n"
                  val _ = say "\n the current import tree is :\n"
                  fun tree (ImportTree.ITNODE []) = ["\n"]
                    | tree (ImportTree.ITNODE xl) =
                        foldr (fn ((i, x), z) =>
                          let val ts = tree x
                              val u = (Int.toString i)  ^ "   "
                           in (map (fn y => (u ^ y)) ts) @ z
                          end) [] xl
                  fun pp (p, n) =
                    (say ("Pid " ^ (PersStamps.toHex p) ^ "\n");
                     app say (tree n))
               in app pp imports; say "\n ****************** \n"
              end
*)
      val plexp =
        let fun get ((_, ANON xl), z) = foldl get z xl
              | get ((_, u as NAMED (_,t,_)), (n,cs,ts)) =
                  (n+1, (n,u)::cs, t::ts)

            (* get the fringe information *)
            val getp = fn ((_, pi), z) => get((0, pi), z)
            val (finfos, lts) =
              let val (_, fx, lx) = foldl getp (0,[],[]) pidinfos
               in (rev fx, rev lx)
              end

            (* do the selection of all import variables *)
            fun mksel (u, xl, be) =
              let fun g ((i, pi), be) =
                    let val (v, xs) = case pi of ANON z => (mkv(), z)
                                               | NAMED(v,_,z) => (v, z)
                     in LET(v, SELECT(i, u), mksel(VAR v, xs, be))
                    end
               in foldr g be xl
              end
            val impvar = mkv()
            val implty = LD.ltc_str lts
            val nbody = mksel (VAR impvar, finfos, body)
         in FN(impvar, implty, nbody)
        end
   in (plexp, imports)
  end (* function wrapPidInfo *)

(** the list of things being exported from the current compilation unit *)
val exportLexp = SRECORD (map VAR exportLvars)

(* val _ = dbsaynl ">>mkDec" *)
(** translating the ML absyn into the PLambda expression *)
val body = mkDec (rootdec, DI.top) exportLexp
(* val _ = dbsaynl "<<mkDec" *)
val _ = if CompInfo.anyErrors compInfo
	then raise EM.Error
	else ()
(** add bindings for intinf constants *)
val body = wrapII body

(** wrapping up the body with the imported variables *)
val (plexp, imports) = wrapPidInfo (body, PersMap.listItemsi (!persmap))

(** type check body (including kind check) **)
val ltyerrors = if !FLINT_Control.checkPLambda
		then ChkPlexp.checkLtyTop(plexp,0)
		else false
val _ = if ltyerrors
        then (print "**** Translate: checkLty failed ****\n";
              with_pp(fn str =>
                (PU.pps str "absyn:"; PP.newline str;
                 ElabDebug.withInternals
                  (fn () => PPAbsyn.ppDec (env,NONE) str (rootdec,1000));
		 PP.newline str;
                 PU.pps str "lexp:"; PP.newline str;
                 PPLexp.ppLexp 25 str plexp));
              complain EM.WARN "checkLty" EM.nullErrorBody;
	     bug "PLambda type check error!")
        else ()


(* print plambda IR if enabled by Control.FLINT flags printAll or
 * printLambda *)
val _ = if !Control.FLINT.printAllIR orelse !Control.FLINT.printPlambda
	then (say ("\n\n[After Translate" ^ " ...]\n\n"); ppLexp plexp)
	else ()

(** normalizing the plambda expression into FLINT *)
val flint = let val _ = dbsaynl ">> FlintNM.norm"
		val n = FlintNM.norm plexp
		val _ = dbsaynl "<< FlintNM.norm"
	    in n end

(* print flint IR if enabled by Control.FLINT flags printAll or
 * printFlint *)
val _ = if !Control.FLINT.printAllIR orelse !Control.FLINT.printFlint
	then (say "\n[After FlintNM.norm ...]\n\n";
	      PrintFlint.printFundec flint;
	      say "\n")
	else ()

in {flint = flint, imports = imports}
end (* function transDec *)

end (* top-level local *)
end (* structure Translate *)
