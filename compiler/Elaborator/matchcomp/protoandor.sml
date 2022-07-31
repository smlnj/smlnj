(* FLINT/trans/protoandor.sml *)
(* revised "old" match compiler *)

(* build a simple "proto"-AndOr tree (type protoAndor), by layering the pattern info for 
 * each of a sequence of patterns (match left-hand-sides) to build up protoAndor nodes. *)

structure ProtoAndor =
struct

local
  structure DA = Access
  structure V = Variable
  structure AS = Absyn
  structure AU = AbsynUtil
  structure TU = TypesUtil
  structure BT = BasicTypes
  structure P = Paths
  structure MC = MCCommon	     
  structure PP = PrettyPrint

  open Absyn Paths MCCommon

  fun bug msg = ErrorMsg.impossible ("ProtoAndor: "^msg)
  val say = Control_Print.say
  fun newline () = say "\n"
  fun saynl msg = (say msg; newline())
  fun says strings = say (concat strings)
  fun saysnl strings = saynl (concat strings)

  fun ppProtoAndor pandor =
      PP.with_default_pp (fn ppstrm => MCPrint.ppProtoAndor ppstrm pandor)

  fun ppPat pat =
      PP.with_default_pp(fn ppstrm => PPAbsyn.ppPat StaticEnv.empty ppstrm (pat, 20))

in

fun numToCon (v, ty) =
    let fun mkWORD sz = WORDcon{ival = v, ty = sz}
	fun mkINT sz = INTcon{ival = v, ty = sz}
     in if TU.equalType(ty, BT.intTy)
	  then mkINT Target.defaultIntSz
	else if TU.equalType(ty, BT.int32Ty)
	  then mkINT 32
	else if TU.equalType(ty, BT.int64Ty)
	  then mkINT 64
	else if TU.equalType(ty, BT.intinfTy)
	  then mkINT 0
	else if TU.equalType(ty, BT.wordTy)
	  then mkWORD Target.defaultIntSz
	else if TU.equalType(ty, BT.word8Ty)
	  then mkWORD Target.defaultIntSz
	else if TU.equalType(ty, BT.word32Ty)
	  then mkWORD 32
        else if TU.equalType(ty, BT.word64Ty)
          then mkWORD 64
	  else bug "numToCon: unrecognized numeric type"
      end

(* intCon : int -> con
 *  default integer pattern constant *)
fun intCon n = INTcon {ival = IntInf.fromInt n, ty = Target.defaultIntSz}

(* charCon : char -> con
 *  pattern constant for character literal; "promoting" char to int *)
(* QUESTION: perhaps this should be a Word8.word literal? Or later Word16.word? Word32.word? *)
fun charCon c = intCon (Char.ord c)

(* addVarRule : ruleno * protoAndor -> protoAndor *)
fun addVarRule (rule: ruleno, ANDp{varRules, children}) =
      ANDp {varRules = RS.add(varRules, rule), children = children}
  | addVarRule (rule, ORp{varRules, sign, cases}) =
      ORp {varRules = RS.add (varRules,rule), cases = cases, sign = sign}
  | addVarRule (rule, VARp{varRules}) =
      VARp {varRules = RS.add (varRules,rule)}
  | addVarRule (rule, WCp) =
      VARp {varRules = RS.singleton rule}

(* makeProtoAndor : pat list -> MC.protoAndor
 *  ASSERT: not (null pats) *)
fun makeProtoAndor (pats: AS.pat list) =
let (* genAndor : pat * ruleno -> andor *)
    (*  the ruleno is incorporated into either VARp (for a variable) or caserule (for a con) *)
    fun genAndor (VARpat var, rule) = VARp {varRules = RS.singleton rule}
      | genAndor (WILDpat, _) = WCp  (* was: VARp {varRules = RS.empty} *)
      | genAndor (CONSTRAINTpat(pat, _), rule) = genAndor(pat, rule) (* ignore type constraint *)
      | genAndor (LAYEREDpat(CONSTRAINTpat(lpat,_), bpat), rule) =  (* ignore type constraint *)
	  genAndor (LAYEREDpat(lpat, bpat), rule)
      | genAndor (LAYEREDpat(VARpat _, basePat), rule) =
	  addVarRule (rule, genAndor (basePat, rule))
      | genAndor (NUMpat(_, {ival, ty}), rule) =
	  let val con = numToCon(ival, ty)
	   in ORp {varRules = RS.empty, sign = DA.CNIL, cases = [(con, RS.singleton rule, CONST)]}
	  end
      | genAndor (STRINGpat s, rule) =
	  ORp {varRules = RS.empty, sign = DA.CNIL, cases = [(STRINGcon s, RS.singleton rule, CONST)]}
      | genAndor (CHARpat s, rule) =
	  (* NOTE: this rule won't work for cross compiling to multi-byte characters. *) 
	  ORp{varRules = RS.empty, sign = DA.CNIL, cases = [(charCon s, RS.singleton rule, CONST)]}
      | genAndor (RECORDpat{fields,...}, rule) =
	  ANDp{varRules = RS.empty, children=multiGen(map #2 fields, rule)}
      | genAndor (VECTORpat(pats,ty), rule) =
	  ORp {varRules = RS.empty, sign = DA.CNIL,
	       cases = [(VLENcon (length pats, ty), RS.singleton rule,
			 VELEMS (multiGen (pats, rule)))]}
      | genAndor (CONpat(k,t), rule) =
	  ORp {varRules = RS.empty, sign = TU.dataconSign k,
	       cases = [(DATAcon(k, t), RS.singleton rule, CONST)]}
      | genAndor (APPpat(k,t,pat), rule) =
	  ORp {varRules = RS.empty, sign = TU.dataconSign k,
	       cases = [(DATAcon(k,t), RS.singleton rule, DCARG (genAndor (pat, rule)))]}
      | genAndor _ =
	  bug "genandor - unexpected pat arg"

    (* multiGen : pat list * ruleno -> simpleAndor list *)
    and multiGen (pats, rule) = map (fn pat => genAndor(pat,rule)) pats

    (* mergeAndor : pat * andor * ruleno -> andor *)
    and mergeAndor (pat, WCp, rule) = genAndor (pat, rule)
      | mergeAndor (VARpat var, andor, rule) = addVarRule (rule, andor)
      | mergeAndor (WILDpat, andor, rule) = andor  (* loose the WC? *)
      | mergeAndor (CONSTRAINTpat(pat, _), andor, rule) =
	  mergeAndor(pat, andor, rule)
      | mergeAndor (LAYEREDpat(CONSTRAINTpat(lpat,_), bpat), andor, rule) =
	  mergeAndor (LAYEREDpat(lpat, bpat), andor, rule)
      | mergeAndor (LAYEREDpat(VARpat var, bpat), andor, rule) =
	  addVarRule (rule, mergeAndor (bpat, andor, rule))
      | mergeAndor (CONpat(k,t), VARp{varRules}, rule) =
	  ORp {varRules = varRules, sign = TU.dataconSign k,
	       cases = [(DATAcon(k,t), RS.singleton rule, CONST)]}
      | mergeAndor (APPpat(k,t,pat), VARp{varRules}, rule) =
	  ORp {varRules = varRules, sign = TU.dataconSign k,
		cases = [(DATAcon(k,t), RS.singleton rule, DCARG (genAndor(pat, rule)))]}
      | mergeAndor (pat, andor as (VARp{varRules}), rule) =
	  (case genAndor(pat, rule)
	     of ORp{sign, cases, ...} =>  (* assume varRules empty *)
		  ORp {varRules=varRules, sign=sign, cases=cases}
	      | ANDp{children, ...} =>  (* assume varRules empty *)
		  ANDp {varRules=varRules, children=children}
	      | VARp {varRules=newVarRules} => 
		  VARp {varRules = RS.union (newVarRules, varRules)}
	      | WCp => andor)
      | mergeAndor (NUMpat(_, {ival, ty}), c as ORp{varRules, cases, sign}, rule) =
	  let val con = numToCon(ival, ty)
	   in ORp{varRules = varRules, sign = sign,
		  cases = addACase(con, [], rule, cases)}
	  end
      | mergeAndor (STRINGpat s, ORp{varRules, cases, sign}, rule) =
	  ORp {varRules = varRules, sign=sign,
		cases = addACase(STRINGcon s, nil, rule, cases)}
      (* NOTE: the following won't work for cross compiling to multi-byte characters *)
      | mergeAndor (CHARpat s, ORp{varRules, cases, sign}, rule) =
	  ORp{varRules = varRules, sign=sign,
	      cases = addACase(charCon s, nil, rule, cases)}
      | mergeAndor (RECORDpat{fields,...}, ANDp{varRules, children}, rule) =
	  ANDp{varRules = varRules,
	       children=multiMerge(map #2 fields, children, rule)}
      | mergeAndor (VECTORpat(pats,t), ORp{varRules, cases, sign}, rule) =
	  ORp {varRules = varRules, sign = sign,
	       cases = addACase(VLENcon(length pats, t), pats, rule, cases)}
      | mergeAndor (CONpat(k,t), ORp{varRules, cases, sign}, rule) =
	  ORp {varRules=varRules, sign=sign,
	       cases=addACase(DATAcon(k,t), nil, rule, cases)}
      | mergeAndor (APPpat(k,t,pat), ORp{varRules, cases, sign}, rule) =
	  ORp {varRules=varRules, sign=sign,
	       cases=addACase(DATAcon(k,t), [pat], rule, cases)}
      | mergeAndor (pat, pandor, rule) =
	(saynl "mergeAndor:\n   pat = "; ppPat pat;
	 saynl "   pandor = "; ppProtoAndor pandor;
	 bug "mergeAndor - incompatible args")

    (* addACase : con * pat list * ruleno * protoVariant list -> protoVariant list *)
    (* if con is constant, pats is nil;
     * if con is a normal nonconstant datacon, pats must be a singleton list;
     * if con is VLENcon(k,_), pats has length = k (the vector length) *)  
    and addACase (con, pats, rule, nil) =  (* a new con has been discovered *)
	  let val subcase =
		  case (con, pats)
		   of ((INTcon _ | WORDcon _ | STRINGcon _ | DATAcon _), nil) => CONST
		    | (DATAcon _, [pat]) => DCARG (genAndor (pat, rule))
		    | (VLENcon _, pats) => VELEMS (multiGen(pats, rule))
		    | _ => bug "addACase: con & pats don't agree"
	   in [(con, RS.singleton rule, subcase)]
	  end
      | addACase (con, pats, rule, (aCase as (con', rules, subcase)) :: rest) =
	  if AU.eqCon (con, con')  (* existing variant has the same con *)
	  then let val subcase' =
		       case (subcase, pats)
			of (CONST, nil) => CONST
			 | (DCARG pandor, [pat]) =>
			     DCARG (mergeAndor (pat, pandor, rule))
		         | (VELEMS velems, _) => VELEMS (multiMerge(pats, velems, rule))
	        in (con, RS.add(rules, rule), subcase') :: rest
	       end
	  else aCase::(addACase(con, pats, rule, rest))

    (* multiMerge : pat list * protoAndor list * int -> andor list
     *  pat list and protoAndor list should have the same length *)
    and multiMerge (pats, sandors, rule) =
	let fun merge1 (pat, sandor) = mergeAndor (pat, sandor, rule)
	 in ListPair.mapEq merge1 (pats, sandors)
	end

    (* mergePats : pat list * int -> simpleAndor *)
    fun mergePats (nil, _, pandor) = pandor
      | mergePats (pat::rest, ruleno, pandor) =
	  mergePats (rest, ruleno+1, mergeAndor (pat, pandor, ruleno))

    val pat0 :: rest = pats

 in mergePats (rest, 1, genAndor (pat0, 0))
end (* fun makeProtoAndor *)

end (* top local *)

end (* structure ProtoAndor *)
