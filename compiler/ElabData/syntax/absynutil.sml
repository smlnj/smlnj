(* absynutil.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *
 * More stuff from ElabUtil should be moved here eventually.
 *)
structure AbsynUtil : sig

    val bogusCON : Types.datacon
    val bogusEXN : Types.datacon
    val unitExp : Absyn.exp
    val unitPat : Absyn.pat
(*  val isWild : Absyn.pat -> bool *)
    val TUPLEexp : Absyn.exp list -> Absyn.exp
    val TUPLEpat : Absyn.pat list -> Absyn.pat
    val eqCon : Absyn.con * Absyn.con -> bool
    val constantCon : Absyn.con -> bool
    val conToSign : Absyn.con -> Access.consig
    val conToString : Absyn.con -> string
    val headStripExp : Absyn.exp -> Absyn.exp
    val headStripPat : Absyn.pat -> Absyn.pat
    val stripPatMarks : Absyn.pat -> Absyn.pat
    val patternVars : Absyn.pat -> Variable.var list
    val noVarsInPat : Absyn.pat -> bool

  end = struct

    structure S = Symbol
    structure T = Types
    structure TU = TypesUtil
    structure BT = BasicTypes
    structure LV = LambdaVar
    structure A = Access
    structure V = Variable
    structure AS = Absyn
    open Absyn

  (* "special" datacons -- moved from VarCon (now Variable - ElabData/syntax/variable.s??) *)

    val bogusCON =
        T.DATACON{name=S.varSymbol "bogus",
                  typ=T.WILDCARDty,
                  rep=A.CONSTANT 0,
                  const=true,
                  lazyp=false,
                  sign=A.CSIG(0,1)}

    val bogusEXN =
        T.DATACON{name=S.varSymbol "bogus",
                  typ=BasicTypes.exnTy,
                  rep=A.CONSTANT 0,
                  const=true,
                  lazyp=false,
                  sign=A.CNIL}

    val unitExp = RECORDexp []
    val unitPat = RECORDpat {fields=nil, flex=false, typ=ref(BasicTypes.unitTy)}

  (* isWild : pat -> bool *)
    fun isWild WILDpat = true
      | isWild _ = false

    fun TUPLEexp l =
        let fun build (_, []) = []
              | build (i, e :: es) =
                (LABEL { number = i-1, name = Tuples.numlabel i }, e)
                :: build (i+1, es)
         in RECORDexp (build (1, l))
        end

    fun TUPLEpat l =
        let fun build (_, []) = []
              | build (i, e :: es) = (Tuples.numlabel i, e) :: build (i+1, es)
         in RECORDpat { fields = build (1, l), flex = false,
                        typ = ref Types.UNDEFty }
        end

    (* eqCon : con * con -> bool *)
    fun eqCon (DATAcon (d1, _), DATAcon (d2, _)) = TU.eqDatacon (d1, d2)
      | eqCon (INTcon n, INTcon n') = (#ival n = #ival n')   (* types assumed compatible *)
      | eqCon (WORDcon n, WORDcon n') = (#ival n = #ival n') (* types assumed compatible *)
      | eqCon (STRINGcon s, STRINGcon s') = (s = s')
      | eqCon (VLENcon (n, _), VLENcon (n', _)) = (n = n')   (* types assumed compatible *)
      | eqCon _ = false

    (* constantCon : con -> bool *)
    fun constantCon (DATAcon(datacon, _)) = TU.dataconIsConst datacon
      | constantCon _ = false

    (* conToSign: con -> Access.consig *)
    fun conToSign (DATAcon (datacon, _)) = TU.dataconSign datacon
      | conToSign _ = Access.CNIL

    (* conToString : Abysyn.con -> string *)
    fun conToString (DATAcon(datacon, _)) = (Symbol.name (TU.dataconName(datacon)))
      | conToString (INTcon{ival, ty=0}) = "II" ^ IntInf.toString ival
      | conToString (INTcon{ival, ty}) = "I" ^ IntInf.toString ival
      | conToString (WORDcon{ival, ty}) = "W" ^ IntInf.toString ival
      | conToString (STRINGcon s) = "S:" ^ PrintUtil.formatString s
      | conToString (VLENcon (n,_)) = "L" ^ Int.toString n

    (* headStripExp : exp -> exp *)
    (* strip MARKexp and CONSTRAINTexp head constructors. Used to access the RECORDexp (pair)
     * argument of an infix constructor in an APPexp (see PPAbsyn.ppAppExp) *)
    fun headStripExp (MARKexp(exp,_)) = headStripExp exp
      | headStripExp (CONSTRAINTexp(exp,_)) = headStripExp exp
      | headStripExp exp = exp

    (* headStripPat : pat -> pat *)
    (* strip MARKpat and CONSTRAINTpat head constructors. Used to access the RECORDpat (pair)
     * argument of an infix constructor in an APPpat (see PPAbsyn.ppDconPat) *)
    fun headStripPat (MARKpat (p,_)) = headStripPat p
      | headStripPat (CONSTRAINTpat (p, ty)) = headStripPat p
      | headStripPat pat = pat

    (* stripPatMarks : AS.pat -> AS.pat *)
    fun stripPatMarks pat =
        case pat
          of (MARKpat(p,_)) => stripPatMarks p
           | RECORDpat{fields, flex, typ} =>
             RECORDpat{fields = map (fn (l,p) => (l, stripPatMarks p)) fields,
                       flex = flex, typ = typ}
           | APPpat (dc, tvs, p) =>
             APPpat (dc, tvs, stripPatMarks p)
           | CONSTRAINTpat (p, ty) => CONSTRAINTpat (stripPatMarks p, ty)
           | LAYEREDpat(pat1, pat2) =>
             LAYEREDpat(stripPatMarks pat1, stripPatMarks pat2)
           | ORpat (pat1, pat2) =>
             ORpat (stripPatMarks pat1, stripPatMarks pat2)
           | VECTORpat (pats,ty) =>
             VECTORpat (map stripPatMarks pats, ty)
           | p => p

    (* patternVars: Absyn.pat -> V.var list *)
    (* returns a list of the variables bound in the pattern, without duplicates, and
     * sorted in order of their lvars (all variables in a pattern have access LVAR).
     * WILDpat is not considered a pattern variable.  *)
    fun patternVars pat =
        let fun enter (new : V.var, l: V.var list) =
                let fun f [] = [new]
                      | f (l as h::t) =
                        case LV.compare (V.varToLvar new, V.varToLvar h)
                          of LESS    => new::l
                           | GREATER => h::f t
                           | EQUAL   => l
                 in f l
                end
            fun collect(pat::rest,vars) =
                (case pat
                  of VARpat var => collect(rest, enter(var,vars))
                   | RECORDpat{fields, ...} => collect (map #2 fields @ rest, vars)
                   | APPpat(_,_,p) => collect(p::rest, vars)
                   | CONSTRAINTpat (p,_) => collect(p::rest,vars)
                   | LAYEREDpat(pat1, pat2) => collect(pat1::pat2::rest, vars)
                   | ORpat(pat1, pat2) => collect(pat1::rest, vars)
                      (* pat1 and pat2 have same variables, so only have to collect vars from pat1 *)
                   | VECTORpat(pats,_) => collect(pats@rest, vars)
                   | _ => collect(rest, vars))
              | collect (nil, vars) = vars
        in collect ([pat],nil)
        end

    (* noVarsInPat : pat -> bool
     * Test pat, for the occurence of variables (including layering variables)
     * or wildcards. Return true if any are present, false otherwise.
     *)
    fun noVarsInPat pat =
          let fun hasVar WILDpat = true (* might want to flag this *)
                | hasVar (VARpat _) = true
                | hasVar (LAYEREDpat _) = true
                | hasVar (CONSTRAINTpat(p,_)) = hasVar p
                | hasVar (APPpat(_,_,p)) = hasVar p
                | hasVar (RECORDpat{fields,...}) = List.exists (hasVar o #2) fields
                | hasVar (VECTORpat(pats,_)) = List.exists hasVar pats
                | hasVar (ORpat (pat1,pat2)) = hasVar pat1 orelse hasVar pat2
                | hasVar (MARKpat (pat, _)) = hasVar pat
                | hasVar _ = false
           in not(hasVar pat)
          end

  end (* structure AbsynUtil *)
