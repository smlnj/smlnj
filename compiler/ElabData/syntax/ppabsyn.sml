(* ppabsyn.sml
 *
 * COPYRIGHT (c) 2021 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature PPABSYN =
sig

  val fmtPat  : StaticEnv.staticEnv -> Absyn.pat * int -> unit
  val fmtExp  : StaticEnv.staticEnv * Source.inputSource option
                -> Absyn.exp * int -> unit
  val fmtRule : StaticEnv.staticEnv * Source.inputSource option
                -> Absyn.rule * int -> unit
  val fmtVB   : StaticEnv.staticEnv * Source.inputSource option
                -> Absyn.vb * int -> unit
  val fmtRVB  : StaticEnv.staticEnv * Source.inputSource option
                -> Absyn.rvb * int -> unit
  val fmtDec  : StaticEnv.staticEnv * Source.inputSource option
                -> Absyn.dec * int -> unit

  val ppStrexp : StaticEnv.staticEnv * Source.inputSource option
                 -> Absyn.strexp * int -> unit

end (* signature PPABSYN *)


structure PPAbsyn: PPABSYN =
struct

local
  structure EM = ErrorMsg
  structure M = Modules
  structure B = Bindings
  structure S = Symbol
  structure F = Fixity
  structure LV = LambdaVar
  structure A = Access
  structure T = Types
  structure V = Variables
  structure AS = Absyn
  structure AU = AbsynUtil
  structure AT = Tuples		     
  structure SE = StaticEnv
  structure PP = NewPP
  structure PPU = NewPPUtil
  structure PPT = PPType
  structure PPV = PPVal

  open Absyn
in

(* debugging *)
val debugging = ElabDataControl.ppabsyndebugging

val say = Control_Print.say
fun dbsaynl (msg: string) =
    if !debugging then (say msg; say "\n") else ()

fun bug msg = ErrorMsg.impossible("PPAbsyn: "^msg)

(* printing flags, from ElabDataControl *)
val lineprint = ElabDataControl.absynLineprint
val internals = ElabDataControl.absynInternals

(* fmtPos : Source.inputSource * int -> PP.format *)
fun fmtPos(source: Source.inputSource, charpos: int) =
    if !lineprint then
      let val {line, column,...} = Source.filepos source charpos
       in PP.concat [PP.integer line, PP.period, PP.integer column]
      end
    else PP.integer charpos

type context = SE.staticEnv * Source.inputSource option

(* isTUPLEpat and isTUPLEexp belong in AbsynUtil *)

(* checkpat (int * (S.symbol * 'a) list -> bool
 *   check that a pattern is a tuple pattern
 *   called with n = 1 in isTUPLEpat *)
fun checkpat (n, nil) = true
  | checkpat (n, (sym,_)::fields) =
    S.eq(sym, numlabel n) andalso checkpat(n+1,fields)

(* isTUPLEpat : AS.pat -> bool *)
fun isTUPLEpat (RECORDpat{fields=[_],...}) = false
  | isTUPLEpat (RECORDpat{flex=false,fields,...}) = checkpat(1,fields)
  | isTUPLEexp (MARKpat (p,_)) = isTUPLEexp p
  | isTUPLEpat _ = false

(* checkexp : (int * (AS.numberedLabel * 'a) list -> bool
 *   check that an expression is a tuple expression
 *   called in isTUPLEexp with n = 1 *)
fun checkexp (n,nil) = true
  | checkexp (n, (LABEL{name=sym,...},_)::fields) =
	S.eq(sym, numlabel n) andalso checkexp(n+1,fields)

(* isTUPLEexp : AS.exp -> bool *)
fun isTUPLEexp (RECORDexp [_]) = false
  | isTUPLEexp (RECORDexp fields) = checkexp(1,fields)
  | isTUPLEexp (MARKexp (e,_)) = isTUPLEexp e
  | isTUPLEexp _ = false

(* lookFIX : SE.staticEnv * S. symbol -> F.fixity *)
fun lookFIX (env : SE.staticEnv, sym: S.symbol) =
    Lookup.lookFix (env, S.fixSymbol (S.name sym))

(* stripMark : AS.exp -> AS.exp  -- belongs in AbsynUtil *)
fun stripMark (MARKexp (exp, _)) = stripMark exp
  | stripMark exp = exp

(* fmtInvPath : InvPath.path -> PP.format *)
fun fmtInvPath (rpath : InvPath.path) = PP.text (InvPath.toString rpath)

(* patTag : pat -> string *)
fun patTag pat =
    case pat
      of WILDpat => "<WILDpat>"
       | VARpat _ => "<VARpat>"
       | NUMpat _ => "<NUMpat>"	(* string is source text of literal *)
       | STRINGpat _ => "<STRINGpat>"
       | CHARpat _ => "<CHARpat>"
       | CONpat _ => "<CONpat>"
       | RECORDpat _ => "<RECORDpat>"
       | APPpat _ => "<APPpat>"
       | CONSTRAINTpat _ => "<CONSTRAINTpat>"
       | LAYEREDpat _ => "<LAYEREDpat>"
       | ORpat _ => "<ORpat>"
       | VECTORpat _ => "<VECTORpat>"
       | MARKpat (pat, _) => patTag pat
       | NOpat => "<NOpat>"

(* expTag : exp -> string *)
fun expTag exp = "<exp>"	      

(* decTag : dec -> string *)
fun decTag dec = "<dec>"	      

fun fmtTyFormals arity =
    (case arity
       of 0 => PP.empty
	| 1 => PP.text "'a"
	| n => PP.tupleFormats (map PP.text (PPT.typeFormals n)))

(* fmtStr : (str : M.Structure) -> PP.format *)
fun fmtStr str =
    (case str
      of M.STR{access,rlzn={rpath,...},...} =>
	 PP.ccat
	   (fmtInvPath rpath,
	    PP.brackets (PPV.fmtAccess access))
       | M.STRSIG _ => PP.text "SIGSTR"
       | M.ERRORstr => PP.text "ERRORstr")

(* fmtFct : (fct : M.Functor) -> PP.format *)
fun fmtFct fct =
    (case fct
      of M.FCT{access,rlzn={rpath,...},...} =>
	   PP.ccat (fmtInvPath rpath, PP.brackets (PPV.fmtAccess access))
       | M.ERRORfct => PP.text "ERRORfct")

(* fmtPat : SE.staticEnv -> AS.pat * int -> PP.format *)
fun fmtPat env (pat, depth) =
        (* fmtPat' : AS.pat * int * int * int -> PP.format
	 *   fmtPat' (pat, lpull, rpull, depth) *)
    let fun fmtPat' (pat, _, _, 0) = PP.text (patTag pat)
	  | fmtPat' (VARpat v, _, _, _) = PV.fmtVar v
	  | fmtPat' (WILDpat, _, _, _) = PP.text "_"
          | fmtPat' (NUMpat(src, _), _, _, _) = PP.text src
	  | fmtPat' (STRINGpat s, _, _, _) = PP.string s
	  | fmtPat' (CHARpat c, _, _, _) = PP.char c
	  | fmtPat' (LAYEREDpat (v,p), lpull, rpull, d) =
	      (* v should be VARpat, so lpull cannot disrupt it *)
	      if lpull > 0 orelse rpull > 0
	      then (* have to parenthsize to hold on to args *)
		  let val leftFmt = fmtPat' (v, 0, 0, d-1)
		      val rightFmt = fmtPat' (rightPat, 0, 0, d-1)
		  in PP.parens (PP.pblock [leftFmt, PP.text "as", rightFmt])
		  end
	      else (* lpull = rpull = 0; can hold both args against outer pulls *)
		  let val leftFmt = fmtPat' (v, lpull, 0, d-1)
		      val rightFmt = fmtPat' (rightPat, 0, rpull, d-1)
		  in PP.pblock [leftFmt, PP.text "as", rightFmt]
		  end
	  | fmtPat' (RECORDpat{fields=[],flex,...},_) =
	      (* Special case 0 length record pats to avoid {,...} *)
	      if flex then PP.text "{...}" else PP.text "()"
	  | fmtPat' (r as RECORDpat{fields, flex, ...},d) =
	      let fun fmtField (sym, pat) =
		      PP.pcat (PP.hcat (PPU.fmtSym sym, PP.text "="),
			       fmtPat' (pat, 0, 0, d-1))
	      in if isTUPLEpat r  (* implies not flex *)
		 then PP.parens 
		        (PP.sequence {alignment = PP.P, sep = PP.comma} 
		           (map (fn (sym,pat) => fmtPat' (pat, 0, 0, d-1)) fields))
		 else PP.braces
			(PP.sequence {alignment = PP.P, sep = PP.comma}
			   (map fmtField fields @
			    if flex then [PP.text "..."] else nil))
	      end
	  | fmtPat' (VECTORpat (nil, _), d) = PP.text "#[]"
	  | fmtPat' (VECTORpat (pats, _), d) =
	      let fun fmtElem pat = fmtPat'(pat, d-1)
	       in PP.ccat
		    (PP.text "#",
		     (PP.brackets
			(PP.sequence {alignment = PP.P, sep = PP.comma}
			   (map fmtElem pats))))
	      end
	  | fmtPat' (pat as (ORpat _), d) =
	      let fun flattenORs (ORpat (p1, p2)) = p1 :: mkList p2
		      (* assuming p1 not an ORpat, but p2 might be one *)
		    | flattenORs p = [p]
		  fun fmtOne pat = fmtPat' (pat, d-1)
	       in PP.parens
		    (PP.sequence {alignment = PP.P, sep = PP.text " |"}
		      (map fmtOne (flattenORs pat)))
	      end
	  | fmtPat' (CONpat (dcon,_),_) = PPV.fmtDatacon dcon
	  | fmtPat' (APPpat(DATACON{name,...}, _, argPat), lpull, rpull, d) =
	      let fun getArgs pat =
                      (case AU.headStripPat pat
			of RECORDpat{fields=[(_,leftPat),(_,rightPat)],...} =>
			   (* assumes pair elements are in the right order in the RECORDpat *)
		           (leftPat, rightPat)
			 | _ => bug "getArgs")
	       in case lookFIX(env,name)
		    of INfix (left, right) => 
			 let val (leftArg, rightArg) = getArgs argPat
			     val appFmt =  
				 PP.pblock [fmtPat' (leftArg, lpull, left, d-1),
					    PPU.fmtSym name,
					    fmtPat' (rightArg, right, rpull, d-1)]
			  in if lpull >= left orelse rpull > right
			     then (* have to parenthsize to hold on to args *)
			       let val leftFmt = fmtPat' (leftPat, 0, left, d-1)
				   val rightFmt = fmtPat' (rightPat, right, 0, d-1)
				in PP.parens (PP.pblock [leftFmt, PPU.fmtSym name, rightFmt])
			       end
			     else (* can hold both args against outer pulls *)
			       let val leftFmt = fmtPat' (leftPat, lpull, left, d-1)
				   val rightFmt = fmtPat' (rightPat, right, rpull, d-1)
				in PP.pblock [leftFmt, PPU.fmtSym name, rightFmt]
			       end
			 end
		     | NONfix =>
		        (PPU.fmtSym name, fmtPat' (argPat, 1000, rpull, d-1));
		  rpcond(atom);
		  closeBox ()
	      end
	  | fmtPat' (CONSTRAINTpat (pat,t), lpull, rpull, d) =
	      let val patFmt = fmtPat' (pat, 0, 0, d-1)
		  val typFmt = PPT.fmtType env t
	      in PP.parens (PP.pblock [patFmt, PP.colon, typFmt])
	      end
          | fmtPat' (MARKpat (pat, region), lpull, rpull d) =
	      fmtPat' (pat, lpull, rpull, d)
	  | fmtPat' _ = bug "fmtPat'"
     in fmtPat' (pat, 0, 0, depth)
    end (* fun fmtPat *)

fun mkAtomic (lpull : int, rpull : int, fmt) =
    if lpull > 0 orelse rpull > 0 then PP.parens fmt else fmt

(* ppExp : context -> AS.exp * int -> PP.format *)
fun ppExp (context as (env,source_opt)) (exp : AS.exp, depth : int)  =
    let fun fmtRule (RULE (pat,exp), d) : PP.format =
	    if d <= 0 then PP.text "<rule>"
	    else PP.pblock
		   [PP.ccat (fmtPat env (pat, d), PP.text "=>"),
		    PP.softIndent (fmtExp (exp, 0, 0, d), 4)]
	fun fmtMatch (lead: string, rules, depth) =
	    PPU.fmtVerticalFormats {header1 = lead, header2 = StringCVT.padLeft #" " (size lead - 1) "|"}
              (map fmtRule rules)
	fun fmtSRule (SRULE (con, _, exp), d) : PP.format =
	    if d <= 0 then PP.text "<srule>"
	    else PP.pblock
		   [PP.ccat (PPV.fmtDcon con, PP.text "=>"),
		    PP.softIndent (fmtExp (exp, 0, 0, d), 4)]
	fun fmtSMatch (rules, defaultOp, depth) =
	    PPU.fmtVerticalFormats {header1 = "of", header2 = " |"}
              (map fmtSRule rules)
        (* fmtExp' : AS.exp * int * int * int -> PP.format
         *   fmtExp' (exp, lpull, rpull, depth) *)
	fun fmtExp' (exp, _, _, 0) = PP.text (expTag exp)
	  | fmtExp' (VARexp(ref var,_), _, _, ,_) = PPV.fmtVar var
	  | fmtExp' (CONexp(con,_), _, _, _) = PPV.fmtDcon con
          | fmtExp' (NUMexp(src, _), _, _, _) = PP.text src
	  | fmtExp' (REALexp(src, _), _, _, _) = PP.text src
	  | fmtExp' (STRINGexp s, _, _, _) = PP.string s
	  | fmtExp' (CHARexp c,_,_) = PP.char c
	  | fmtExp' (r as RECORDexp fields,_,d) =
	      let fun fmtField (sym, exp) =
		      PP.pcat (PP.hcat (PPU.fmtSym sym, PP.text "="),
			       fmtExp' (exp, 0, 0, d-1))
	       in if isTUPLEpat r  (* implies not flex *)
		  then PP.parens 
		         (PP.sequence {alignment = PP.P, sep = PP.comma} 
		            (map (fn (sym,exp) => fmtExp' (pat, 0, 0, d-1)) fields))
		  else PP.braces
			 (PP.sequence {alignment = PP.P, sep = PP.comma}
			    (map fmtField fields))
	      end
	  | fmtExp' (RSELECTexp (exp, index), atom, d) =
	      let val expFmt0 = fmtExp' (exp, 0, 0, d-1)
		  val expFmt = if rpull > 0 then PP.parens expFmt0 else expFmt0
		  val selectorFmt = PP.ccat (PP.text "#", PP.integer index)
	       in PP.hcat (selectorFmt, vexpFmt)
	      end
	  | fmtExp' (VSELECTexp (exp, _, index), _, rpull, d) =
	      let val vexpFmt0 = fmtExp' (exp, 0, 0, d-1)
		  val vexpFmt = if rpull > 0 then PP.parens vexpFmt0 else vexpFmt0
		  val selectorFmt = PP.ccat (PP.text "##", PP.integer index)
	       in PP.hcat (selectorFmt, vexpFmt)
	      end
	  | fmtExp' (VECTORexp(nil,_), _, _, _) = PP.text "#[]"
	  | fmtExp' (VECTORexp(exps,_), _, _, d) =
	      let fun fmtElem exp = fmtExp' (exp, 0, 0, d-1)
	       in PP.ccat
		    (PP.text "#",
		     (PP.brackets
			(PP.sequence {alignment = PP.P, sep = PP.comma}
			   (map fmtElem exps))))
	      end
	  | fmtExp' (SEQexp exps,_,d) =
	      let fun fmtElem exp = fmtExp' (exp, 0, 0, d-1)
	       in PP.parens
		    (PP.sequence {alignment = PP.p, sep = PP.semicolon}
		       (map fmtElem exps))
	      end
	  | fmtExp' (APPexp (rator, rand), lpull, rpull, d) =
	      (* FIX from ppAppExp *)
	      let val pathOp = 
		    (case stripMark rator
		       of CONexp(DATACON{name,...},_) => SOME [name]
		        | VARexp(varRef,_) =>
			    (case !varRef
			       of VALvar{path=SymPath.SPATH path', access, ...} =>
					  SOME path'
				| OVLDvar{name,...} => SOME [name]
				| ERRORvar => NONE)
			| _ => NONE)
		   val fixity = 
		         case pathOp
			   of SOME [name] => lookFIX (env, name)
		            | _ => NONfix
	       in case fixity
		    of NONfix =>
			 PP.hcat (fmtExp' (rator, lpull, 1000, d-1),
				  fmtExp' (rand, 1000, rpull, d-1))
		     | INfix (left, right) => 
		       let val SOME [name] = pathOp
			   fun getArgs pat =
			       (case AU.headStripPat pat
				 of RECORDexp{fields=[(_,leftArg),(_,rightArg)],...} =>
			   (* assumes pair elements are in the right order in the RECORDpat,
			    * hence not checking field labels *)
				    (leftArg, rightArg)
				  | _ => bug "getArgs")
			   val (leftArg, rightArg) = getArgs argPat
			   val appFmt =  
			         PP.pblock [fmtExp' (leftArg, lpull, left, d-1),
					    PPU.fmtSym name,
					    fmtPat' (rightArg, right, rpull, d-1)]
		       in if lpull >= left orelse rpull > right
			  then (* have to parenthsize to hold on to args *)
			    let val leftFmt = fmtExp' (leftArg, 0, left, d-1)
				val rightFmt = fmtExp' (rightArg, right, 0, d-1)
			     in PP.parens (PP.pblock [leftFmt, PPU.fmtSym name, rightFmt])
			    end
			  else (* can hold both args against outer pulls *)
			    let val leftFmt = fmtExp' (leftArg, lpull, left, d-1)
				val rightFmt = fmtExp' (rightArg, right, rpull, d-1)
			     in PP.pblock [leftFmt, PPU.fmtSym name, rightFmt]
			    end
		       end
	      end

	  | fmtExp' (CONSTRAINTexp(e, t), lpull, rpull, d) =
	      PP.parens (PP.pblock [fmtExp'(e, 0, 0, d), PP.colon, ppType env t])
	  | fmtExp' (HANDLEexp(exp, (rules,_,_)), lpull, rpull, d) =
	      PP.parens
	        (PP.vblock
	           [fmtExp' (exp, 0, 0, d-1),
		    hardIndent (fmtMatch ("handle", rules, d), 2)])
	  | Fmtexp' (RAISEexp (exp,_), lpull, rpull, d) =
	      mkAtomic (lpull, rpull,
			PP.hcat (PP.text "raise", fmtExp' (exp, 0, 0, d-1)))
	  | fmtExp' (LETexp(dec, exp), _, _, d) =
	      PP.hvblock
		[PP.hcat (PP.text "let", fmtDec context (dec, d-1)),
		 PP.hcat (PP.text " in", fmtExp' (exp, 0, 0, d-1)),
		 PP.text "end"]
	  | fmtExp' (LETVexp(var, defexp, bodyexp), _, _, d) =
	      let val dec = VALdec [VB{pat=VARpat var, exp = defexp,
				       typ = Types.UNDEFty, boundtvs = nil,
				       tyvars = ref []}]
	       in PP.hvblock
		    [PP.hcat (PP.text "let", fmtDec context (dec, d-1)),
		     PP.hcat (PP.text " in", fmtExp' (exp, 0, 0, d-1)),
		     PP.text "end"]
	      end
	  | fmtExp' (CASEexp(exp, rules), _, d) =
	      PP.parens
	        (PP.vblock
	           [PP.hcat (PP.text "case", fmtExp' (exp, 0, 0, d-1)),
		    hardIndent (fmtMatch ("of", rules, d), 2)])
	  | fmtExp' (IFexp { test, thenCase, elseCase }, lpull, rpull, d) =
	      mkAtomic (lpull, rpull, 
		PP.hvblock
		  [PP.hcat (PP.text "if", fmtExp' (test, 0, 0, d-1)),
		   nPP.hcat (PP.text "then", fmtExp' (thenCase, 0, 0, d-1)),
		   PP.hcat (PP.text "else", fmtExp' (elseCase, 0, ?, d-1))])
	  | fmtExp' (ANDALSOexp (e1, e2), lpull, rpull, d) =
	      let val opFmt = PP.text "andalso"
	       in if lpull > 4 orelse rpull > 5
		  then PP.parens
			 (PP.pblock [fmtExp' (e1, 0, 4, d-1),
				     opFmt,
				     fmtExp' (e2, 5, 0, d-1)])
		  else (PP.pblock [fmtExp' (e1, lpull, 4, d-1),
				   opFmat,
				   fmtExp' (e2, 5, rpull, d-1)])
	      end
	  | fmtExp' (ORELSEexp (e1, e2), lpull, rpull, d) =
	      let val opFmt = PP.text "orelse"
	       in if lpull > 2 orelse rpull > 3
		  then PP.parens
			 (PP.pblock [fmtExp' (e1, 0, 2, d-1),
				     opFmt,
				     fmtExp' (e2, 3, 0, d-1)])
		  else (PP.pblock [fmtExp' (e1, lpull, 2, d-1),
				   opFmat,
				   fmtExp' (e2, 3, rpull, d-1)])
	      end
	  | fmtExp' (WHILEexp { test, expr }, lpull, rpull, d) =
              PP.pcat
	        (PP.hcat (PP.text "while", fmtExp'(test, 0, 0, d-1)),
	         PP.hcat (PP.text "do", fmtExp' (expr, 0, rpull, d-1)))
	  | fmtExp' (FNexp(rules, _, _), _, d) =
	      PP.parens (fmtMatch ("fn", rules, d), 2))
	  | fmtExp' (MARKexp (exp, (s,e)), lpull, rpull, d) =
	      (case source_opt
		of SOME source =>
		     if !internals
		     then PP.enclose {front = PP.text "<", back = PP.text ">"}
			    (PP.pblock
			       [PP.text "MARK",
				PP.parens
				   (PP.concat [fmtPos (source,s), PP.comma,
					       fmtPos (source,e)]),
				PP.colon,
				fmtExp' (exp, 0, 0, d)])
		     else fmtExp'(exp, lpull, rpull, d)
	         | NONE => fmtExp'(exp, lpull, rpull, d))
	  | fmtExp' (SWITCHexp (exp, srules, defaultOp), _, _, d) =
	      PP.vcat
	        (PP.hcat (PP.text "SWITCH", fmtExp' (exp, 0, 0, d-1)),
	         hardIndent (fmtSMatch (srules, defaultOp, d-1), 2)
	  | fmtExp' (VSWITCHexp (exp, _, srules, default), _, _, d) =
	      PP.vcat
	        (PP.hcat (PP.text "VSWITCH", fmtExp' (exp, 0, 0, d-1)),
	         hardIndent (fmtSMatch (srules, SOME default, d-1), 2)
          (* end fmtExp' *)

     in fmtExp' (exp, 0, 0, depth)
    end (* end fmtExp *)

and fmtVB (context as (env,source_opt)) (VB{pat,exp,...},d) =
    if d <= 0 then PP.text "<VB>"
    else PP.pblock [fmtPat env (pat, d-1), PP.text "=", fmtExp context (exp,d-1)]

and fmtRVB context (RVB{var, exp, ...},d) =
    if d <= 0 then PP.text "<RVB>"
    then PP.pblock [PPV.fmtVar var, PP.text "=", ppExp context (exp,d-1)]

and fmtVARSEL (var1, var2, index) =
    PP.hblock
      [PP.text "val", PPV.fmtVar var1, PP.text " = #", PP.integer index, PPV.fmtVar var2]


and fmtDec (context as (env,source_opt)) (dec, depth) =
    let fun ppDec' (dec, 0) = PP.text (decTag dec)  (* "<dec>" *)
          | ppDec' (VALdec vbs, d) =
	      PPU.fmtVerticalFormats {header1 = "val", header2 = "and"}
	        (map (fn vb => ppVB context (vb,d-1)) vbs)
          | ppDec'(VALRECdec rvbs,d) =
	      PPU.fmtVerticalFormats {header1 = "val rec", header2 = "and"}
	        (map (fn rvb => ppRVB context (rvb,d-1)),rvbs)
	  | ppDec'(DOdec exp, d) =
	      PP.hcat (PP.text "do", ppExp context (exp,d-1))
          | ppDec'(TYPEdec tycs,d) =
	    let fun fmtDEFtyc (DEFtyc{path, tyfun=TYFUN{arity,body},...}) =
		    PP.hblock [fmtTyFormals arity, PPU.fmtSym (InvPath.last path),
			       PP.text "=", ppType env body]
		  | fmtDEFtyc _ = bug "fmtDEFtyc"
	     in PPU.fmtVerticalFormats {header1 = "type", header2 = "and"}
 	          (map fmtDEFtyc tycs)
	    end
          | ppDec'(DATATYPEdec{datatycs,withtycs},d) =
	    let fun ppDATA (GENtyc { path, arity, kind, ... }) =
		  (case kind
		     of DATATYPE(_) =>
			fmtTyFormals arity,
			PU.ppSym (InvPath.last path),
			PP.text "=", 

		        PU.ppSequence ppstrm
			{sep= PP.text " |",
			 pr=(fn => fn (DATACON{name,...}) =>
					     PU.ppSym name),
			 style=PU.INCONSISTENT}
			dcons*))
		     | _ => bug "ppDec'(DATATYPEdec) 1.1")
		  | ppDATA _ _ = bug "ppDec'(DATATYPEdec) 1.2"
		fun fmtWITHTYPE (DEFtyc{path, tyfun=TYFUN{arity,body},...}) =
		  (fmtTyFormals arity,
		   PU.ppSym (InvPath.last path);
		   pps "="; ppType env body)
		| ppWITH _ _ = bug "ppDec'(DATATYPEdec) 2"
	    in
	      (* could call PPDec.ppDec here *)
	      openHVBox 0;
	      PU.ppvlist ("datatype ","and ", ppDATA, datatycs);
	      PP.newline ppstrm;
	      PU.ppvlist ("withtype ","and ", ppWITH, withtycs);
	      closeBox ()
	    end
        | ppDec'(ABSTYPEdec _,d) = pps "ppDec'[ABSTYPEdec]"

        | ppDec'(EXCEPTIONdec ebs,d) =
	    let fun f (EBgen{exn=DATACON{name,...}, etype}) =
		      (PU.ppSym name;
		       case etype
			of NONE => ()
			 | SOME ty' => (pps " of "; ppType env ty'))
		  | f (EBdef{exn=DATACON{name,...},
				    edef=DATACON{name=dname,...}}) =
		      (PU.ppSym name; pps "="; PU.ppSym dname)
	     in openHVBox 0;
	        PU.ppvlist ("exception ","and ", f, ebs);
	        closeBox ()
	    end
        | ppDec'(STRdec sbs,d) = let
	      fun f (STRB{name, str=M.STR { access, ... }, def}) =
		  (PU.ppSym name;
		   PV.ppAccess access;
		   pps " = ";
		   PP.break {nsp=1,offset=2};
		   ppStrexp context (def,d-1))
		| f _ _ = bug "ppDec:STRdec:STRB"
	  in
	      openHVBox 0;
	      PU.ppvlist ("structure ","and ", f, sbs);
	      closeBox ()
	  end
        | ppDec'(FCTdec fbs,d) = let
	      fun f (FCTB{name=fname, fct=M.FCT { access, ... }, def}) =
                  (PU.ppSym fname;
		   PV.ppAccess access;
		   pps " = ";
		   PP.break {nsp=1,offset= 2};
		   ppFctexp context (def,d-1))
		| f _ _ = bug "ppDec':FCTdec"
	  in
	      openHVBox 0;
	      PU.ppvlist ("functor ","and ", f, fbs);
              closeBox ()
	  end
        | ppDec'(SIGdec sigvars,d) = let
	      fun f (M.SIG { name, ... }) =
		  (pps "signature ";
		   case name of
		       SOME s => PU.ppSym s
                     | NONE => pps "ANONYMOUS")
		| f _ _ = bug "ppDec':SIGdec"
	  in
	      openHVBox 0;
	      PU.ppSequence {sep=PP.newline, pr=f,
				 style=PU.CONSISTENT} sigvars;
	      closeBox ()
	  end
        | ppDec'(FSIGdec sigvars,d) = let
	      fun f (M.FSIG{kind, ...}) =
		  (pps "funsig ";
                   case kind of SOME s => PU.ppSym s
                              | NONE => pps "ANONYMOUS")
		| f _ _ = bug "ppDec':FSIGdec"
	  in
	      openHVBox 0;
	      PU.ppSequence ppstrm
			 {sep=PP.newline, pr = f, style = PU.CONSISTENT} sigvars;
	      closeBox ()
	  end
        | ppDec'(LOCALdec(inner,outer),d) =
	  (openHVBox 0;
	   pps "local"; PU.nl_indent 2;
	   ppDec'(inner,d-1); PP.newline ppstrm;
	   pps "in ";
	   ppDec'(outer,d-1); PP.newline ppstrm;
	   pps "end";
	   closeBox ())
        | ppDec'(SEQdec decs,d) =
	  (openHVBox 0;
	   PU.ppSequence ppstrm
	     {sep=PP.newline,
	      pr=(fn => fn dec => ppDec'(dec,d)),
	      style=PU.CONSISTENT}
	     decs;
	   closeBox ())
        | ppDec'(FIXdec {fixity,ops},d) =
	  (openHVBox 0;
	   case fixity
	     of NONfix => pps "nonfix "
	      | INfix (i,_) =>
		    (if i mod 2 = 0 then
		       pps "infix "
		     else pps "infixr ";
		     if i div 2 > 0 then
		       (pps(Int.toString(i div 2));
			pps " ")
		     else ());
	   PU.ppSequence ppstrm
	     {sep=(fn => PP.break {nsp=1,offset=0}),
	      pr=PU.ppSym,style=PU.INCONSISTENT}
	     ops;
	   closeBox ())

        | ppDec'(OVLDdec ovldvar,d) =
	  (pps "overload "; PV.ppVar ovldvar)

        | ppDec'(OPENdec strbs,d) =
	  (openHVBox 0;
	   pps "open ";
	   PU.ppSequence ppstrm
	     {sep=(fn => PP.break {nsp=1,offset=0}),
	      pr=(fn => fn (sp,_) =>
                        pps (SymPath.toString sp)),
	      style=PU.INCONSISTENT}
            strbs;
	   closeBox ())

        | ppDec'(MARKdec(dec,(s,e)),d) =
	  (case source_opt
	    of SOME source =>
	       (pps "MARKdec(";
		ppDec'(dec,d); pps ",";
		prpos(ppstrm,source,s); pps ",";
		prpos(ppstrm,source,e); pps ")")
	     | NONE => ppDec'(dec,d))

        | ppDec' (VARSELdec (v1,v2,i), _) =
	  ppVARSEL (v1,v2,i)

     in ppDec'
    end

and ppStrexp (context as (statenv,source_opt)) =
    let val pps = PP.text

      fun ppStrexp'(_,0) = pps "<strexp>"

	| ppStrexp'(VARstr str, d) = (ppStr str)

	| ppStrexp'(APPstr{oper, arg, ...}, d) =
	  (ppFct oper; pps"("; ppStr arg; pps")")
        | ppStrexp'(STRstr bindings, d) =
              (PP.openVBox (PP.Abs 0);
	       pps "struct";
	       PU.ppvseq 2 ""
		 (fn => fn binding =>
		     PPModules.ppBinding statenv
			(Bindings.bindingSymbol binding, binding, d-1))
		 bindings;
	       pps "end";
               PP.closeBox ppstrm)
	| ppStrexp'(LETstr(dec,body),d) =
	      (PP.openHVBox (PP.Abs 0);
	       pps "let "; ppDec context (dec,d-1);
               PP.cut ppstrm;
	       pps " in "; ppStrexp'(body,d-1);
	       PP.cut ppstrm;
	       pps "end";
	       PP.closeBox ppstrm)
        | ppStrexp'(MARKstr(body,(s,e)),d) =
	      (case source_opt
		of SOME source =>
	           (pps "MARKstr(";
		    ppStrexp'(body,d); pps ",";
		    prpos(ppstrm,source,s); pps ",";
		    prpos(ppstrm,source,e); pps ")")
	         | NONE => ppStrexp'(body,d))

   in ppStrexp'
  end

and ppFctexp (context as (_,source_opt)) =
  let val pps = PP.text

      fun ppFctexp'(_, 0) = pps "<fctexp>"

        | ppFctexp'(VARfct fct, d) = ppFct fct

        | ppFctexp'(FCTfct{param, def, ...}, d) =
            (pps "FCT(";
	     ppStr param;
	     pps ") => ";
	     PP.openHVBox (PP.Abs 2);
	       PP.cut ppstrm;
 	       ppStrexp context (def,d-1);
	     PP.closeBox ppstrm)

        | ppFctexp'(LETfct(dec,body),d) =
	    (PP.openHVBox (PP.Abs 0);
	     pps "let "; ppDec context (dec,d-1);
             PP.cut ppstrm;
	     pps " in "; ppFctexp'(body,d-1);
	     PP.cut ppstrm;
	     pps "end";
	     PP.closeBox ppstrm)

	| ppFctexp'(MARKfct(body,(s,e)),d) =
	    (case source_opt
	      of SOME source =>
	           (pps "MARKfct(";
		    ppFctexp'(body,d); pps ",";
		    prpos(ppstrm,source,s); pps ",";
		    prpos(ppstrm,source,e); pps ")")
               | NONE => ppFctexp'(body,d))

   in ppFctexp'
  end

end (* top-level local *)
end (* structure PPAbsyn *)
