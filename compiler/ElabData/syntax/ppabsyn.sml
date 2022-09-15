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

(* fmtSymPath : SymPath.path -> PP.format *)
fun fmtSymPath (path : SymPath.path) = PP.text (SymPath.toString path)

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
		   [PP.ccat (PPV.fmtDatacon con, PP.text "=>"),
		    PP.softIndent (fmtExp (exp, 0, 0, d), 4)]
	fun fmtSMatch (rules, defaultOp, depth) =
	    PPU.fmtVerticalFormats {header1 = "of", header2 = " |"}
              (map fmtSRule rules)
        (* fmtExp' : AS.exp * int * int * int -> PP.format
         *   fmtExp' (exp, lpull, rpull, depth) *)
	fun fmtExp' (exp, _, _, 0) = PP.text (expTag exp)
	  | fmtExp' (VARexp(ref var,_), _, _, ,_) = PPV.fmtVar var
	  | fmtExp' (CONexp(con,_), _, _, _) = PPV.fmtDatacon con
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
    let fun fmtDec' (dec, 0) = PP.text (decTag dec)  (* "<dec>" *)
          | fmtDec' (VALdec vbs, d) =
	      PPU.fmtVerticalFormats {header1 = "val", header2 = "and"}
	        (map (fn vb => ppVB context (vb,d-1)) vbs)
          | fmtDec'(VALRECdec rvbs,d) =
	      PPU.fmtVerticalFormats {header1 = "val rec", header2 = "and"}
	        (map (fn rvb => ppRVB context (rvb,d-1)),rvbs)
	  | fmtDec'(DOdec exp, d) =
	      PP.hcat (PP.text "do", ppExp context (exp,d-1))
          | fmtDec'(TYPEdec tycs,d) =
	    let fun fmtDEFtyc (DEFtyc{path, tyfun=TYFUN{arity,body},...}) =
		    PP.hblock [PPT.fmtFormals arity, PPU.fmtSym (InvPath.last path),
			       PP.text "=", ppType env body]
		  | fmtDEFtyc _ = bug "fmtDEFtyc"
	     in PPU.fmtVerticalFormats {header1 = "type", header2 = "and"}
 	          (map fmtDEFtyc tycs)
	    end
          | fmtDec'(DATATYPEdec{datatycs,withtycs},d) =
	    let fun fmtDATA (GENtyc { path, arity, kind, ... }) =
		      (case kind
			 of DATATYPE({index, family={members,...}, ...}) =>
			    let val {dcons, ...} = Vector.sub (members, index)
				val dconNames = map #name dcons
			     in PP.hblock
				  [PPT.fmtFormals arity,
				   PPU.ppSym (InvPath.last path),
				   PP.text "=", 
				   PP.sequence {alignment = PP.P, sep = PP.text " |"}
				     (map PPU.fmtSym dconNames)]
			  | _ => bug "fmtDec'(DATATYPEdec) 1")
		  | fmtDATA _ = bug "fmtDec'(DATATYPEdec) 2"
		fun fmtWITHTYPE (DEFtyc{path, tyfun=TYFUN{arity,body},...}) =
		    PP.hblock
		      [PPT.fmtFormals arity,
		       PPU.fmtSym (InvPath.last path),
		       PP.text "=", fmtType env body]
		  | fmtWITHTYPE _ = bug "fmtDec'(DATATYPEdec) 3"
	     in (* could call PPDec.ppDec here *)
	        PP.vcat
		  (PPU.fmtVerticalFormats {header1 = "datatype", header2 = "and"}
                     (map fmtDATA datatycs),
	           PPU.fmtVerticalFormats {header1 = "withtype", header2 = "and"}
                     (map ppWITH withtycs))
	    end
        | fmtDec'(ABSTYPEdec _, _) = PP.text "<ABSTYPEdec>"
        | fmtDec'(EXCEPTIONdec ebs,d) =
	    let fun fmtEB (EBgen{exn=DATACON{name,...}, etype}) =
		      PP.hcat
			(PPV.fmtSym name,
			 case etype
			  of NONE => PP.empty
			   | SOME ty' => PP.hcat (PP.text "of", ppType env ty'))
		  | fmtEB (EBdef{exn=DATACON{name,...}, edef=DATACON{name=dname,...}}) =
		      PP.hblock [PPU.fmtSym name, PP.text "=", PPU.fmtSym dname]
	     in PPU.fmtVerticalFormats {header1 = "exception", header2 = "and"} (map fmtEB ebs)
	    end
        | fmtDec' (STRdec sbs,d) =
	    let fun fmtSTRB (STRB{name, str=M.STR { access, ... }, def}) =
		    PP.pcat (PP.hblock [PPU.fmtSym name, PPV.fmtAccess access, PP.equal],
			     fmtStrexp context (def, d-1))
		  | fmtSTRB _ = bug "ppDec:STRdec:STRB"
	     in PPU.fmtVerticalFormats {header1 = "structure", header2 = "and"} (map fmtSTRB sbs)
	    end
        | fmtDec' (FCTdec fbs,d) =
	    let fun fmtFCTB (FCTB{name=fname, fct=M.FCT { access, ... }, def}) =
                      PP.pcat (PP.hblock [PPU.fmtSym fname, PPV.fmtAccess access, PP.equal],
			       ppFctexp context (def,d-1))
		  | fmtFCTB _ = bug "fmtDec':FCTdec"
	     in PPU.fmtVerticalFormats {header1 = "functor", header2 = "and"} (map fmtFCTB fbs)
	    end
        | fmtDec' (SIGdec sigvars, d) =
	    let fun fmtSIG (M.SIG { name, ... }) =
		    PP.hcat
		      (PP.text "signature",
		       case name
			 of SOME s => PPU.fmtSym s
			  | NONE => PP.text "ANONYMOUS")
		  | fmtSIG _ = bug "fmtDec':SIGdec"
	     in PP.vblock (map fmtSIG sigvars)
	    end
        | fmtDec' (FSIGdec sigvars, d) =
	    let fun fmtFSIG (M.FSIG{kind, ...}) =
		    PP.hcat
		      (PP.text "funsig",
                       case kind
			 of SOME s => PPU.fmtSym s
                          | NONE => PP.text "ANONYMOUS")
		  | fmtFSIG _ = bug "fmtDec':FSIGdec"
	     in PP.vblock (map fmtFSIG sigvars)
	    end
        | fmtDec' (LOCALdec(inner,outer), d) =
	    PP.vblock
	      [PP.text "local",
	       PP.hardIndent (fmtDec' (inner, d-1), 2)
	       PP.text "in",
	       PP.hardIndent (fmtDec' (outer, d-1), 2),
	       PP.text "end"]
        | fmtDec' (SEQdec decs, d) =
	    PP.vblock (map (fn dec => fmtDec' (dec, d)) decs)
        | fmtDec' (FIXdec {fixity,ops},d) =
	    PP.hcat
	      (case fixity
	         of NONfix => PP.text "nonfix"
	          | INfix (i,_) =>
		    PP.hcat
		      (PP.text (if i mod 2 = 0 then "infix" else "infixr"),
		       if i div 2 > 0 then (PP.integer (i div 2)) else PP.empty),
	       PP.hblock (map PPU.fmtSym ops))

        | fmtDec' (OVLDdec ovldvar, _) =
	    PP.hcat (PP.text "overload ", PPV.fmtVar ovldvar)

        | fmtDec' (OPENdec strbs, _) =
	   PP.hblock
	     (PP.text "open" :: map (fn (sp,_) => fmtSymPath sp) strbs)

        | fmtDec' (MARKdec (dec, (s,e)), d) = fmtDec' (dec, d)
(*
	  (case source_opt
	    of SOME source =>  (* ??? *)
	         PP.text "MARKdec"
		  (PP.parens
		     (PP.pblock
		        [fmtDec'(dec,d), PP.comma,
			 fmtPos (source, s), PP.comma,
			 fmtPos (source, e)]))
	     | NONE => fmtDec' (dec, d))
*)
        | fmtDec' (VARSELdec (v1,v2,i), _) =
	    fmtVARSEL (v1,v2,i)

     in fmtDec' (dec, depth)
    end

and ppStrexp (context as (statenv,source_opt)) =
    let fun ppStrexp' (_,0) = PP.text "<strexp>"

	  | ppStrexp' (VARstr str, _) = fmtStr str

	  | ppStrexp' (APPstr{oper, arg, ...}, _) =
	      PP.hcat (fmtFct oper, PP.parens (fmtStr arg))

          | ppStrexp' (STRstr bindings, d) =
              PP.vblock
	        [PP.text "struct",
	         PP.sequence {alignment = PP.V, sep = PP.empty}
		   (map (fn binding =>
		            PPModules.fmtBinding statenv
			      (Bindings.bindingSymbol binding, binding, d-1))
			bindings),
		 PP.text "end"]
		
	   | ppStrexp' (LETstr(dec,body),d) =
	       PP.vblock
	         [PP.hcat (PP.text "let ", ppDec context (dec,d-1)),
	          PP.hcat (PP.text "in",  ppStrexp'(body,d-1)),
		  PP.text "end"]

           | ppStrexp' (MARKstr(body,(s,e)),d) = ppStrexp' (body, d)
(*
	      (case source_opt
		of SOME source =>
		     PP.hblock
	               [PP.text "MARKstr",
			PP.ccat (fmtStrexp' (body, d), PP.comma),
			PP.ccat (fmtPos (source,s), PP.comma),
			fmtPos (source,e)]
	         | NONE => ppStrexp' (body,d))
*)
     in ppStrexp'
    end

and ppFctexp (context as (_,source_opt)) =
  let val pps = PP.text

      fun ppFctexp' (_, 0) = PP.text "<fctexp>"

        | ppFctexp' (VARfct fct, d) = fmtFct fct

        | ppFctexp' (FCTfct {param, def, ...}, d) =
	    PP.vcat
              (PP.hblock
	         [PP.text "FCT",
	          PP.parens (fmtStr param),
	          PP.text "=>"],
 	       ppStrexp context (def,d-1))

        | ppFctexp' (LETfct(dec,body),d) =
            PP.vblock
	      [PP.hcat (PP.text "let", ppDec context (dec, d-1)),
  	       PP.hcat (PP.text "in", ppFctexp' (body, d-1)),
	       PP.text "end"]

	| ppFctexp' (MARKfct(body,(s,e)),d) = ppFctexp' (body, d)
(*
	    (case source_opt
	      of SOME source =>
	           (pps "MARKfct(";
		    ppFctexp'(body,d); pps ",";
		    prpos(ppstrm,source,s); pps ",";
		    prpos(ppstrm,source,e); pps ")")
               | NONE => ppFctexp'(body,d))
*)
   in ppFctexp'
  end

end (* top-level local *)
end (* structure PPAbsyn *)
