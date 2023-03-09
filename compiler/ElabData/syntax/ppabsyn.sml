(* ppabsyn.sml
 *
 * COPYRIGHT (c) 2021 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature PPABSYN =
sig

  val fmtPat  : StaticEnv.staticEnv -> Absyn.pat * int -> Formatting.format
  val fmtExp  : StaticEnv.staticEnv * Source.source option
                -> Absyn.exp * int -> Formatting.format
  val fmtRule : StaticEnv.staticEnv * Source.source option
                -> Absyn.rule * int -> Formatting.format
  val fmtVB   : StaticEnv.staticEnv * Source.source option
                -> Absyn.vb * int -> Formatting.format
  val fmtRVB  : StaticEnv.staticEnv * Source.source option
                -> Absyn.rvb * int -> Formatting.format
  val fmtDec  : StaticEnv.staticEnv * Source.source option
                -> Absyn.dec * int -> Formatting.format

  val fmtStrexp : StaticEnv.staticEnv * Source.source option
                 -> Absyn.strexp * int -> Formatting.format

end (* signature PPABSYN *)


structure PPAbsyn: PPABSYN =
struct

local
  structure EM = ErrorMsg
  structure M = Modules
  structure B = Bindings
  structure S = Symbol
  structure IP = InvPath
  structure F = Fixity
  structure LV = LambdaVar
  structure A = Access
  structure T = Types
  structure V = Variable
  structure AS = Absyn
  structure AU = AbsynUtil
  structure SE = StaticEnv
  structure SR = Source
  structure SM = SourceMap
  structure PP = Formatting
  structure PPS = PPSymbols
  structure PPP = PPSymPaths
  structure PPT = PPType
  structure PPV = PPVal
  structure PPSM = PPSourceMap

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

fun fmtField (sym: S.symbol, fmt: PP.format) =
    PP.pblock [PP.hblock [PPS.fmtSym sym, PP.equal], fmt]

(* fmtPos : Source.source * Source.charpos -> PP.format *)
fun fmtPos({sourceMap, ...}: SR.source, charpos: SR.charpos) =
    if !lineprint then PPSM.fmtLocation (SM.charposToLocation (!sourceMap, charpos))
    else PP.integer charpos

type context = SE.staticEnv * Source.source option

(* lookFIX : SE.staticEnv * S. symbol -> F.fixity *)
fun lookFIX (env : SE.staticEnv, sym: S.symbol) =
    Lookup.lookFix (env, S.fixSymbol (S.name sym))

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
  	   PP.cblock [PPP.fmtInvPath rpath, PP.brackets (PPV.fmtAccess access)]
       | M.STRSIG _ => PP.text "SIGSTR"
       | M.ERRORstr => PP.text "ERRORstr")

(* fmtFct : (fct : M.Functor) -> PP.format *)
fun fmtFct fct =
    (case fct
      of M.FCT{access,rlzn={rpath,...},...} =>
	   PP.cblock [PPP.fmtInvPath rpath, PP.brackets (PPV.fmtAccess access)]
       | M.ERRORfct => PP.text "ERRORfct")

(* fmtPat : SE.staticEnv -> AS.pat * int -> PP.format *)
fun fmtPat env (pat, depth) =
        (* fmtPat' : AS.pat * int * int * int -> PP.format
	 *   fmtPat' (pat, lpull, rpull, depth) *)
    let fun fmtPat' (pat, _, _, 0) = PP.text (patTag pat)
	  | fmtPat' (VARpat v, _, _, _) = PPV.fmtVar v
	  | fmtPat' (WILDpat, _, _, _) = PP.text "_"
          | fmtPat' (NUMpat(src, _), _, _, _) = PP.text src
	  | fmtPat' (STRINGpat s, _, _, _) = PP.string s
	  | fmtPat' (CHARpat c, _, _, _) = PP.char c
	  | fmtPat' (LAYEREDpat (v,p), lpull, rpull, d) =
	      (* v should be VARpat, so lpull cannot disrupt it *)
	      if lpull > 0 orelse rpull > 0
	      then (* have to parenthsize to hold on to args *)
		  let val leftFmt = fmtPat' (v, 0, 0, d-1)
		      val rightFmt = fmtPat' (p, 0, 0, d-1)
		  in PP.parens (PP.pblock [leftFmt, PP.text "as", rightFmt])
		  end
	      else (* lpull = rpull = 0; can hold both args against outer pulls *)
		  let val leftFmt = fmtPat' (v, lpull, 0, d-1)
		      val rightFmt = fmtPat' (p, 0, rpull, d-1)
		  in PP.pblock [leftFmt, PP.text "as", rightFmt]
		  end
	  | fmtPat' (r as RECORDpat {fields, flex, ...}, _, _, d) =
	      (case fields
	         of nil => if flex then PP.text "{...}" else PP.text "()"
		  | _ => 
		     let fun fmtPatField (sym, pat) = fmtField (sym, fmtPat' (pat, 0, 0, d-1))
		      in (case AU.destTuplePat r
			    of SOME pats => (* tuple special case  -- implies not flex *)
			         PP.tuple (map (fn pat => fmtPat' (pat, 0, 0, d-1)) pats)
			     | NONE =>
			         PP.braces (* record *)
				   (PP.psequence PP.comma
				      (map fmtPatField fields @ (if flex then [PP.text "..."] else nil))))
		     end)
	  | fmtPat' (VECTORpat (pats, _), _, _, d) =
	      let fun fmtElem pat = fmtPat'(pat, 0, 0, d-1)
	       in PP.cblock
		    [PP.text "#",
		     PP.brackets (PP.psequence PP.comma (map fmtElem pats))]
	      end
	  | fmtPat' (pat as (ORpat _), _, _, d) =
	      let fun flattenORs (ORpat (p1, p2)) = p1 :: flattenORs p2
		      (* assuming p1 not an ORpat, but p2 might be one *)
		    | flattenORs p = [p]
		  fun fmtfn pat = fmtPat' (pat, 0, 0, d-1)
	       in PP.parens (PP.psequence (PP.text " |") (map fmtfn (flattenORs pat)))
	      end
	  | fmtPat' (CONpat (dcon,_), _, _, _) = PPV.fmtDatacon dcon
	  | fmtPat' (APPpat(T.DATACON{name,...}, _, argPat), lpull, rpull, d) =
	      (case lookFIX (env, name)  (* this may not be correct for the actual datacon _path_ *)
		 of F.INfix (left, right) => 
		      (case AU.destTuplePat (AU.headStripPat argPat)
			 of SOME [leftArg, rightArg] =>  (* argpat is a pair *)
			      let val appFmt =  
				      PP.pblock [fmtPat' (leftArg, lpull, left, d-1),
						 PPS.fmtSym name,
						 fmtPat' (rightArg, right, rpull, d-1)]
			      in if lpull >= left orelse rpull > right
				 then (* have to parenthsize to hold on to args *)
				   let val leftFmt = fmtPat' (leftArg, 0, left, d-1)
				       val rightFmt = fmtPat' (rightArg, right, 0, d-1)
				    in PP.parens (PP.pblock [leftFmt, PPS.fmtSym name, rightFmt])
				   end
				 else (* can hold both args against outer pulls *)
				   let val leftFmt = fmtPat' (leftArg, lpull, left, d-1)
				       val rightFmt = fmtPat' (rightArg, right, rpull, d-1)
				    in PP.pblock [leftFmt, PPS.fmtSym name, rightFmt]
				   end
			      end
			  | SOME _ => bug "AU.destTuplePat returned bad result"
			  | NONE =>  (* argPat is not a tuple pat or does not have two elements *)
			      PP.hblock [PPS.fmtSym name, fmtPat' (argPat, 1000, rpull, d-1)])
		  | F.NONfix =>
		      PP.hblock [PPS.fmtSym name, fmtPat' (argPat, 1000, rpull, d-1)])
	  | fmtPat' (CONSTRAINTpat (pat,t), lpull, rpull, d) =
	      let val patFmt = fmtPat' (pat, 0, 0, d-1)
		  val typFmt = PPT.fmtType env t
	      in PP.parens (PP.pblock [patFmt, PP.colon, typFmt])
	      end
          | fmtPat' (MARKpat (pat, _), lpull, rpull, d) =
	      fmtPat' (pat, lpull, rpull, d)
	  | fmtPat' _ = bug "fmtPat'"
     in fmtPat' (pat, 0, 0, depth)
    end (* fun fmtPat *)

(* mkAtomic : int * int * PP.format -> PP.format *)
fun mkAtomic (lpull : int, rpull : int, fmt) =
    if lpull > 0 orelse rpull > 0 then PP.parens fmt else fmt

val ruleArrow: PP.format = PP.text "=>"

(* fmtCon : AS.con -> PP.format *)
fun fmtCon (DATAcon (dcon, _)) = PPV.fmtDatacon dcon
  | fmtCon (INTcon ({ival, ...})) = PP.text (IntInf.toString ival)
  | fmtCon (WORDcon ({ival, ...})) = PP.text (IntInf.toString ival)
  | fmtCon (STRINGcon s) = PP.string s
  | fmtCon (VLENcon (n, _)) = PP.cblock [PP.text "VL", PP.integer n]

(* ruleFmt : PP.format * PP.format -> PP.format *)
fun ruleFmt (patFmt: PP.format, expFmt: PP.format) =
    PP.pblock [PP.hblock [patFmt, ruleArrow], PP.indent 2 expFmt]

(* fmtExp : context -> AS.exp * int -> PP.format *)
fun fmtExp (context as (env, sourceOp)) (exp : AS.exp, depth : int) =
    let fun fmtRule (RULE (pat,exp), d) : PP.format =
	    if d <= 0 then PP.text "<rule>" else
	    ruleFmt (fmtPat env (pat, d), fmtExp' (exp, 0, 0, d-1))

	and fmtMatch (lead: string, rules: rule list, d: int) : PP.format =
	    if d <= 0 then PP.text "<match>" else
	    PP.vHeaders {header1 = lead, header2 = "|"}
			       (map (fn rule => fmtRule (rule, d-1)) rules)

	and fmtSRule (SRULE (con, _, exp) : srule, d: int) : PP.format =
	    if d <= 0 then PP.text "<srule>" else
	    ruleFmt (fmtCon con, fmtExp' (exp, 0, 0, d-1))

	and fmtSMatch (srules : srule list, defaultOp: exp option, d: int) : PP.format =
	    if d <= 0 then PP.text "<smatch>" else
	    let val d' = d - 1
	     in PP.vHeaders {header1 = "of", header2 = " |"}
                   ((map (fn srule => fmtSRule (srule, d')) srules) @
		    (case defaultOp
		       of NONE => nil
		        | SOME exp => [ruleFmt (PP.text "_", fmtExp' (exp, 0, 0, d'))]))
	    end

        (* fmtExp' : AS.exp * int * int * int -> PP.format
         *   fmtExp' (exp, lpull, rpull, depth) *)
	and fmtExp' (exp, _, _, 0) = PP.text (expTag exp)
	  | fmtExp' (VARexp (ref var, _), _, _, _) = PPV.fmtVar var
	  | fmtExp' (CONexp (con, _), _, _, _) = PPV.fmtDatacon con
          | fmtExp' (NUMexp (src, _), _, _, _) = PP.text src
	  | fmtExp' (REALexp (src, _), _, _, _) = PP.text src
	  | fmtExp' (STRINGexp s, _, _, _) = PP.string s
	  | fmtExp' (CHARexp c, _, _, _) = PP.char c
	  | fmtExp' (r as RECORDexp fields, _, _, d) =
	      let fun fmtExpField (LABEL {name, ...}, exp) = fmtField (name, fmtExp' (exp, 0, 0, d-1))
	       in case AU.destTupleExp r  (* is the record actually a tuple? *)
		    of SOME exps =>  (* tuple special case (implies not flex) *)
		         PP.tuple (map (fn exp => fmtExp' (exp, 0, 0, d-1)) exps)
		     | NONE => PP.braces (PP.psequence PP.comma (map fmtExpField fields))
	      end
	  | fmtExp' (RSELECTexp (exp, index), _, rpull, d) =
	      let val expFmt0 = fmtExp' (exp, 0, 0, d-1)
		  val expFmt = if rpull > 0 then PP.parens expFmt0 else expFmt0  (* ??? *)
		  val selector = PP.cblock [PP.text "#", PP.integer index]
	       in PP.hblock [selector, expFmt]
	      end
	  | fmtExp' (VSELECTexp (exp, _, index), _, rpull, d) =
	      let val vexpFmt0 = fmtExp' (exp, 0, 0, d-1)
		  val vexpFmt = if rpull > 0 then PP.parens vexpFmt0 else vexpFmt0
		  val selector = PP.cblock [PP.text "##", PP.integer index]  (* new "##" 'syntax' *)
	       in PP.hblock [selector, vexpFmt]
	      end
	  | fmtExp' (VECTORexp(exps,_), _, _, d) =
	      let fun fmtElem exp = fmtExp' (exp, 0, 0, d-1)
	       in PP.cblock [PP.text "#",
			   (PP.brackets (PP.psequence PP.comma (map fmtElem exps)))]
	      end
	  | fmtExp' (SEQexp exps, _, _, d) =
	      let fun fmtElem exp = fmtExp' (exp, 0, 0, d-1)
	       in PP.parens (PP.psequence PP.semicolon (map fmtElem exps))
	      end
	  | fmtExp' (APPexp (rator, rand), lpull, rpull, d) =
	      let val pathOp = 
		    (case AU.headStripExp rator
		       of CONexp(T.DATACON{name,...},_) => SOME [name]
		        | VARexp(varRef,_) =>
			    (case !varRef
			       of V.VALvar {path=SymPath.SPATH path', access, ...} =>
					    SOME path'
				| V.OVLDvar {name,...} => SOME [name]
				| V.ERRORvar => NONE)
			| _ => NONE)
		   val fixity = 
		         case pathOp
			   of SOME [name] => lookFIX (env, name)
		            | _ => F.NONfix
	       in case fixity
		    of F.NONfix =>
			 PP.hblock [fmtExp' (rator, lpull, 1000, d-1),
				  fmtExp' (rand, 1000, rpull, d-1)]
		     | F.INfix (left, right) => 
			 let val SOME [name] = pathOp
			  in case AU.destTupleExp (AU.headStripExp rand)
			       of SOME [leftArg, rightArg] =>
				  let val appFmt =  
					  PP.pblock [fmtExp' (leftArg, lpull, left, d-1),
						     PPS.fmtSym name,
						     fmtExp' (rightArg, right, rpull, d-1)]
				  in if lpull >= left orelse rpull > right
				     then (* have to parenthsize to hold on to args *)
				       let val leftFmt = fmtExp' (leftArg, 0, left, d-1)
					   val rightFmt = fmtExp' (rightArg, right, 0, d-1)
					in PP.parens (PP.pblock [leftFmt, PPS.fmtSym name, rightFmt])
				       end
				     else (* can hold both args against outer pulls *)
				       let val leftFmt = fmtExp' (leftArg, lpull, left, d-1)
					   val rightFmt = fmtExp' (rightArg, right, rpull, d-1)
					in PP.pblock [leftFmt, PPS.fmtSym name, rightFmt]
				       end
				  end
				| SOME _ => bug "AU.destTupleExp returned bad arg for binary app"
				| NONE =>  (* rand is not a pair *)
				    PP.hblock [fmtExp' (rator, lpull, 1000, d-1),
					     fmtExp' (rand, 1000, rpull, d-1)]
			 end
	      end

	  | fmtExp' (CONSTRAINTexp(exp, ty), lpull, rpull, d) =
	      PP.parens (PP.pblock [fmtExp'(exp, 0, 0, d), PP.colon, PPT.fmtType env ty])
	  | fmtExp' (HANDLEexp(exp, (rules,_,_)), lpull, rpull, d) =
	      PP.parens
	        (PP.vblock
	           [fmtExp' (exp, 0, 0, d-1),
		    PP.indent 2 (fmtMatch ("handle", rules, d))])
	  | fmtExp' (RAISEexp (exp,_), lpull, rpull, d) =
	      mkAtomic (lpull, rpull,
			PP.hblock [PP.text "raise", fmtExp' (exp, 0, 0, d-1)])
	  | fmtExp' (LETexp(dec, exp), _, _, d) =
	      PP.vblock
		[PP.hblock [PP.text "let", fmtDec context (dec, d-1)],
		 PP.hblock [PP.text " in", fmtExp' (exp, 0, 0, d-1)],
		 PP.text "end"]
	  | fmtExp' (LETVexp(var, defexp, bodyexp), _, _, d) =
	      PP.hvblock
		[PP.pblock [PP.hblock [PP.text "letv", PPV.fmtVar var, PP.equal],
			  PP.indent 2 (fmtExp' (defexp, 0, 0, d-1))],
		 PP.hblock [PP.text " in", fmtExp' (bodyexp, 0, 0, d-1)],
		 PP.text "end"]
	  | fmtExp' (CASEexp(exp, (rules,_,_)), _, _, d) =
	      PP.parens
	        (PP.vblock
	           [PP.hblock [PP.text "case", fmtExp' (exp, 0, 0, d-1)],
		    PP.indent 2 (fmtMatch ("of", rules, d))])
	  | fmtExp' (IFexp { test, thenCase, elseCase }, lpull, rpull, d) =
	      mkAtomic (lpull, rpull, 
		PP.hvblock
		  [PP.hblock [PP.text "if", fmtExp' (test, 0, 0, d-1)],
		   PP.hblock [PP.text "then", fmtExp' (thenCase, 0, 0, d-1)],
		   PP.hblock [PP.text "else", fmtExp' (elseCase, 0, 0, d-1)]])
	  | fmtExp' (ANDALSOexp (e1, e2), lpull, rpull, d) =
	      let val operator = PP.text "andalso"
	       in if lpull > 4 orelse rpull > 5
		  then PP.parens
			 (PP.pblock [fmtExp' (e1, 0, 4, d-1), operator, fmtExp' (e2, 5, 0, d-1)])
		  else (PP.pblock [fmtExp' (e1, lpull, 4, d-1), operator, fmtExp' (e2, 5, rpull, d-1)])
	      end
	  | fmtExp' (ORELSEexp (e1, e2), lpull, rpull, d) =
	      let val operator = PP.text "orelse"
	       in if lpull > 2 orelse rpull > 3
		  then PP.parens
			 (PP.pblock [fmtExp' (e1, 0, 2, d-1), operator, fmtExp' (e2, 3, 0, d-1)])
		  else (PP.pblock [fmtExp' (e1, lpull, 2, d-1), operator, fmtExp' (e2, 3, rpull, d-1)])
	      end
	  | fmtExp' (WHILEexp { test, expr }, lpull, rpull, d) =
              PP.pblock
	        [PP.hblock [PP.text "while", fmtExp'(test, 0, 0, d-1)],
	         PP.hblock [PP.text "do", fmtExp' (expr, 0, rpull, d-1)]]
	  | fmtExp' (FNexp (rules, _, _), _, _, d) =
	      PP.parens (fmtMatch ("fn", rules, d))
	  | fmtExp' (MARKexp (exp, region), lpull, rpull, d) =
	      (case sourceOp
		of SOME source =>
		     if !internals
		     then PP.enclose {front = PP.text "<", back = PP.text ">"}
			    (PP.pblock
			       [PP.cblock [PP.text "@",
					   PP.parens (PPSM.fmtRegion region),
					   PP.colon],
				fmtExp' (exp, 0, 0, d)])
		     else fmtExp'(exp, lpull, rpull, d)
	         | NONE => fmtExp'(exp, lpull, rpull, d))
	  | fmtExp' (SWITCHexp (exp, srules, defaultOp), _, _, d) =
	      PP.vblock
	        [PP.hblock [PP.text "SWITCH", fmtExp' (exp, 0, 0, d-1)],
	         PP.indent 2 (fmtSMatch (srules, defaultOp, d-1))]
	  | fmtExp' (VSWITCHexp (exp, _, srules, default), _, _, d) =
	      PP.vblock
	        [PP.hblock [PP.text "VSWITCH", fmtExp' (exp, 0, 0, d-1)],
	         PP.indent 2 (fmtSMatch (srules, SOME default, d-1))]
          (* end fmtExp' *)

     in fmtExp' (exp, 0, 0, depth)
    end (* end fmtExp *)

and fmtVB (context as (env,sourceOp)) (VB {pat, exp, ...}, d) =
    if d <= 0 then PP.text "<VB>"
    else PP.pblock [fmtPat env (pat, d-1), PP.equal, fmtExp context (exp, d-1)]

and fmtRVB context (RVB {var, exp, ...}, d) =
    if d <= 0 then PP.text "<RVB>" else
    PP.pblock [PPV.fmtVar var, PP.equal, fmtExp context (exp, d-1)]

and fmtVARSEL (var1, var2, index) =
    PP.hblock
      [PP.text "val", PPV.fmtVar var1, PP.text " = #", PP.integer index, PPV.fmtVar var2]

and fmtDec (context as (env,sourceOp)) (dec, depth) =
    let fun fmtDec' (dec, 0) = PP.text (decTag dec)  (* "<dec>" *)
          | fmtDec' (VALdec vbs, d) =
	      PP.vHeaders {header1 = "val", header2 = "and"}
	        (map (fn vb => fmtVB context (vb,d-1)) vbs)
          | fmtDec' (VALRECdec rvbs, d) =
	      PP.vHeaders {header1 = "val rec", header2 = "and"}
	        (map (fn rvb => fmtRVB context (rvb,d-1)) rvbs)
	  | fmtDec' (DOdec exp, d) =
	      PP.hblock [PP.text "do", fmtExp context (exp,d-1)]
          | fmtDec' (TYPEdec tycs, d) =
	    let fun fmtDEFtyc (T.DEFtyc{path, tyfun=T.TYFUN{arity,body},...}) =
		    PP.hblock [PPT.fmtFormals arity, PPP.fmtTycName path,
			       PP.equal, PPT.fmtType env body]
		  | fmtDEFtyc _ = bug "fmtDEFtyc"
	     in PP.vHeaders {header1 = "type", header2 = "and"}
 	          (map fmtDEFtyc tycs)
	    end
          | fmtDec' (DATATYPEdec{datatycs,withtycs}, d) =
	    let fun fmtDATATYPE (T.GENtyc { path, arity, kind, ... }) =
		      (case kind
			 of T.DATATYPE ({index, family={members,...}, ...}) =>
			      let val {dcons, ...} = Vector.sub (members, index)
				  val dconNames = map #name dcons
			       in PP.hblock
				    [PPT.fmtFormals arity,
				     PPP.fmtTycName path,
				     PP.equal, 
				     PP.psequence (PP.text " |") (map PPS.fmtSym dconNames)]
			      end
			  | _ => bug "fmtDec'(DATATYPEdec) 1")
		  | fmtDATATYPE _ = bug "fmtDec'(DATATYPEdec) 2"
		fun fmtWITHTYPE (T.DEFtyc{path, tyfun=T.TYFUN{arity,body},...}) =
		    PP.hblock
		      [PPT.fmtFormals arity,
		       PPP.fmtTycName path,
		       PP.equal, PPT.fmtType env body]
		  | fmtWITHTYPE _ = bug "fmtDec'(DATATYPEdec) 3"
	     in (* could call PPDec.fmtDec here *)
	        PP.vblock
		  [PP.vHeaders {header1 = "datatype", header2 = "and"}
                     (map fmtDATATYPE datatycs),
	           PP.vHeaders {header1 = "withtype", header2 = "and"}
                     (map fmtWITHTYPE withtycs)]
	    end
        | fmtDec' (ABSTYPEdec _, _) = PP.text "<ABSTYPEdec>"
        | fmtDec' (EXCEPTIONdec ebs, d) =
	    let fun fmtEB (EBgen{exn=T.DATACON{name,...}, etype}) =
		      PP.hblock
			[PPS.fmtSym name,
			 case etype
			  of NONE => PP.empty
			   | SOME ty' => PP.hblock [PP.text "of", PPT.fmtType env ty']]
		  | fmtEB (EBdef{exn=T.DATACON{name,...}, edef=T.DATACON{name=dname,...}}) =
		      PP.hblock [PPS.fmtSym name, PP.equal, PPS.fmtSym dname]
	     in PP.vHeaders {header1 = "exception", header2 = "and"} (map fmtEB ebs)
	    end
        | fmtDec' (STRdec sbs,d) =
	    let fun fmtSTRB (STRB {name, str=M.STR { access, ... }, def}) =
		    PP.pblock [PP.hblock [PPS.fmtSym name, PPV.fmtAccess access, PP.equal],
			     fmtStrexp context (def, d-1)]
		  | fmtSTRB (STRB {name, str = M.ERRORstr, ...}) =
		    PP.hblock [PPS.fmtSym name, PP.equal, PP.text "<unbound>"]
		  | fmtSTRB _ = bug "fmtDec:STRdec:STRB"
	     in PP.vHeaders {header1 = "structure", header2 = "and"} (map fmtSTRB sbs)
	    end
        | fmtDec' (FCTdec fbs,d) =
	    let fun fmtFCTB (FCTB{name=fname, fct=M.FCT { access, ... }, def}) =
                      PP.pblock [PP.hblock [PPS.fmtSym fname, PPV.fmtAccess access, PP.equal],
			       fmtFctexp context (def,d-1)]
		  | fmtFCTB _ = bug "fmtDec':FCTdec"
	     in PP.vHeaders {header1 = "functor", header2 = "and"} (map fmtFCTB fbs)
	    end
        | fmtDec' (SIGdec sigvars, d) =
	    let fun fmtSIG (M.SIG { name, ... }) =
		    PP.hblock
		      [PP.text "signature",
		       case name
			 of SOME s => PPS.fmtSym s
			  | NONE => PP.text "ANONYMOUS"]
		  | fmtSIG _ = bug "fmtDec':SIGdec"
	     in PP.vblock (map fmtSIG sigvars)
	    end
        | fmtDec' (FSIGdec sigvars, d) =
	    let fun fmtFSIG (M.FSIG{kind, ...}) =
		    PP.hblock
		      [PP.text "funsig",
                       case kind
			 of SOME s => PPS.fmtSym s
                          | NONE => PP.text "ANONYMOUS"]
		  | fmtFSIG _ = bug "fmtDec':FSIGdec"
	     in PP.vblock (map fmtFSIG sigvars)
	    end
        | fmtDec' (LOCALdec(inner,outer), d) =
	    PP.vblock
	      [PP.text "local",
	       PP.indent 2 (fmtDec' (inner, d-1)),
	       PP.text "in",
	       PP.indent 2 (fmtDec' (outer, d-1)),
	       PP.text "end"]
        | fmtDec' (SEQdec decs, d) =
	    PP.vblock (map (fn dec => fmtDec' (dec, d)) decs)
        | fmtDec' (FIXdec {fixity,ops},d) =
	    PP.hblock
	      [case fixity
	         of F.NONfix => PP.text "nonfix"
	          | F.INfix (i,_) =>
		      PP.hblock
			[PP.text (if i mod 2 = 0 then "infix" else "infixr"),
			 if i div 2 > 0 then (PP.integer (i div 2)) else PP.empty],
	       PP.hblock (map PPS.fmtSym ops)]

        | fmtDec' (OVLDdec ovldvar, _) =
	    PP.hblock [PP.text "overload ", PPV.fmtVar ovldvar]

        | fmtDec' (OPENdec strbs, _) =
	   PP.hblock (PP.text "open" :: (map (fn (sp,_) => PPP.fmtSymPath sp) strbs))

        | fmtDec' (MARKdec (dec, _), d) = fmtDec' (dec, d)
(*
	  (case sourceOp
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

and fmtStrexp (context as (statenv,sourceOp)) =
    let fun fmtStrexp' (_,0) = PP.text "<strexp>"

	  | fmtStrexp' (VARstr str, _) = fmtStr str

	  | fmtStrexp' (APPstr{oper, arg, ...}, _) =
	      PP.hblock [fmtFct oper, PP.parens (fmtStr arg)]

          | fmtStrexp' (STRstr bindings, d) =
              PP.vblock
	        [PP.text "struct",
	         PP.indent 2
	           (PP.vblock
		     (map (fn binding =>
		            PPModules.fmtBinding statenv
			      (Bindings.bindingSymbol binding, binding, d-1))
			  bindings)),
		 PP.text "end"]
		
	   | fmtStrexp' (LETstr(dec,body),d) =
	       PP.vblock
	         [PP.hblock [PP.text "let ", fmtDec context (dec,d-1)],
	          PP.hblock [PP.text "in",  fmtStrexp'(body,d-1)],
		  PP.text "end"]

           | fmtStrexp' (MARKstr (body, _), d) = fmtStrexp' (body, d)  (* ignore region *)
(*
	      (case sourceOp
		of SOME source =>
		     PP.hblock
	               [PP.text "MARKstr",
			PP.cblock [fmtStrexp' (body, d), PP.comma],
			PP.cblock [fmtPos (source,s), PP.comma],
			fmtPos (source,e)]
	         | NONE => fmtStrexp' (body,d))
*)
     in fmtStrexp'
    end

and fmtFctexp (context as (_,sourceOp)) =
    let fun fmtFctexp' (_, 0) = PP.text "<fctexp>"

	  | fmtFctexp' (VARfct fct, d) = fmtFct fct

	  | fmtFctexp' (FCTfct {param, def, ...}, d) =
	      PP.vblock
		[PP.hblock
		   [PP.text "FCT",
		    PP.parens (fmtStr param),
		    PP.text "=>"],
		 fmtStrexp context (def, d-1)]

	  | fmtFctexp' (LETfct(dec,body),d) =
	      PP.vblock
		[PP.hblock [PP.text "let", fmtDec context (dec, d-1)],
		 PP.hblock [PP.text "in", fmtFctexp' (body, d-1)],
		 PP.text "end"]

	  | fmtFctexp' (MARKfct (body,_), d) = fmtFctexp' (body, d)
  (*        [if MARKs were to be printed:]
	      (case sourceOp
		of SOME source =>
		     (pps "MARKfct(";
		      fmtFctexp'(body,d); pps ",";
		      prpos(ppstrm,source,s); pps ",";
		      prpos(ppstrm,source,e); pps ")")
		 | NONE => fmtFctexp'(body,d))
  *)
     in fmtFctexp'
    end

fun fmtRule (c as (env, _): context) (RULE (pat,exp): rule, d: int) : PP.format =
    if d <= 0 then PP.text "<rule>"
    else ruleFmt (fmtPat env (pat, d-1), fmtExp c (exp, d))

end (* top-level local *)
end (* structure PPAbsyn *)
