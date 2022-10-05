(* ppast.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Authors: Jing Cao and Lukasz Ziarek
 *)

structure PPAst: PPAST =
struct

local

  structure S = Symbol
  structure F = Fixity
  structure SRC = Source

  structure PP = NewPP
  structure PPU = NewPPUtil
  structure PPS = PPSymbols  (* fmtSym, fmtSymList *)

  open Ast

  val internals = ParserControl.astInternals
  val lineprint = ParserControl.astLineprint

  fun bug msg = ErrorMsg.impossible("PPAst: "^msg)

in

(* fmtPos : Source.inputSource * int -> PP.format *)
fun fmtPos (source: Source.inputSource, charpos: int) =
    if (!lineprint) then
      let val {line, column, ...} = Source.filepos source charpos
       in PP.cblock [PP.integer line, PP.period, PP.integer column]
      end
    else PP.integer charpos

(* strength : Ast.ty -> int *)
fun strength ty =
    case ty
      of VarTy(_) => 1
       | ConTy(tycon, args) =>
          (case tycon
            of [tyc] =>
	         if S.eq(S.tycSymbol("->"), tyc) then 0
		 else 2
	     | _ => 2)
       | RecordTy _ => 2
       | TupleTy _ => 1
       | _ => 2

fun stripMarkExp (MarkExp (a, _)) = stripMarkExp a
  | stripMarkExp x = x

(* fmtPath : S.symbol list -> PP.format *)
fun fmtPath (symbols: S.symbol list) =
    PP.sequence {alignment = PP.C, sep = PP.period} (map PPS.fmtSym symbols)

(* fmtTyvar : tyvar -> PP.format *)
fun fmtTyvar tyvar =
    (case tyvar
       of Tyv sym => PPS.fmtSym sym
	| MarkTyv (tyvar', _) => fmtTyvar tyvar')

(* fmtTyvars : tyvar list -> PP.format *)
fun fmtTyvars nil = PP.empty
  | fmtTyvars [tyvar] = fmtTyvar tyvar
  | fmtTyvars tyvars =
      PP.parens (PP.psequence PP.comma (map fmtTyvar tyvars))

fun fmtPat sourceOp (pat, d) =
    let fun fmtPat' (WildPat, _) = PP.text "_"
	  | fmtPat' (VarPat p, d) = fmtPath p
	  | fmtPat' (IntPat (src, _), _) = PP.text src
	  | fmtPat' (WordPat(src, _), _) = PP.text src
	  | fmtPat' (StringPat s, _) = PP.text s
	  | fmtPat' (CharPat c, _) = PP.char (Option.valOf (Char.fromString c))  (* c: string, should be char *)
	  | fmtPat' (LayeredPat {varPat,expPat},d) =
	      PP.pblock [fmtPat'(varPat, d), PP.text "as", fmtPat'(expPat,d-1)]
	  | fmtPat' (RecordPat {def=[], flexibility}, _) =
	      if flexibility then PP.text "{...}" else PP.text "()"
	  | fmtPat' (r as RecordPat {def, flexibility}, d) =
              PP.braces
		(PP.psequence PP.comma
		   (map (fn (sym,pat) => (PP.hblock [PPS.fmtSym sym, PP.equal, fmtPat' (pat, d-1)])) def
		    @ (if flexibility then [PP.text "..."] else nil)))
	  | fmtPat' (ListPat nil, d) = PP.text "[]"  (* may not need the special case *)
	  | fmtPat' (ListPat elems, d) = PP.listFormats (map (fn pat => fmtPat' (pat, d-1)) elems)
	  | fmtPat' (TuplePat elems, d) = PP.tupleFormats (map (fn pat => fmtPat' (pat, d-1)) elems)
	  | fmtPat' (FlatAppPat fap, d) = PP.hblock (map (fn {item,fixity,region} => fmtPat'(item,d-1)) fap)
	  | fmtPat' (AppPat {constr, argument}, d) =
	      PP.hblock [fmtPat' (constr,d), PP.parens (fmtPat'(argument,d))]
	  | fmtPat' (ConstraintPat {pattern, constraint}, d) =
              PP.hblock [fmtPat' (pattern, d-1), PP.colon, fmtTy sourceOp (constraint, d)]
	  | fmtPat' (VectorPat nil, d) = PP.text "#[]"
	  | fmtPat' (VectorPat pats, d) =
	      PP.ccat (PP.text "#", PP.listFormats (map (fn pat => fmtPat' (pat, d-1)) pats))
	  | fmtPat' (MarkPat (pat, (s,e)), d) =
	      (case sourceOp
		 of SOME source =>
		      if !internals
		      then PP.enclose {front = PP.text "<", back = PP.text ">"}
			     (PP.hcat
				(PP.ccat (PP.text "MARK",
					  PP.parens
					    (PP.hblock [fmtPos (source, s), PP.comma, fmtPos (source, e)])),
				 PP.hcat (PP.colon, fmtPat' (pat,d))))
		      else fmtPat' (pat,d)
		  | NONE => fmtPat' (pat,d))
          | fmtPat' (OrPat pats, d) =
	      PP.parens (PP.sequence {alignment = PP.P, sep = PP.text " |"}
				     (map (fn pat => fmtPat' (pat, d-1)) pats))
    in fmtPat' (pat, d)
    end

and fmtExp (sourceOp: SRC.inputSource option) (exp: exp, depth: int) =
  let fun fmtExp' (_, 0) = PP.text "<exp>"
	| fmtExp' (VarExp p, _) = fmtPath p
	| fmtExp' (FnExp nil, _ ) = PP.text "<function>"
	| fmtExp' (FnExp rules, d) =
	    PP.label "fn"
              (PP.tryFlat
		 (PP.vsequence (PP.text " |")
		    (map (fn rule => fmtRule (rule, d-1)) rules)))
	| fmtExp' (FlatAppExp fap, d) =
	    PP.pblock (map (fn {item,...} => fmtExpClosed (item, d)) fap)
	| fmtExp' (AppExp {function,argument}, d) =
	    if d <= 0 then PP.text "<exp>" else
	       (case stripMarkExp function
		  of VarExp v =>
		      PP.hcat (fmtPath v, fmtExpClosed (argument, d-1))
		   | rator =>
		      PP.hcat (fmtExpClosed (rator, d-1), fmtExpClosed (argument, d-1)))
	| fmtExp' (CaseExp {expr, rules}, d) =
            PP.parens
	      (PP.vcat
		 (PP.hcat (PP.text "case", fmtExp' (expr, d-1)),
	          PPU.vHeaders {header1 = "of ", header2 ="   | ",
			       formatter = (fn r => fmtRule (r,d-1))}
			      rules))
	| fmtExp' (LetExp {dec, expr}, d) =
	    PP.vblock
	      [PP.hcat (PP.text "let ", fmtDec sourceOp (dec, d-1)),
	       PP.hcat (PP.text "in ", fmtExp'(expr, d-1)),
	       PP.text "end"]
 	| fmtExp' (SeqExp exps, d) =
            let val defaultFmt =
		    PP.parens
		      (PP.sequence {alignment = PP.P, sep = PP.semicolon}
		         (map (fn exp => fmtExp' (exp, d-1)) exps))
                fun subExpCount (MarkExp (expr, _)) = subExpCount expr
                  | subExpCount (FlatAppExp subexps) = length subexps
                  | subExpCount _ = 1
             in case exps
                  of [expr] => if (subExpCount expr) < 2
                               then fmtExp' (expr, d-1)
                               else defaultFmt
                   | _ => defaultFmt
            end
	| fmtExp' (IntExp (src, _), _) = PP.text src
	| fmtExp' (WordExp (src, _), _) = PP.text src
	| fmtExp' (RealExp (src, _), _) = PP.text src
	| fmtExp' (StringExp s, _) = PP.string s
	| fmtExp' (CharExp s, _) = PP.ccat (PP.text "#", PP.string s)
	| fmtExp' (r as RecordExp fields, d) =
	    let fun fmtField (name, exp) = PP.hblock [PPS.fmtSym name, PP.equal, fmtExp' (exp, d)]
	     in PP.braces
	          (PP.sequence {alignment = PP.P, sep = PP.comma}
			       (map fmtField fields))
            end
	| fmtExp' (ListExp exps, d) =
	    PP.list (fn exp => fmtExp' (exp, d-1)) exps
	| fmtExp' (TupleExp exps, d) =
	    PP.tuple (fn exp => (fmtExp' (exp, d-1))) exps
	| fmtExp' (SelectorExp name, d) =
	    PP.ccat (PP.text "#", PPS.fmtSym name)
	| fmtExp' (ConstraintExp {expr,constraint}, d) =
	    PP.hblock [fmtExp'(expr, d), PP.colon, fmtTy sourceOp (constraint, d)]
        | fmtExp'(HandleExp{expr,rules}, d) =
	    PP.vcat
	      (fmtExp' (expr, d-1),
	       PP.hcat (PP.text "handle",
			PPU.vHeaderFormats {header1 = "  ", header2 = "| "}
		          (map (fn r => fmtRule (r,d-1)) rules)))
	| fmtExp' (RaiseExp exp, d) =
	    PP.hcat (PP.text "raise", fmtExpClosed (exp, d-1))
	| fmtExp' (IfExp {test, thenCase, elseCase}, d) =
	    PP.vblock
	      [PP.hcat (PP.text "if", fmtExp' (test, d-1)),
	       PP.hcat (PP.text "then ", fmtExp' (thenCase, d-1)),
	       PP.hcat (PP.text "else ", fmtExpClosed (elseCase, d-1))]
	| fmtExp' (AndalsoExp (e1, e2), d) =
	    PP.pblock [fmtExpClosed (e1, d-1), PP.text "andalso", fmtExpClosed (e2, d-1)]
	 | fmtExp' (OrelseExp (e1, e2), d) =
	    PP.pblock [fmtExp' (e1, d-1), PP.text "orelse", fmtExp' (e2, d-1)]
	 | fmtExp' (WhileExp {test, expr}, d) =
	     PP.vcat
	       (PP.hcat (PP.text "while", fmtExp' (test, d-1)),
		PP.hcat (PP.text "do", fmtExpClosed (expr, d-1)))
	 | fmtExp'(VectorExp nil, d) = PP.text "#[]"
	 | fmtExp'(VectorExp exps, d) =
	     PP.ccat (PP.text "#", PP.list (fn exp => fmtExp' (exp, d-1)) exps)
	 | fmtExp'(MarkExp (exp,(s,e)), d) =
	      (case sourceOp
		 of SOME source =>
		      if !internals
		      then PP.enclose {front = PP.text "<", back = PP.text ">"}
			     (PP.hcat
				(PP.ccat
				   (PP.text "MARK",
				    PP.parens (PP.hblock [fmtPos (source, s), PP.comma, fmtPos (source, e)])),
				 PP.hcat (PP.colon, fmtExp' (exp, d))))
		      else fmtExp' (exp, d)
		  | NONE => fmtExp' (exp, d))

     and fmtRule (Rule {pat,exp}, d) =
	 if d <= 0 then PP.text "<rule>"
	 else PP.pcat (PP.hcat (fmtPat sourceOp (pat, d-1), PP.text "=>"),
		       PP.softIndent 3 (fmtExpClosed (exp, d-1)))

     and fmtExpClosed (exp, d) = 
	 (case stripMarkExp exp
	   of (IfExp _ | CaseExp _ | FnExp _ | HandleExp _ | OrelseExp _) => 
	      PP.parens (fmtExp' (exp, d))
           | _ => fmtExp' (exp, d))

   in fmtExp' (exp, depth)
  end (* fun fmtExp *)


and fmtStrExp sourceOp (strexp, d) =
    if d <= 0 then PP.text "<strexp>" else
    (case strexp
       of VarStr p => fmtPath p

        | BaseStr (SeqDec nil) => PP.hcat (PP.text "struct", PP.text "end")

        | BaseStr de =>
            PP.vblock
              [PP.text "struct",
               PP.hardIndent 2 (fmtDec sourceOp (de, d-1)),
               PP.text "end"]

	| ConstrainedStr (stre, constraint) =>
            PP.hcat
	      (fmtStrExp sourceOp (stre, d-1),
               case constraint
                 of NoSig => PP.empty
                  | Transparent sigexp =>
                      PP.hcat (PP.colon, fmtSigExp sourceOp (sigexp, d-1))
                  | Opaque sigexp =>
                      PP.hcat (PP.text ":>", fmtSigExp sourceOp (sigexp, d-1)))

	| AppStr (path, args) =>
	    let fun argFormatter (strexp, _) = PP.parens (fmtStrExp sourceOp (strexp, d))
	     in PP.hcat
		  (fmtPath path,
                   PP.pblock (map argFormatter args))
            end

        | AppStrI (path, args) =>
	    let fun argFormatter (strexp, _) = PP.parens (fmtStrExp sourceOp (strexp, d))
	     in PP.hcat
		  (fmtPath(path),
		   PP.pblock (map argFormatter args))
            end

	| LetStr (dec, body) =>
	    PP.vblock
	      [PP.label "let" (fmtDec sourceOp (dec, d-1)),
	       PP.label "in" (fmtStrExp sourceOp (body, d-1)),
	       PP.text "end"]

        | MarkStr (body,(s,e)) => fmtStrExp sourceOp (body, d))

and fmtFctExp sourceOp (fctexp, d) =
    if d <= 0 then PP.text "<fctexp>" else
    (case fctexp
       of VarFct (path, _) => fmtPath path
        | LetFct(dec,body) =>
            PP.vblock
	      [PP.label "let" (fmtDec sourceOp (dec, d-1)),
	       PP.label " in" (fmtFctExp sourceOp (body, d-1)),
	       PP.text "end"]
	| AppFct(path, sblist, fsigconst) =>
            PP.hcat
	      (fmtPath path,
	       PP.parens
		 (PP.hblock
		    (map (fn (strexp,_) => fmtStrExp sourceOp (strexp, d-1)) sblist)))
	| MarkFct(body,(s,e)) => fmtFctExp sourceOp (body, d)
        | BaseFct _ => ErrorMsg.impossible "fmtFctExp: BaseFct")

and fmtWhereSpec sourceOp =
    let fun fmtWhereSpec' (_,0) = PP.text "<WhereSpec>"
	  | fmtWhereSpec' (WhType ([], [], ty), d) = fmtTy sourceOp (ty, d)
	  | fmtWhereSpec' (WhType (slist, tvlist, ty), d) =
	      PP.hblock
		[PP.text "type",
		 PP.pblock (map fmtTyvar tvlist),
		 PPS.fmtSymList slist,
		 PP.equal,
		 fmtTy sourceOp (ty,d)]
	  | fmtWhereSpec'(WhStruct (slist, slist'), d) =
	      let fun pr _ sym = PPS.fmtSym sym
	       in PP.hblock
		    [PP.text "structure",
		     PPS.fmtSymList slist,
		     PP.equal,
		     PPS.fmtSymList slist']
	      end
     in fmtWhereSpec'
    end

and fmtSigExp sourceOp =
    let	fun fmtSigExp'(_,0) = PP.text "<SigExp>"
	  | fmtSigExp'(VarSig s,d) = (PPS.fmtSym s)
	  | fmtSigExp'(AugSig (sign, wherel),d) =  (* ??? *)
	     PP.vcat
	       (fmtSigExp' (sign, d),
		(case sign
		   of VarSig s =>
			PPU.vHeaders {header1 = "where ", header2 = "and",
				     formatter = (fn r => fmtWhereSpec sourceOp (r,d-1))}
				    wherel
		    | MarkSig(VarSig s, r) =>
			PPU.vHeaders {header1 = "where", header2 = "and",
				     formatter = (fn r => fmtWhereSpec sourceOp (r,d-1))}
				    wherel
		    | _ =>
			PPU.vHeaders {header1 = "where", header2 = "and",
				     formatter = (fn r => fmtWhereSpec sourceOp (r,d-1))}
				    wherel))
	  | fmtSigExp'(BaseSig [],d) = PP.hcat (PP.text "sig", PP.text "end")
	  | fmtSigExp'(BaseSig specl,d) =
	    let val specFmts = map (fn speci => fmtSpec sourceOp (speci,d)) specl
	     in PP.vblock
		  [PP.text "sig",
		   PP.viblock (PP.HI 2) specFmts,
		   PP.text "end"]
	    end
	  | fmtSigExp'(MarkSig (m,r),d) = fmtSigExp sourceOp (m,d)
    in
	fmtSigExp'
    end

and fmtFsigExp sourceOp (fsigexp, d) =
    if d <= 0 then PP.text "<fsigexp>" else
    (case fsigexp
       of VarFsig s => PPS.fmtSym s
	| BaseFsig {param, result} =>
	    let fun formatter (SOME symbol, sigexp) =
		      PP.parens 
			(PP.pcat (PP.hcat (PPS.fmtSym symbol, PP.colon),
				  fmtSigExp sourceOp (sigexp, d-1)))
		  | formatter (NONE, sigexp) =
		      PP.softIndent 4 (PP.parens (fmtSigExp sourceOp (sigexp, d)))
	     in PP.hblock
		  [PP.vblock (map formatter param),
		   PP.text "=>",
		   fmtSigExp sourceOp (result, d)]
	    end
	| MarkFsig (m,r) => fmtFsigExp sourceOp (m, d))

and fmtSpec sourceOp (spec, d) =
    let fun fmtDataBind dbing = fmtDb sourceOp (dbing, d)
	fun fmtTypeBind tbing = fmtTb sourceOp (tbing, d)

	fun fmtSpec'(StrSpec sspo_list, d) =
	    if d <= 0 then PP.text "<Spec>" else
	    let fun formatter (symbol, sigexp, pathOp) =
		    let val specFmt = PP.hblock [PPS.fmtSym symbol, PP.equal, fmtSigExp sourceOp (sigexp,d)]
		     in case pathOp
			  of SOME p => PP.vcat (specFmt, PP.label "path" (fmtPath p))
			   | NONE => specFmt
		    end
	     in PP.hcat (PP.text "structure", PP.vblock (map formatter sspo_list))
	    end

	| fmtSpec' (TycSpec (stto_list, bool), d) =
	    let fun formatter (symbol, tyvars, tyo) =
		    let val front = PP.hcat (fmtTyvars tyvars, PPS.fmtSym symbol)
		     in (case tyo
			  of SOME ty => PP.hblock [front, PP.equal, fmtTy sourceOp (ty, d)]
			   | NONE => front)
		    end
	     in PPU.vHeaders {header1 = "text", header2 = "and", formatter = formatter} stto_list
	    end

	| fmtSpec' (FctSpec sf_list, d) =
	  let fun formatter (symbol, fsigexp) =
                    PP.hblock [PPS.fmtSym symbol, PP.colon, fmtFsigExp sourceOp (fsigexp, d-1)]
	   in PPU.vHeaders {header1 = "functor", header2 = "and", formatter = formatter} sf_list
	  end

	| fmtSpec' (ValSpec st_list, d) =
	  let fun formatter (symbol, ty) =
                  PP.hblock [PPS.fmtSym symbol, PP.colon, fmtTy sourceOp (ty, d)]
	   in PPU.vHeaders {header1 = "val", header2 = "and", formatter = formatter} st_list
	  end

        | fmtSpec' (DataReplSpec(name,path), d) =
	  if d = 0 then PP.text "<DT.repl>" else
          PP.hblock [PP.text "datatype", PPS.fmtSym name, PP.equal, PP.text "datatype",
		     PP.csequence PP.period (map PPS.fmtSym path)]

	| fmtSpec' (DataSpec{datatycs,withtycs=[]}, d) =
	    let fun formatter dbing = fmtDb sourceOp (dbing, d)
	     in PPU.vHeaders {header1 = "datatype", header2 = "and", formatter = formatter} datatycs
	    end

	| fmtSpec' (DataSpec {datatycs, withtycs}, d) =
	    let fun fmtd dbing = (fmtDb sourceOp (dbing, d))
		fun fmtw tbing = (fmtTb sourceOp (tbing, d))
	     in PP.vcat
		 (PPU.vHeaders {header1 = "datatype", header2 = "and", formatter = fmtd} datatycs,
		  PPU.vHeaders {header1 = "withtype", header2 = "and", formatter = fmtw} withtycs)
	    end

	| fmtSpec' (ExceSpec sto_list, d) =
	  let fun fmtr (symbol, tyo) =
		  (case tyo
		    of SOME ty =>
                         PP.hblock [PPS.fmtSym symbol, PP.colon, fmtTy sourceOp (ty, d)]
		     | NONE =>  PPS.fmtSym symbol)
	   in PPU.vHeaders {header1 = "exception", header2 = "and", formatter = fmtr} sto_list
	  end

	| fmtSpec' (ShareStrSpec paths, d) =
            PPU.vHeaders {header1 = "sharing", header2 = "=", formatter = fmtPath} paths
            
        | fmtSpec' (ShareTycSpec paths, d) =
            PPU.vHeaders {header1 = "sharing type", header2 = "=", formatter = fmtPath} paths

	| fmtSpec' (IncludeSpec sigexp, d) = fmtSigExp sourceOp (sigexp, d)

	| fmtSpec' (MarkSpec (m,r), d) = fmtSpec sourceOp (m,d)
  in
      fmtSpec' (spec, d)
  end

and fmtDec sourceOp (dec, depth) =
    let fun fmtDataBind (dbing, d) = (fmtDb sourceOp (dbing, d))
	fun fmtTypeBind (tbing, d) = (fmtTb sourceOp (tbing, d))
	fun fmtDec' (_, 0) = PP.text "<dec>"
	  | fmtDec' (ValDec (vbs, tyvars), d) =
	     PPU.vHeaderFormats {header1 = "val", header2 = "and"}
	       (map (fn vb => fmtVb sourceOp (vb, d-1)) vbs)

	  | fmtDec' (ValrecDec (rvbs, tyvars), d) =
	     PPU.vHeaderFormats {header1 = "val rec", header2 = "and"}
	       (map (fn rvb => fmtRvb sourceOp (rvb, d-1)) rvbs)

	  | fmtDec' (DoDec exp, d) = PP.label "do" (fmtExp sourceOp (exp,d-1))

	  | fmtDec' (FunDec (fbs,tyvars), d) =
	     PPU.vHeaderFormats {header1 = "fun", header2 = "and"}
	       (map (fn fb => fmtFb sourceOp (fb, d-1)) fbs)

	  | fmtDec' (TypeDec tycs, d) =
	      PP.label "type" (PP.pblock (map (fn tyc => fmtTb sourceOp (tyc, d-1)) tycs))

	  | fmtDec' (DatatypeDec{datatycs,withtycs=[]}, d) =
	      PP.label "datatype"
	        (PP.pblock (map (fn dbing => fmtDataBind (dbing, d-1)) datatycs))

	  | fmtDec' (DatatypeDec{datatycs,withtycs},d) =
	      PP.vcat
		(PP.label "datatype"
		   (PP.pblock (map (fn db => fmtDataBind (db, d)) datatycs)),
		 PP.label "withtype"
		   (PP.pblock (map (fn tb => fmtTypeBind (tb, d)) withtycs)))

	  | fmtDec' (DataReplDec(symb, path), _) =
	      PP.hblock
		[PP.text "datatype ", PPS.fmtSym symb, PP.equal, PP.text "datatype", fmtPath path]

	  | fmtDec' (AbstypeDec{abstycs,withtycs=[],body}, d) =
	      PP.vcat
		(PP.label "abstype"
		   (PP.pblock (map (fn db => fmtDataBind (db, d-1)) abstycs)),
		 fmtDec' (body, d))

	  | fmtDec' (AbstypeDec{abstycs,withtycs,body}, d) =
	      PP.vblock
		[PP.label "abstype"
		   (PP.pblock (map (fn db => fmtDataBind (db, d-1)) abstycs)),
		 PP.label "withtype"
		   (PP.pblock (map (fn tb => fmtTypeBind (tb, d-1)) withtycs)),
		 fmtDec' (body, d)]

	  | fmtDec' (ExceptionDec ebs, d) =
	      PP.pblock (map (fn eb => fmtEb sourceOp (eb,d-1)) ebs)

	  | fmtDec' (StrDec strbs, d) =
	      PPU.vHeaderFormats {header1 = "structure", header2 = "and"}
		(map (fn strb => fmtStrb sourceOp (strb, d-1)) strbs)

	  | fmtDec' (FctDec fctbs, d) =
	      PPU.vHeaderFormats {header1 = "functor", header2 = "and"}
		(map (fn fctb => fmtFctb sourceOp (fctb,d)) fctbs)

	  | fmtDec' (SigDec sigbs, d) =
	      let fun fmt (Sigb{name=fname, def}) =
		        PP.vcat (PP.hcat (PPS.fmtSym fname, PP.equal),
				 PP.hardIndent 4 (fmtSigExp sourceOp (def,d)))
		    | fmt (MarkSigb(sigb,r)) = fmt sigb
	       in PPU.vHeaderFormats {header1 = "signature", header2 = "and"}
		    (map fmt sigbs)
	      end

	  | fmtDec' (FsigDec fsigbs, d) =
	      PPU.vHeaderFormats {header1 = "funsig", header2 = "and"}
                (map (fn fsigb => fmtFsigb sourceOp (fsigb, d)) fsigbs)

	  | fmtDec' (LocalDec(inner,outer), d) =
	      PP.vblock
	       [PP.text "local",
	        PP.hardIndent 2 (fmtDec' (inner, d-1)),
		PP.text "in",
	        PP.hardIndent 2 (fmtDec' (outer, d-1)),
	        PP.text "end"]

	  | fmtDec' (SeqDec decs, d) =
	       PP.vblock (map (fn dec => fmtDec'(dec,d)) decs)

	  | fmtDec' (OpenDec paths, d) =
	      PP.label "open " (PP.hblock (map fmtPath paths))

	  | fmtDec' (OvldDec (sym, explist), d) = PP.label "overload" (PPS.fmtSym sym)

	  | fmtDec' (FixDec {fixity, ops}, d) =
              PP.hcat
	        (case fixity
		   of F.NONfix => PP.text "nonfix"
		    | F.INfix (i,_) =>
			PP.hcat
			  (if i mod 2 = 0
			   then PP.text "infix"
			   else PP.text "infixr",
			   if i div 2 > 0
			   then PP.integer (i div 2)
			   else PP.empty),
		 PP.pblock (map PPS.fmtSym ops))

	  | fmtDec'(MarkDec(dec,(s,e)),d) =
	     (case sourceOp
		of SOME source =>
		     PP.hcat
		       (PP.ccat (PP.text "@",
			         PP.parens
				   (PP.cblock [fmtPos (source,s), PP.comma, fmtPos (source,e)])),
			fmtDec' (dec, d))
		 | NONE => fmtDec' (dec, d))

     in fmtDec' (dec, depth)
    end

and fmtVb sourceOp (vb, d) =
    if d <= 0 then PP.text "<binding>" else
    (case vb
       of Vb{pat,exp,...} =>
            PP.pcat
	      (PP.hcat (fmtPat sourceOp (pat, d-1), PP.equal),
	       PP.softIndent 4 (fmtExp sourceOp (exp,d-1)))
	| MarkVb (vb,region) => fmtVb sourceOp (vb, d))

and fmtRvb sourceOp (rvb, d) =
    if d <= 0 then PP.text "<rec binding>" else
    (case rvb
      of Rvb {var, exp, ...} =>
           PP.pcat
	     (PP.hcat (PPS.fmtSym var, PP.equal),
	      PP.softIndent 4 (fmtExp sourceOp (exp,d-1)))
       | MarkRvb (rvb, _) => fmtRvb sourceOp (rvb, d))

and fmtFb sourceOp (fb, d) =
    if d <= 0 then PP.text "<FunBinding>" else
    (case fb
       of Fb (clauses, ops) =>
              PPU.vHeaderFormats {header1 = "", header2 = "|"}
	       (map (fn (cl: clause) => (fmtClause sourceOp (cl,d))) clauses)
	| MarkFb (fb, _) => fmtFb sourceOp (fb, d))

and fmtClause sourceOp (Clause {pats, resultty, exp}, d) =
    if d <= 0 then PP.text "<clause>" else
    let fun fmt {item: pat, fixity: symbol option, region: region} =
	    (case (fixity, item)
	       of (NONE, (FlatAppPat _ | ConstraintPat _ | LayeredPat _ | OrPat _)) =>
		    PP.parens (fmtPat sourceOp (item, d))
		| _ => fmtPat sourceOp (item, d))
	in PP.pblock
	    [PP.hcat
	      (PP.pblock (map fmt pats),
	       (case resultty
		  of SOME ty => PP.hcat (PP.colon, fmtTy sourceOp (ty,d))
		   | NONE => PP.empty)),
	     PP.equal,
	     fmtExp sourceOp (exp,d)]
	end

and fmtTb sourceOp (tb, d) =
    if d <= 0 then PP.text "<T.binding>" else
    (case tb
       of Tb {tyc, def, tyvars} =>
	      PP.hblock [PP.tupleFormats (map fmtTyvar tyvars),
			 PPS.fmtSym tyc, PP.equal, fmtTy sourceOp (def, d-1)]
	  | MarkTb (tb', _) => fmtTb sourceOp (tb', d))

and fmtDb sourceOp (db, d) =
    (case db
       of Db {tyc, tyvars, rhs, lazyp} =>
            PP.hblock
	      [fmtTyvars tyvars, PPS.fmtSym tyc, PP.equal, fmtDbrhs sourceOp (rhs,d)]
	| MarkDb (db, _) => fmtDb sourceOp (db, d))

and fmtDbrhs sourceOp (constrs : (S.symbol * Ast.ty option) list, d: int) =
    if d <= 0 then PP.text "<datatypebinding.rhs>" else
    let fun fmtConstr (sym: S.symbol, tv: Ast.ty option) =
	      (case tv
		 of SOME a =>
		    PP.hblock [PPS.fmtSym sym, PP.text "of", fmtTy sourceOp (a, d)]
		  | NONE => PPS.fmtSym sym)
    in  PP.sequence {alignment = PP.P, sep = PP.text " |"} (map fmtConstr constrs)
    end

and fmtEb sourceOp (eb, d) =
    if d <= 0 then PP.text "<exnbind>" else
    (case eb
       of EbGen{exn, etype} =>
	    (case etype
	       of SOME ty =>
	  	    PP.hblock [PPS.fmtSym exn, PP.colon, fmtTy sourceOp (ty,d-1)]
		 | NONE => PPS.fmtSym exn)
	| EbDef {exn, edef} => 
	    PP.hblock [PPS.fmtSym exn, PP.equal, fmtPath edef]
	| MarkEb (eb, _) => fmtEb sourceOp (eb, d))

and fmtStrb sourceOp (strb, d) =
    if d <= 0 then PP.text "<Strb>" else
    (case strb
       of Strb {name,def,constraint} =>
	    PP.hblock [PPS.fmtSym name, PP.equal, fmtStrExp sourceOp (def, d-1)]
	| MarkStrb (strb, _) => fmtStrb sourceOp (strb, d))

and fmtFctb sourceOp (fctb, d) =
    if d <= 0 then PP.text "<Fctb>" else
    (case fctb
       of Fctb {name, def = BaseFct{params, body, constraint}} =>
            PP.hcat
             (PPS.fmtSym name,
	      let fun fmtParam (SOME symbol, sigexp) =
			PP.parens (PP.hblock [PPS.fmtSym symbol, PP.colon, fmtSigExp sourceOp (sigexp, d)])
		    | fmtParam (NONE, sigexp) = PP.parens (fmtSigExp sourceOp (sigexp, d))
	       in PP.pblock
		    [PP.hblock (map fmtParam params),
		     PP.hcat
		       ((case constraint
			  of NoSig => PP.empty
			   | Transparent sigexp =>
			       PP.hcat (PP.colon, fmtSigExp sourceOp (sigexp,d))
			   | Opaque(sigexp) =>
			       PP.hcat (PP.text ":>", fmtSigExp sourceOp (sigexp,d))),
			PP.equal),
		     PP.softIndent 2 (fmtStrExp sourceOp (body,d))]
	       end)
	| Fctb {name, def} =>
            PP.hblock [PPS.fmtSym name, PP.equal, fmtFctExp sourceOp (def,d-1)]
	| MarkFctb (fctb, _) => fmtFctb sourceOp (fctb, d))

and fmtFsigb sourceOp (fsigb, d) =
    if d <= 0 then PP.text "<Fsigb>" else
    (case fsigb
       of Fsigb {name, def} =>
	    PP.pblock
	      [PP.text "funsig ", PPS.fmtSym name, PP.equal,
	       PP.softIndent 2 (fmtFsigExp sourceOp (def,d-1))]
	| MarkFsigb (fsigb', _) => fmtFsigb sourceOp (fsigb', d))

(* fmtTy : SRC.source option -> ty * int -> PP.format *)
and fmtTy sourceOp (ty, d) =
  let fun fmtTy' (_,0) = PP.text "<type>"
        | fmtTy' (VarTy t, _) = fmtTyvar t
	| fmtTy' (ConTy (tycon, []), _) = fmtPath tycon
	| fmtTy' (ConTy (tycon, args),d) =
	    (case tycon
	       of [tyc] =>
		   if S.eq (S.tycSymbol "->", tyc) then
		     (case args
			of [dom,ran] =>
			     PP.pblock [PP.hcat (fmtTy' (dom, d-1), PP.text "->"), fmtTy' (ran, d-1)]
			 | _ => bug "fmtTy: wrong args for -> type")
		   else PP.hcat (fmtTypeArgs (args, d), PPS.fmtSym tyc)
		| _ => PP.hcat (fmtTypeArgs (args, d), fmtPath tycon))
	| fmtTy' (RecordTy fields, d) =
            PP.braces
	      (PP.psequence PP.comma
		(map (fn (sym:symbol, tv:Ast.ty) =>
			 PP.hblock [PPS.fmtSym sym, PP.colon, fmtTy sourceOp (tv, d)]) fields))
	| fmtTy' (TupleTy tys, d) =
	    PP.sequence {alignment = PP.P, sep = PP.text "*"}
	      (map (fn ty => fmtTy sourceOp (ty, d)) tys)
	| fmtTy' (MarkTy (ty,_), d) = fmtTy sourceOp (ty, d)

      and fmtTypeArgs (tys, d) = PP.tuple (fn ty => fmtTy' (ty, d)) tys

   in fmtTy' (ty, d)
  end

end (* top-level local *)
end (* structure PPAst *)

(*
[Riehl, 4/28/2009]:
   Fixed some "bugs" in the pretty printer that were making
   "round trips" fail ((pp o parse o pp o parse) s != (pp o parse) s).
   Specifically:

   - The "fn" was not being prepended to FnExp's.

   - Removed parenthesis for sequence expressions of one
     subexpression.  (Iteratively sending this back to the parser kept
     adding nested parenthesis.)

   - A ">" was being appended after selector expressions ("#x" would
     pretty print as "#x>").

   - Fixed spelling error (was "stuct") and added whitespace between the
     structure body and the "end" keyword for BaseStr's.

   - Changed ":" to "=" for Strb's (note that constraints were not and
     are still not handled).

[DBM, 2022.09.26]
   Converted to use NewPP from old pretty printer library.

 *)
