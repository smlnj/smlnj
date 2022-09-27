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
  structure EM = ErrorMsg
  structure S = Symbol
  structure PP = NewPP
  structure PPU = NewPPUtil

  open Ast Fixity

  val internals = ParserControl.astInternals
  val lineprint = ParserControl.astLineprint

  fun bug msg = ErrorMsg.impossible("PPAst: "^msg)

in

(* fmtPos : Source.inputSource * int -> PP.format *)
fun fmtPos(source: Source.inputSource, charpos: int) =
    if (!lineprint) then
      let val {line,column,...} = Source.filepos source charpos
       in PP.concat [PP.integer line, PP.period, PP.integer column]
      end
    else PP.integer charpos

(* strength : Ast.ty -> int *)
fun strength(ty) =
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

fun stripMarkExp (MarkExp (a, _)) = stripMark a
  | stripMarkExp x = x

(* fmtPath : S.symbol list -> PP.format *)
fun fmtPath (symbols: S.symbol list) =
    PP.sequence {alignment = PP.C, sep = PP.period} (map PPU.fmtSym symbols)

(* fmtTyvar : tyvar -> PP.format *)
fun fmtTyvar tyvar =
    (case tyvar
       of Tyv sym => PPU.fmtSym sym
	| MarkTyv (tyvar', _) = fmtTyvar tyvar')

fun fmtPat sourceOp =
    let fun fmtPat' (WildPat, _) = PP.text "_"
	  | fmtPat' (VarPat p, d) = fmtPath p
	  | fmtPat' (IntPat (src, _), _) = PP.text src
	  | fmtPat' (WordPat(src, _), _) = PP.text src
	  | fmtPat' (StringPat s, _) = PP.text s
	  | fmtPat' (CharPat s, _) = PP.char s
	  | fmtPat' (LayeredPat {varPat,expPat},d) =
	      PP.pblock [fmtPat'(varPat, d), PP.text "as", fmtPat'(expPat,d-1)]
	  | fmtPat' (RecordPat {def=[], flexibility}, _) =
	      if flexibility then PP.text "{...}" else PP.text "()"
	  | fmtPat' (r as RecordPat{def,flexibility}, d) =
              PP.braces
		(PP.sequence {alignment = PP.P, sep = PP.comma}
		   (map (fn (sym,pat) => (PP.hblock [PPU.fmtSym sym, PP.equal, fmtPat' (pat, d-1)])) def
		    @ if flexibility then [PP.text "..."] else nil))
	  | fmtPat' (ListPat nil, d) = PP.text "[]"  (* may not need the special case *)
	  | fmtPat' (ListPat elems, d) = PP.listFormats (map (fn pat => fmtPat' (pat, d-1)) elems)
	  | fmtPat' (TuplePat elems, d) = PP.tupleFormats (map (fn pat => fmtPat' (pat, d-1)) elems)
	  | fmtPat' (FlatAppPat fap, d) = PP.hblock (map (fn {item,fixity,region} => fmtPat'(item,d-1)) fap
	  | fmtPat' (AppPat {constr, argument}, d) =
	      PP.hblock [fmtPat' (constr,d), PP.parens (fmtPat'(argument,d))]
	  | fmtPat' (ConstraintPat {pattern, constraint}, d) =
              PP.hblock [fmtPat' (pattern, d-1), PP.colon, fmtTy sourceOp (constraint, d)]
	  | fmtPat' (VectorPat nil, d) = PP.text "#[]"
	  | fmtPat' (VectorPat v, d) =
	      PP.ccat (PP.text "#", PP.listFormats (map (fn pat => fmtPat' (pat, d-1) v)))
	  | fmtPat' (MarkPat (pat, (s,e)), d) =
	      (case sourceOp
		 of SOME source =>
		      if !internals
		      then PP.enclose {front = PP.text "<", back = PP.text ">"}
			     (PP.hcat
				(PP.ccat (PP.text "MARK", PP.parens (PP.hblock [fmtPos s, PP.comma, fmtPos e])),
				 PP.hcat (PP.colon, fmtPat' (pat,d))))
		      else fmtPat' (pat,d)
		  | NONE => fmtPat' (pat,d))
          | fmtPat' (OrPat pats, d) =
	      PP.parens (PP.sequence {alignment = PP.P, sep = PP.text " |"}
				     (map (fn pat => fmtPat' (pat, d-1)) pats))
    in fmtPat'
    end

and fmtExp sourceOp (exp, depth) =
  let fun fmtExp' (_, 0) = PP.text "<exp>"
	| fmtExp' (VarExp p, _) = fmtPath p
	| fmtExp' (FnExp nil, _ ) = PP.text "<function>"
	| fmtExp' (FnExp rules, d) =
	    PP.hcat 
              (PP.text "fn", 
               PP.tryFlat
		 (PP.sequence {alignment = PP.V, sep = PP.text " |"}
			      (map (fn rule => fmtRule sourceOp (rule, d-1)))))
	| fmtExp' (FlatAppExp fap, d) =
	    PP.pblock (map (fn {item,...} => fmtExpClosed (item, d))) fap)
	| fmtExp' (AppExp {function,argument}, d) =
	    if d <= 0 then PP.text "<exp>" else
	       (case stripMark function
		  of VarExp v =>
		      PP.hcat (fmtPath v, fmtExpClosed (argument, d-1))
		   | rator =>
		      PP.hcat (fmtExpClosed (rator, d-1), fmtExpClosed (argument, d-1)))
	| fmtExp' (CaseExp {expr, rules}, d) =
            PP.parens
	      (PP.hcat (PP.text "case", fmtExp'(expr,d-1)),
	       PU.ppvlist ("of ","   | ",
		 (fn => fn r => fmtRule sourceOp (r,d-1)),
                  rules);
	       rparen();
	| fmtExp' (LetExp {dec, expr},_,d) =
	    PP.vblock
	      [PP.hcat (PP.text "let ", fmtDec sourceOp (dec, d-1)),
	       PP.hcat (PP.text "in ", fmtExp'(expr, d-1)),
	       PP.text "end"]
 	| fmtExp' (SeqExp exps, d) =
            let val defaultFmt =
		    PP.parens
		      (PU.sequence {alignment = PP.P, sep = PP.semicolon}
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
	| fmtExp' (IntExp(src, _), _) = PP.text src
	| fmtExp' (WordExp(src, _), _) = PP.text src
	| fmtExp' (RealExp(src, _), _) = PP.text src
	| fmtExp' (StringExp s, _) = PP.string s
	| fmtExp' (CharExp s, _) = PP.ccat (PP.text "#", PP.string s)
	| fmtExp' (r as RecordExp fields, d) =
	    let fun fmtField (name, exp) = (PPU.fmtSym name, PP.equal, fmtExp'(exp, d)))
	     in PP.braces
	          (PP.sequence {alignment = PP.P, sep = PP.comma}
			       (map fmtField fields))
            end
	| fmtExp' (ListExp exps, d) =
	    PP.listFormats (map (fn exp => fmtExp' (exp, d-1)) exps)
	| fmtExp' (TupleExp exps, d) =
	    PP.formatTuple (fn exp => (fmtExp'(exp, d-1))) exps
	| fmtExp' (SelectorExp name, d) =
	    PP.ccat (PP.text "#" PPU.fmtSym name)
	| fmtExp' (ConstraintExp {expr,constraint}, d) =
	    PP.hcat [fmtExp'(expr, d), PP.colon, fmtTy sourceOp (constraint,d)]
        | fmtExp'(HandleExp{expr,rules}, d) =
	    PP.vcat
	      (fmtExp' (expr, d-1),
	       PP.hcat (PP.text "handle",
			PPU.fmtVerticalFormats {header1 = "  ", header2 = "| "}
		          (map (fn r => fmtRule sourceOp (r,d-1)) rules)))
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
	 | fmtExp'(VectorExp nil,_,d) = PP.text "#[]"
	 | fmtExp'(VectorExp exps,_,d) =
	     PP.ccat (PP.text "#", PP.formatList (fn exp => fmtExp (exp, d-1)) exps)
	 | fmtExp'(MarkExp (exp,(s,e)), d) =
	      (case sourceOp
		 of SOME source =>
		      if !internals
		      then PP.enclose {front = PP.text "<", back = PP.text ">"}
			     (PP.hcat
				(PP.ccat (PP.text "MARK", PP.parens (PP.hblock [fmtPos s, PP.comma, fmtPos e])),
				 PP.hcat (PP.colon, fmtExp' (exp, d))))
		      else fmtExp' (exp, d)
		  | NONE => fmtExp' (exp, d))
	     end

     and fmtRule (Rule {pat,exp}, d) =
	 if d <= 0 then PP.text "<rule>"
	 else PP.pcat (PP.hcat (fmtPat sourceOp (pat, d-1), PP.text "=>"),
		       PP.softIndent 3 (fmtExpClosed (exp, d-1)))

     and fmtExpClosed (exp, d) = 
	 (case stripMarkExp exp
	   of (IFexp _ | CaseExp _ | FnExp _ | HandleExp _ | OrelseExp _) => 
	      PP.parens (fmtExp' (exp, d))
           | _ => fmtExp' (exp, d))

   in fmtExp' (exp, depth)
  end (* fun fmtExp *)


and fmtStrExp sourceOp (strexp, depth) =
    let fun  fmtStrExp' (_, 0) = PP.text "<strexp>"

	   | fmtStrExp' (VarStr p, d) = fmtPath p

           | fmtStrExp' (BaseStr(SeqDec nil), d) =
	       PP.hcat (PP.text "struct", PP.text "end")

           | fmtStrExp' (BaseStr de, d) =
               PP.vblock
                 [PP.text "struct",
                  PP.hardIndent 2 (fmtDec sourceOp (de, d-1)),
                  PP.text "end"]

	   | fmtStrExp' (ConstrainedStr (stre, constraint), d) =

                fmtStrExp' (stre, d-1);
                case constraint
                  of NoSig => ()
                   | Transparent sigexp =>
                     (PP.text " :"; PP.break {nsp=1,offset=2};
                      fmtSigExp sourceOp (sigexp, d-1))
                   | Opaque sigexp =>
                     (PP.text " :>"; PP.break {nsp=1,offset=2};
                      fmtSigExp sourceOp (sigexp, d-1));


	   | fmtStrExp' (AppStr (path, args), d) =
	       let fun argFormatter (strexp, _) = PP.parens (fmtStrExp' (strexp, d))
	       in PP.hcat
		    (fmtPath path,
                     PP.pblock (map argFormatter args))
               end

           | fmtStrExp' (AppStrI (path, args), d) =
	       let fun argFormatter (strexp, _) = PP.parens (fmtStrExp' (strexp, d))
	        in PP.hcat
		     (fmtPath(path),
		      PP.pblock (map argFormatter args))
               end

	   | fmtStrExp' (LetStr (dec, body), d) =
	       PP.vblock
	         [PP.hcat (PP.text "let", fmtDec sourceOp (dec,d-1)),
	          PP.hcat (PP.text " in", fmtStrExp'(body,d-1)),
		  PP.text "end"]

           | fmtStrExp'(MarkStr(body,(s,e)),d) =
               fmtStrExp' (body,d)

     in fmtStrExp' (strexp, depth)
    end

and fmtFctExp sourceOp =
    let val PP.text = PP.text 
	fun fmtFctExp' (_, 0) = PP.text "<fctexp>"
	  | fmtFctExp' (VarFct (path, _), _) = fmtPath path
          | fmtFctExp' (LetFct(dec,body),d) =
              PP.vblock
	        [PP.labeled ("let", fmtDec sourceOp (dec,d-1)),
		 PP.labeled (" in", fmtFctExp'(body,d-1)),
	         PP.text "end"]
	  | fmtFctExp'(AppFct(path,sblist, fsigconst),d) =
              PP.hcat
		(fmtPath path,
		 PP.parens
		   (PP.hblock
		      (map (fn strexp => fmtStrExp sourceOp (strexp, d-1)) sblist)))
	  | fmtFctExp'(MarkFct(body,(s,e)),d) = fmtFctExp' (body,d)
          | fmtFctExp'(BaseFct _, d) = ErrorMsg.impossible "fmtFctExp: BaseFct"
    in
	fmtFctExp'
    end

and ppWhereSpec sourceOp =
  let val pps = PP.text 
      fun ppWhereSpec' (_,0) = PP.text "<WhereSpec>"
        | ppWhereSpec' (WhType([],[],ty), d) = fmtTy sourceOp (ty, d)
        | ppWhereSpec' (WhType(slist,tvlist,ty),d) =
	    (  PP.text "type ";
		  PU.ppSequence 
		  {sep=(fn => (PP.space 1)),
		   pr=pr',
		   style=PU.INCONSISTENT}
		   (map fmtTyvar tvlist)
              PU.ppSequence 
		  {sep=(fn => (PP.space 1)),
		   pr=pr,
		   style=PU.INCONSISTENT}
		   (map PPU.fmtSym slist)
		   PP.text" ="; PP.space 1; fmtTy sourceOp (ty,d)
             )
        | ppWhereSpec'(WhStruct(slist,slist'),d) =
	      let fun pr _ sym = PPU.fmtSym sym
            in PP.text "structure "; PU.ppSequence 
                {sep=(fn => (PP.space 1)),
                 pr=pr,
                 style=PU.INCONSISTENT}
                 slist;PP.space 1;
                PU.ppSequence 
                {sep=(fn => (PP.space 1)),
                 pr=pr,
                 style=PU.INCONSISTENT}
                 slist'
             end
  in
      ppWhereSpec'
  end

and fmtSigExp sourceOp =
  let val {openVBox, openHOVBox, openHVBox, closeBox, pps, ppi, newline, break} = PU.en_pp 
      fun fmtSigExp'(_,0) = PP.text "<SigExp>"
	| fmtSigExp'(VarSig s,d) = (PPU.fmtSym s)
	| fmtSigExp'(AugSig (sign, wherel),d) =
           (fmtSigExp' (sign, d); break {nsp=1,offset=0};
	   (case sign
		of VarSig s => PU.ppvlist ("where ","and ",
		 (fn => fn r => ppWhereSpec sourceOp (r,d-1)),wherel)
		 | MarkSig(VarSig s, r) => PU.ppvlist ("where ","and ",
		 (fn => fn r => ppWhereSpec sourceOp (r,d-1)),wherel)
                 | _ => (newline ();  PU.ppvlist ("where ","and ",
		 (fn => fn r => ppWhereSpec sourceOp (r,d-1)),wherel))
		))
	| fmtSigExp'(BaseSig [],d) =
	    (PP.text "sig"; PP.nbSpace 1; PP.text"end")
	| fmtSigExp'(BaseSig specl,d) =
	  let fun pr speci = (ppSpec sourceOp (speci,d))
	   in (PP.openVBox (PP.Abs 0);
                PP.text "sig";
                PU.ppvseq 2 "" pr specl;
                PP.text "end";
               PP.closeBox )
	  end
	| fmtSigExp'(MarkSig (m,r),d) = fmtSigExp sourceOp (m,d)
  in
      fmtSigExp'
  end

and ppFsigExp sourceOp =
  let val pps = PP.text 
      fun ppFsigExp'(_,0) = PP.text "<FsigExp>"
	| ppFsigExp'(VarFsig s,d) = PPU.fmtSym s
	| ppFsigExp'(BaseFsig {param,result},d) =
	  let fun pr (SOME symbol, sigexp) =
		  (PU.PP.text "("; PPU.fmtSym symbol; PU.PP.text ":";
		   fmtSigExp sourceOp (sigexp, d);
                   PU.PP.text ")")
		| pr (NONE, sigexp) =
		  (PU.PP.text "("; fmtSigExp sourceOp (sigexp, d);
                   PU.PP.text ")")
	   in PU.ppSequence 
               {sep=(fn => (PP.newline )),
		pr = pr,
		style = PU.INCONSISTENT}
	       param;
	      PP.break {nsp=1,offset=2};
	      PP.text "=> ";
	      fmtSigExp sourceOp (result,d)
	  end

	| ppFsigExp'(MarkFsig (m,r),d) = ppFsigExp sourceOp (m,d)
  in
      ppFsigExp'
  end

and ppSpec sourceOp =
    let fun fmtDataBind dbing = (fmtDb sourceOp (dbing, d))
	fun fmtTypeBind tbing = (fmtTb sourceOp (tbing, d))

	fun fmtTyvar_list ([],d) = ()
          | fmtTyvar_list ([tyvar], d) = (fmtTyvar sourceOp (tyvar, d); PP.space 1)
          | fmtTyvar_list (tyvars, d) =
            PP.parens
              (PP.sequence {alignment = PP.P, sep = PP.comma}
	         (map (fn tyvar => fmtTyvar sourceOp (tyvar, d)) tyvars))

      fun ppSpec'(_,0) = PP.text "<Spec>"
	| ppSpec'(StrSpec sspo_list,d) =
	  let fun pr _ (symbol, sigexp, path) =
		  (case path
                    of SOME p => (PPU.fmtSym symbol; PP.text " = ";
                                  fmtSigExp sourceOp (sigexp,d);
                                  PP.space 1; fmtPath p)
                     | NONE => (PPU.fmtSym symbol; PP.text " = ";
                                fmtSigExp sourceOp (sigexp,d)))
	   in PU.ppClosedSequence 
	       {front=(C PP.text "structure "),
		sep=PU.sepWithSpc ",",
		back=(C PP.text ""),
		pr=pr,
		style=PU.INCONSISTENT}
	       sspo_list
	  end
	| ppSpec'(TycSpec (stto_list, bool),d) =
	  let fun pr _ (symbol, tyvar_list, tyo) =
		  (case tyo
		    of SOME ty =>
                       (pp_tyvar_list (tyvar_list,d);PPU.fmtSym symbol; PP.text "= ";
                        fmtTy sourceOp (ty, d))
		     | NONE =>  (pp_tyvar_list (tyvar_list,d);PPU.fmtSym symbol))
	   in PU.ppClosedSequence 
	        {front=(C PP.text "type "),
		 sep=(fn => (PP.text "|"; PP.newline )),
		 back=(C PP.text ""),
		 pr=pr,
		 style=PU.INCONSISTENT}
		stto_list
	  end
	| ppSpec'(FctSpec sf_list,d) =
	  let fun pr (symbol, fsigexp) =
                  (PPU.fmtSym symbol; PP.text " : ";
                   ppFsigExp sourceOp (fsigexp, d-1))
	   in openHVBox 0;
              PU.ppvlist ("functor ", "and ", pr, sf_list);
              closeBox ()
	  end
	| ppSpec'(ValSpec st_list,d) =
	  let fun pr (symbol, ty) =
                  (PPU.fmtSym symbol; PP.text ":"; fmtTy sourceOp (ty, d))
	   in openHVBox 0;
              PU.ppvlist ("val ", "and ", pr, st_list);
              closeBox ()
	  end
        | ppSpec'(DataReplSpec(name,path),d) =
	  if d = 0 then PP.text "<DT.repl>"
          else (openHOVBox 0;
		 PP.text "datatype "; PPU.fmtSym name; PP.text " =";
		 PP.space 1; PP.text "datatype ";
		 PU.ppSequence 
		   {sep=(fn => (PP.text ".")),
		    pr=PPU.fmtSym,
		    style=PU.INCONSISTENT}
		   path;
		closeBox ())
	| ppSpec'(DataSpec{datatycs,withtycs=[]},d) =
	  let fun pr (dbing) = (fmtDb sourceOp (dbing, d))
	   in openHVBox 0;
              PU.ppvlist ("datatype ", "and ", pr, datatycs);
              closeBox ()
	  end
	| ppSpec'(DataSpec {datatycs, withtycs},d) =
	  let fun prd (dbing) = (fmtDb sourceOp (dbing, d))
	      fun prw (tbing) = (fmtTb sourceOp (tbing, d))
	   in (openHVBox 0;
               PU.ppvlist ("datatype ", "and ", prd, datatycs);
	       PP.newline ;
               PU.ppvlist ("datatype ", "and ", prw, withtycs);
	       closeBox ())
	  end
	| ppSpec'(ExceSpec sto_list,d) =
	  let fun pr (symbol, tyo) =
		  (case tyo
		    of SOME ty =>
                       (PPU.fmtSym symbol; PP.text " : ";
                        fmtTy sourceOp (ty, d))
		     | NONE =>  PPU.fmtSym symbol)
	   in openHVBox 0;
              PU.ppvlist ("exception ", "and ", pr, sto_list);
              closeBox ()
	  end
	| ppSpec'(ShareStrSpec paths,d) =
	   (openHVBox 0;
            PU.ppvlist ("sharing ", " = ", fmtPath, paths);
            closeBox ())
        | ppSpec'(ShareTycSpec paths,d) =
	   (openHVBox 0;
            PU.ppvlist ("sharing type ", " = ", fmtPath, paths);
            closeBox ())
	| ppSpec'(IncludeSpec sigexp ,d) = fmtSigExp sourceOp (sigexp, d)
	| ppSpec'(MarkSpec (m,r),d) = ppSpec sourceOp (m,d)
  in
      ppSpec'
  end

and fmtDec sourceOp (dec, depth) =
    let fun fmtDataBind (dbing, d) = (fmtDb sourceOp (dbing, d))
	fun fmtTypeBind (tbing, d) = (fmtTb sourceOp (tbing, d))
	fun fmtDec' (_, 0) = PP.text "<dec>"
	  | fmtDec' (ValDec (vbs, tyvars), d) =
	     PPU.fmtVerticalFormats {header1 = "val", header2 = "and"}
	       (map (fn vb => fmtVb sourceOp (vb, d-1)) vbs)

	  | fmtDec' (ValrecDec (rvbs, tyvars), d) =
	     PPU.formatVerticalFormats {heading1 = "val rec", heading2 = "and"}
	       (map (fn rvb => fmtRvb sourceOp (rvb, d-1)) rvbs)

	  | fmtDec' (DoDec exp, d) =
	     PP.labeled ("do", fmtExp sourceOp (exp,d-1))

	  | fmtDec' (FunDec (fbs,tyvars), d) =
	     PPU.fmtVerticalFormats {header1 = "fun", header2 = "and"}
	       (map (fn fb => fmtFb sourceOp (fb, d-1)) fbs)

	  | fmtDec' (TypeDec tycs, d) =
	      PP.labeled "type"
	        (PP.pblock (map (fn tyc => fmtTb sourceOp (tyc, d-1)) tycs))

	  | fmtDec' (DatatypeDec{datatycs,withtycs=[]}, d) =
	      PP.labeled "datatype"
	        (PP.pblock (map (fn dbing => fmtDataBind (dbing, d-1)) datatycs))

	  | fmtDec' (DatatypeDec{datatycs,withtycs},d) =
	      PP.vcat
		(PP.labeled "datatype"
		   (PP.pblock (map (fn db => fmtDataBind (db, d)) datatycs)),
		 PP.labeled "withtype"
		   (PP.pblock (map (fn tb => fmtTypeBind (tb, d)) datatycs)))

	  | fmtDec' (DataReplDec(symb, path), _) =
	      PP.hblock
		[PP.text "datatype ", PPU.fmtSym symb, PP.equal, PP.text "datatype", fmtPath path]

	  | fmtDec' (AbstypeDec{abstycs,withtycs=[],body}, d) =
	      PP.vcat
		(PP.labeled "abstype"
		   (PP.pblock (map (fn db => fmtDataBind (db, d-1)) abstycs)),
		 fmtDec' (body, d))

	  | fmtDec' (AbstypeDec{abstycs,withtycs,body}, d) =
	      PP.vblock
		[PP.labeled "abstype"
		   (PP.pblock (map (fn db => fmtDataBind (db, d-1)) abstycs)),
		 PP.labeled "withtype"
		   (PP.pblock (map (fn db => fmtDataBind (db, d-1)) withtycs)),
		 fmtDec' (body, d)]

	  | fmtDec' (ExceptionDec ebs, d) =
	      PP.pblock (map (fn eb => fmtEb sourceOp (eb,d-1)) ebs)

	  | fmtDec' (StrDec strbs, d) =
	      PP.fmtVerticalFormats {header1 = "structure", header2 = "and"}
		(map (fn strb => fmtStrb sourceOp (strb, d-1)) strbs)

	  | fmtDec' (FctDec fctbs, d) =
	      PP.fmtVerticalFormats {header1 = "functor", header2 = "and"}
		(map (fn fctb => fmtFctb sourceOp (fctb,d)) fctbs)

	  | fmtDec' (SigDec sigbs, d) =
	      let fun fmt (Sigb{name=fname, def}) =
		        PP.vcat (PP.hcat (PPU.fmtSym fname, PP.equal),
				 PP.hardIndent 4 (fmtSigExp sourceOp (def,d)))
		    | fmt (MarkSigb(sigb,r)) = fmt sigb
	       in PPU.fmtVerticalFormats {header1 = "signature", header2 = "and"}
		    (map fmt sigbs)
	      end

	  | fmtDec' (FsigDec fsigbs, d) =
	      PPU.fmtVerticalFormats {header1 = "funsig", header2 = "and"}
                (map (fn fsigb => fmtFsigb sourceOp (fsigb, d)) fsigbs)

	  | fmtDec' (LocalDec(inner,outer), d) =
	      PP.vblock
	       [PP.text "local",
	        PP.hardIndent 2 (fmtDec'(inner,d-1)),
		PP.text "in",
	        PP.hardIndent 2 (fmtDec'(outer,d-1))
	        PP.text "end"]

	  | fmtDec' (SeqDec decs, d) =
	       PP.vblock (map (fn dec => fmtDec'(dec,d)) decs)

	  | fmtDec' (OpenDec paths, d) =
	      PP.labeled "open " (PP.hblock (map fmtPath paths))

	  | fmtDec' (OvldDec (sym, explist), d) = PP.labeled "overload" (PPU.fmtSym sym)

	  | fmtDec' (FixDec {fixity, vars}, d) =
              PP.hcat
	        (case fixity
		   of NONfix => PP.text "nonfix"
		    | INfix (i,_) =>
			PP.hcat
			  (if i mod 2 = 0
			   then PP.text "infix"
			   else PP.text "infixr",
			   if i div 2 > 0
			   then PP.integer (i div 2)
			   else PP.empty),
		 PP.pblock (map PPU.fmtSym vars))

	  | fmtDec'(MarkDec(dec,(s,e)),d) =
	     (case sourceOp
		of SOME source =>
		     PP.hcat
		       (PP.ccat (PP.text "@",
			         PP.parens
				   (PP.concat [fmtPos (source,s), PP.comma, fmtPos (source,e); PP.text ")"])),
			fmtDec' (dec, d))
		 | NONE => fmtDec' (dec, d))

     in fmtDec'
    end

and fmtVb sourceOp (vb, d) =
    if d <= 0 then PP.text "<binding>" else
    (case vb
       of Vb{pat,exp,...} =
            PP.pcat
	      (PP.hcat (ppPat sourceOp (pat, d-1), PP.equal),
	       PP.softIndent 4 (fmtExp sourceOp (exp,d-1)))
	| MarkVb (vb,region) = fmtVb sourceOp (vb, d))

and fmtRvb sourceOp (rvb, d) =
    if d <= o then PP.text "<rec binding>" else
    (case rvb
      of Rvb {var, exp, ...} =
           PP.pcat
	     (PP.hcat (PPU.fmtSym var, PP.equal),
	      PP.softIndent 4 (fmtExp sourceOp (exp,d-1)))
       | MarkRvb (rvb, _) = fmtRvb sourceOp (rvb, d))

and fmtFb sourceOp (fb, d) =
    if d <= 0 then PP.text "<FunBinding>" else
    (case fb
       of Fb (clauses, ops) =>
              PPU.fmtVertialFormats {header1 = "", header2 = "|"}
	       (map (fn (cl: clause) => (fmtClause sourceOp (cl,d))) clauses)
	| MarkFb (fb, _) = fmtFb sourceOp (fb, d))

and fmtClause sourceOp (Clause {pats, resultty, exp}, d) =
    if d <= 0 then PP.text "<clause>" else
    let fun fmt {item:pat, fixity:symbol option, region:region} =
	    (case (fixity, item)
	       of (NONE, (FlatAppPat p | ConstraintPat p | LayeredPat p | OrPat p)) =>
		    PP.parens (ppPat sourceOp (item,d))
		| _ => ppPat sourceOp (item,d))
	in PP.pblock
	    [PP.hcat
	      (PP.pblock (map fmt pats),
	       (case resultty
		  of SOME ty => (PP.colon, fmtTy sourceOp (ty,d))
		   | NONE => PP.empty)),
	     PP.equal,
	     fmtExp sourceOp (exp,d)]
	end

and fmtTb sourceOp (tb, d) =
    if d <= 0 then PP.text "<T.binding>" else
    (case tb
       of Tb {tyc, def, tyvars} =>
	      PP.hblock [PP.tupleFormats (map fmtTyvar tyvars),
			 PPU.fmtSym tyc, PP.equal, fmtTy sourceOp (def, d-1)]
	  | MarkTb (tb', _) => fmtTb sourceOp (tb', d))

and fmtDb sourceOp (db, d) =
    let fun fmtTyvars (tyvars: S.symbol list) =
	    PP.sequence {alignment = PP.P, sep = PP.comma} (map fmtTyvar symbols)
     in case db
	  of Db {tyc,tyvars,rhs,lazyp} =>
               PP.hblock
	         [fmtTyvars tyvars, PPU.fmtSym tyc, PP.equal, fmtDbRhs sourceOp (rhs,d)]
	   | MarkDb (db, _) = fmtDb sourceOp (db, d)
    end

and fmtDbRhs sourceOp (constrs : (S.symbol * Ast.ty option) list, d: int) =
    if d <= 0 then PP.text "<datatypebinding.rhs>" else
    let fun fmtConstr (sym: S.symbol, tv: Ast.ty option) =
	      (case tv
		 of SOME a =>
		    PP.hblock [PPU.fmtSym sym, PP.text "of", fmtTy sourceOp (a, d)]
		  | NONE =>
		    PPU.fmtSym sym)
    in  PP.sequence {alignment = PP.P, sep = PP.text " |"} (map fmtConstr constrs)
    end

and fmtEb sourceOp (eb, d) =
    if d <= 0 then PP.text "<exnbind>" else
    (case eb
       of EbGen{exn, etype} =>
	    (case etype
	       of SOME ty =>
	  	    PP.hblock [PPU.fmtSym exn, PP.colon, fmtTy sourceOp (ty,d-1)]
		 | NONE => PPU.fmtSym exn)
	| EbDef {exn, edef} => 
	    PP.hblock [PPU.fmtSym exn, PP.equal, fmtPath edef]
	| MarkEb (eb, _) => fmtEb sourceOp (eb, d))

and fmtStrb sourceOp (strb, depth) =
    if depth <= 0 then PP.text "<Strb>" else
    (case strb
       of Strb {name,def,constraint} =
	    PP.hblock [PPU.fmtSym name, PP.equal, fmtStrExp sourceOp (def, d-1)]
	| MarkStrb (strb, _) = fmtStrb sourceOp (strb, d))

and fmtFctb sourceOp (fctb, d) =
    if d <= 0 then PP.text "<Fctb>" else
    (case fctb
       of Fctb {name, def = BaseFct{params, body, constraint}} =>
            PP.hcat
             (PPU.fmtSym name,
	      let fun fmtParam (SOME symbol, sigexp) =
			PP.parens (PP.hblock [PPU.fmtSym symbol, PP.colon, fmtSigExp sourceOp (sigexp, d)])
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
            PP.hblock [PPU.fmtSym name, PP.equal, fmtFctExp sourceOp (def,d-1)]
	| MarkFctb (fctb, _) => fmtFctb sourceOp (fctb, d))

and fmtFsigb sourceOp (fsigb, d) =
    if d <= 0 then PP.text "<Fsigb>" else
    (case fsigb
       of Fsigb {name, def} =
	    PP.pblock
	      [PP.text "funsig ", PPU.fmtSym name, PP.equal,
	       PP.softIndent 2 (fmtFsigExp sourceOp (def,d-1))]
	| MarkFsigb (fsigb', _) = fmtFsigb sourceOp (fsigb', d))

and fmtTy sourceOp =
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
			 | _ => EM.impossible "fmtTy: wrong args for -> type")
		   else PP.hcat (fmtTypeArgs (args, d), PPU.fmtSym tyc)
		| _ => PP.hcat (fmtTypeArgs (args, d), fmtPath tycon))
	| fmtTy' (RecordTy fields, d) =
            PP.braces
	      (PP.sequence {alignment = PP.P, sep = PP.comma}
		(map (fn (sym:symbol, tv:Ast.ty) => (PPU.fmtSym sym, PP.colon, fmtTy sourceOp (tv, d))) fields))
	| fmtTy' (TupleTy tys, d) =
	    PP.sequence {alignment = PP.P, sep = PP.text "*"}
	      (map (fn ty => fmtTy sourceOp (ty, d)) tys)
	| fmtTy' (MarkTy (ty,_), d) = fmtTy sourceOp (ty, d)

      and fmtTypeArgs (tys, d) = PP.formatTuple (fn ty => fmtTy' (ty, d)) tys

   in fmtTy'
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
