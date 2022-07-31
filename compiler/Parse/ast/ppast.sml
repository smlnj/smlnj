(* ppast.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Authors: Jing Cao and Lukasz Ziarek
 *)

structure PPAst: PPAST =
struct

local structure EM = ErrorMsg
      structure S = Symbol
      structure PP = PrettyPrint
      structure PU = PPUtil

      open Ast Fixity
in

val internals = ParserControl.astInternals

val lineprint = ref false

fun C f x y = f y x

fun prpos(ppstrm: PP.stream,
          source: Source.inputSource, charpos: int) =
    if (!lineprint) then
      let val {line,column,...} = Source.filepos source charpos
       in PP.string ppstrm (Int.toString line);
	  PP.string ppstrm ".";
	  PP.string ppstrm (Int.toString column)
      end
    else PP.string ppstrm (Int.toString charpos)

fun bug msg = ErrorMsg.impossible("PPAst: "^msg)

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

fun stripMark (MarkExp(a,_)) = stripMark a
  | stripMark x = x

fun pp_path ppstrm symbols =
    let fun pr ppstrm (symbol) = (PU.ppSym ppstrm symbol)
     in PU.ppSequence ppstrm
         {sep=(fn ppstrm => (PP.string ppstrm ".")),
          pr=pr,
          style=PU.INCONSISTENT}
         symbols
    end

fun ppPat sourceOp ppstrm =
    let val {openHOVBox, openHVBox, closeBox, pps, ppi, ...} = PU.en_pp ppstrm
        val pp_symbol_list = pp_path ppstrm
	fun ppPat' (WildPat,_) = (pps "_")
	  | ppPat' (VarPat p, d) =  pp_symbol_list(p)
	  | ppPat' (IntPat(src, _),_) = pps src
	  | ppPat' (WordPat(src, _),_) = pps src
	  | ppPat' (StringPat s, _) = PU.ppString ppstrm s
	  | ppPat' (CharPat s,_) = (pps "#"; PU.ppString ppstrm s)
	  | ppPat' (LayeredPat {varPat,expPat},d) =
	      (openHVBox 0;
	       ppPat'(varPat,d); pps " as "; ppPat'(expPat,d-1);
	       closeBox ())
	  | ppPat' (RecordPat{def=[],flexibility},_) =
	      if flexibility then pps "{...}"
	      else pps "()"
	  | ppPat' (r as RecordPat{def,flexibility},d) =
              (PU.ppClosedSequence ppstrm
		 {front=(C PP.string "{"),
                  sep=PU.sepWithCut ",",
                  back=(fn ppstrm => if flexibility then PP.string ppstrm ",...}"
                                     else PP.string ppstrm "}"),
                  pr=(fn ppstrm => fn (sym,pat) => (PU.ppSym ppstrm sym;
                                                    PP.string ppstrm "=";
						    ppPat' (pat, d-1))),
                  style=PU.INCONSISTENT}
                 def)
	  | ppPat' (ListPat nil, d) = pps "[]"
	  | ppPat' (ListPat l, d) =
		let fun pr _ pat = ppPat'(pat, d-1)
		in PU.ppClosedSequence ppstrm
		   {front=(C PP.string "["),
		    sep=PU.sepWithCut ",",
		    back=(C PP.string "]"),
	 	    pr=pr,
		    style=PU.INCONSISTENT}
		   l
		end
	  | ppPat' (TuplePat t, d) =
	    	let fun pr _ pat = ppPat'(pat, d-1)
	    	in PU.ppClosedSequence ppstrm
			     {front=(C PP.string "("),
			      sep=PU.sepWithCut ",",
			      back=(C PP.string ")"),
			      pr=pr,
			      style=PU.INCONSISTENT}
			     t
	    	end
	  | ppPat' (FlatAppPat fap, d) =
		let fun pr _ {item,fixity,region} = ppPat'(item,d-1)
		in PU.ppSequence ppstrm
			{sep=(fn ppstrm => (PP.space ppstrm 1)),
			 pr=pr,
			 style=PU.INCONSISTENT}
			fap
		end
	  | ppPat' (AppPat {constr, argument}, d) =
		(openHVBox 0;
		 ppPat'(constr,d); pps " as "; ppPat'(argument,d);
		 closeBox ())
	  | ppPat' (ConstraintPat {pattern, constraint}, d) =
		(openHOVBox 0;
		 ppPat' (pattern, d-1); pps " :";
		 PP.break ppstrm {nsp=1,offset=2};
		 ppTy sourceOp ppstrm (constraint, d);
		 closeBox ())
	  | ppPat' (VectorPat nil, d) = pps "#[]"
	  | ppPat' (VectorPat v, d) =
		let fun pr _ pat = ppPat'(pat, d-1)
		in PU.ppClosedSequence ppstrm
		   {front=(C PP.string "#["),
		    sep=PU.sepWithSpc ",",
		    back=(C PP.string "]"),
	 	    pr=pr,
		    style=PU.INCONSISTENT}
		   v
		end
	  | ppPat' (MarkPat (pat, (s,e)), d) =
	    (case sourceOp
		of SOME source =>
		     if !internals
		     then (pps "<MARK(";
			   prpos(ppstrm,source,s); pps ",";
			   prpos(ppstrm,source,e); pps "): ";
			   ppPat'(pat,d); pps ">")
		     else ppPat'(pat,d)
	         | NONE => ppPat'(pat,d))

          | ppPat' (OrPat orpat, d) =
		let fun pr _ pat = ppPat'(pat, d-1)
		in PU.ppClosedSequence ppstrm
			{front=(C PP.string "("),
			 sep=(fn ppstrm => (PP.space ppstrm 1; PP.string ppstrm "| ")),
			 back=(C PP.string ")"),
			 pr=pr,
			 style=PU.INCONSISTENT}
		end (orpat)

    in ppPat'
    end


and ppExp sourceOp ppstrm =
  let val {openHOVBox, openHVBox, closeBox, pps, ppi, ...} = PU.en_pp ppstrm
      fun lparen() = pps "("
      fun rparen() = pps ")"
      fun lpcond(atom) = if atom then pps "(" else ()
      fun rpcond(atom) = if atom then pps ")" else ()
      val pp_symbol_list = pp_path ppstrm
      fun ppExp' (_,_,0) = pps "<exp>"
	| ppExp' (VarExp p,_,_) = pp_symbol_list(p)
	| ppExp' (FnExp nil,_,d) = pps "<function>"
	| ppExp' (FnExp rules,_,d)=
		let fun pr _ pat = ppRule sourceOp ppstrm(pat, d-1)
		in
                    pps "fn"; PP.space ppstrm 1;
                    PU.ppSequence ppstrm
		               {sep=(fn ppstrm => (PP.string ppstrm "|"; PP.cut ppstrm)),
	 	                pr=pr,
		                style=PU.INCONSISTENT}
		               rules
		end
	| ppExp' (FlatAppExp fap,_,d) =
		let fun pr _ {item,fixity,region} = ppExp'(item,true, d)
		in PU.ppSequence ppstrm
			{sep=(fn ppstrm => (PP.space ppstrm 1)),
			 pr=pr,
			 style=PU.INCONSISTENT}
			fap
		end
	| ppExp'(AppExp{function,argument},atom,d) =
	       (lpcond(atom);
		ppAppExp(function,argument,d);
		rpcond(atom))
	| ppExp' (CaseExp {expr, rules},_,d) =
	      (openHVBox 0;
	       pps "(case "; ppExp'(expr,true,d-1); PU.nl_indent ppstrm 2;
	       PU.ppvlist ppstrm ("of ","   | ",
		 (fn ppstrm => fn r => ppRule sourceOp ppstrm (r,d-1)),
                  rules);
	       rparen();
	       closeBox ())
	| ppExp' (LetExp {dec, expr},_,d) =
	      (openHVBox 0;
		pps "let ";
		openHVBox 0;
		 ppDec sourceOp ppstrm (dec,d-1);
		closeBox ();
		PP.space ppstrm 1;
		pps "in ";
		openHVBox 0;
		 ppExp'(expr,false,d-1);
		closeBox ();
		PP.space ppstrm 1;
		pps "end";
	       closeBox ())
 	| ppExp'(SeqExp exps,_,d) =
                let fun parenThunk () = PU.ppClosedSequence ppstrm
	        {front=(C PP.string "("),
		 sep=PU.sepWithSpc ";",
		 back=(C PP.string ")"),
		 pr=(fn _ => fn exp => ppExp'(exp,false,d-1)),
		 style=PU.INCONSISTENT}
		exps
                    fun subExpCount (MarkExp (expr, _)) = subExpCount expr
                      | subExpCount (FlatAppExp subexps) = length subexps
                      | subExpCount _ = 1
                in case exps
                    of [expr] => if (subExpCount expr) < 2
                                 then ppExp'(expr,false,d-1)
                                 else parenThunk()
                     | _ => parenThunk()
                end
	| ppExp' (IntExp(src, _),_,_) = pps src
	| ppExp' (WordExp(src, _),_,_) = pps src
	| ppExp' (RealExp(src, _),_,_) = pps src
	| ppExp' (StringExp s,_,_) = PU.ppString ppstrm s
	| ppExp' (CharExp s,_,_) = (pps "#"; PU.ppString ppstrm s)
	| ppExp'(r as RecordExp fields,_,d) =
	   (PU.ppClosedSequence ppstrm
	      {front=(C PP.string "{"),
	       sep=PU.sepWithCut ",",
	       back=(C PP.string "}"),
	       pr=(fn ppstrm => fn (name,exp) =>
		      (PU.ppSym ppstrm name; pps "=";
		       ppExp'(exp,false,d))),
	       style=PU.INCONSISTENT}
	      fields)
	| ppExp' (ListExp p,_,d) =
		 PU.ppClosedSequence ppstrm
		     {front=(C PP.string "["),
		      sep=PU.sepWithCut ",",
		      back=(C PP.string "]"),
		      pr=(fn ppstrm => fn exp =>
			  (ppExp'(exp,false,d-1))),
		      style=PU.INCONSISTENT}
		     p
	| ppExp' (TupleExp p,_,d)=
		PU.ppClosedSequence ppstrm
		     {front=(C PP.string "("),
		      sep=PU.sepWithCut ",",
		      back=(C PP.string ")"),
		      pr=(fn ppstrm => fn exp =>
			  (ppExp'(exp,false,d-1))),
		      style=PU.INCONSISTENT}
		     p
	| ppExp'(SelectorExp name, atom,d) =
	      (openHVBox 0;
	        lpcond(atom);
	        pps "#"; PU.ppSym ppstrm name;
		rpcond(atom);
	       closeBox ())
	| ppExp' (ConstraintExp {expr,constraint},atom,d) =
	     (openHOVBox 0;
	       lpcond(atom);
	       ppExp'(expr,false,d); pps ":";
	       PP.break ppstrm {nsp=1,offset=2};
	       ppTy sourceOp ppstrm (constraint,d);
	       rpcond(atom);
	      closeBox ())
        | ppExp'(HandleExp{expr,rules},atom,d) =
	     (openHVBox 0;
	       lpcond(atom);
	       ppExp'(expr,atom,d-1); PP.newline ppstrm; pps "handle ";
	       PU.nl_indent ppstrm 2;
	       PU.ppvlist ppstrm ("  ","| ",
		  (fn ppstrm => fn r => ppRule sourceOp ppstrm (r,d-1)), rules);
	       rpcond(atom);
	      closeBox ())
	| ppExp' (RaiseExp exp,atom,d) =
	      (openHVBox 0;
	       lpcond(atom);
	       pps "raise "; ppExp'(exp,true,d-1);
	       rpcond(atom);
	       closeBox ())
	| ppExp' (IfExp { test, thenCase, elseCase },atom,d) =
	      (openHVBox 0;
	       lpcond(atom);
	       pps "if ";
	       openHVBox 0;
	        ppExp' (test, false, d-1);
	       closeBox ();
	       PP.space ppstrm 1;
	       pps "then ";
	       openHVBox 0;
	        ppExp' (thenCase, false, d-1);
	       closeBox ();
	       PP.space ppstrm 1;
	       pps "else ";
	       openHVBox 0;
	        ppExp' (elseCase, false, d-1);
	       closeBox ();
	       rpcond(atom);
	       closeBox ())
	| ppExp' (AndalsoExp (e1, e2),atom,d) =
	      (openHVBox 0;
	       lpcond(atom);
	       openHVBox 0;
	       ppExp' (e1,true,d-1);
	       closeBox ();
	       PP.space ppstrm 1;
	       pps "andalso ";
	       openHVBox 0;
	       ppExp' (e2,true,d-1);
	       closeBox ();
	       rpcond(atom);
	       closeBox ())
	 | ppExp' (OrelseExp (e1, e2),atom,d) =
	      (openHVBox 0;
	       lpcond(atom);
	       openHVBox 0;
	       ppExp' (e1,true,d-1);
	       closeBox ();
	       PP.space ppstrm 1;
	       pps "orelse ";
	       openHVBox 0;
	       ppExp' (e2,true,d-1);
	       closeBox ();
	       rpcond(atom);
	       closeBox ())
	 | ppExp' (WhileExp { test, expr },atom,d) =
	      (openHVBox 0;
	       pps "while ";
	       openHVBox 0;
	        ppExp'(test,false,d-1);
	       closeBox ();
	       PP.space ppstrm 1;
	       pps "do ";
	       openHVBox 0;
	         ppExp'(expr,false,d-1);
	       closeBox ();
	       closeBox ())

	 | ppExp'(VectorExp nil,_,d) = pps "#[]"
	 | ppExp'(VectorExp exps,_,d) =
	      let fun pr _ exp = ppExp'(exp,false,d-1)
	      in  PU.ppClosedSequence ppstrm
		    {front=(C PP.string "#["),
		     sep=PU.sepWithSpc ",",
		     back=(C PP.string "]"),
		     pr=pr,
		     style=PU.INCONSISTENT}
		    exps
	      end
	 | ppExp'(MarkExp (exp,(s,e)),atom,d) =
	      (case sourceOp
		of SOME source =>
		     if !internals
		     then (pps "<MARK(";
			   prpos(ppstrm,source,s); pps ",";
			   prpos(ppstrm,source,e); pps "): ";
			   ppExp'(exp,false,d); pps ">")
		     else ppExp'(exp,atom,d)
	         | NONE => ppExp'(exp,atom,d))
	 and ppAppExp (_,_,0) = PP.string ppstrm "<exp>"
	   | ppAppExp (function,argument,d) =
             let val {openHOVBox, openHVBox, closeBox, pps, ppi, ...} = PU.en_pp ppstrm
	      in case stripMark function
		  of VarExp v =>
		     (openHOVBox 2;
		      pp_symbol_list v; PP.space ppstrm 1;
		      ppExp'(argument,true,d-1);
		      closeBox ())
		   | rator =>
		     (openHOVBox 2;
		      ppExp'(rator,true,d-1); PP.break ppstrm {nsp=1,offset=2};
		      ppExp'(argument,true,d-1);
		      closeBox ())
	     end
  in (fn(exp,depth)=> ppExp'(exp,false,depth))
  end

and ppRule sourceOp ppstrm (Rule{pat,exp},d) =
    if d>0 then (PP.openHVBox ppstrm (PP.Rel 0);
	  ppPat sourceOp ppstrm (pat,d-1);
	  PP.string ppstrm " =>"; PP.break ppstrm {nsp=1,offset=2};
	  ppExp sourceOp ppstrm (exp,d-1);
	  PP.closeBox ppstrm)
    else PP.string ppstrm "<rule>"

and ppStrExp sourceOp ppstrm =
    let val {openHOVBox, openHVBox, closeBox, pps, ppi, ...} = PU.en_pp ppstrm
        val pp_symbol_list = pp_path ppstrm
        fun  ppStrExp'(_,0) = pps "<strexp>"

	   | ppStrExp'(VarStr p, d) = pp_symbol_list(p)

           | ppStrExp'(BaseStr(SeqDec nil), d) =
	       (pps "struct"; PP.nbSpace ppstrm 1; pps "end")
           | ppStrExp'(BaseStr de, d) =
               (PP.openVBox ppstrm (PP.Rel 0);
                pps "struct";  PU.nl_indent ppstrm 2;
                ppDec sourceOp ppstrm (de, d-1);
                PP.space ppstrm 1;
                pps "end";
                PP.closeBox ppstrm)
	   | ppStrExp'(ConstrainedStr (stre, constraint), d) =
               (openHOVBox 0;
                ppStrExp' (stre, d-1);
                case constraint
                  of NoSig => ()
                   | Transparent sigexp =>
                     (pps " :"; PP.break ppstrm {nsp=1,offset=2};
                      ppSigExp sourceOp ppstrm (sigexp, d-1))
                   | Opaque sigexp =>
                     (pps " :>"; PP.break ppstrm {nsp=1,offset=2};
                      ppSigExp sourceOp ppstrm (sigexp, d-1));
                closeBox ())
	   | ppStrExp'(AppStr (path, str_list), d) =
	       let fun pr ppstrm (strl, bool) =
                       (pps "("; ppStrExp sourceOp ppstrm (strl,d); pps ")")
	        in pp_symbol_list(path); PU.ppSequence ppstrm
		    {sep=(fn ppstrm => (PP.space ppstrm 1)),
		     pr=pr,
		     style=PU.INCONSISTENT}
		    str_list
               end

           | ppStrExp'(AppStrI (path, str_list), d) =
	       let fun pr ppstrm (strl, bool) =
                       (pps "("; ppStrExp sourceOp ppstrm (strl,d); pps ")")
	        in pp_symbol_list(path); PU.ppSequence ppstrm
		    {sep=(fn ppstrm => (PP.space ppstrm 1)),
		     pr=pr,
		     style=PU.INCONSISTENT}
		    str_list
               end

	   | ppStrExp'(LetStr(dec,body),d) =
	      (openHVBox 0;
	       pps "let "; ppDec sourceOp ppstrm (dec,d-1);
               PP.newline ppstrm;
	       pps " in "; ppStrExp'(body,d-1); PP.newline ppstrm;
	       pps "end";
	       closeBox ())

         | ppStrExp'(MarkStr(body,(s,e)),d) =
             ppStrExp' (body,d)
    in
	ppStrExp'
    end

and ppFctExp sourceOp ppstrm =
    let val pps = PP.string ppstrm
        val pp_symbol_list = pp_path ppstrm
	fun ppFctExp'(_, 0) = pps "<fctexp>"
	  | ppFctExp'(VarFct (p, _), d) = pp_symbol_list(p)
          | ppFctExp'(LetFct(dec,body),d) =
	      (PP.openHVBox ppstrm (PP.Rel 0);
	        pps "let "; ppDec sourceOp ppstrm (dec,d-1);
                PP.newline ppstrm;
	        pps " in "; ppFctExp'(body,d-1); PP.newline ppstrm;
	        pps "end";
	       PP.closeBox ppstrm)
	  | ppFctExp'(AppFct(path,sblist, fsigconst),d) =
              let fun pr ppstrm (strexp, _) =
                      (pps "("; ppStrExp sourceOp ppstrm (strexp,d); pps ")")
               in PP.openHVBox ppstrm (PP.Rel 0);
                  pp_symbol_list path;
                  PU.ppSequence ppstrm
		     {sep=(fn ppstrm => (PP.space ppstrm 1)),
		      pr = pr,
		      style = PU.INCONSISTENT}
		     sblist;
                  PP.closeBox ppstrm
              end
	  | ppFctExp'(MarkFct(body,(s,e)),d) =
	     ppFctExp' (body,d)
          | ppFctExp'(BaseFct _, d) = ErrorMsg.impossible "ppFctExp: BaseFct"
    in
	ppFctExp'
    end

and ppWhereSpec sourceOp ppstrm =
  let val pps = PP.string ppstrm
      fun ppWhereSpec'(_,0) = pps "<WhereSpec>"
        | ppWhereSpec'(WhType([],[],ty),d) = ppTy sourceOp ppstrm (ty, d)
        | ppWhereSpec'(WhType(slist,tvlist,ty),d) =
            let fun pr _ sym = PU.ppSym ppstrm sym
                fun pr' _ tyv = ppTyvar sourceOp ppstrm (tyv,d)
	      in  pps "type "; PU.ppSequence ppstrm
		  {sep=(fn ppstrm => (PP.space ppstrm 1)),
		   pr=pr',
		   style=PU.INCONSISTENT}
		   tvlist; PP.space ppstrm 1;
              PU.ppSequence ppstrm
		  {sep=(fn ppstrm => (PP.space ppstrm 1)),
		   pr=pr,
		   style=PU.INCONSISTENT}
		   slist;
		   pps" ="; PP.space ppstrm 1; ppTy sourceOp ppstrm (ty,d)
	      end
        | ppWhereSpec'(WhStruct(slist,slist'),d) =
	      let fun pr _ sym = PU.ppSym ppstrm sym
            in pps "structure "; PU.ppSequence ppstrm
                {sep=(fn ppstrm => (PP.space ppstrm 1)),
                 pr=pr,
                 style=PU.INCONSISTENT}
                 slist;PP.space ppstrm 1;
                PU.ppSequence ppstrm
                {sep=(fn ppstrm => (PP.space ppstrm 1)),
                 pr=pr,
                 style=PU.INCONSISTENT}
                 slist'
             end
  in
      ppWhereSpec'
  end

and ppSigExp sourceOp ppstrm =
  let val {openVBox, openHOVBox, openHVBox, closeBox, pps, ppi, newline, break} = PU.en_pp ppstrm
      fun ppSigExp'(_,0) = pps "<SigExp>"
	| ppSigExp'(VarSig s,d) = (PU.ppSym ppstrm s)
	| ppSigExp'(AugSig (sign, wherel),d) =
           (ppSigExp' (sign, d); break {nsp=1,offset=0};
	   (case sign
		of VarSig s => PU.ppvlist ppstrm ("where ","and ",
		 (fn ppstrm => fn r => ppWhereSpec sourceOp ppstrm (r,d-1)),wherel)
		 | MarkSig(VarSig s, r) => PU.ppvlist ppstrm ("where ","and ",
		 (fn ppstrm => fn r => ppWhereSpec sourceOp ppstrm (r,d-1)),wherel)
                 | _ => (newline ();  PU.ppvlist ppstrm ("where ","and ",
		 (fn ppstrm => fn r => ppWhereSpec sourceOp ppstrm (r,d-1)),wherel))
		))
	| ppSigExp'(BaseSig [],d) =
	    (pps "sig"; PP.nbSpace ppstrm 1; pps"end")
	| ppSigExp'(BaseSig specl,d) =
	  let fun pr ppstrm speci = (ppSpec sourceOp ppstrm (speci,d))
	   in (PP.openVBox ppstrm (PP.Abs 0);
                pps "sig";
                PU.ppvseq ppstrm 2 "" pr specl;
                pps "end";
               PP.closeBox ppstrm)
	  end
	| ppSigExp'(MarkSig (m,r),d) = ppSigExp sourceOp ppstrm (m,d)
  in
      ppSigExp'
  end

and ppFsigExp sourceOp ppstrm =
  let val pps = PP.string ppstrm
      fun ppFsigExp'(_,0) = pps "<FsigExp>"
	| ppFsigExp'(VarFsig s,d) = PU.ppSym ppstrm s
	| ppFsigExp'(BaseFsig {param,result},d) =
	  let fun pr ppstrm (SOME symbol, sigexp) =
		  (PU.pps ppstrm "("; PU.ppSym ppstrm symbol; PU.pps ppstrm ":";
		   ppSigExp sourceOp ppstrm (sigexp, d);
                   PU.pps ppstrm ")")
		| pr ppstrm (NONE, sigexp) =
		  (PU.pps ppstrm "("; ppSigExp sourceOp ppstrm (sigexp, d);
                   PU.pps ppstrm ")")
	   in PU.ppSequence ppstrm
               {sep=(fn ppstrm => (PP.newline ppstrm)),
		pr = pr,
		style = PU.INCONSISTENT}
	       param;
	      PP.break ppstrm {nsp=1,offset=2};
	      pps "=> ";
	      ppSigExp sourceOp ppstrm (result,d)
	  end

	| ppFsigExp'(MarkFsig (m,r),d) = ppFsigExp sourceOp ppstrm (m,d)
  in
      ppFsigExp'
  end

and ppSpec sourceOp ppstrm =
  let val {openHOVBox, openHVBox, closeBox, pps, ppi, ...} = PU.en_pp ppstrm
      fun pp_tyvar_list([],d) = ()
        | pp_tyvar_list([tyvar],d) =
              (ppTyvar sourceOp ppstrm (tyvar,d); PP.space ppstrm 1)
        |  pp_tyvar_list (tyvar_list,d) =
		let fun pr _ (tyvar) = (ppTyvar sourceOp ppstrm (tyvar,d))
                in PU.ppClosedSequence ppstrm
                    {front=(fn ppstrm => PP.string ppstrm "("),
                     sep=PU.sepWithSpc ",",
                     back=(fn ppstrm =>
                              (PP.string ppstrm ")"; PP.space ppstrm 1)),
                     pr=pr,
                     style=PU.INCONSISTENT}
                    tyvar_list
                end

      fun ppSpec'(_,0) = pps "<Spec>"
	| ppSpec'(StrSpec sspo_list,d) =
	  let fun pr _ (symbol, sigexp, path) =
		  (case path
                    of SOME p => (PU.ppSym ppstrm symbol; pps " = ";
                                  ppSigExp sourceOp ppstrm (sigexp,d);
                                  PP.space ppstrm 1; pp_path ppstrm p)
                     | NONE => (PU.ppSym ppstrm symbol; pps " = ";
                                ppSigExp sourceOp ppstrm (sigexp,d)))
	   in PU.ppClosedSequence ppstrm
	       {front=(C PP.string "structure "),
		sep=PU.sepWithSpc ",",
		back=(C PP.string ""),
		pr=pr,
		style=PU.INCONSISTENT}
	       sspo_list
	  end
	| ppSpec'(TycSpec (stto_list, bool),d) =
	  let fun pr _ (symbol, tyvar_list, tyo) =
		  (case tyo
		    of SOME ty =>
                       (pp_tyvar_list (tyvar_list,d);PU.ppSym ppstrm symbol; pps "= ";
                        ppTy sourceOp ppstrm(ty, d))
		     | NONE =>  (pp_tyvar_list (tyvar_list,d);PU.ppSym ppstrm symbol))
	   in PU.ppClosedSequence ppstrm
	        {front=(C PP.string "type "),
		 sep=(fn ppstrm => (PP.string ppstrm "|"; PP.newline ppstrm)),
		 back=(C PP.string ""),
		 pr=pr,
		 style=PU.INCONSISTENT}
		stto_list
	  end
	| ppSpec'(FctSpec sf_list,d) =
	  let fun pr ppstrm (symbol, fsigexp) =
                  (PU.ppSym ppstrm symbol; pps " : ";
                   ppFsigExp sourceOp ppstrm (fsigexp, d-1))
	   in openHVBox 0;
              PU.ppvlist ppstrm ("functor ", "and ", pr, sf_list);
              closeBox ()
	  end
	| ppSpec'(ValSpec st_list,d) =
	  let fun pr ppstrm (symbol, ty) =
                  (PU.ppSym ppstrm symbol; pps ":"; ppTy sourceOp ppstrm (ty, d))
	   in openHVBox 0;
              PU.ppvlist ppstrm ("val ", "and ", pr, st_list);
              closeBox ()
	  end
        | ppSpec'(DataReplSpec(name,path),d) =
	  if d = 0 then pps "<DT.repl>"
          else (openHOVBox 0;
		 pps "datatype "; PU.ppSym ppstrm name; pps " =";
		 PP.space ppstrm 1; pps "datatype ";
		 PU.ppSequence ppstrm
		   {sep=(fn ppstrm => (PP.string ppstrm ".")),
		    pr=PU.ppSym,
		    style=PU.INCONSISTENT}
		   path;
		closeBox ())
	| ppSpec'(DataSpec{datatycs,withtycs=[]},d) =
	  let fun pr ppstrm (dbing) = (ppDb sourceOp ppstrm (dbing, d))
	   in openHVBox 0;
              PU.ppvlist ppstrm ("datatype ", "and ", pr, datatycs);
              closeBox ()
	  end
	| ppSpec'(DataSpec {datatycs, withtycs},d) =
	  let fun prd ppstrm (dbing) = (ppDb sourceOp ppstrm (dbing, d))
	      fun prw ppstrm (tbing) = (ppTb sourceOp ppstrm (tbing, d))
	   in (openHVBox 0;
               PU.ppvlist ppstrm ("datatype ", "and ", prd, datatycs);
	       PP.newline ppstrm;
               PU.ppvlist ppstrm ("datatype ", "and ", prw, withtycs);
	       closeBox ())
	  end
	| ppSpec'(ExceSpec sto_list,d) =
	  let fun pr ppstrm (symbol, tyo) =
		  (case tyo
		    of SOME ty =>
                       (PU.ppSym ppstrm symbol; pps " : ";
                        ppTy sourceOp ppstrm (ty, d))
		     | NONE =>  PU.ppSym ppstrm symbol)
	   in openHVBox 0;
              PU.ppvlist ppstrm ("exception ", "and ", pr, sto_list);
              closeBox ()
	  end
	| ppSpec'(ShareStrSpec paths,d) =
	   (openHVBox 0;
            PU.ppvlist ppstrm ("sharing ", " = ", pp_path, paths);
            closeBox ())
        | ppSpec'(ShareTycSpec paths,d) =
	   (openHVBox 0;
            PU.ppvlist ppstrm ("sharing type ", " = ", pp_path, paths);
            closeBox ())
	| ppSpec'(IncludeSpec sigexp ,d) = ppSigExp sourceOp ppstrm (sigexp, d)
	| ppSpec'(MarkSpec (m,r),d) = ppSpec sourceOp ppstrm (m,d)
  in
      ppSpec'
  end

and ppDec sourceOp ppstrm =
  let val {openHOVBox, openHVBox, closeBox, pps, ppi, ...} = PU.en_pp ppstrm
      val pp_symbol_list = pp_path ppstrm

      fun ppDec'(_,0) = pps "<dec>"
        | ppDec'(ValDec (vbs, tyvars),d) =
	  (openHVBox 0;
	   PU.ppvlist ppstrm ("val ","and ",
	     (fn ppstrm => fn vb => ppVb sourceOp ppstrm (vb,d-1)),vbs);
	   closeBox ())
	| ppDec'(ValrecDec (rvbs, tyvars),d) =
	  (openHVBox 0;
	   PU.ppvlist ppstrm ("val rec ","and ",
	     (fn ppstrm => fn rvb => ppRvb sourceOp ppstrm (rvb,d-1)),rvbs);
	   closeBox ())
	| ppDec'(DoDec exp,d) =
	  (openHVBox 0;
	   pps "do";
	   PP.break ppstrm {nsp=1,offset=2}; ppExp sourceOp ppstrm (exp,d-1);
	   closeBox ())
        | ppDec'(FunDec (fbs,tyvars),d) =
	  (openHVBox 0;
	   PU.ppvlist' ppstrm ("fun ","and ",
	     (fn ppstrm => fn str => fn fb => ppFb sourceOp ppstrm str (fb,d-1)),
             fbs);
	   closeBox ())
        | ppDec'(TypeDec tycs,d) =
	  let fun pr ppstrm (tyc) = (ppTb sourceOp ppstrm (tyc, d))
	   in PU.ppClosedSequence ppstrm
		{front=(C PP.string "type "),
		 sep=(fn ppstrm => (PP.space ppstrm 1)),
		 back=(C PP.string ""),
		 pr=pr,
		 style=PU.INCONSISTENT}
		tycs
	  end
	| ppDec'(DatatypeDec{datatycs,withtycs=[]},d) =
	  let fun prd _ (dbing) = (ppDb sourceOp ppstrm (dbing, d))
	   in PU.ppClosedSequence ppstrm
		{front=(C PP.string "datatype "),
		 sep=(fn ppstrm => (PP.space ppstrm 1)),
		 back=(C PP.string ""),
		 pr=prd,
		 style=PU.INCONSISTENT}
		datatycs
	  end
        | ppDec'(DatatypeDec{datatycs,withtycs},d) =
	  let fun prd ppstrm dbing = (ppDb sourceOp ppstrm (dbing, d))
	      fun prw ppstrm tbing = (ppTb sourceOp ppstrm (tbing, d))
	   in (openHVBox 0;
	        PU.ppClosedSequence ppstrm
		  {front=(C PP.string "datatype "),
		   sep=(fn ppstrm => (PP.space ppstrm 1)),
		   back=(C PP.string ""),
		   pr=prd,
		   style=PU.INCONSISTENT}
		  datatycs;
	       PP.newline ppstrm;
	       PU.ppClosedSequence ppstrm
		 {front=(C PP.string "withtype "),
		  sep=(fn ppstrm => (PP.space ppstrm 1)),
		  back=(C PP.string ""),
		  pr=prw,
		  style=PU.INCONSISTENT}
		 withtycs;
	       closeBox ())
	  end
	| ppDec'(DataReplDec(symb, path), _) = (
	    openHVBox 0;
	      pps "datatype "; PU.ppSym ppstrm symb; pps " = datatype ";
	      pp_path ppstrm path;
	    closeBox())
	| ppDec'(AbstypeDec{abstycs,withtycs=[],body},d) =
	  let fun prd ppstrm dbing = (ppDb sourceOp ppstrm (dbing, d))
	      fun prw ppstrm tbing = (ppTb sourceOp ppstrm (tbing, d))
	   in (openHVBox 0;
	        (PU.ppClosedSequence ppstrm
		   {front=(C PP.string "datatype "),
		    sep=(fn ppstrm => (PP.space ppstrm 1)),
		    back=(C PP.string ""),
		    pr=prd,
		    style=PU.INCONSISTENT}
		   abstycs);
	       PP.newline ppstrm;
	       ppDec' (body, d);
	       closeBox ())
	  end
        | ppDec'(AbstypeDec{abstycs,withtycs,body},d) =
	  let fun prd _ (dbing) = (ppDb sourceOp ppstrm (dbing, d))
	      fun prw _ (tbing) = (ppTb sourceOp ppstrm (tbing, d))
	   in (openHVBox 0;
	        (PU.ppClosedSequence ppstrm
		   {front=(C PP.string "datatype "),
		    sep=(fn ppstrm => (PP.space ppstrm 1)),
		    back=(C PP.string ""),
		    pr=prd,
		    style=PU.INCONSISTENT}
		   abstycs);
	       PP.newline ppstrm;
	       (PU.ppClosedSequence ppstrm
		  {front=(C PP.string "withtype "),
		   sep=(fn ppstrm => (PP.space ppstrm 1)),
		   back=(C PP.string ""),
		   pr=prw,
		   style=PU.INCONSISTENT}
		  withtycs);
	       PP.newline ppstrm;
	       ppDec' (body, d);
	       closeBox ())
	  end
        | ppDec'(ExceptionDec ebs,d) =
	  (openHVBox 0;
	   ((fn ppstrm => fn eb => ppEb sourceOp ppstrm (eb,d-1)),ebs);
	   closeBox ())
        | ppDec'(StrDec sbs,d) =
          let fun pr _ (sbing) = (ppStrb sourceOp ppstrm (sbing, d))
		in PU.ppClosedSequence ppstrm
		  {front=(C PP.string "structure "),
		   sep=(fn ppstrm => (PP.space ppstrm 1)),
		   back=(C PP.string ""),
		   pr=pr,
		   style=PU.INCONSISTENT}
		   sbs
		end
        | ppDec'(FctDec fbs,d) =
	  let fun f ppstrm fctb = ppFctb sourceOp ppstrm (fctb,d)
	   in openHVBox 0;
	      PU.ppvlist ppstrm ("functor ","and ", f, fbs);
              closeBox ()
	  end
        | ppDec'(SigDec sigvars,d) =
	  let fun f ppstrm (Sigb{name=fname, def}) =
                  (PU.ppSym ppstrm fname; pps " =";
		   PP.newline ppstrm;
		   ppSigExp sourceOp ppstrm (def,d))
		| f ppstrm (MarkSigb(t,r)) = f ppstrm t
	   in openHVBox 0;
	      PU.ppvlist ppstrm ("signature ","and ", f, sigvars);
              closeBox ()
	  end
        | ppDec'(FsigDec sigvars,d) =
	  let fun pr ppstrm sigv = ppFsigb sourceOp ppstrm (sigv,d)
	   in openHVBox 0;
	       PU.ppSequence ppstrm
	         {sep=PP.newline,
	          pr=pr,
	          style=PU.CONSISTENT}
	         sigvars;
	      closeBox ()
	  end
        | ppDec'(LocalDec(inner,outer),d) =
	    (openHVBox 0;
	     pps "local"; PU.nl_indent ppstrm 2;
	     ppDec'(inner,d-1); PP.newline ppstrm;
	     pps "in ";
	     ppDec'(outer,d-1); PP.newline ppstrm;
	     pps "end";
	     closeBox ())
        | ppDec'(SeqDec decs,d) =
	    (openHVBox 0;
	     PU.ppSequence ppstrm
	       {sep=PP.newline,
	        pr=(fn ppstrm => fn dec => ppDec'(dec,d)),
	        style=PU.CONSISTENT}
	        decs;
	     closeBox ())
        | ppDec'(OpenDec strbs,d) =
	  (openHVBox 0;
	   pps "open ";
	   PU.ppSequence ppstrm
	     {sep=(fn ppstrm => PP.space ppstrm 1),
	      pr=(fn ppstrm => fn sp => pp_symbol_list sp),
	      style=PU.INCONSISTENT}
             strbs;
	   closeBox ())
        | ppDec'(OvldDec (sym, explist),d) = PU.ppSym ppstrm sym
	| ppDec'(FixDec {fixity,ops},d) =
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
	     {sep=(fn ppstrm => PP.space ppstrm 1),
	      pr=PU.ppSym,style=PU.INCONSISTENT}
	     ops;
	   closeBox ())

        | ppDec'(MarkDec(dec,(s,e)),d) =
	   (case sourceOp
	      of SOME source =>
	         (pps "MarkDec(";
		  ppDec'(dec,d); pps ",";
		  prpos(ppstrm,source,s); pps ",";
		  prpos(ppstrm,source,e); pps ")")
	       | NONE => ppDec'(dec,d))

     in ppDec'
    end

and ppVb sourceOp ppstrm =
    let val pps = PP.string ppstrm
	fun ppVb'(_,0)= pps "<binding>"
	  | ppVb'(Vb{pat,exp,...},d) =
		(PP.openHVBox ppstrm (PP.Rel 0);
	  	 ppPat sourceOp ppstrm (pat,d-1); PP.string ppstrm " =";
	  	 PP.break ppstrm {nsp=1,offset=2}; ppExp sourceOp ppstrm (exp,d-1);
	  	 PP.closeBox ppstrm)
	  | ppVb'(MarkVb (vb,region),d) = ppVb' (vb,d)
    in
	ppVb'
    end

and ppRvb sourceOp ppstrm  =
    let val pps = PP.string ppstrm
	fun ppRvb'(_,0)= pps "<rec binding>"
	  | ppRvb'(Rvb{var, exp, ...},d) =
	     (PP.openHOVBox ppstrm (PP.Rel 0);
	      PU.ppSym ppstrm var; PP.string ppstrm " =";
	      PP.break ppstrm {nsp=1,offset=2}; ppExp sourceOp ppstrm (exp,d-1);
	      PP.closeBox ppstrm)
	  | ppRvb'(MarkRvb (rvb, region), d) = ppRvb' (rvb, d)
    in
	ppRvb'
    end

and ppFb sourceOp ppstrm head =
    let val pps = PP.string ppstrm
	fun ppFb'(_,0)= pps "<FunBinding>"
	  | ppFb'(Fb (clauses, ops),d) =
              PU.ppvlist ppstrm (head, "  | ",
	       (fn ppstrm => fn (cl: clause) => (ppClause sourceOp ppstrm (cl,d))),
               clauses)
	  | ppFb'(MarkFb (t,r),d) = ppFb sourceOp ppstrm head (t,d)
    in
	ppFb'
    end

and ppClause sourceOp ppstrm =
    let val pps = PP.string ppstrm
        fun ppClause' (Clause{pats, resultty, exp}, d) =
	    let fun pr _ {item:pat,fixity:symbol option,region:region} =
		    (case fixity
		      of SOME a => ppPat sourceOp ppstrm (item,d)
		       | NONE => (
			 case item
			  of FlatAppPat p =>
			     (PP.string ppstrm "(";ppPat sourceOp ppstrm (item,d);
			      PP.string ppstrm ")")
			   | ConstraintPat p  =>
			     (PP.string ppstrm "(";ppPat sourceOp ppstrm (item,d);
			      PP.string ppstrm ")")
			   | LayeredPat p =>
			     (PP.string ppstrm"(";ppPat sourceOp ppstrm (item,d);
			      PP.string ppstrm ")")
			   | OrPat p =>
			     (PP.string ppstrm "(";ppPat sourceOp ppstrm (item,d);
			      PP.string ppstrm ")")
			   | _ => ppPat sourceOp ppstrm (item,d)))

		in PP.openHOVBox ppstrm (PP.Rel 0);
	  	      (PU.ppSequence ppstrm
			{sep=(fn ppstrm => (PP.space ppstrm 1)),
			 pr=pr,
			 style=PU.INCONSISTENT}
			pats);
		    (case resultty
		      of SOME ty => (PP.string ppstrm ":";ppTy sourceOp ppstrm (ty,d))
		       | NONE => ()
		    );
		    PP.space ppstrm 1;
		    PP.string ppstrm "=";
		    PP.space ppstrm 1;
		    ppExp sourceOp ppstrm (exp,d);
		  PP.closeBox ppstrm
		end

    in
	ppClause'
    end

and ppTb sourceOp ppstrm  =
    let val pps = PP.string ppstrm
	fun pp_tyvar_list (symbol_list, d) =
	    let fun pr _ (tyvar) = (ppTyvar sourceOp ppstrm (tyvar, d))
	     in PU.ppSequence ppstrm
		 {sep=(fn ppstrm => (PP.string ppstrm "*"; PP.space ppstrm 1)),
		  pr=pr,
		  style=PU.INCONSISTENT}
		 symbol_list
	    end

	  fun ppTb'(_,0)= pps "<T.binding>"
	    | ppTb'(Tb{tyc,def,tyvars},d) =
		(PP.openHOVBox ppstrm (PP.Rel 0);
	  	  PU.ppSym ppstrm tyc; PP.string ppstrm " =";
	  	  PP.space ppstrm 1; ppTy sourceOp ppstrm (def, d);
		  pp_tyvar_list (tyvars,d);
	  	 PP.closeBox ppstrm)
	    | ppTb'(MarkTb (t,r),d) = ppTb sourceOp ppstrm (t,d)
    in
	ppTb'
    end

and ppDb sourceOp ppstrm  =
    let val pps = PP.string ppstrm
	fun pp_tyvar_list (symbol_list, d) =
	    let fun pr _ (tyvar) = (ppTyvar sourceOp ppstrm (tyvar, d))
	     in PU.ppSequence ppstrm
		 {sep=(fn ppstrm => (PP.string ppstrm ",";
				     PP.space ppstrm 1)),
		  pr=pr,
		  style=PU.INCONSISTENT}
		 symbol_list
	    end
	fun ppDb'(_,0) = pps "<D.binding>"
	  | ppDb'(Db{tyc,tyvars,rhs,lazyp},d) =
	     (PP.openHOVBox ppstrm (PP.Rel 0);
	      pp_tyvar_list (tyvars,d);
	      PU.ppSym ppstrm tyc; PP.string ppstrm " =";
	      PP.space ppstrm 1; ppDbrhs sourceOp ppstrm (rhs,d);
	      PP.closeBox ppstrm)
	  | ppDb'(MarkDb (t,r),d) = ppDb sourceOp ppstrm (t,d)
    in
	ppDb'
    end

and ppDbrhs sourceOp ppstrm =
    let val pps = PP.string ppstrm
	fun ppDbrhs'(_,0)= pps "<DT.rhs>"
	  | ppDbrhs'(constrs,d) =
	    let fun pr ppstrm (sym:symbol, tv:Ast.ty option) =
		    (case tv
		       of SOME a =>
			  (PU.ppSym ppstrm sym; pps" of "; ppTy sourceOp ppstrm (a, d))
		        | NONE =>
			  (PU.ppSym ppstrm sym))
	    in  PU.ppSequence ppstrm
		  {sep=(fn ppstrm => (PP.string ppstrm " |";
				      PP.space ppstrm 1)),
		   pr=pr,
		   style=PU.INCONSISTENT}
		  constrs
	    end
     in ppDbrhs'
    end

and ppEb sourceOp ppstrm =
    let val pps = PP.string ppstrm
        val pp_symbol_list = pp_path ppstrm
	fun ppEb'(_,0)= pps "<Eb>"
	  | ppEb'(EbGen{exn, etype},d) =
	     (case etype
	        of SOME a =>
		   (PP.openHVBox ppstrm (PP.Rel 0);
	  	    PU.ppSym ppstrm exn; PP.string ppstrm " =";
	  	    PP.break ppstrm {nsp=1,offset=2}; ppTy sourceOp ppstrm (a,d-1);
	  	    PP.closeBox ppstrm)
		 | NONE =>
		   (PP.openHVBox ppstrm (PP.Rel 0);
	  	    PU.ppSym ppstrm exn;
	  	    PP.closeBox ppstrm))
	  | ppEb'(EbDef{exn, edef},d) =
		(*ASK MACQUEEN IF WE NEED TO PRINT EDEF*)
	     (PP.openHVBox ppstrm (PP.Rel 0);
	      PU.ppSym ppstrm exn; PP.string ppstrm " =";
	      PP.break ppstrm {nsp=1,offset=2}; pp_symbol_list(edef);
	      PP.closeBox ppstrm)
	  | ppEb'(MarkEb (t,r),d) = ppEb sourceOp ppstrm (t,d)
    in
	ppEb'
    end

and ppStrb sourceOp ppstrm =
    let val pps = PP.string ppstrm
	fun ppStrb'(_,0)= pps "<Strb>"
	  | ppStrb'(Strb{name,def,constraint},d) =
	     (PP.openHVBox ppstrm (PP.Rel 0);
	      PU.ppSym ppstrm name; PP.string ppstrm " =";
	      PP.break ppstrm {nsp=1,offset=2}; ppStrExp sourceOp ppstrm (def,d-1);
	      PP.closeBox ppstrm)
	  | ppStrb'(MarkStrb (t,r),d) = ppStrb sourceOp ppstrm (t,d)
    in
	ppStrb'
    end

and ppFctb sourceOp ppstrm =
    let val pps = PP.string ppstrm
	fun ppFctb'(_,0)= pps "<Fctb>"
	  | ppFctb'(Fctb{name,def=BaseFct{params,body,constraint}},d) =
	    (PP.openHVBox ppstrm (PP.Rel 0);
             PU.ppSym ppstrm name;
             let fun pr ppstrm (SOME symbol, sigexp) =
                     (pps "("; PU.ppSym ppstrm symbol; pps " : ";
                      ppSigExp sourceOp ppstrm (sigexp, d); pps ")")
                   | pr ppstrm (NONE, sigexp) =
                     (pps "("; ppSigExp sourceOp ppstrm (sigexp, d); pps ")")
              in (PU.ppSequence ppstrm
                    {sep=(fn ppstrm => (PP.space ppstrm 1)),
                     pr = pr,
                     style = PU.INCONSISTENT}
                    params;
                  case constraint
                    of NoSig => ()
                     | Transparent(sigexp) =>
                       (pps " :"; PP.break ppstrm {nsp=1,offset=2};
                        ppSigExp sourceOp ppstrm (sigexp,d))
                     | Opaque(sigexp) =>
                       (pps " :>"; PP.break ppstrm {nsp=1,offset=2};
                        ppSigExp sourceOp ppstrm (sigexp,d));
                  PP.nbSpace ppstrm 1;
                  pps "="; PP.space ppstrm 1;
                  ppStrExp sourceOp ppstrm (body,d))
              end;
             PP.closeBox ppstrm)
	  | ppFctb'(Fctb{name,def},d) =
	    (PP.openHVBox ppstrm (PP.Rel 0);
	     PU.ppSym ppstrm name; PP.string ppstrm " =";
	     PP.break ppstrm {nsp=1,offset=2}; ppFctExp sourceOp ppstrm (def,d-1);
	     PP.closeBox ppstrm)
	  | ppFctb'(MarkFctb (t,r),d) = ppFctb sourceOp ppstrm (t,d)
    in
	ppFctb'
    end

and ppFsigb sourceOp ppstrm =
    let val pps = PP.string ppstrm
	fun ppFsigb'(_,0)= pps "<Fsigb>"
	  | ppFsigb'(Fsigb{name,def},d) =
	    (PP.openHVBox ppstrm (PP.Rel 0);
	      pps "funsig "; PU.ppSym ppstrm name; pps " =";
	      PP.break ppstrm {nsp=1,offset=2}; ppFsigExp sourceOp ppstrm (def,d-1);
	     PP.closeBox ppstrm)
	  | ppFsigb'(MarkFsigb (t,r),d) = ppFsigb sourceOp ppstrm (t,d)
    in
	ppFsigb'
    end

and ppTyvar sourceOp ppstrm =
    let val pps = PP.string ppstrm
	fun ppTyvar'(_,0)= pps "<tyvar>"
	  | ppTyvar'(Tyv s,d) = (PU.ppSym ppstrm s)
	  | ppTyvar'(MarkTyv (t,r),d) = ppTyvar sourceOp ppstrm (t,d)
    in
	ppTyvar'
    end

and ppTy sourceOp ppstrm =
  let val pps = PP.string ppstrm
      fun ppTy' (_,0) = pps "<type>"
        | ppTy' (VarTy t,d) =  (ppTyvar sourceOp ppstrm (t,d))
	| ppTy' (ConTy (tycon, []),d) = (
	   PP.openHVBox ppstrm (PP.Rel 1);
	     pp_path ppstrm tycon;
	   PP.closeBox ppstrm)
	| ppTy' (ConTy (tycon, args),d) = (
	   PP.openHVBox ppstrm (PP.Rel 1);
           case tycon
             of [tyc] =>
	         if S.eq(S.tycSymbol("->"), tyc) then
                   (case args
                      of [dom,ran] =>
                         (ppTy' (dom,d-1); pps " ->"; PP.break ppstrm {nsp=1,offset=2};
                          ppTy' (ran,d-1))
                       | _ => EM.impossible "wrong args for -> type")
		 else (ppTypeArgs(args,d);
	               PU.ppSym ppstrm tyc;
	               PP.closeBox ppstrm)
              | _ => (ppTypeArgs(args,d);
	              pp_path ppstrm tycon;
	              PP.closeBox ppstrm))

	| ppTy' (RecordTy s, d) =
	  let fun pr ppstrm (sym:symbol, tv:Ast.ty) =
		  (PU.ppSym ppstrm sym; pps ":"; ppTy sourceOp ppstrm (tv, d))
	  in  PU.ppClosedSequence ppstrm
	        {front=(C PP.string "{"),
		 sep=PU.sepWithSpc ",",
		 back=(C PP.string "}"),
		 pr=pr,
		 style=PU.INCONSISTENT}
		s
	  end
	| ppTy' (TupleTy t, d) =
	  let fun pr _ (tv:Ast.ty) = (ppTy sourceOp ppstrm (tv, d))
	  in  PU.ppSequence ppstrm
	       {sep=(fn ppstrm => (PP.string ppstrm " *"; PP.space ppstrm 1)),
		pr=pr,
		style=PU.INCONSISTENT}
	       t
	  end
	| ppTy' (MarkTy (t,r),d) = (ppTy sourceOp ppstrm (t,d))
      and ppTypeArgs ([],d) = ()
	| ppTypeArgs ([ty],d) =
	  (if strength ty <= 1
	   then (PP.openHOVBox ppstrm (PP.Rel 1);
                 pps "(";
                 ppTy' (ty,d);
                 pps ")";
                 PP.closeBox ppstrm)
	   else ppTy' (ty,d);
	   PP.space ppstrm 1)
	| ppTypeArgs (tys,d) =
          PU.ppClosedSequence ppstrm
	   {front=C PP.string "(",
	    sep=PU.sepWithCut ",",
	    back=C PP.string ") ",
	    style=PU.INCONSISTENT,
	    pr=fn _ => fn ty => ppTy' (ty,d)}
	   tys
   in ppTy'
  end

end (* top-level local *)
end (* structure PPAst *)

(* 4/28/2009: Fixed some "bugs" in the pretty printer that were making
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

   Jon Riehl *)
