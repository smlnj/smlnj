(* print-sml.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure PrintSML : sig

    val output : TextIOPP.stream -> SML.top_decl -> unit

  end = struct

    structure PP = TextIOPP
    structure S = SML

    val indent0 = (PP.Abs 0)
    val indent2 = (PP.Abs 2)
    val indent4 = (PP.Abs 4)

    fun isParenPat (S.RECORDpat _) = true
      | isParenPat (S.TUPLEpat _) = true
      | isParenPat (S.GRPpat _) = true
      | isParenPat _ = false

    fun isParenExp (S.RECORDexp _) = true
      | isParenExp (S.TUPLEexp _) = true
      | isParenExp (S.GRPexp _) = true
      | isParenExp _ = false

    fun ppTopDecl (strm, dcl) = let
          val str = PP.string strm
          fun sp () = PP.space strm 1
          fun nl () = PP.newline strm
	  in
	    case dcl
	     of S.SIGtop(id, sigExp) => (
		  PP.openHBox strm;
		    str "signature"; sp(); str id; sp(); str "="; sp();
		  PP.closeBox strm;
		  ppSigExp (strm, sigExp);
		  nl())
	      | S.STRtop(id, optSig, strExp) => (
		  PP.openHBox strm;
		    str "structure"; sp(); str id; sp();
		    case optSig
		     of SOME(true, sigExp) => (str ":>"; sp(); ppSigExp(strm, sigExp); sp())
		      | SOME(false, sigExp) => (str ":"; sp(); ppSigExp(strm, sigExp); sp())
		      | NONE => ()
		    (* end case *);
		    str "="; sp();
		  PP.closeBox strm;
		  ppStrExp (strm, strExp);
		  nl())
	      | S.VERBtop strs => List.app str strs
	    (* end case *)
	  end

    and ppSigExp (strm, sigExp) = let
          val str = PP.string strm
          fun sp () = PP.space strm 1
          fun nl () = PP.newline strm
	  fun ppSpec (S.STRspec _) = () (* FIXME *)
	    | ppSpec (S.TYPEspec(eqTy, tvs, tyc, rhs)) = (
		PP.openHBox strm;
		  if eqTy then str "eqtype" else str "type";
		  sp(); ppTycBind(strm, tvs, tyc);
		  case rhs
		   of SOME ty => (sp(); str "="; sp(); ppTy(strm, ty))
		    | NONE => ()
		  (* end case *);
		PP.closeBox strm)
	    | ppSpec (S.DATATYPEspec(dbs, tbs)) = ppDatatypes (strm, dbs, tbs)
	    | ppSpec (S.VALspec(id, ty)) = (
		PP.openHBox strm;
		  str "val"; sp(); str id; sp(); str ":"; sp(); ppTy(strm, ty);
		PP.closeBox strm)
	    | ppSpec (S.EXNspec con) = (
		PP.openHBox strm;
		  str "exception"; sp(); ppCon (strm, con);
		PP.closeBox strm)
	    | ppSpec (S.VERBspec strs) = (
		PP.openVBox strm indent0;
		  List.app str strs;
		PP.closeBox strm)
	  fun ppSig (S.IDsig id) = str id
	    | ppSig (S.AUGsig(sigExp, wTys)) = let
		fun ppWTy (S.WHERETY(tvs, qid, ty)) = (
		      nl();
		      PP.openHBox strm;
			str "where"; sp(); str "type"; sp();
			ppTycBind(strm, tvs, String.concatWith "." qid);
			sp(); str "="; sp(); ppTy(strm, ty);
		      PP.closeBox strm)
		in
		  PP.openVBox strm indent2;
		    PP.openHBox strm; ppSig sigExp; PP.closeBox strm;
		    List.app ppWTy wTys;
		  PP.closeBox strm
		end
	    | ppSig (S.BASEsig specs) = (
		PP.openVBox strm indent2;
		  str "sig";
		  PP.openVBox strm indent2;
		    List.app (fn spc => (nl(); ppSpec spc)) specs;
		  PP.closeBox strm;
		  nl();
		  str "end";
		PP.closeBox strm)
	  in
	    ppSig sigExp
	  end

    and ppStrExp (strm, strExp) = let
          val str = PP.string strm
          fun sp () = PP.space strm 1
          fun nl () = PP.newline strm
	  in
	   case strExp
	    of S.IDstr id => str id
	     | S.BASEstr dcls => (
		PP.openVBox strm indent2;
		  str "struct";
		  PP.openVBox strm indent2;
		    List.app (fn dcl => (nl(); ppDec(strm, dcl))) dcls;
		  PP.closeBox strm;
		  nl();
		  str "end";
		PP.closeBox strm)
	     | S.VERBstr strs => List.app str strs
	    (* end case *)
	  end

    and ppDec (strm, dcl) = let
          fun inHBox f = (PP.openHBox strm; f(); PP.closeBox strm)
          val str = PP.string strm
          fun sp () = PP.space strm 1
          fun nl () = PP.newline strm
	  fun ppTB (prefix, (tvs, tyc, ty)) = (
		PP.openHBox strm;
		  str prefix; sp();
		  ppTycBind (strm, tvs, tyc);
		  sp(); str "="; sp();
		  ppTy (strm, ty);
		PP.closeBox strm)
	  fun ppPat S.WILDpat = str "_"
	    | ppPat (S.IDpat id) = str id
	    | ppPat (S.NUMpat n) = str n
	    | ppPat (S.STRINGpat s) = str(concat["\"", String.toString s, "\""])
	    | ppPat (S.CHARpat s) = str(concat["#\"", String.toString s, "\""])
	    | ppPat (S.CONpat(c, p)) = inHBox (fn () => (
		str c;
		if isParenPat p then () else sp();
		ppPat p))
            | ppPat (S.INFIXpat(p1, con, p2)) = raise Fail "FIXME: INFIXpat"
	    | ppPat (S.RECORDpat{fields, flex}) = let
		fun ppFld (x, S.IDpat y) = if (x = y)
		      then str x
		      else (str x; sp(); str "="; sp(); str y)
		  | ppFld (x, S.ASpat(y, p)) = (
		      if (x = y)
			then str x
			else (str x; sp(); str "="; sp(); str y);
		      sp(); str "as"; sp(); ppPat p)
		  | ppFld (x, p) = inHBox (fn () => (str x; sp(); str "="; sp(); ppPat p))
		in
		  inHBox (fn () => (
		    str "{";
		    case (fields, flex)
		     of ([], false) => ()
		      | ([], true) => str "..."
		      | (fld::flds, _) => (
			  ppFld fld;
			  List.app (fn fld => (str ","; sp(); ppFld fld)) flds;
			  if flex then (str ","; sp(); str "...") else ())
		      (* end case *);
		    str "}"))
		end
	    | ppPat (S.TUPLEpat[]) = str "()"
	    | ppPat (S.TUPLEpat(p::ps)) = inHBox (fn () => (
		str "("; ppPat p;
		List.app (fn p => (str ","; sp(); ppPat p)) ps;
		str ")"))
	    | ppPat (S.CONSTRAINTpat(p, ty)) = raise Fail "FIXME: CONSTRAINTpat"
	    | ppPat (S.ASpat(x, p)) = inHBox (fn () => (
		str x; sp(); str "as"; sp(); ppPat p))
            | ppPat (S.GRPpat p) = inHBox (fn () => (str "("; ppPat p; str ")"))
	  fun ppAtomicPat p = (case p
		 of S.WILDpat => ppPat p
		  | S.IDpat _ => ppPat p
		  | S.NUMpat _ => ppPat p
		  | S.STRINGpat _ => ppPat p
		  | S.CHARpat _ => ppPat p
		  | S.RECORDpat _ => ppPat p
		  | S.TUPLEpat _ => ppPat p
		  | S.GRPpat _ => ppPat p
		  | _ => ppPat (S.GRPpat p)
		(* end case *))
	  fun ppExp (S.IDexp id) = str id
	    | ppExp (S.NUMexp n) = str n
	    | ppExp (S.STRINGexp s) = str(concat["\"", String.toString s, "\""])
	    | ppExp (S.CHARexp s) = str(concat["#\"", String.toString s, "\""])
	    | ppExp (S.RECORDexp fields) = let
		fun ppFld (lab, e) = inHBox (fn () => (
		      str lab; sp(); str "="; sp(); ppExp e))
		in
		  str "{";
		  case fields
		   of [] => ()
		    | [fld] => ppFld fld
		    | fld::flds => (
			PP.openHVBox strm indent2;
			  PP.cut strm;
			  ppFld fld;
			  List.app (fn fld => (str ","; sp(); ppFld fld)) flds;
			PP.closeBox strm)
		  (* end case *);
		  str "}"
		end
	    | ppExp (S.TUPLEexp[]) = str "()"
	    | ppExp (S.TUPLEexp(e::es)) = inHBox (fn () => (
		str "("; ppExp e;
		List.app (fn e => (str ","; sp(); ppExp e)) es;
		str ")"))
	    | ppExp (S.SELECTexp(proj, e)) = inHBox (fn () => (
		str "#"; str proj;
		if isParenExp e then () else sp();
		ppExp e))
	    | ppExp (S.APPexp(e1, e2)) = (
		PP.openHVBox strm indent2;
		  ppExp e1; sp(); ppExp e2;
		PP.closeBox strm)
	    | ppExp (S.INFIXexp(e1, rator, e2)) = raise Fail "FIXME: INFIXexp"
	    | ppExp (S.HANDLEexp(e, rules)) = raise Fail "FIXME: HANDLEexp"
	    | ppExp (S.RAISEexp e) = inHBox (fn () => (str "raise"; sp(); ppExp e))
	    | ppExp (S.CASEexp(e, [])) = raise Fail "empty case expression"
	    | ppExp (S.CASEexp(e, rule::rules)) = let
		fun ppRule (p, e) = (
		      PP.openHVBox strm indent4;
			inHBox (fn () => (ppPat p; sp(); str "=>"; sp(); ppExp e));
		      PP.closeBox strm)
		in
		  PP.openVBox strm indent0;
		    inHBox (fn () => (str "case"; sp(); ppExp e)); nl();
		    inHBox (fn () => (sp(); str "of"; sp(); ppRule rule));
		    List.app
		      (fn r => inHBox (fn () => (nl(); PP.space strm 2; str "|"; sp(); ppRule r)))
			rules;
		  PP.closeBox strm
		end
	    | ppExp (S.IFexp(e1, e2, e3)) = raise Fail "FIXME: IFexp"
	    | ppExp (S.FNexp []) = raise Fail "empty FNexp"
	    | ppExp (S.FNexp(rule::rules)) = let
		fun ppRule (p, e) = (
		      PP.openHVBox strm indent4;
			inHBox (fn () => (ppPat p; sp(); str "=>"; sp(); ppExp e));
		      PP.closeBox strm)
		in
		  PP.openVBox strm indent0;
		    inHBox (fn () => (str "fn"; sp(); ppRule rule));
		    List.app
		      (fn r => inHBox (fn () => (
			  nl(); PP.space strm 2; str "|"; sp(); ppRule r)))
			rules;
		  PP.closeBox strm
		end
            | ppExp (S.LETexp(decs, body)) = let
		fun ppBody body = (case body
		       of S.SEQexp[e] => ppBody e
			| S.SEQexp es => let
			    fun pp [] = raise Fail "empty let-expression body"
			      | pp [e] = inHBox (fn () => ppExp e)
			      | pp (e::er) = (
				  inHBox (fn () => (ppExp e; str ";"));
				  sp();
				  pp er)
			    in
			      PP.openVBox strm indent0;
				nl();
				pp es;
			      PP.closeBox strm
			    end
			| e => (
			    PP.break strm {nsp=1, offset=2};
			    inHBox (fn () => ppExp e))
		      (* end case *))
		in
		  case decs
		   of [dec] => (
			PP.openHVBox strm indent0;
			  str "let"; sp(); ppDec(strm, dec); sp(); str "in";
			  PP.openHOVBox strm indent2;
			    ppBody body;
			  PP.closeBox strm;
			  sp(); str "end";
			PP.closeBox strm)
		    | decs => (
			PP.openVBox strm indent0;
			  str "let"; nl();
			  List.app (fn d => (ppDec(strm, d); nl())) decs;
			  PP.openVBox strm indent2;
			    str "in"; ppBody body;
			  PP.closeBox strm;
			  nl(); str "end";
			PP.closeBox strm)
		end
	    | ppExp (S.SEQexp es) = let
		fun pp [] = raise Fail "empty sequence expression"
		  | pp [e] = ppExp e
		  | pp (e::es) = (
		      inHBox (fn () => (ppExp e; str ";"));
		      sp();
		      pp es)
		in
		  PP.openHVBox strm indent0;
		    PP.cut strm; pp es;
		  PP.closeBox strm
		end
	    | ppExp (S.CONSTRAINTexp(e, ty)) = raise Fail "FIXME: CONSTRAINTexp"
            | ppExp (S.GRPexp e) = inHBox (fn () => (str "("; ppExp e; str ")"))
	    | ppExp (S.VERBexp s) = str s
	  in
	    case dcl
	     of S.VALdec(pat, exp) => (
		  PP.openHBox strm;
		    str "val"; sp(); ppPat pat; sp(); str "="; sp(); ppExp exp;
		  PP.closeBox strm)
	      | S.FUNdec fbs => let
		  fun pp (prefix, S.FB(f, rules)) = let
			fun ppRule (pats, rhs) = (
			      PP.openHVBox strm indent4;
				inHBox (fn () => (
			          List.app (fn p => (sp(); ppAtomicPat p)) pats;
			          sp(); str "="; sp() ;ppExp rhs));
			      PP.closeBox strm)
			in
			  case rules
			   of rule::rules => (
				PP.openVBox strm indent2;
				  inHBox (fn () => (
				    str "fun"; sp(); str f; ppRule rule));
				  List.app
				    (fn rule => (
					nl();
					inHBox (fn () => (
					  str "|"; sp(); str f; ppRule rule))))
				      rules;
				PP.closeBox strm)
			    | [] => raise Fail "empty rule list for function binding"
			  (* end case *)
			end
		  in
		    case fbs
		     of fb::fbs => (
			  PP.openVBox strm indent0;
			    pp ("fun", fb);
			    List.app (fn fb => (nl(); pp ("and", fb))) fbs;
			  PP.closeBox strm)
		      | [] => raise Fail "empty function-binding list"
		    (* end case *)
		  end
	      | S.TYPEdec tb => ppTB ("type", tb)
	      | S.DATATYPEdec(dbs, tbs) => ppDatatypes (strm, dbs, tbs)
	      | S.EXCEPTIONdec con => (
		  PP.openHBox strm;
		    str "exception"; sp(); ppCon (strm, con);
		  PP.closeBox strm)
	      | S.STRdec(id, sign, strExp) => () (* FIXME *)
	      | S.OPENdec ids => () (* FIXME *)
	      | S.LOCALdec(dcls1, dcls2) => () (* FIXME *)
	      | S.VERBdec strs => (
		  PP.openVBox strm indent0;
		    List.app str strs;
		  PP.closeBox strm)
	    (* end case *)
	  end

    and ppDatatypes (strm, dbs, tbs) = let
          val str = PP.string strm
          fun sp () = PP.space strm 1
          fun nl () = PP.newline strm
	  fun ppTB (prefix, (tvs, tyc, ty)) = (
		PP.openHBox strm;
		  str prefix; sp();
		  ppTycBind (strm, tvs, tyc);
		  sp(); str "="; sp();
		  ppTy (strm, ty);
		PP.closeBox strm)
	  fun db (S.DB(tvs, tyc, cons), isFirst) = (
		PP.openHBox strm;
		  if isFirst
		    then str "datatype"
		    else (nl(); str "and");
		  sp(); ppTycBind (strm, tvs, tyc);
		  case cons
		   of [] => raise Fail "impossible"
		    | [con] => (sp(); str "="; sp(); ppCon(strm, con))
		    | cons => let
			fun ppCon' (con, isFirst) = (
			      nl();
			      PP.openHBox strm;
				if isFirst then str "=" else str "|";
				sp(); ppCon(strm, con);
			      PP.closeBox strm;
			      false)
			in
			  PP.openVBox strm indent2;
			    List.foldl ppCon' true cons;
			  PP.closeBox strm
			end
		  (* end case *);
		PP.closeBox strm;
		false)
	  fun tb (tyb, prefix) = (nl(); ppTB(prefix, tyb); "and")
	  in
	    PP.openVBox strm indent0;
	      List.foldl db true dbs;
	      List.foldl tb "withtype" tbs;
	    PP.closeBox strm
	  end

    and ppTycBind (strm, tvs, tyc) = let
          val str = PP.string strm
          fun sp () = PP.space strm 1
	  in
	    case tvs
	     of [] => ()
	      | [tv] => (str tv; sp())
	      | tvs => (str "("; str(String.concatWith "," tvs); str ")"; sp())
	    (* end case *);
	    str tyc
	  end

    and ppTy (strm, ty) = let
          val str = PP.string strm
          fun sp () = PP.space strm 1
	  fun pp (S.VARty tv) = str tv
	    | pp (S.CONty([], tyc)) = str tyc
	    | pp (S.CONty([ty], tyc)) = (atomic ty; sp(); str tyc)
	    | pp (S.CONty(ty::tys, tyc)) = (
		str "(";
		pp ty;
		List.app (fn ty => (str ","; sp(); pp ty)) tys;
		str ")"; sp(); str tyc)
	    | pp (S.FUNty(ty1 as S.FUNty _, ty2)) = (atomic ty1; sp(); str "->"; sp(); pp ty2)
	    | pp (S.FUNty(ty1, ty2)) = (pp ty1; sp(); str "->"; sp(); pp ty2)
	    | pp (S.RECORDty fields) = let
		fun field (label, ty) = (str label; sp(); str ":"; sp(); pp ty)
		in
		  str "{";
		  case fields
		   of [] => ()
		    | [fld] => field fld
		    | fld::flds => (
			field fld;
			List.app (fn fld => (str ","; sp(); field fld)) flds)
		  (* end case *);
		  str "}"
		end
	    | pp (S.TUPLEty[]) = str "unit"
	    | pp (S.TUPLEty[ty]) = pp ty
	    | pp (S.TUPLEty(ty::tys)) = (
		atomic ty;
		List.app (fn ty => (sp(); str "*"; sp(); atomic ty)) tys)
	    | pp (S.VERBty ty) = str ty
	  and atomic ty = let
		fun paren () = (str "("; pp ty; str ")")
		in
		  case ty
		   of (S.TUPLEty _) => paren()
		    | (S.FUNty _) => paren()
		    | _ => pp ty
		  (* end case *)
		end
	  in
	    PP.openHBox strm;
	      pp ty;
	    PP.closeBox strm
	  end

    and ppCon (strm, con) = let
          val str = PP.string strm
	  fun sp () = PP.space strm 1
	  in
	    case con
	     of (id, NONE) => str id
	      | (id, SOME ty) => (
		  PP.openHBox strm;
		    str id; sp(); str "of"; sp(); ppTy(strm, ty);
		  PP.closeBox strm)
	    (* end case *)
	  end

    fun output strm decl = (
	  PP.openVBox strm indent0;
	    ppTopDecl (strm, decl);
	    PP.newline strm;
	  PP.closeBox strm)

  end
