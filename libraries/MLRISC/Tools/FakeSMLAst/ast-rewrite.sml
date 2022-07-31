(*
 * Various translation functions
 *)
functor MDLAstRewriter(Ast : MDL_AST) : MDL_AST_REWRITER =
struct

   structure Ast = Ast
   structure Error = MDLError

   type 'a rewriter = ('a -> 'a) -> ('a -> 'a)

   type clients = {exp  : Ast.exp rewriter,
                   decl : Ast.decl rewriter,
                   sexp : Ast.structexp rewriter,
                   pat  : Ast.pat rewriter,
                   ty   : Ast.ty rewriter
                  }
   type trans = {exp  : Ast.exp -> Ast.exp,
                 decl : Ast.decl -> Ast.decl,
                 sexp : Ast.structexp -> Ast.structexp,
                 pat  : Ast.pat -> Ast.pat,
                 ty   : Ast.ty -> Ast.ty
                }

   open Ast

   fun noRewrite f e = e

   fun opt f NONE = NONE
     | opt f (SOME e) = SOME(f e)

   fun rewrite{exp=rwExp, decl=rwDecl, pat=rwPat, sexp=rwSexp, ty=rwTy} = 
   let fun exp e =
       let val e = case e of
                     CONSexp(id,SOME e) => CONSexp(id,SOME(exp e))
                   | LISTexp(es,e) => LISTexp(map exp es,opt exp e)
                   | TUPLEexp es => TUPLEexp(map exp es)
                   | VECTORexp es => VECTORexp(map exp es)
                   | RECORDexp es => RECORDexp(map (fn (l,e) => (l,exp e)) es)
                   | SEQexp es => SEQexp(map exp es)
                   | APPexp(f,x) => APPexp(exp f, exp x) 
                   | IFexp(x,y,z) => IFexp(exp x, exp y, exp z)
                   | RAISEexp e => RAISEexp(exp e)
                   | HANDLEexp(e,c) => HANDLEexp(exp e,map clause c)
                   | CASEexp(e,c) => CASEexp(exp e,map clause c)
                   | LAMBDAexp c => LAMBDAexp(map clause c)
                   | LETexp(d,e) => LETexp(map decl d,map exp e)
                   | TYPEDexp(e,t) => TYPEDexp(exp e,ty t)
                   | MARKexp(l,e) => (Error.setLoc l; MARKexp(l,exp e))
                   | LOCexp(id,e,region) => LOCexp(id,exp e,region)
                   | BITSLICEexp(e,slices) => BITSLICEexp(exp e,slices) 
                   | TYPEexp t => TYPEexp(ty t)
                   | CONTexp(e,x) => CONTexp(exp e,x)
                   | e => e
        in  rwExp exp e end

        and decl d =
        let val d = case d of
              DATATYPEdecl(dbs,tbs) => DATATYPEdecl(map dbind dbs,map tbind tbs)
            | FUNdecl(fbs) => FUNdecl(map fbind fbs)
            | RTLdecl(p,e,l) => RTLdecl(pat p,exp e,l) 
            | RTLSIGdecl(id,t) => RTLSIGdecl(id,ty t)
            | VALdecl(vbs) => VALdecl(map vbind vbs)
            | VALSIGdecl(id,t) => VALSIGdecl(id,ty t)
            | TYPESIGdecl(id,tvs) => TYPESIGdecl(id,tvs)
            | LOCALdecl(d1,d2) => LOCALdecl(map decl d1,map decl d2)
            | SEQdecl ds => SEQdecl(map decl ds)
            | STRUCTUREdecl(id,ds,s,se) => 
                 STRUCTUREdecl(id,map decl ds,sigconopt s,sexp se)
            | FUNCTORdecl(id,ds,s,se) => 
                 FUNCTORdecl(id, map decl ds,sigconopt s, sexp se)
            | INCLUDESIGdecl s => INCLUDESIGdecl(sigexp s)
            | SIGNATUREdecl(id,s) => SIGNATUREdecl(id, sigexp s)
            | STRUCTURESIGdecl(id,s) => STRUCTURESIGdecl(id, sigexp s)
            | OPENdecl ids => OPENdecl ids 
            | FUNCTORARGdecl(id,se) => FUNCTORARGdecl(id,sigcon se)
            | EXCEPTIONdecl ebs => EXCEPTIONdecl(map ebind ebs)
            | MARKdecl(l,d) => (Error.setLoc l; MARKdecl(l,decl d))
            | d => d
        in rwDecl decl d end

        and sigcon{abstract,sigexp=se} = {abstract=abstract,sigexp=sigexp se}

        and sigconopt s = Option.map sigcon s

        and ebind(b as EXCEPTIONbind(id,NONE)) = b
          | ebind(EXCEPTIONbind(id,SOME t)) = EXCEPTIONbind(id,SOME(ty t))
          | ebind(b as EXCEPTIONEQbind _) = b

        and sigexp se = 
            let val se = case se of
                  IDsig _ => se
                | WHEREsig(se,ident,s) => 
                     WHEREsig(sigexp se,ident,sexp s)
                | WHERETYPEsig(se,ident,t) =>
                     WHERETYPEsig(sigexp se,ident,ty t)
                | DECLsig ds => DECLsig(map decl ds)
            in  se end

        and sexp se =
            let val se = case se of
                  APPsexp(a,se) => APPsexp(sexp a,sexp se)
                | DECLsexp ds => DECLsexp(map decl ds)
                | CONSTRAINEDsexp(s, si) => CONSTRAINEDsexp(sexp s,sigexp si)
                | IDsexp _ => se
            in  rwSexp sexp se end

        and ty t = 
            let val t = case t of
                IDty _ => t
              | TYVARty _ => t
              | INTVARty _ => t
              | VARty(_,_,_,ref(SOME t)) => ty t
              | VARty(_,_,_,ref NONE) => t
              | APPty(f, ts) => APPty(f, map ty ts)
              | FUNty(a,b) => FUNty(ty a, ty b) 
              | TUPLEty ts => TUPLEty(map ty ts)
              | RECORDty lts => RECORDty(map (fn (l,t) => (l,ty t)) lts)
              | POLYty(ts,t) => POLYty(map ty ts, ty t)
              | LAMBDAty(ts, t) => LAMBDAty(map ty ts, ty t)
              | CELLty _ => t
            in  rwTy ty t end
   
        and pat p =
            let val p = case p of
                  IDpat id => p
                | WILDpat => p
                | ASpat(id,p) => ASpat(id, pat p)
                | LITpat l => p
                | LISTpat(ps,p) => LISTpat(map pat ps,opt pat p)
                | TUPLEpat ps => TUPLEpat(map pat ps)
                | VECTORpat ps => VECTORpat(map pat ps)
                | RECORDpat(lps,flex) => 
                     RECORDpat(map (fn (l,p) => (l,pat p)) lps, flex)
                | TYPEDpat(p,t) => TYPEDpat(pat p,ty t)
                | CONSpat(id,NONE) => p
                | CONSpat(id,SOME p) => CONSpat(id,SOME(pat p))
                | ORpat ps => ORpat(map pat ps)
                | ANDpat ps => ANDpat(map pat ps)
                | NOTpat p => NOTpat(pat p)
                | WHEREpat(p,e) => WHEREpat(pat p,exp e)
                | NESTEDpat(p,e,p') => NESTEDpat(pat p,exp e,pat p')
            in rwPat pat p end

        and fbind(FUNbind(id,c)) = FUNbind(id,map clause c)

        and clause(CLAUSE(ps,g,e)) = CLAUSE(map pat ps,guard g,exp e)

        and guard NONE = NONE
          | guard (SOME e) = SOME(exp e)

        and vbind(VALbind(p,e)) = VALbind(pat p,exp e)

        and dbind db = db

        and tbind(TYPEbind(x,tvs,t)) = TYPEbind(x,tvs,ty t)
    in  { pat=pat,
          exp=exp,
          decl=decl,
          sexp=sexp,
          ty=ty
        }
    end
end
