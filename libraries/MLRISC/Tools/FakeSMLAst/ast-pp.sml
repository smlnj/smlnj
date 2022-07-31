(*
 * Pretty printer for the AST
 * 
 * Allen Leung (leunga@cs.nyu.edu)
 *)
functor MDLAstPrettyPrinter(AstUtil : MDL_AST_UTIL) : MDL_AST_PRETTY_PRINTER =
struct

   structure Ast = AstUtil.Ast

   open PP Ast MDLError
   infix ++

   fun error msg = MDLError.error("error while processing "^msg)

   val goodBreak = nl'(5,3) (* if over column 75, tab and indent by 3 *)
   val goodFunBreak = nl'(5,3) (* if over column 75, tab and indent by 3 *)

   val comma = !! ", "
   val semi = !! "; "
   val cons = !! "::"
   val dot  = !! "."
   val list = seq(!! "[",comma++goodBreak,!! "]")
   val tuple = seq(!! "(",comma++goodBreak,!! ")")
   val vector = seq(!! "#[",comma++goodBreak,!! "]")
   val record = seq(!! "{",comma++goodBreak,!! "}")
   val bars = seq(settab,nl'(5,0) ++ tab' ~2 ++ ! "|" ++ tab,unindent)
   val ands = seq(settab,tab' ~4 ++ ! "and" ++ tab,unindent)

   fun isAlpha "" = true
     | isAlpha s  = Char.isAlpha(String.sub(s,0))

   fun isMLSym #"'" = false
     | isMLSym #"_" = false
     | isMLSym #"." = false
     | isMLSym c    = Char.isPunct c

   fun isComplex s = 
   let fun loop(~1, alpha, sym) = alpha andalso sym
         | loop(i, alpha, sym) =
           let val c = String.sub(s,i)
           in  loop(i-1, alpha orelse Char.isAlphaNum c,
                         sym   orelse isMLSym c)
           end
   in  loop(String.size s - 1, false, false) end

   fun encodeChar c = if isMLSym c then "_"^Int.toString(Char.ord c)
                      else Char.toString c

   fun encodeName s = String.translate encodeChar s

   fun name id = if isComplex id then encodeName id else id

   fun ident(IDENT([],id)) = if isSym id then !"op" ++ ! id 
                             else if isAlpha id then !(name id)
                             else sp ++ !id
     | ident(IDENT(p,id)) = seq(nop,dot,nop) (map ! (p @[name id]))

   and literal(WORDlit w) = word w
     | literal(WORD32lit w) = word32 w
     | literal(INTINFlit i) = 
         select
           (fn "code" =>
             (!"(IntInf.fromInt" ++ int(IntInf.toInt i)  ++ !!")"
              handle Overflow =>
             !"(Option.valOf(IntInt.fromString"++string(IntInf.toString i)++ !!"))"
             )
           | _ => intinf i
           )
     | literal(INTlit i) = int i
     | literal(INT32lit i) = int32 i
     | literal(STRINGlit s) = string s
     | literal(CHARlit c) = char c
     | literal(BOOLlit b) = bool b
     | literal(REALlit r) = !r

   and exp(LITexp l) = literal l
     | exp(IDexp id) = ident id
     | exp(CONSexp(id,e)) = ident id ++ sp ++ exp' e
     | exp(LISTexp(es,NONE)) = if length es >= 10 then longlistexp es 
                               else list (map appexp es)
     | exp(LISTexp([],SOME e)) = exp e
     | exp(LISTexp(es,SOME e)) = seq(nop,cons,cons) (map exp es) ++ exp e
     | exp(TUPLEexp [e]) = exp e
     | exp(TUPLEexp es) = tuple (map appexp es)
     | exp(VECTORexp es) = vector (map appexp es)
     | exp(RECORDexp es) = record(map labexp es)
     | exp(SEQexp []) = ! "()"
     | exp(SEQexp [e]) = exp e
     | exp(SEQexp es) = nl ++ tab ++ 
                        seq(! "(" ++ sp ++ 
                            settab,semi++nl++tab,unindent ++ tab ++ ! ")")
                          (map appexp es)
     | exp(APPexp(e as IDexp(IDENT([],f)),e' as TUPLEexp[x,y])) = 
         if isSym f then
            paren(exp x ++ sp ++ ! f ++ sp ++ exp y)
         else
            paren(exp e ++ !! " " ++ exp e')
     | exp(APPexp(f,x)) = paren(appexp f ++ !! " " ++ exp x)
     | exp(IFexp(x,y,z)) = paren(line(! "if" ++ sp ++ exp x) ++ 
                           block(line(! "then" ++ sp ++ exp y) ++
                                 tab ++ ! "else" ++ sp ++ exp z))
     | exp(RAISEexp e) = ! "raise" ++ exp e
     | exp(HANDLEexp(e,c)) = paren(exp e ++ sp ++ ! "handle" ++ sp ++ clauses c)
     | exp(CASEexp(e,c)) = 
           nl ++ line(! "(case" ++ sp ++ appexp e ++ sp ++ ! "of") 
           ++ tab' 2 ++ settab ++ block(clauses c) ++ unindent ++ tab ++ !!")"
     | exp(LAMBDAexp c) = group ("(",")") (! "fn" ++ sp ++ clauses c)
     | exp(LETexp([],e)) = expseq e
     | exp(LETexp(d,e)) = nl ++ tab ++ ! "let" ++ sp ++ settab ++
                          decls d ++ unindent ++
                          line(! "in" ++ sp ++ expseq e) ++ tab ++ ! "end"
     | exp(TYPEDexp(e,t)) = paren(exp e ++ sp ++ !!":" ++ sp ++ ty t)
     | exp(MARKexp(_,e)) = exp e
     | exp(LOCexp(id,e,region)) = locexp(id,e,region)
     | exp(BITSLICEexp(e,slices)) = 
         select(fn "code" => exp(AstUtil.BITSLICE(e,slices))
                 | "pretty"   => exp e ++ sp ++ ! "at"  ++
                     list(map (fn (i,j) => int i ++ !! ".." ++ int j) slices)
                 | mode => (error mode; nop)
               )
     | exp(TYPEexp t) = ty t
     | exp(ASMexp a) = (error "PP.ASMexp"; nop)
     | exp(RTLexp r) =
         select(fn "pretty" => rtl r
                 | mode => (error mode; nop)
               )
     | exp(CONTexp(e,x)) = exp e

   and rtl r = seq(!"[[",sp,!"]]") (map rtlterm r)

   and rtlterm(LITrtl s) = string s
     | rtlterm(IDrtl x)  = ! x
 
   and longlistexp es =
         select(fn "pretty" => list(map appexp es)
                 | "code" => codelonglistexp es)

   and prettylonglistexp es =
          nl ++ tab ++ seq(! "[",comma++nl++tab,! "]") (map appexp es)
   and codelonglistexp es =
          nl ++
          line(!"let infix $$ fun x $$ y = y::x") ++
          line(!"in  nil") ++
          block(concat(map (fn e => line(!"$$" ++ appexp e)) (rev es))) ++
          line(!"end")
       
   and appexp(APPexp(e as IDexp(IDENT([],f)),e' as TUPLEexp[x,y])) = 
         if isSym f then exp x ++ sp ++ ! f ++ sp ++ exp y
         else exp e ++ !! " " ++ exp e'
     | appexp(APPexp(f,x)) = (appexp f ++ !! " " ++ exp x)
     | appexp(SEQexp[e])   = appexp e
     | appexp(TUPLEexp[e]) = appexp e
     | appexp e = exp e

   and exp' NONE = nop
     | exp'(SOME e) = if isParenedExp e then exp e else paren(exp e)

   and isParenedExp(IDexp _) = true
     | isParenedExp(TUPLEexp []) = true
     | isParenedExp(TUPLEexp [x]) = isParenedExp x
     | isParenedExp(TUPLEexp _) = true
     | isParenedExp(RECORDexp _) = true
     | isParenedExp(LISTexp _) = true
     | isParenedExp(VECTORexp _) = true
     | isParenedExp _ = false

   and isSym "+" = true
     | isSym "-" = true
     | isSym "*" = true
     | isSym "mod" = true
     | isSym "div" = true
     | isSym "=" = true
     | isSym "<>" = true
     | isSym "<" = true
     | isSym ">" = true
     | isSym ">=" = true
     | isSym "<=" = true
     | isSym "<<" = true
     | isSym ">>" = true
     | isSym "~>>" = true
     | isSym "||" = true
     | isSym "&&" = true
     | isSym "^" = true
     | isSym ":=" = true
     | isSym "::" = true
     | isSym "@" = true
     | isSym "andalso" = true
     | isSym "orelse" = true
     | isSym "o" = true
     | isSym _ = false

   and locexp(id,e,region) = 
          select(fn "pretty" => 
                  !!"$" ++ ! id ++ !!"[" ++ exp e ++ 
                    (case region of
                      SOME r => ! ":" ++ ! r
                    | NONE => nop
                    ) ++
                  !!"]"
                  | "code" => paren(exp e ++ ! "+" ++ !("offset"^id))
                  | mode => (error mode; nop)
                )

   and decl(DATATYPEdecl(dbs,tbs)) = datatypedecl(dbs,tbs)
     | decl(FUNdecl(fbs)) = fundecl fbs
     | decl(RTLdecl(p,e,_)) = 
	   line(! "rtl " ++ pat p ++ ! "=" ++ exp e)
     | decl(VALdecl(vbs)) = valdecl vbs
     | decl(VALSIGdecl(ids,ty)) = valsig("val",ids,ty)
     | decl(RTLSIGdecl(ids,ty)) = valsig("rtl",ids,ty)
     | decl(TYPESIGdecl(id,tvs)) = typesig(id,tvs)
     | decl(LOCALdecl([],d2)) = decls d2
     | decl(LOCALdecl(d1,d2)) = 
           line(! "local") ++ block(decls d1) ++ line(! "in ") ++
           block(decls d2) ++ line(! "end")
     | decl(SEQdecl ds) = decls ds
     | decl($ ds) = concat(map line (map !! ds))
     | decl(STRUCTUREdecl(id,[],s,se)) = 
           line(! "structure" ++ ! id ++ sigconOpt(s) ++ ! "=" ++ sexp se)
     | decl(STRUCTURESIGdecl(id,se)) = 
           line(! "structure" ++ ! id ++ !":" ++ sigexp se)
     | decl(STRUCTUREdecl(id,ds,s,se)) = 
           line(! "functor" ++ ! id ++ settab ++ !! "(" ++ settab ++
                 decls ds ++ unindent ++
                 tab ++ !! ")" ++ unindent ++ sigconOpt(s) ++ 
                 ! "=" ++ nl ++ sexp se)
     | decl(FUNCTORdecl(id,[],s,se)) = 
           line(! "functor" ++ ! id ++ sigconOpt(s) ++ ! "=" ++ nl ++ sexp se)
     | decl(FUNCTORdecl(id,ds,s,se)) = 
           line(! "functor" ++ ! id ++ settab ++ !! "(" ++ settab ++
                 decls ds ++ unindent ++
                 tab ++ !! ")" ++ unindent ++ sigconOpt(s) ++ 
                 ! "=" ++ nl ++ sexp se)
     | decl(SIGNATUREdecl(id,se)) = 
           line(! "signature" ++ ! id ++ ! "=" ++ sigexp se)
     | decl(OPENdecl ids) = 
           line(! "open" ++ seq(nop,sp,nop)(map ident ids))
     | decl(INCLUDESIGdecl s) = line(! "include" ++ sigexp s) 
     | decl(FUNCTORARGdecl(id,se)) = ! id ++ sigcon se
     | decl(EXCEPTIONdecl ebs) =
           line(!"exception" ++ ands(map exceptionbind ebs))
     | decl(SHARINGdecl s) = line(! "sharing" ++ ands(map share s))
     | decl(MARKdecl(l,d)) = 
        nl++ !(SourceMapping.directive l) ++nl ++ decl d 
     | decl(INFIXdecl(i,ids)) = line(! "infix" ++ int i ++ concat(map ! ids))
     | decl(INFIXRdecl(i,ids)) = line(! "infixr" ++ int i ++ concat(map ! ids))
     | decl(NONFIXdecl ids) = line(! "nonfix" ++ concat(map ! ids))
     | decl(ARCHdecl(id,ds)) = 
          line(! "architecture" ++ ! id ++ !"=" ++ decls ds)
     | decl(BITSORDERINGdecl _) = line(! "bitsordering...")
     | decl(FORMATdecl _) = line(!"instruction formats ...")
     | decl(ARCHKINDdecl SUPERSCALAR) = line(!"superscalar")
     | decl(ARCHKINDdecl VLIW) = line(!"vliw")
     | decl(ENDIANESSdecl LITTLE) = line(!"little endian")
     | decl(ENDIANESSdecl BIG) = line(!"big endian")
     | decl(STORAGEdecl _) = line(!"storage ...")
     | decl(LOCATIONSdecl _) = line(!"locations ...")
     | decl(NAMEdecl _) = line(!"name ...")
     | decl(VERSIONdecl _) = line(!"version ...")
     | decl(ASSEMBLYCASEdecl _) = line(!"assembly ...")
     | decl(INSTRUCTIONdecl cbs) = line(!"instruction" ++ 
                                      tab' ~6 ++ consbinds cbs)
     | decl(DEBUGdecl _) = line(!"debug ...")
     | decl(RESOURCEdecl _) = line(!"resource ...")
     | decl(CPUdecl _) = line(!"cpu ...")
     | decl(PIPELINEdecl _) = line(!"pipeline ...")
     | decl(LATENCYdecl _) = line(!"latency ...")

   and exceptionbind(EXCEPTIONbind(id,NONE)) = ! id
     | exceptionbind(EXCEPTIONbind(id,SOME t)) = !id ++ !"of" ++ ty t
     | exceptionbind(EXCEPTIONEQbind(id,id')) = !id ++ !"=" ++ ident id'

   and share(TYPEshare ids) = !"type" ++ seq(nop,! "=",nop) (map ident ids)
     | share(STRUCTshare ids) = seq(nop,! "=",nop) (map ident ids)

   and sigexp(IDsig id) = ident id
     | sigexp(WHEREsig(se,x,s)) = 
	sigexp se ++ !"where" ++ ident x ++ !! "=" ++ sexp s
     | sigexp(WHERETYPEsig(se,x,t)) = 
	sigexp se ++ !"where type" ++ ident x ++ !! "=" ++ ty t
     | sigexp(DECLsig ds) = line(!"sig") ++ block(decls ds) ++ line(!"end")

   and sigconOpt(NONE) = nop
     | sigconOpt(SOME s) = sigcon s

   and sigcon{abstract=false,sigexp=s} = !":" ++ sigexp s
     | sigcon{abstract=true,sigexp=s} = !":>" ++ sigexp s

   and sexp (IDsexp id) = ident id
     | sexp (APPsexp(a,DECLsexp ds)) = sexp a ++ nl ++ 
                             block(line(group("(",")") (decls ds)))
     | sexp (APPsexp(a,IDsexp id)) = sexp a ++ paren(ident id)
     | sexp (APPsexp(a,b)) = sexp a ++ nl ++ paren(sexp b)
     | sexp (DECLsexp ds) = line(!"struct") ++ block(decls ds) ++ line(!"end")
     | sexp (CONSTRAINEDsexp(s,si)) = sexp s ++ !":" ++ sigexp si

   and decls ds = concat (map decl ds)

   and valsig (keyword,[],t) = nop
     | valsig (keyword,id::ids,t) = 
          line(! keyword ++ ! id ++ ! ":" ++ sp ++ ty t) ++ 
          valsig(keyword,ids,t)

   and typesig (id,tvs) = line(! "type" ++ tyvars tvs ++ ! id) 

   and expseq es = block(seq(nop,semi++nl++tab,nop) (map appexp es))

   and labexp(id,e) = ! id ++ !! "=" ++ appexp e

   and ty(IDty id) = ident id
     | ty(TYVARty tv) = tyvar tv
     | ty(APPty(id,[t])) = pty t ++ sp ++ ident id
     | ty(APPty(id,tys)) = tuple(map ty tys) ++ sp ++ ident id
     | ty(FUNty(x,y)) = ty x ++ !! " -> " ++ fty y
     | ty(TUPLEty []) = ! "unit"
     | ty(TUPLEty [t]) = ty t
     | ty(TUPLEty tys) = seq(nop,!! " * ",nop) (map pty tys)
     | ty(RECORDty labtys) = record(map labty labtys)
     | ty(CELLty id) = 
           select( fn "pretty" => !!"$" ++ !id 
                    | "code" => !(if id = "cellset" then "C.cellset" 
                                  else "CellsBasis.cell")
                    | mode => (error mode; nop)
                 )
     | ty(VARty(TYPEkind,i,_,ref NONE)) = !("'X"^Int.toString i)
     | ty(VARty(INTkind,i,_,ref NONE)) = 
           select (fn "pretty" => !("#X"^Int.toString i)
                    | "code"   => !("T"^Int.toString i))
     | ty(VARty(_,_,_,ref(SOME t))) = ty t
     | ty(POLYty(vars,t)) = ty t
     | ty(INTVARty i) = select (fn "pretty" => !!"#" ++ int i
                                 | "code" => int i) 
     | ty(LAMBDAty(vars,t)) = !!"\\" ++ tuple(map ty vars) ++ !!"." ++ ty t 

   and fty(t as FUNty _) = ty t
     | fty t = pty t

   and pty(t as FUNty _) = paren(ty t)
     | pty(TUPLEty[t]) = pty t
     | pty(t as TUPLEty []) = ty t
     | pty(t as TUPLEty _) = paren(ty t)
     | pty(t as RECORDty _) = ty t
     | pty(t as IDty _) = ty t
     | pty(t as APPty _) = ty t
     | pty(t as VARty _) = ty t
     | pty(t as TYVARty _) = ty t
     | pty t = paren(ty t)

   and labty (id,t) = ! id ++ !! ":" ++ ty t 

   and pat(IDpat id)   = if isSym id then !"op" ++ !id else !(name id)
     | pat(WILDpat)    = ! "_"
     | pat(ASpat(id,p)) = paren(!id ++ !"as" ++ sp ++ pat p)
     | pat(LITpat l)   = literal l
     | pat(LISTpat(ps,NONE)) = list(map pat ps)
     | pat(LISTpat([],SOME p)) = pat p 
     | pat(LISTpat(ps,SOME p)) = seq(nop,cons,cons) (map pat ps) ++ pat p
     | pat(TUPLEpat [p]) = pat p
     | pat(TUPLEpat ps) = tuple(map pat ps)
     | pat(VECTORpat ps) = vector(map pat ps)
     | pat(RECORDpat(lps,flex)) = 
           record(map labpat lps @ (if flex then [! "..."] else []))
     | pat(TYPEDpat(p,t)) = paren(pat p ++ !! ":" ++ ty t)
     | pat(CONSpat(id,NONE)) = ident id 
     | pat(CONSpat(IDENT([],"::"),SOME(TUPLEpat[x,y]))) = 
           paren(pat x ++ sp ++ !!"::" ++ sp ++ pat y)
     | pat(CONSpat(id,SOME p)) = ident id ++ ppat p
     | pat(ORpat [p]) = pat p
     | pat(ORpat ps) = 
          if length ps > 10 
          then nl ++ tab ++ seq(! "(",! "|"++nl++tab,! ")") (map pat ps)
          else seq(!! "(", ! "|"++sp, !! ")") (map pat ps)
     | pat(ANDpat [p]) = pat p
     | pat(ANDpat ps) = seq(!! "(",sp ++ !"and" ++ sp, !!")") (map pat ps)
     | pat(NOTpat p) = !"not" ++ sp ++ pat p
     | pat(WHEREpat(p,e)) = pat p ++ sp ++ !"where" ++ sp ++ exp e
     | pat(NESTEDpat(p,e,p')) = pat p ++ sp ++ !"where" ++ sp ++ exp e ++
                                 sp ++ !"in" ++ sp ++ pat p'    

   and ppat(p as (CONSpat _ | ASpat _)) = paren(pat p)
     | ppat p = pat p

   and pats ps = concat(map pat ps)

   and ppats ps = concat(map (fn p => ppat p ++ sp) ps)

   and labpat(id,p as IDpat id') = 
         if id = id' then  ! id
         else ! id ++ !! "=" ++ pat p
     | labpat(id,p) = ! id ++ !! "=" ++ pat p

   and funbind(FUNbind(id,c)) = bars (map (funclause id) c)

   and funbinds fbs = ands (map funbind fbs) 

   and funclause id (CLAUSE(ps,g,e)) = 
        line(!(name id) ++ sp ++ ppats ps ++ sp ++ guard g ++ ! "=" ++ 
            sp ++ goodFunBreak ++ appexp e)

   and guard NONE = nop
     | guard (SOME e) = ! "where" ++ sp ++ appexp e ++ sp

   and clause (CLAUSE([p],g,e)) = 
        line(settab ++ pat p ++ sp ++ guard g ++ 
             ! "=>" ++ sp ++ goodFunBreak ++ appexp e ++ unindent)
     | clause (CLAUSE(ps,g,e)) = 
        line(settab ++ ppats ps ++ sp ++ guard g ++ 
             ! "=>" ++ sp ++ appexp e ++ unindent)

   and clauses c = block(bars (map clause c))

   and fundecl [] = nop
     | fundecl fbs = (* nl ++ *) tab ++ ! "fun" ++ sp ++ settab ++ 
                     funbinds fbs ++ unindent

   and valbind (VALbind(p,e)) = 
         line(settab ++ pat p ++ sp ++ ! "=" ++ sp ++ appexp e ++ unindent)

   and valbinds vbs = block(ands (map valbind vbs))

   and valdecl [] = nop
     | valdecl vbs = tab ++ ! "val" ++ sp ++ valbinds vbs 
 
   and datatypebind(DATATYPEbind{id,tyvars=ts,cbs,...}) =
       line(tyvars ts ++ ! id ++ ! "=") ++ 
       tab' ~6 ++ consbinds cbs
     | datatypebind(DATATYPEEQbind{id,tyvars=ts,ty=t,...}) =
       line(tyvars ts ++ ! id ++ ! "=" ++ !"datatype" ++ ty t)

   and datatypebinds ds = block(ands(map datatypebind ds))

   and consbinds cbs = bars(map consbind cbs)

   and consbind(CONSbind{id,ty=NONE,...}) = line(! id)
     | consbind(CONSbind{id,ty=SOME t,...}) = line(! id ++ ! "of" ++ sp ++ ty t)

   and typebind(TYPEbind(id,ts,t)) =
       line (tyvars ts ++ !id ++ ! "=" ++ sp ++ ty t)

   and typebinds tbs = block(ands (map typebind tbs))

   and tyvars []  = nop
     | tyvars [t] = tyvar t
     | tyvars tvs = tuple(map tyvar tvs)

   and tyvar (VARtv tv) = ! tv
     | tyvar (INTtv tv) = sp ++ !! "#" ++ ! tv

   and range(x,y) = paren(int x ++ comma ++ int y)

   and datatypedecl([],t) = tab ++ ! "type" ++ block(ands (map typebind t))
     | datatypedecl(d,t) =
       tab ++ ! "datatype" ++
       datatypebinds d ++
       (case t of
           [] => nop
        |  _  => tab ++ ! "withtype" ++ typebinds t
       )

end
