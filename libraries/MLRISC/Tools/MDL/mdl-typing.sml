(*
 * Type checking for RTL.
 * We also perform arity raising to convert the program into explicit type
 * passing style at the same time.
 *
 * Note: there are quite a lot of bugs in the algorithm. 
 *       I don't have time to fix them.
 *
 * Allen Leung (leunga@cs.nyu.edu)
 *)
functor MDLTyping
  (structure Env      : MDL_ENV
   structure TypeUtil : MDL_TYPE_UTILS
   structure AstUtil  : MDL_AST_UTIL
   structure AstPP    : MDL_AST_PRETTY_PRINTER
   structure Comp     : MDL_COMPILE
     sharing Env.Ast = TypeUtil.Ast = AstUtil.Ast = AstPP.Ast
     sharing Comp.Env = Env
  ) : MDL_TYPING =
struct
   structure Env      = Env
   structure Ast      = Env.Ast
   structure TypeUtil = TypeUtil
   structure Comp     = Comp
   structure R        = Comp.Rewriter

   open Comp.Error Ast AstUtil

   infix ++

   val op ++ = Env.++

   fun e2s e = PP.text(AstPP.exp e)
   fun p2s e = PP.text(AstPP.pat e)
   fun d2s e = PP.text(AstPP.decl e)
   fun id2s e = PP.text(AstPP.ident e)
   fun t2s e = PP.text(AstPP.ty e)

   fun unifyExp(e,t1,t2) = TypeUtil.unify(fn _ => " in "^e2s e,t1,t2)
   fun unifyPat(p,t1,t2) = TypeUtil.unify(fn _ => " in "^p2s p,t1,t2)
   fun undefinedCons(pat,id) = 
       error("undefined constructor "^id2s id^" in "^p2s pat)

   fun lookupCons E id = Env.lookupVal E id

   fun isPolymorphic t =
   let val poly = ref false
       fun ty ==> (t as VARty(_,_,_,ref NONE)) = (poly := true; t)
         | ty ==> (t as POLYty _) = (poly := true; t)
         | ty ==> (t as TYVARty _) = (poly := true; t)
         | ty ==> t = t
   in  #ty (R.rewrite{ty=ty,pat=R.noRewrite,decl=R.noRewrite,
                      sexp=R.noRewrite,exp=R.noRewrite}) t;
       !poly
   end

   fun openStrs E ids =
   let val Es = map (Env.lookupStr E) ids
   in  foldr op++ Env.empty Es end

   fun boundVar E (VARtv _) = TypeUtil.newVar 0
     | boundVar E (INTtv _) = TypeUtil.newIVar 0

   fun polyTy name = let val tv = TypeUtil.newIVar 0
                     in POLYty([tv],APPty(IDENT([],name),[tv])) end
   fun mkTy name = IDty(IDENT([],name))
   val bitsTy    = polyTy "bits"
   val regionTy  = mkTy "region"
   val effectTy  = mkTy "effect"
   val boolTy    = mkTy "bool"
   val stringTy  = mkTy "string"
   val intTy     = mkTy "int"
   val wordTy    = mkTy "word"
   fun listTy(ps,t) = 
         APPty(IDENT([],"list"),[INTVARty(length ps),TypeUtil.deref t])

   fun appTy name x = APPty(IDENT([],name), [x])
   fun intappTy name n = appTy name (INTVARty n)

   (*
    * Perform typechecking
    *)
   fun typeCheck md d = 
   let val _   = TypeUtil.init()
       val bitsOf = intappTy "bits"
       val cellOf = intappTy "bits"

       val CELLdecl{bits=widthOfGP, ...} = Comp.lookupCellKind md "GP"

       fun map2 f [] = ([], [])
         | map2 f (x::xs) = let val (a,b) = f x
                                val (c,d) = map2 f xs
                            in  (a::c,b::d) end

       fun memOf E (exp, e, "cellset", region) = 
             (case region of
                SOME _ => error("illegal region in "^e2s exp)
              | NONE => ();
             (bitsOf widthOfGP, cellOf widthOfGP, e)
             )
         | memOf E (exp, e, m, region) =
           let val CELLdecl{bits=n, count, id, aggregable, ...} = 
                Comp.lookupCellKind md m
               fun log2 1 = 0
                 | log2 n = log2(n div 2) + 1
               val argTy = case count of  
                     SOME m => bitsOf(log2 m)
                   | NONE   => 
                      (case id of
                         "MEM" => bitsOf widthOfGP
                       | "CTRL" => bitsOf widthOfGP
                       | _ => 
                         (error("$"^m^"["^e2s e^"] is illegal"); Env.var E)
                      )
                val _ = 
                   if id = "MEM" then 
                   (case region of 
                      SOME r => let val (_,t') = W E (ID r)
                                in  unifyExp(exp,t',regionTy) end
                    | NONE => warning("missing region in "^e2s exp)
                   ) else
                   (case region of 
                      SOME _ => error("illegal region in "^e2s exp)
                   | _ => ()
                   )
                val retTy =
                    if aggregable then appTy "bits" (TypeUtil.newIVar 0)
                    else cellOf n
           in  (argTy, retTy, e)
           end

       and W E (IDexp id) = Env.lookupVal E id
         | W E (e as TUPLEexp []) = (e, effectTy)
         | W E (TUPLEexp [e]) = W E e
         | W E (TUPLEexp es) = 
           let val (es, ts) = Ws E es
           in  (TUPLEexp es, TUPLEty ts) end
         | W E (RECORDexp les) = 
           let val (les, lts) = LWs E les
           in  (RECORDexp les, RECORDty lts) end
         | W E (e as LITexp(INTlit _))  = (e, intTy)
         | W E (e as LITexp(WORD32lit _)) = (e, wordTy)
         | W E (e as LITexp(WORDlit _)) = (e, wordTy)
         | W E (e as LITexp(BOOLlit _)) = (e, boolTy)
         | W E (e as LITexp(STRINGlit _)) = (e, stringTy)
         | W E (exp as TYPEDexp(e,t)) = 
           let val (e, t1) = W E e
               val t2 = T E t
           in  unifyExp(exp,t1,t2); (e, t2) end
         | W E (exp as LISTexp(es,NONE)) = 
           let val (es, ts) = Ws E es
               val t  = Env.var E
           in  foldr (fn (a,b) => (unifyExp(exp,a,b); a)) t ts;
               (LISTexp(es,NONE), listTy(es,t))
           end
         | W E (exp as BITSLICEexp(e,l)) = 
           let val (e,t) = W E e
               val n = foldr (fn ((a,b),l) => b-a+1+l) 0 l
               val (e,t') = Env.inst E (BITSLICEexp(e,l), bitsTy)
           in  unifyExp(exp,t,t'); 
               (e, bitsOf n)
           end
         | W E (exp as LOCexp(id,e,region)) = 
           let val (e, t) = W E e
               val (argTy, retTy, e) = memOf E (exp, e, id, region)
           in  unifyExp(exp,t,argTy);
               (LOCexp(id,e,region), retTy)
           end
         | W E (exp as APPexp(f,x)) = 
           let val (f, t1) = W E f
               val (x, t2) = W E x
               val t  = Env.var E
           in  unifyExp(exp,t1,FUNty(t2,t)); 
               (APPexp(f, x), t) 
           end
         | W E (exp as IFexp(a,b,c)) = 
           let val (a, t1) = W E a
               val (b, t2) = W E b
               val (c, t3) = W E c
           in  unifyExp(a,t1,boolTy); unifyExp(exp,t2,t3); 
               (IFexp(a,b,c), t2)
           end
         | W E (exp as CASEexp(e,cs)) = 
           let val (e,  t1) = W E e
               val (cs, t2) = CSs E cs
               val t3 = Env.var E
           in  unifyExp(exp,t2,FUNty(t1,t3));
               (CASEexp(e, cs), t3) 
           end
         | W E (LAMBDAexp cs) = 
           let val (cs, t) = CSs E cs
           in  (LAMBDAexp cs, t) end
         | W E (e as SEQexp []) = (e, effectTy)
         | W E (SEQexp [e]) = W E e
         | W E (SEQexp(e::es)) = 
           let val (e, _)  = W E e
               val (es, t) = Wseq E es
           in  (SEQexp(e::es), t) end
         | W E (LETexp(ds,es)) = 
           let val (ds, E') = Ds E ds
               val (es, t)  = Wseq (E ++ E') es 
           in  (LETexp(ds, es), t) end
         | W E (MARKexp(l,e)) = (setLoc l; W E e)
         | W E exp = fail("W "^e2s exp)
    
       and Ws E es = map2 (W E) es

       and Wseq E [] = 
           let val (e, t) = W E (SEQexp [])
           in  ([e], t) end
         | Wseq E [e] = 
           let val (e, t) = W E e
           in  ([e], t) end
         | Wseq E (e::es) =
           let val (e, _)  = W E e
               val (es, t) = Wseq E es  
           in  (e::es, t) end
    
       and LW E (l,e) = 
           let val (e, t) = W E e
           in  ((l,e), (l,t)) end

       and LWs E les = map2 (LW E) les
    
       and CSs E [] = ([], Env.var E)
         | CSs E (all as c::cs) =
           let val (c, t) = CS E c
               val (cs, t') = CSs E cs
           in  unifyExp(LAMBDAexp all,t,t'); 
               (c::cs, t) 
           end
       
       and CS E (CLAUSE(ps, g, e)) =
           let val (ts, Es) = map2 (P E) ps
               val E' = foldr op++ Env.empty Es   
               val (e, t2) = W (E ++ E') e
               fun f []      = t2
                 | f (t::ts) = FUNty(t,f ts)
               val g = 
                 case g of 
                    NONE => NONE
                  | SOME ge => 
                    let val (ge', tg) = W E ge
                    in  unifyExp(ge, tg, BOOLty);
                        SOME ge
                    end
           in  (CLAUSE(ps, g, e), f ts) end
    
       and P E (IDpat id) = Pvar E id
         | P E (ASpat(id,p)) =
           let val (t1,E') = P E p
               val E''     = Env.VALbind(id,ID id,t1)
           in  (t1, E'' ++ E') end
         | P E (TUPLEpat [p]) = P E p
         | P E (TUPLEpat ps) = 
           let val (ts,E') = Ps E ps in (TUPLEty ts,E') end
         | P E (pat as ORpat ps) =
           let val (ts,E') = Ps E ps
               val t       = Env.var E
           in  foldr (fn (t1,t2) => (unifyPat(pat,t1,t2); t1)) t ts;
               (t,E')
           end
         | P E (RECORDpat(lps,false)) =
           let val (lts,E') = LPs E lps in (RECORDty lts,E') end
         | P E WILDpat = (Env.var E, Env.empty)
         | P E (LITpat(INTlit _)) = (INTty, Env.empty)
         | P E (LITpat(BOOLlit _)) = (BOOLty, Env.empty)
         | P E (LITpat(WORDlit _)) = (WORD32ty, Env.empty)
         | P E (LITpat(STRINGlit _)) = (STRINGty, Env.empty)
         | P E (pat as CONSpat(id,NONE)) =
           (let val (_,t1) = lookupCons E id
            in  (t1, Env.empty)
            end handle _ => 
                case id of
                  IDENT([],id) => Pvar E id
                | _ => (undefinedCons(pat,id); (Env.var E, Env.empty))
           )
         | P E (pat as CONSpat(id,SOME p)) =
           (let val (_,t1) = lookupCons E id
                val (t2,E') = P E p
                val t3 = Env.var E
            in  unifyPat(pat,t1,FUNty(t2,t3)); (t3,E')
            end handle _ => 
                case id of
                  IDENT([],id) => Pvar E id
                | _ => (undefinedCons(pat,id); (Env.var E, Env.empty))
           )
         | P E (pat as LISTpat(ps,NONE)) = 
            let val (ts,E') = Ps E ps
                val t = Env.var E
            in  foldr (fn (a,b) => (unifyPat(pat,a,b); a)) t ts;
                (listTy(ps,t), E')
            end
         | P E p = (error("pattern "^p2s p^
                          " not allowed in semantics description"); 
                    (Env.var E, Env.empty)
                   )
       and Ps E ps = 
           let val xs = map (P E) ps
               val ts = map #1 xs
               val Es = map #2 xs
           in  (ts, foldr op++ Env.empty Es) end
    
       and LPs E lps =
           let val xs  = map (LP E) lps
               val lts = map #1 xs
               val Es  = map #2 xs
           in  (lts, foldr op++ Env.empty Es) end
    
       and LP E (l,p) = let val (t,E) = P E p in ((l,t), E) end
    
       and Pvar E id = let val t = Env.var E
                       in  (t, Env.VALbind(id,ID id,t)) end
    
       and D E (DATATYPEdecl(dbs,tbs)) = 
           let val (dbs, tbs, E) = DTs E (dbs,tbs) 
           in  (DATATYPEdecl(dbs, tbs), E) end   
         | D E (FUNdecl fbs) = 
           let val (fbs, E) = FDs E fbs
           in  (FUNdecl fbs, E) end
         | D E (RTLdecl(pat,e,loc)) = 
           let val (VALbind(pat,e), E) = VD E (VALbind(pat,e)) 
           in  (RTLdecl(pat,e,loc), E) end
         | D E (RTLSIGdecl(ids,ty)) = 
           let val E = VS E (ids,ty)
           in  (RTLSIGdecl(ids, ty), E) end
         | D E (VALdecl vbs) = 
           let val (vbs, E) = VDs E vbs
           in  (VALdecl vbs, E) end
         | D E (TYPESIGdecl(id,tvs)) =
           let val E = TS E (id,tvs)
           in  (TYPESIGdecl(id,tvs), E) end  
         | D E (VALSIGdecl(ids,ty)) = 
           let val E = VS E (ids,ty)
           in  (VALSIGdecl(ids,ty), E) end
         | D E (LOCALdecl(d1,d2)) =
           let val (d1, E1) = Ds E d1 
               val (d2, E2) = Ds (E ++ E1) d2 
           in  (LOCALdecl(d1, d2), E2)  end
         | D E (SEQdecl ds) = 
           let val (ds, E) = Ds E ds
           in  (SEQdecl ds, E) end
         | D E (d as OPENdecl ids) = 
           let val E = openStrs E ids
           in  (d, E) end
         | D E (STRUCTUREdecl(id,args,s,sexp)) =
           let val (sexp, E') = SE E sexp
           in  (STRUCTUREdecl(id,args,s,sexp), Env.STRbind(id,args,E')) end
         | D E (d as STRUCTURESIGdecl _) = (d, Env.empty)
         | D E (d as INFIXdecl _) = (d, Env.empty)
         | D E (d as INFIXRdecl _) = (d, Env.empty)
         | D E (d as NONFIXdecl _) = (d, Env.empty)
         | D E (MARKdecl(l,d)) = 
           let val (d, E) = withLoc l (D E) d
           in  (MARKdecl(l,d), E) end  
         | D E (d as $[]) = (d, Env.empty)
         | D _ d = (error("illegal declaration: "^d2s d); (d, Env.empty))
    
       and Ds E [] = ([], Env.empty)
         | Ds E (d::ds) = 
           let val (d, E1)  = D E d 
               val (ds, E2) = Ds (E ++ E1) ds
           in  (d::ds, E1 ++ E2) end
    
       and TS E (id,[]) = Env.TYPEbind(id,IDty(IDENT([],id)))
         | TS E (id,tvs) =
           let val vs = map (boundVar E) tvs
               val t  = LAMBDAty(vs,APPty(IDENT([],id),vs))
           in  Env.TYPEbind(id,t) end
    
       and VS E (ids,ty) = 
           let val t = T E ty
           in  foldr (fn (id,E) => Env.VALbind(id,ID id,t) ++ E) Env.empty ids 
           end
    
       and FDs E [] = ([], Env.empty)
         | FDs E (fb::fbs) =
           let val (fb, E') = FD E fb 
               val (fbs, E'') = FDs (E ++ E') fbs
           in  (fb::fbs, E' ++ E'') end
    
       and FD E (FUNbind(f,cs)) =
           let val (cs, t) = CSs E cs
               val (LAMBDAexp cs, t)  = Env.gen E (LAMBDAexp cs, t)
           in  (FUNbind(f,cs), Env.VALbind(f,ID f,t)) end
    
       and VDs E [] = ([], Env.empty)
         | VDs E (vb::vbs) = 
           let val (vb, E') = VD E vb 
               val (vbs, E'') = VDs (E ++ E') vbs
           in  (vb::vbs, E' ++ E'') end
    
       and VD E (vb as VALbind(p,e)) = 
           let val (t,E') = P E p
               val (e, t')     = W (E ++ E') e
           in  TypeUtil.unify(fn _ => PP.text(AstPP.valbind vb),t,t');
               (VALbind(p,e), E')
           end
    
       and DTs E (dbs,tbs) =
           let val (dbs, E1) = DBs E dbs
               val (tbs, E2) = TDs E tbs
           in  (dbs, tbs, E1 ++ E2) end
    
       and DBs E [] = ([], Env.empty)
         | DBs E (db::dbs) = 
           let val (db, E)   = DB E db 
               val (dbs, E') = DBs E dbs
           in  (db::dbs, E++E') end
    
       and DB E (db as DATATYPEbind{id,tyvars,cbs,...}) = (db, Env.empty)
    
       and TDs E [] = ([], Env.empty)
         | TDs E (tb::tbs) = 
           let val (tb,E)   = TD E tb 
               val (tbs,E') = TDs E tbs
           in  (tb::tbs, E++E') end
    
       and TD E (TYPEbind(id,tvs,t)) = 
           let val tve = ref(map (fn tv => (tv,Env.var E)) tvs)
               val t'   = Env.lambda E (T' E tve t)
           in  (TYPEbind(id,tvs,t), Env.TYPEbind(id,t')) end
    
       and T E t = 
           let val tvs = ref []
               val t   =  T' E tvs t 
               val (_, t) = Env.gen E (INTexp 0, t) 
           in t
           end
    
       and T' E tvs (IDty id) = Env.lookupTy E id
         | T' E tvs (ty as APPty(f,tys)) = 
           let val t = Env.lookupTy E f
               val ts = map (T' E tvs) tys
           in  TypeUtil.apply(" in type "^t2s ty, t, ts) end
         | T' E tvs (FUNty(x,y))   = FUNty(T' E tvs x,T' E tvs y)
         | T' E tvs (TUPLEty ts)   = TUPLEty(map (T' E tvs) ts)
         | T' E tvs (RECORDty lts) = RECORDty(map (LT' E tvs) lts)
         | T' E tvs (t as INTVARty _) = t
         | T' E tvs (ty as TYVARty tv) =
           let fun scan [] = let val v = boundVar E tv
                             in  tvs := (tv,v) :: !tvs; v end
                 | scan((k,v)::tvs) = if k = tv then v else scan tvs
           in  scan(!tvs) end
         | T' E tvs t = (error("unknown type "^t2s t); t)
    
       and LT' E tvs (l,t) = (l,T' E tvs t)
    
       and SE E (IDsexp id) = (IDsexp id, Env.lookupStr E id)
         | SE E (DECLsexp ds) = 
           let val (ds, E) = Ds E ds
           in  (DECLsexp ds, E) end

       val E = Env.empty
       val (d, E) = D E d 
   in  (d, E) 
   end 

end
