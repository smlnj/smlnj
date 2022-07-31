(*
 * Translation from one sort to another
 *)
functor MDLAstTranslation
  (structure AstPP       : MDL_AST_PRETTY_PRINTER
   structure AstRewriter : MDL_AST_REWRITER
     sharing AstRewriter.Ast = AstPP.Ast
  ) : MDL_AST_TRANSLATION = 
struct

   structure Ast = AstPP.Ast
   structure A   = Ast
   structure R   = AstRewriter
   structure H   = HashTable

   fun error msg = MLRiscErrorMsg.error("MDLAstTranslation",msg)

   type 'a map = {origName : Ast.id,
                  newName  : Ast.id,
                  ty       : Ast.ty
                 } -> 'a

   type 'a folder = {origName : Ast.id,
                     newName  : Ast.id,
                     ty       : Ast.ty} * 'a -> 'a

   fun ID x = A.IDexp(A.IDENT([],x))

   exception NoName

   (*
    * Treat a type expression as a pattern and compute its set of 
    * variable bindings.  Duplicates are given unique suffixes.  
    *) 
   fun bindingsInTy ty = 
   let val namesTable = H.mkTable (HashString.hashString,op =)(32,NoName)
       val variables = ref 0

       fun enterName id = 
       let val _ = variables := !variables + 1
           val (_, totalCount) = H.lookup namesTable id
       in  totalCount := !totalCount + 1
       end handle _ => H.insert namesTable (id, (ref 0,ref 1))

       fun enter(A.IDty(A.IDENT(_,id))) = enterName id
         | enter(A.TYVARty(A.VARtv id)) = enterName id
         | enter(A.APPty(A.IDENT(_,id),_)) = enterName id
         | enter(A.CELLty id) = enterName id
         | enter(A.TUPLEty tys) = app enter tys
         | enter(A.RECORDty ltys) = app (fn (id, _) => enterName id) ltys
         | enter t = error("bindingsInTy: "^PP.text(AstPP.ty t))
       val stripTicks = String.map (fn #"'" => #"t" | c => c) 
       fun getName id = 
           let val (currentCount, totalCount) = H.lookup namesTable id
           in  stripTicks(
                 if !totalCount = 1 then id (* use the same name *)
                 else 
                 (currentCount := !currentCount + 1;
                  id^Int.toString(!currentCount)
                 )
               )
           end
   in  enter ty;
       (!variables, getName)
   end

   (*
    * Translate a type into a pattern expression
    *)
   fun mapTyToPat f' ty =
   let val (_,getName) = bindingsInTy ty
       fun f(id,ty) = f'{origName=id,newName=getName id,ty=ty}
       fun g(A.IDty(A.IDENT(_,id)), ty) = f(id,ty)
         | g(A.TYVARty(A.VARtv id), ty) = f(id, ty)
         | g(A.APPty(A.IDENT(_,id),_), ty) = f(id, ty)
         | g(A.CELLty id, ty) = f(id, ty)
         | g(A.TUPLEty tys, _) = A.TUPLEpat(map g' tys)
         | g(A.RECORDty ltys, _) = A.RECORDpat(map h ltys,false)
         | g(t, _) = error("tyToPat: "^PP.text(AstPP.ty t))
       and g' t = g(t,t)
       and h(lab, ty) = (lab, f(lab, ty))
   in  g' ty
   end

   fun foldTy f' x ty =
   let val (_,getName) = bindingsInTy ty
       fun f(id,ty,x) = f'({origName=id,newName=getName id,ty=ty},x)
       fun g(A.IDty(A.IDENT(_,id)),ty,x) = f(id,ty,x)
         | g(A.TYVARty(A.VARtv id), ty, x) = f(id,ty,x)
         | g(A.APPty(A.IDENT(_,id),_), ty, x) = f(id,ty, x)
         | g(A.CELLty id,ty,x) = f(id,ty,x)
         | g(A.TUPLEty tys,ty,x) = foldr g' x (rev tys)
         | g(A.RECORDty ltys,ty,x) = foldr h x (rev ltys)
         | g(t, ty, x) = error("foldTyBindings: "^PP.text(AstPP.ty t))
       and g'(t,x) = g(t,t,x)
       and h((lab, ty),x) = f(lab,ty,x)
   in  g'(ty,x)
   end

   fun foldCons f x (A.CONSbind{ty=NONE, ...}) = x
     | foldCons f x (A.CONSbind{ty=SOME ty, ...}) = foldTy f x ty
       
   (*
    * Translate a type into an expression
    *)
   fun mapTyToExp f' ty =
   let val (_,getName) = bindingsInTy ty
       fun f(id,ty) = f'{origName=id,newName=getName id,ty=ty}
       fun g(A.IDty(A.IDENT(_,id)), ty) = f(id, ty)
         | g(A.TYVARty(A.VARtv id), ty) = f(id, ty)
         | g(A.APPty(A.IDENT(_,id),_), ty) = f(id, ty)
         | g(A.CELLty id, ty) = f(id, ty)
         | g(A.TUPLEty tys, ty) = A.TUPLEexp(map g' tys)
         | g(A.RECORDty ltys, ty) = A.RECORDexp(map h ltys)
         | g(t, _) = error("tyToPat: "^PP.text(AstPP.ty t))
       and g' t = g(t,t)
       and h(lab, ty) = (lab, f(lab, ty))
   in  g' ty 
   end

   (*
    * Translate a constructor into a pattern 
    *)
   fun mapConsToPat {prefix, id} (A.CONSbind{id=x, ty, ...}) =
       A.CONSpat(A.IDENT(prefix,x), Option.map (mapTyToPat id) ty)
   (*
    * Translate a constructor into an expression  
    *)
   fun mapConsToExp {prefix,id} (A.CONSbind{id=x, ty, ...}) =
       A.CONSexp(A.IDENT(prefix,x), Option.map (mapTyToExp id) ty)

   fun mapConsArgToExp id (A.CONSbind{ty=NONE, ...}) = A.TUPLEexp []
     | mapConsArgToExp id (A.CONSbind{ty=SOME ty, ...}) = mapTyToExp id ty

   fun mapConsToClause {prefix, pat, exp} cons = 
       A.CLAUSE([pat(mapConsToPat 
                     {prefix=prefix, id=fn {newName,...} => A.IDpat newName} 
                     cons)],
                NONE,
                exp)

   fun consBindings cons =
   let fun enter({newName,origName,ty},bindings) = (newName, ty)::bindings
       val bindings = foldCons enter [] cons 
       fun lookup(id : Ast.id) =
       let fun find((b as (x,t))::bs) = if x = id then (ID x,t) else find bs 
             | find [] = raise NoName
       in  find bindings end
   in  lookup
   end
 
   (* Simplification *)
   local
      val NIL = R.noRewrite

      fun hasBindings ps = 
      let val bindings = ref false
          fun pat _ (p as A.IDpat x) = (bindings := true; p) 
            | pat _ p = p
      in  app (fn p => 
            (#pat(R.rewrite{pat=pat,decl=NIL,sexp=NIL,exp=NIL,ty=NIL}) p; 
             ())) ps;
          !bindings
      end

      fun allTheSame [] = true
        | allTheSame (x::xs) = List.all (fn x' => x = x') xs

      exception Don'tApply
 
      fun reduceExp ==> (exp as A.CASEexp(e,[])) = exp
        | reduceExp ==> (A.SEQexp es) =
             (A.SEQexp(foldr (fn (A.TUPLEexp [],es) => es
                           | (A.SEQexp [],es) => es
                           | (e,es) => e::es
                         ) [] es))
        | reduceExp ==> 
              (exp as A.CASEexp(e,allCs as (c as A.CLAUSE(p1,NONE,e1))::cs)) = 
          let fun collect(A.CLAUSE([p],NONE,e),Ps) = 
                  let fun ins [] = [([p],e)]
                        | ins((ps,e')::Ps) = 
                          if e = e' then (p::ps,e)::Ps
                          else (ps,e')::ins Ps
                  in  ins Ps end
              val Ps = foldr collect [] (c::cs)
              fun orPat [p] = p
                | orPat ps =
                  if List.all (fn A.WILDpat => true | _ => false) ps then
                     A.WILDpat
                  else A.ORpat ps  
              fun tuplepat [p] = p
                | tuplepat ps  = A.TUPLEpat ps
              fun join([p],e) = A.CLAUSE([p],NONE,e)
                | join(ps,e)  = 
                  let val xs = map (fn A.TUPLEpat(p::ps) => (p,ps)
                                          | _ => raise Don'tApply) ps
                      val firstPats = map #1 xs
                      val restPats  = map #2 xs
                  in  if allTheSame (map tuplepat restPats) then 
                         A.CLAUSE([tuplepat(orPat firstPats::hd restPats)],
                                  NONE,e)
                      else raise Don'tApply
                  end handle Dont'Apply => A.CLAUSE([orPat ps],NONE,e)
              val cs = map join (rev Ps)
          in  case cs of
                [A.CLAUSE([A.TUPLEpat []],NONE,body)] => body
              | [A.CLAUSE([_],NONE,body as A.LISTexp([],NONE))] => body
              | [A.CLAUSE([A.TUPLEpat(ps)],NONE,body)] => 
                if hasBindings ps then 
                let fun elimOr(pat as A.ORpat p) =
                         if hasBindings p then pat else A.WILDpat
                      | elimOr pat = pat
                in  A.CASEexp(e,
                      [A.CLAUSE([A.TUPLEpat(map elimOr ps)],NONE,body)])
                end 
                else body
              | [A.CLAUSE(ps,NONE,body)] => 
                 if hasBindings ps then A.CASEexp(e,cs) else body
              | _ => A.CASEexp(e,cs) 
          end
        | reduceExp ==> (exp as A.IFexp(a,b,c)) = if b = c then b else exp
        | reduceExp ==> e = e

      val simplifier = 
          R.rewrite{pat=NIL,decl=NIL,exp=reduceExp,sexp=NIL,ty=NIL}
   in
      val simplifyExp = #exp simplifier 
      val simplifyDecl = #decl simplifier 
      val simplifyPat = #pat simplifier 
      val simplifySexp = #sexp simplifier 
      val simplifyTy = #ty simplifier 

      fun stripMarks d =
      let fun decl ==> (A.MARKdecl(_,d)) = d
            | decl ==> d = d
      in  #decl (R.rewrite{pat=NIL,decl=decl,sexp=NIL,exp=NIL,ty=NIL}) d end
   end

end
