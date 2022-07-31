functor PolyGen
   (structure AstPP    : MDL_AST_PRETTY_PRINTER
    structure AstTrans : MDL_AST_TRANSLATION
       sharing AstPP.Ast = AstTrans.Ast
   ) : POLY_GEN =
struct
   structure Ast = AstPP.Ast
   structure A   = Ast
   structure T   = AstTrans
   structure H   = HashTable

   fun bug msg = MLRiscErrorMsg.error("PolyGen",msg)
   exception PolyGen
   fun error msg = (MDLError.error msg; raise PolyGen)
 
   datatype hook =
      HOOK of 
      { name   : string,      (* name of function *)
        factor : bool,        (* factor rules by constructor? *)
        args   : string list, (* function arguments *)
        ret    : string,      (* return argument name *)
        unit   : Ast.exp -> Ast.exp,
        gen    : (Ast.ty * Ast.exp -> Ast.exp) * Ast.consbind -> Ast.exp
      }  

   fun ID id = A.IDexp(A.IDENT([],id))
   val argExp = ID "arg"
   val argPat = A.IDpat "arg"

   fun gen(HOOK{name, args, ret, unit, factor, gen, ...}) isNonTerm ruleSet =
   let val redex = ID(hd args) (* the redex must be the first argument *)

       (*
        * Given a type, returns the appropriate function that performs the  
        * transformation
        *)
       val nullTrans = A.LAMBDAexp[A.CLAUSE([argPat],NONE,argExp)]
       fun ty2Exp(A.IDty(A.IDENT(_,id))) = 
              if isNonTerm id then SOME(ID(name^"'"^id)) else NONE
         | ty2Exp(A.APPty(A.IDENT(_,id),args)) =
              if isNonTerm id then 
                 let val args = map ty2Exp args
                 in  if List.exists Option.isSome args then
                       SOME(A.APPexp(ID(name^"'"^id),
                         A.TUPLEexp(map (fn SOME f => f | NONE => nullTrans)
                                    args)))
                     else NONE 
                 end
              else NONE
         | ty2Exp(A.TUPLEty tys) = (* create a functional *)
              let val args = map ty2Exp tys
                  fun bind([], i, pats, exps, some) = (rev pats, rev exps, some)
                    | bind(arg::args, i, pats, exps, some) = 
                      let val v = "v_"^Int.toString i
                          val pat = A.IDpat v
                          val exp = ID v
                          val (exp, some) = 
                             case arg of NONE => (exp, some)
                                       | SOME f => (A.APPexp(f,exp), true)
                      in  bind(args, i+1, pat::pats, exp::exps, some) end
                  val (pats, exps, some) = bind(args, 0, [], [], false)
              in  if some then
                     SOME(A.LAMBDAexp[A.CLAUSE([A.TUPLEpat pats],NONE,
                                                A.TUPLEexp exps)])
                  else NONE
              end
         | ty2Exp(A.RECORDty ltys) = 
              let val args = map (fn (l,t) => (l, ty2Exp t)) ltys
                  fun bind([], i, pats, exps, some) = (rev pats, rev exps, some)
                    | bind((l,arg)::args, i, pats, exps, some) = 
                      let val pat = (l, A.IDpat l)
                          val exp = ID l
                          val (exp, some) = 
                             case arg of NONE => (exp, some)
                                       | SOME f => (A.APPexp(f,exp), true)
                      in  bind(args, i+1, pat::pats, (l,exp)::exps, some)
                      end
                  val (pats, exps, some) = bind(args, 0, [], [], false)
              in  if some then
                     SOME(A.LAMBDAexp[A.CLAUSE([A.RECORDpat(pats,false)],NONE,
                                                A.RECORDexp exps)])
                  else NONE
              end
         | ty2Exp(A.TYVARty(A.VARtv id)) = SOME(ID("param"^id))
         | ty2Exp t = error("Can't handle type "^PP.text(AstPP.ty t))

       fun genOneRule(A.DATATYPEbind{id,tyvars,cbs, ...}, rules) =  
       let val prefix = []
           val subTerm = ref false
           fun appTrans(ty,e) =
               case ty2Exp ty of 
                 NONE => unit e
               | SOME f => (subTerm := true; A.APPexp(f,e))

           (* arguments for this function *)
           (* How to generate the traversal for one constructor *)
           fun genNonFactoredTraversal(cons)  =
           let val _ = subTerm := false
               val exp = gen(appTrans,cons)
               val exp = if !subTerm then exp else unit redex
               fun mapPat{origName,newName,ty} = A.IDpat newName
           in  T.mapConsToClause{prefix=prefix,pat=fn p => p,exp=exp} cons
           end

           exception Can'tFactor

           (* How to generate the traversal for one constructor *)
           fun genFactoredTraversal(cons as A.CONSbind{id, ty, ...},rules)  =
           let val _ = subTerm := false
               val resultExp as A.CONSexp(_,caseExp) = gen(appTrans,cons)
               val caseExp = case caseExp of SOME e => e
                                           | NONE => A.TUPLEexp []
               val body =
                   case (rules, !subTerm) of  
                     ([], false) => redex
                   | ([], true) => resultExp
                   | (_, _) =>
                     A.CASEexp
                      (caseExp, 
                       rules @
                       [A.CLAUSE([argPat],NONE,
                           case ty of 
                             SOME _ => A.CONSexp(A.IDENT([],id), SOME argExp)
                           | NONE => redex
                         ) 
                       ]
                      )
               fun mapPat{origName,newName,ty} = A.IDpat newName
           in  T.mapConsToClause{prefix=prefix,pat=fn p => p, exp=body} cons
           end

           (* first factor all rules by their top level constructors *)
           fun factorRules(rules) = 
           let exception Bad  
               val tbl = H.mkTable(HashString.hashString,op=)(32,Can'tFactor)   
               val _     = app (fn A.CONSbind{id,...} => H.insert tbl (id,[]))
                               cbs
               fun factor(r,A.CONSpat(A.IDENT([],id),arg),g,e) =
                   enterRule(r, id, arg, g, e)
                 | factor(r,A.IDpat id, g, e) =
                   enterRule(r, id, NONE, g, e)
                 | factor(r,A.ASpat(_,p), g, e) = factor(r, p, g, e)
                 | factor _ = raise Can'tFactor
               and factorRule(r as A.CLAUSE([p],g,e)) = factor(r,p,g,e)
                 | factorRule _ = raise Can'tFactor
               and enterRule(r, consName, arg, g, e) =
                  let val rs = H.lookup tbl consName
                      val r  = A.CLAUSE([case arg of NONE => A.WILDpat
                                                   | SOME p => p],g,e)  
                  in  H.insert tbl (consName,r::rs)
                  end 
               val _ = app factorRule rules
    
           in  map (fn c as A.CONSbind{id,...} => (c,rev(H.lookup tbl id))) cbs
           end

           fun factoredBody rules =
                A.CASEexp(redex,map genFactoredTraversal (factorRules rules))
           fun nonfactoredBody rules =
                A.LETexp([A.VALdecl[A.VALbind(A.IDpat ret, 
                          A.CASEexp(redex,map genNonFactoredTraversal cbs))]],
                         [A.CASEexp(A.TUPLEexp(map ID args), rules)]
                        )

           val body = 
               if factor then (factoredBody rules handle Can'tFactor =>
                               nonfactoredBody rules)
               else nonfactoredBody rules
           fun curriedArg(A.VARtv id) = A.IDpat("param"^id)
             | curriedArg _ = bug "curriedArg"
           val args = [A.TUPLEpat(map A.IDpat args)] 
           val args = case tyvars of
                        []  => args
                      | vs  => A.TUPLEpat(map curriedArg vs)::args
       in  A.FUNbind(name^"'"^id,[A.CLAUSE(args,NONE,body)]) 
       end
         | genOneRule _ = bug "genOneRule"
   in  A.FUNdecl(map genOneRule ruleSet)
   end
end
