functor RewriteGen
   (structure AstRewriter : MDL_AST_REWRITER
    structure AstPP       : MDL_AST_PRETTY_PRINTER
    structure AstTrans    : MDL_AST_TRANSLATION
    structure Parser      : MDL_PARSER_DRIVER
    structure PolyGen     : POLY_GEN
       sharing AstRewriter.Ast = AstPP.Ast = AstTrans.Ast = 
               Parser.Ast = PolyGen.Ast
   ) : REWRITE_GEN = 
struct
   structure Ast = AstRewriter.Ast 
   structure A   = Ast
   structure T   = AstTrans

   val NO = AstRewriter.noRewrite
   val RW = AstRewriter.rewrite

   exception RewriteGen
   fun bug msg = MLRiscErrorMsg.error("RewriteGen",msg)
   fun error msg = (TextIO.output(TextIO.stdErr,msg^"\n"); raise RewriteGen) 

   fun ID x = A.IDexp(A.IDENT([],x))

   (*
    * Collect datatype and function declaractions
    *)
   fun processDecls hook (decls,exps) = 
   let val datatypeBinds = ref []
       val funBinds      = ref []
       val miscDecls     = ref []

       fun enterDb(db as A.DATATYPEbind _) = 
              datatypeBinds := db :: !datatypeBinds
         | enterDb _ = ()
       fun enterFb fbs = funBinds := fbs :: !funBinds
       fun enterMisc d = miscDecls := d :: !miscDecls

       fun decl _ (d as A.DATATYPEdecl(dbs, _)) = (app enterDb dbs; d)
         | decl _ (d as A.FUNdecl fbs) = (enterFb fbs; d)
         | decl _ (d as A.VALdecl vbs) = (enterMisc d; d)
         | decl _ (d as A.OPENdecl vbs) = (enterMisc d; d)
         | decl _ d = d (* ignore the rest *)

       (* Collect info *) 
       val _ = map (#decl (RW{sexp=NO,ty=NO,decl=decl,exp=NO,pat=NO})) decls

       (* Collect rules  *)
       fun findDb name =
       let fun find((db as A.DATATYPEbind{id, ...})::dbs) =
                if name = id then db else find dbs
             | find(_::dbs) = find dbs
             | find [] = error("unknown datatype "^name)
       in  find(!datatypeBinds)
       end

       fun processRules(A.FUNbind(name, clauses)) =  (findDb(name), clauses)
       val rules     = map (map processRules) (rev(!funBinds))
       val nonTerms  = foldr (fn (fbs,ids) => 
                               foldr (fn (A.FUNbind(id,_),ids) => id::ids) 
                                  ids fbs) [] (!funBinds)
       fun isNonTerm id = List.exists(fn id' => id=id') nonTerms
       val generated = map (PolyGen.gen hook isNonTerm) rules
       val miscs     = rev(!miscDecls)
   in  A.LETexp(miscs @ generated, exps)
   end

   (*
    * Hooks for various things
    *)
   val rewriteHook = 
       PolyGen.HOOK
       { name  ="rewrite",
         factor=true,
         args  =["redex"],
         ret   ="redex",
         unit  =fn x => x,
         gen   =fn(trans,cons) => 
                  T.mapConsToExp
                    {id=fn{newName,ty,...} => trans(ty,ID newName),
                     prefix=[]
                    } cons
       }

   val appHook = 
       PolyGen.HOOK
       { name  ="app",
         factor=false,
         args  =["redex"],
         ret   ="_",
         unit  =fn _ => A.TUPLEexp [],
         gen   =fn (trans,cons) => 
                let fun f({origName,newName,ty},es) = trans(ty,ID newName)::es
                in  A.SEQexp (rev(T.foldCons f [] cons))
                end
       }

   val foldHook = 
       PolyGen.HOOK
       { name  ="fold",
         factor=false,
         args  =["redex","foldArg"],
         ret   ="foldArg",
         unit  = fn _ => ID "foldArg",
         gen   = fn (trans,cons) => 
                    T.foldCons  
                     (fn({origName,newName,ty},e) => 
                          trans(ty,A.TUPLEexp[ID newName,e]))
                      (ID "foldArg") cons
       }       

   fun compile decl =
   let fun exp _ (A.APPexp(A.IDexp(A.IDENT(["Generic"],"rewrite")),
                           A.LETexp(decls,exp))) = 
               processDecls rewriteHook (decls,exp)
         | exp _ (A.APPexp(A.IDexp(A.IDENT(["Generic"],"app")),
                           A.LETexp(decls,exp))) = 
               processDecls appHook (decls,exp)
         | exp _ (A.APPexp(A.IDexp(A.IDENT(["Generic"],"fold")),
                           A.LETexp(decls,exp))) = 
               processDecls foldHook (decls,exp)
         | exp _ e = e
   in  #decl(RW{sexp=NO,ty=NO,decl=NO,exp=exp,pat=NO}) decl
   end

   fun gen filename =
   let val decl = A.SEQdecl(Parser.load filename)
       val decl = 
        A.SEQdecl
          [A.$["(* WARNING: This file is generated using 'rwgen "^
               filename^"' *)"],
           compile decl
          ]
   in  PP.text(AstPP.decl decl) 
   end

   fun main(_, [filename]) =
         ((print(gen filename); 0) 
            handle e => (print("Uncaught exception "^exnName e^"\n"); 1))
     | main(_, _) = (print("Usage: rwgen <filename>\n"); 1)

end
