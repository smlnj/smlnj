(*
 * Various translation functions
 *)
signature MDL_AST_REWRITER =
sig

   structure Ast : MDL_AST

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

   val noRewrite : 'a rewriter
   val rewrite : clients -> trans
end
