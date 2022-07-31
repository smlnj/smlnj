(* Copyright (c) 1998 by Lucent Technologies *)

local 
  type tidtab = Tables.tidtab
  type aidtab = Tables.aidtab
  type esctab = unit Pidtab.uidtab

  type extDeclExt = (Ast.expression, Ast.statement, Ast.binop, Ast.unop)
                   AstExt.externalDeclExt

  type expExt = (Ast.expression, Ast.statement, Ast.binop, Ast.unop)
                AstExt.expressionExt 

  type stmtExt = (Ast.expression, Ast.statement, Ast.binop, Ast.unop) 
                 AstExt.statementExt 

  type expSimplifier =
       Ast.expression
       -> {decs:Ast.declaration list,pre:Ast.statement list,exp:Ast.expression}

  type stmtSimplifier =
       Ast.statement -> {decs:Ast.declaration list,stmts:Ast.statement list}
in

signature SIMPLIFYASTEXT =
sig

  val simplifyExtDeclExt  :
      (tidtab * aidtab * aidtab) 
      -> (expSimplifier * stmtSimplifier)
      -> extDeclExt
      -> Ast.coreExternalDecl

  val simplifyExpExt  :
      (tidtab * aidtab * aidtab) 
      -> (expSimplifier * stmtSimplifier)
      -> expExt
      -> {decs:Ast.declaration list, pre:Ast.statement list, coreExp:Ast.coreExpression}

  val simplifyStmtExt :
      (tidtab * aidtab * aidtab) 
      -> (expSimplifier * stmtSimplifier)
      -> stmtExt
      -> {decs:Ast.declaration list, coreStmt:Ast.coreStatement}

end (* signature SIMPLIFYASTEXT *)


signature SIMPLIFYAST =
sig

  val simplifyAst : Ast.ast * tidtab * aidtab * aidtab (* opaidtab *)
                    -> {ast: Ast.ast, escapetab: esctab}

end (* signature SIMPLIFYAST *)

end (* local *)
