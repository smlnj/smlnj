(* Copyright (c) 1998 by Lucent Technologies *)

local 
  type aidctx = Tables.aidtab
  type 'a copier = aidctx -> 'a -> 'a

  type 'a extCopier = (Ast.expression copier * Ast.statement copier * Ast.externalDecl copier) -> 'a copier

  type expExt =
      (Ast.expression,Ast.statement,Ast.binop,Ast.unop) AstExt.expressionExt 
  type stmtExt =
      (Ast.expression,Ast.statement,Ast.binop,Ast.unop) AstExt.statementExt 
  type extDeclExt =
      (Ast.expression,Ast.statement,Ast.binop,Ast.unop) AstExt.externalDeclExt
in

signature COPYASTEXT = sig
  val copyExprExt    : expExt extCopier
  val copyStmtExt     : stmtExt extCopier
  val copyExtDeclExt  : extDeclExt extCopier
end

signature COPYAST = sig
  val copyAid         : Aid.uid copier
  val copyAst         : Ast.ast copier
  val copyExtDecl     : Ast.externalDecl copier
  val copyCoreExtDecl : Ast.coreExternalDecl copier
  val copyDecl        : Ast.declaration copier
  val copyStmt        : Ast.statement copier
  val copyCoreStmt    : Ast.coreStatement copier
  val copyExpr        : Ast.expression copier
  val copyCoreExpr    : Ast.coreExpression copier
  val copyInitExpr    : Ast.initExpression copier
end

end
