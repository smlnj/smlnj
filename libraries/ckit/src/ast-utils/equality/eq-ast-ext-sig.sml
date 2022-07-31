(* Copyright (c) 1998 by Lucent Technologies *)

local 
  type tables = Tables.tidtab * Tables.tidtab
  type maps = Tid.uid Tidtab.uidtab * Pidtab.uid Pidtab.uidtab

  type expExt = (Ast.expression, Ast.statement, Ast.binop, Ast.unop) AstExt.expressionExt
  type stmtExt = (Ast.expression, Ast.statement, Ast.binop, Ast.unop) AstExt.statementExt
  type extDeclExt = (Ast.expression, Ast.statement, Ast.binop, Ast.unop) AstExt.externalDeclExt
in
signature EQASTEXT = sig
  val eqExpressionExt   : tables -> maps -> (expExt * expExt) -> unit
  val eqStatementExt    : tables -> maps -> (stmtExt * stmtExt) -> unit
  val eqExternalDeclExt : tables -> maps -> (extDeclExt * extDeclExt) -> unit
end
end
