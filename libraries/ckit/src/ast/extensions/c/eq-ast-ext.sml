structure EqAstExt = struct
  type expExt = (Ast.ctype,Ast.expression,Ast.statement,Ast.binop) AstExt.expressionExt 
  type stmtExt = (Ast.ctype,Ast.expression,Ast.statement,Ast.binop) AstExt.statementExt 
  fun eqExpressionExt pair maps (ee1,ee2) = true 
  fun eqStatementExt  pair maps (se1,se2) = maps
end
