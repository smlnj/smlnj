(* Copyright (c) 1998 by Lucent Technologies 
 * pretty-printer which simply ignores any aidinfo.
 *)

local 
  structure PPAstAdornment : PPASTADORNMENT =
  struct
    type aidinfo = unit

    fun ppExpressionAdornment ppCoreExpr aidinfo tidtab pps (Ast.EXPR (coreExpr,_,_)) = 
	ppCoreExpr aidinfo tidtab pps coreExpr

    fun ppStatementAdornment ppCoreStmt aidinfo tidtab pps  (Ast.STMT (coreStmt,_,_)) = 
	ppCoreStmt aidinfo tidtab pps coreStmt

    fun ppExternalDeclAdornment ppCoreExternalDecl aidinfo tidtab pps
	  (Ast.DECL (coreExtDecl,_,_)) = 
	ppCoreExternalDecl aidinfo tidtab pps coreExtDecl
  end

in
  structure PPAst = PPAstFn(structure PPAstAdornment=PPAstAdornment)
end
