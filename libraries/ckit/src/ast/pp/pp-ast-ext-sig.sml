(* Copyright (c) 1998 by Lucent Technologies *)

local 
  type 'a pp =  Tables.tidtab -> OldPrettyPrint.ppstream -> 'a -> unit
  type ('a, 'aidinfo) ppExt =
       (('aidinfo -> Ast.expression pp) * ('aidinfo -> Ast.statement pp) *
	('aidinfo -> Ast.binop pp) * ('aidinfo -> Ast.unop pp))
       -> 'aidinfo
       -> Tables.tidtab -> OldPrettyPrint.ppstream -> 'a -> unit
in

signature PPASTEXT = sig
  type aidinfo
  val ppUnopExt : aidinfo -> AstExt.unopExt pp
  val ppBinopExt : aidinfo -> AstExt.binopExt pp
  val ppExpressionExt :
      ((Ast.expression, Ast.statement, Ast.binop, Ast.unop) AstExt.expressionExt,
       aidinfo) ppExt
  val ppStatementExt  :
      ((Ast.expression, Ast.statement, Ast.binop, Ast.unop) AstExt.statementExt,
       aidinfo) ppExt
  val ppExternalDeclExt :
      ((Ast.expression, Ast.statement, Ast.binop, Ast.unop) AstExt.externalDeclExt,
       aidinfo) ppExt
end

end
