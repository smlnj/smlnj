(* Copyright (c) 1998 by Lucent Technologies *)

local 
  type 'a pp =  Tables.tidtab -> OldPrettyPrint.ppstream -> 'a -> unit

  type ('aidinfo,'a,'b) adornment_pp = ('aidinfo -> 'a) -> 'aidinfo -> 'b
in
signature PPASTADORNMENT = sig
  type aidinfo
  val ppExpressionAdornment: (aidinfo,Ast.coreExpression pp,Ast.expression pp) adornment_pp
  val ppStatementAdornment : (aidinfo,Ast.coreStatement pp,Ast.statement pp) adornment_pp
  val ppExternalDeclAdornment: (aidinfo,Ast.coreExternalDecl pp,Ast.externalDecl pp) adornment_pp
end
end
