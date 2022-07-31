(* Copyright (c) 1998 by Lucent Technologies *)

signature PP_AST =
sig

  type aidinfo
  type 'a pp = OldPrettyPrint.ppstream -> 'a -> unit

  val printConst      : bool ref
  val ppId            : Ast.id pp
  val ppTid           : Tables.tidtab -> Tid.uid pp
  val ppStorageClass  : Ast.storageClass pp
  val ppDecl          : aidinfo -> Tables.tidtab -> (Ast.id * Ast.ctype) pp
  val ppCtype         : aidinfo -> Tables.tidtab -> Ast.ctype pp
  val ppQualifier     : Ast.qualifier pp
  val ppSignedness    : Ast.signedness pp
  val ppFractionality : Ast.fractionality pp
  val ppSaturatedness : Ast.saturatedness pp
  val ppIntKind       : Ast.intKind pp
  val ppNamedCtype    : aidinfo -> Tables.tidtab -> Bindings.namedCtype pp

  val ppBinop            : aidinfo -> Tables.tidtab -> Ast.binop pp
  val ppUnop             : aidinfo -> Tables.tidtab -> Ast.unop pp
  val ppDeclaration      : aidinfo -> Tables.tidtab -> Ast.declaration pp
  val ppStatement        : aidinfo -> Tables.tidtab -> Ast.statement pp
  val ppCoreStatement    : aidinfo -> Tables.tidtab -> Ast.coreStatement pp
  val ppExpression       : aidinfo -> Tables.tidtab -> Ast.expression pp
  val ppCoreExpression   : aidinfo -> Tables.tidtab -> Ast.coreExpression pp
  val ppInitExpression   : aidinfo -> Tables.tidtab -> Ast.initExpression pp
  val ppCoreExternalDecl : aidinfo -> Tables.tidtab -> Ast.coreExternalDecl pp
  val ppExternalDecl     : aidinfo -> Tables.tidtab -> Ast.externalDecl pp
  val ppAst              : aidinfo -> Tables.tidtab -> Ast.ast pp

end

