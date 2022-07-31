(* Copyright (c) 1998 by Lucent Technologies *)

(* parse tree extension signature for empty extension *)

structure ParseTreeExt = struct
  type operatorExt = unit
  type ('specifier, 'declarator, 'ctype, 'decltype, 'operator, 'expression, 'statement) expressionExt = unit
  type ('specifier, 'declarator, 'ctype, 'decltype, 'operator, 'expression, 'statement) specifierExt = unit
  type ('specifier, 'declarator, 'ctype, 'decltype, 'operator, 'expression, 'statement) declaratorExt = unit
  type ('specifier, 'declarator, 'ctype, 'decltype, 'operator, 'expression, 'statement) statementExt = unit
  type ('specifier, 'declarator, 'ctype, 'decltype, 'operator, 'expression, 'statement) declarationExt = unit
  type ('specifier, 'declarator, 'ctype, 'decltype, 'operator, 'expression, 'statement) externalDeclExt = unit
end
