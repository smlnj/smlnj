(* Copyright (c) 1998 by Lucent Technologies *)

(* cmodel extension signature for empty extension *)

(* for documentation only -- not currently used *)

signature PARSETREEEXT =
sig
  (* DBM: may need equality operations for some or all of these types *)
  type operatorExt = unit
  type ('specifier, 'declarator, 'ctype, 'decltype, 'operator, 'expression, 'statement) expressionExt
  type ('specifier, 'declarator, 'ctype, 'decltype, 'operator, 'expression, 'statement) specifierExt
  type ('specifier, 'declarator, 'ctype, 'decltype, 'operator, 'expression, 'statement) declaratorExt
  type ('specifier, 'declarator, 'ctype, 'decltype, 'operator, 'expression, 'statement) statementExt
  type ('specifier, 'declarator, 'ctype, 'decltype, 'operator, 'expression, 'statement) declarationExt
  type ('specifier, 'declarator, 'ctype, 'decltype, 'operator, 'expression, 'statement) externalDeclExt
end

