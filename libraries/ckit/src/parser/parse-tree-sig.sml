(* Copyright (c) 1998 by Lucent Technologies *)

(* C parse trees, produced by the parser *)

signature PARSETREE =
sig

  datatype qualifier = CONST | VOLATILE

  (* storage class attributes *)
  datatype storage
    = TYPEDEF
    | STATIC
    | EXTERN 
    | REGISTER
    | AUTO

  (* built in unary and binary operators *)
  datatype operator
    = Plus | Minus | Times | Divide | Mod
    | Gt | Lt | Gte | Lte | Eq | Neq | And | Or
    | BitOr | BitAnd | BitXor | Lshift | Rshift
    | Star | AddrOf | Dot | Arrow | Sub | Sizeof
    | PreInc | PostInc | PreDec | PostDec | Comma
    | Not | Negate | BitNot | Assign
    | PlusAssign | MinusAssign | TimesAssign | DivAssign
    | ModAssign | XorAssign | OrAssign | AndAssign
    | LshiftAssign | RshiftAssign 
    | Uplus 
    | SizeofType of ctype
    | OperatorExt of operatorExt

  and expression
    = EmptyExpr
    | IntConst of LargeInt.int
    | RealConst of real
    | String of string
    | Id of string
    | Unop of operator * expression
    | Binop of operator * expression * expression
    | QuestionColon of expression * expression * expression
    | Call of expression * expression list
    | Cast of ctype * expression
    | InitList of expression list
    | MARKexpression of (SourceMap.location * expression)
    | ExprExt of expressionExt

  and specifier
    = Void
    | Ellipses
    | Signed
    | Unsigned
    | Char
    | Short
    | Int
    | Long
    | Float 
    | Double
    | Fractional
    | Wholenum
    | Saturate
    | Nonsaturate
    | Array of expression * ctype
    | Pointer of ctype
    | Function of
        {retType : ctype,  
	 params : (decltype * declarator) list}
    | Enum of
        {tagOpt : string option,
	 enumerators : (string * expression) list,
	 trailingComma : bool}  (* true if there was there a trailing comma in the declaration *)
    | Struct of
        {isStruct : bool,   (* struct or union; true => struct *)
	 tagOpt : string option,  (* optional tag *)
	 members: (ctype * (declarator * expression) list) list} (* member specs *)
    | TypedefName of string
    | StructTag of
	{isStruct : bool,   (* ??? *)
	 name : string}
    | EnumTag of string 
    | SpecExt of specifierExt

  and declarator  (* constructor suffix: "Decr" *)
    = EmptyDecr
    | EllipsesDecr
    | VarDecr of string
    | ArrayDecr of declarator * expression
    | PointerDecr of declarator
    | QualDecr of qualifier * declarator
    | FuncDecr of declarator * (decltype * declarator) list
    | MARKdeclarator of (SourceMap.location * declarator)
    | DecrExt of declaratorExt

  (* supports extensions of C in which expressions contain statements *)
  and statement
    = Decl of declaration
    | Expr of expression 
    | Compound of statement list
    | While of expression * statement
    | Do of expression * statement
    | For of expression * expression * expression * statement
    | Labeled of string * statement
    | CaseLabel of expression * statement
    | DefaultLabel of statement
    | Goto of string
    | Break
    | Continue
    | Return of expression
    | IfThen of expression * statement
    | IfThenElse of expression * statement * statement
    | Switch of expression * statement
    | MARKstatement of (SourceMap.location * statement)
    | StatExt of statementExt

  and declaration
    = Declaration of decltype * (declarator * expression) list
    | MARKdeclaration of (SourceMap.location * declaration)
    | DeclarationExt of declarationExt

  (* the top-level constructs in a translation unit (i.e. source file) *)
  and externalDecl
    = ExternalDecl of declaration
    | FunctionDef of
       {retType : decltype,      (* return type *)
	funDecr : declarator,   (* function name declarator *)
        krParams : declaration list, (* K&R-style parameter declarations *)
        body : statement}        (* function body *)
    | MARKexternalDecl of (SourceMap.location * externalDecl)
    | ExternalDeclExt of externalDeclExt

  withtype ctype =
      {qualifiers : qualifier list,
       specifiers : specifier list}
  and decltype =
      {qualifiers : qualifier list,
       specifiers : specifier list,
       storage : storage list}

  (* extension types for basic constructs *)
  and externalDeclExt = 
      (specifier, declarator, ctype, decltype, operator, expression, statement)
      ParseTreeExt.externalDeclExt
  and declarationExt = 
      (specifier, declarator, ctype, decltype, operator, expression, statement)
      ParseTreeExt.declarationExt
  and statementExt = 
      (specifier, declarator, ctype, decltype, operator, expression, statement)
      ParseTreeExt.statementExt
  and declaratorExt =
      (specifier, declarator, ctype, decltype, operator, expression, statement)
      ParseTreeExt.declaratorExt
  and specifierExt = 
      (specifier, declarator, ctype, decltype, operator, expression, statement)
      ParseTreeExt.specifierExt
  and expressionExt =
      (specifier, declarator, ctype, decltype, operator, expression, statement)
      ParseTreeExt.expressionExt
  and operatorExt = ParseTreeExt.operatorExt

end (* signature PARSETREE *)

(* Note: in structure declarations, the bool is IsStruct/IsUnion, and the expression
 * after the declarator is the bit field.
 *)

(* Location Marking:
 * The expression, declarator, statement, declaration, and externalDecl
 * types have a MARK variant for annotating the corresponding constructs
 * with source file locations.
 *)

(* Syntax Extensions:
 * The operator, expression, specification, declarator, statement, declaration,
 * and externalDecl types have an ...Ext variant for supporting syntax
 * extensions.  The types of these variants, operatorExt, expressionExt, etc.
 * are defined by instantiating corresponding type operators defined in
 * the ParseTreeExt structure (see src/parser/extensions/c/parse-tree-ext*.sml
 * for the dummy definitions for ansi C).  In general, extensions for 
 * a construct may need to build on other constructs, which is why
 * the ParseTreeExt type constructors are parameterized by the collection
 * of syntax tree types.
 * 
 * A user-defined extension (call it x) would need it's own version of
 * ParseTreeExt defined in files parse-tree-ext-sig.sml and parse-tree-ext.sml
 * in a new directory src/parser/extensions/x/.
 * 
 *) 
