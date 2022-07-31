(* Copyright (c) 1998 by Lucent Technologies *)

structure ParseTree : PARSETREE =
struct

  datatype qualifier = CONST | VOLATILE

  datatype storage
    = TYPEDEF
    | STATIC 
    | EXTERN 
    | REGISTER 
    | AUTO

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

  and externalDecl
    = ExternalDecl of declaration
    | FunctionDef of (* record? *)
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

end (* structure ParseTree *)
