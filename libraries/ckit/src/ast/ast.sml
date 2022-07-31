(* Copyright (c) 1998 by Lucent Technologies *)

structure Ast : AST =
struct

  type pid = Pid.uid
  type aid = Aid.uid
  type tid = Tid.uid

 (* TYPES: preliminary definitions *)
  datatype storageClass = AUTO | EXTERN | REGISTER | STATIC | DEFAULT

  datatype qualifier = CONST | VOLATILE
      
  datatype signedness = SIGNED | UNSIGNED

 (* signednessTag determines whether a type was declared signed or unsigned, or
  * the type was assumed to be one or the other. *)
  datatype signednessTag = SIGNDECLARED | SIGNASSUMED

  datatype intKind = CHAR | SHORT | INT | LONG | LONGLONG | FLOAT | DOUBLE | LONGDOUBLE

  (* BEGIN D *)
  datatype fractionality = FRACTIONAL | WHOLENUM (* FRACTIONAL dominates WHOLENUM *)

  datatype saturatedness = SATURATE | NONSATURATE (* SATURATE dominates NONSATURATE *)
  (* END D *)
 
   (* note: definition of ctype appears later, is in the mutual recursive clump. *)

  (* IDENTIFIERS: preliminary definitions *)

  (* labels *)
  type label =
    {name: Symbol.symbol,  (* the name of the label *)
     uid : Pid.uid,        (* unique identifier *)
     location : SourceMap.location}

  datatype declStatus
    = IMPLICIT (* used, but not yet declared or defined *)
    | DECLARED (* declared, but not yet defined *)
    | DEFINED  (* defined, i.e. there is a FunctionDef or
		* initializer for this identifier *)

  (* identifiers - we call these "id"s *)
  datatype idKind
    = NONFUN       (* is not of functional type *)
    | FUNCTION of  (* is of functional type *)
       {hasFunctionDef: bool}  (* was defined by a FunctionDef *)
 
  (* OPERATORS *)
  datatype binop
    = Plus | Minus | Times | Divide | Mod
    | Gt | Lt | Gte | Lte | Eq | Neq | And | Or
    | BitOr | BitAnd | BitXor | Lshift | Rshift
    | PlusAssign | MinusAssign | TimesAssign | DivAssign
    | ModAssign | XorAssign | OrAssign | AndAssign
    | LshiftAssign | RshiftAssign | BinopExt of AstExt.binopExt
      
  datatype unop
    = Uplus | Not | Negate | BitNot
    | PreInc | PostInc | PreDec | PostDec | UnopExt of AstExt.unopExt


  (* DECLARATIONS *)

  datatype declaration
    = TypeDecl of {shadow: {strct:bool} option, tid:tid}
        (* placeholder to indicate where typedefs/enums/structs should be printed *)
    | VarDecl of id * initExpression option


  (* STATEMENTS *)

  and statement = STMT of coreStatement * aid * SourceMap.location

  and coreStatement
    = Expr of expression option
    | Compound of declaration list * statement list
    | While of expression * statement
    | Do of expression * statement
    | For of expression option * expression option * expression option * statement
    | Labeled of label * statement
    | CaseLabel of LargeInt.int * statement
    | DefaultLabel of statement
    | Goto of label
    | Break
    | Continue
    | Return of expression option
    | IfThen of expression * statement
    | IfThenElse of expression * statement * statement
    | Switch of expression * statement
    | StatExt of (expression, statement, binop, unop) AstExt.statementExt
    | ErrorStmt

  (* EXPRESSIONS *)

  and expression = EXPR of coreExpression * aid * SourceMap.location

  and coreExpression
    = IntConst of LargeInt.int
    | RealConst of real
    | StringConst of string
    | Call of expression * expression list
    | QuestionColon of expression * expression * expression
    | Assign of expression * expression
    | Comma of expression * expression
    | Sub of expression * expression          
    | Member of expression * member
    | Arrow of expression * member
    | Deref of expression                     
    | AddrOf of expression                    
    | Binop of binop * expression * expression
    | Unop of unop * expression
    | Cast of ctype * expression
    | Id of id
    | EnumId of member * LargeInt.int
    | SizeOf of ctype  (* not used in compiler mode; sizeof expr becomes sizeof (typeof expr)  *)
    | ExprExt of (expression, statement, binop, unop) AstExt.expressionExt
    | ErrorExpr

  and initExpression
    = Simple of expression
    | Aggregate of initExpression list

  and ctype
    = Void
    | Ellipses
    | Qual of qualifier * ctype
    | Numeric of (* D *) saturatedness * (* D *) fractionality * signedness * intKind 
                                       * signednessTag
    | Array of (LargeInt.int * expression) option * ctype
    | Pointer of ctype
    | Function of ctype * (ctype * id option) list
    | StructRef of tid (* reference to a tid bound by a struct decl *)
    | UnionRef of tid  (* reference to a tid bound by a union decl *)
    | EnumRef of tid   (* reference to a tid bound by a enumeration decl *)
    | TypeRef of tid   (* reference to a tid bound by a typedef decl *)
    | Error

  (* INVARIANT: whenever the Error ctype is introduced, an error message will be printed.
   * Thus any downstream code processing the Error value does not need to (and should not)
   * generate additional error messages. *)

  (* MEMBERS AND IDENTIFIERS *)

  (* Members in structs and unions. Also used for named constants in
   * enumerations; the ISO Standard calls these "members". *)
  and memberKind
    = STRUCTmem
    | UNIONmem
    | ENUMmem of LargeInt.int

  withtype member =
    {name: Symbol.symbol,  (* the name of the member *)
     uid : Pid.uid,        (* unique identifier *)
     location : SourceMap.location,
     ctype: ctype,
     kind: memberKind}   (* member type *)

  and id =
    {name: Symbol.symbol,
     uid: Pid.uid,          (* unique identifier *)
     location: SourceMap.location,
     ctype: ctype,    (* associated type *)
     stClass: storageClass,
     status: declStatus,
     global: bool, (* defined at top level *)
     kind: idKind}  

  (* the common fields of id and member could be factored out, but
   * this would increase space usage and access time, and require
   * nested patterns when accessing the common fields.  E.g.:

    type id =
      {base : basicId,
       stClass: storageClass option,
       kind: idKind}
  *)

 
  (* top-level program elements *)
  datatype coreExternalDecl
    = ExternalDecl of declaration
    | FunctionDef of id * id list * statement
    | ExternalDeclExt of (expression, statement, binop, unop) AstExt.externalDeclExt

  (* marked and (potentially) annotated external declarations *)
  datatype externalDecl = DECL of coreExternalDecl * aid * SourceMap.location

  (* PROGRAMS *)
  (* abstract syntax of "programs", i.e. the result of processing a source file
   * also known as a "translation unit" *)
  type ast = externalDecl list


end (* structure Ast *)
