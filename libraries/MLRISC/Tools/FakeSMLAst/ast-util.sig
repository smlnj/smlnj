signature MDL_AST_UTIL =
sig

   structure Ast : MDL_AST

   (* Abbreviations *)
   val ID      : string -> Ast.exp
   val APP     : string * Ast.exp -> Ast.exp
   val PLUS    : Ast.exp * Ast.exp -> Ast.exp
   val MINUS   : Ast.exp * Ast.exp -> Ast.exp
   val ANDB    : Ast.exp * Ast.exp -> Ast.exp
   val ORB     : Ast.exp * Ast.exp -> Ast.exp
   val SLL     : Ast.exp * Ast.exp -> Ast.exp
   val SLR     : Ast.exp * Ast.exp -> Ast.exp
   val SAR     : Ast.exp * Ast.exp -> Ast.exp
   val FALSE   : Ast.exp
   val TRUE    : Ast.exp
   val ANDALSO : Ast.exp * Ast.exp -> Ast.exp
   val ORELSE  : Ast.exp * Ast.exp -> Ast.exp
   val UNIT    : Ast.exp
   val NILexp  : Ast.exp

   val INTexp    : int -> Ast.exp
   val INT32exp  : Int32.int -> Ast.exp
   val INTINFexp : IntInf.int -> Ast.exp
   val STRINGexp : string -> Ast.exp
   val BOOLexp   : bool -> Ast.exp
   val WORDexp   : word -> Ast.exp
   val WORD32exp : Word32.word -> Ast.exp
   val CHARexp   : char -> Ast.exp

   val INTpat    : int -> Ast.pat
   val INT32pat  : Int32.int -> Ast.pat
   val INTINFpat : IntInf.int -> Ast.pat
   val STRINGpat : string -> Ast.pat
   val BOOLpat   : bool -> Ast.pat
   val WORDpat   : word -> Ast.pat
   val WORD32pat : Word32.word -> Ast.pat
   val CHARpat   : char -> Ast.pat

   val UNITty     : Ast.ty
   val BOOLty     : Ast.ty
   val INTty      : Ast.ty
   val REGISTERty : Ast.ty
   val REGISTERLISTty : Ast.ty
   val INTLISTty  : Ast.ty
   val STRINGty   : Ast.ty
   val WORD32ty   : Ast.ty
   val WORDty     : Ast.ty
   val LABELty    : Ast.ty
   val LABEXPty   : Ast.ty
   val CONSTty    : Ast.ty
   val CELLKINDty : Ast.ty
   val CELLSETty  : Ast.ty

   val DATATYPE : Ast.id * Ast.tyvar list * Ast.consbind list 
        -> Ast.datatypebind
   val CONS : Ast.id * Ast.ty option -> Ast.consbind
   val VAL : Ast.id * Ast.exp -> Ast.decl 
   val FUN : Ast.id * Ast.pat * Ast.exp -> Ast.decl
   val FUN': Ast.id * Ast.pat * Ast.exp -> Ast.funbind
   val LET : Ast.decl list * Ast.exp -> Ast.exp

   val ERROR : string -> Ast.clause
   val ERRORfun : string -> Ast.decl
   val DUMMYfun : string -> Ast.decl

   val cons : Ast.exp * Ast.exp -> Ast.exp
   val append : Ast.exp * Ast.exp -> Ast.exp

   (* Generate an expression that computes a bitslice *)
   val BITSLICE : Ast.exp * Ast.range list -> Ast.exp

   val compareLiteral : Ast.literal * Ast.literal -> order

end
