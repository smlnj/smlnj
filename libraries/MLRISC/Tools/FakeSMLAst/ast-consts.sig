signature MDL_AST_CONSTANTS =
sig

   structure Ast : MDL_AST
   
   type constTable
   val newConstTable : unit -> constTable
   val const         : constTable -> Ast.exp -> Ast.exp
   val genConsts     : constTable -> Ast.decl list
   val withConsts    : ((Ast.exp -> Ast.exp) -> Ast.decl) -> Ast.decl

end
