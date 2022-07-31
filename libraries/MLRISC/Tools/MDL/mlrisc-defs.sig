signature MLRISC_DEFS =
sig

   structure Ast : MDL_AST

   val isPredefinedCellKind : Ast.id -> bool 
   val isPseudoCellKind     : Ast.id -> bool

end
