signature MDL_AST_PRETTY_PRINTER =
sig

   structure Ast : MDL_AST

   val ident   : Ast.ident -> PP.pp
   val literal : Ast.literal -> PP.pp
   val exp     : Ast.exp -> PP.pp
   val labexp  : Ast.id * Ast.exp -> PP.pp
   val sexp    : Ast.structexp -> PP.pp
   val sigexp  : Ast.sigexp -> PP.pp
   val pat     : Ast.pat -> PP.pp
   val labpat  : Ast.id * Ast.pat -> PP.pp
   val ty      : Ast.ty -> PP.pp
   val labty   : Ast.id * Ast.ty -> PP.pp
   val tyvar   : Ast.tyvar -> PP.pp
   val decl    : Ast.decl -> PP.pp
   val decls   : Ast.decl list -> PP.pp
   val valbind : Ast.valbind -> PP.pp
   val valbinds: Ast.valbind list -> PP.pp
   val funbind : Ast.funbind -> PP.pp
   val funbinds: Ast.funbind list -> PP.pp
   val clause  : Ast.clause -> PP.pp
   val clauses : Ast.clause list -> PP.pp
   val consbind : Ast.consbind -> PP.pp
   val consbinds : Ast.consbind list -> PP.pp
   val datatypebind : Ast.datatypebind -> PP.pp
   val datatypebinds : Ast.datatypebind list -> PP.pp
   val typebind : Ast.typebind -> PP.pp
   val typebinds : Ast.typebind list -> PP.pp

   val encodeName : Ast.id -> Ast.id

end
