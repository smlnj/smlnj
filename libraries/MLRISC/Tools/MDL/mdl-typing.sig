(*
 * Type checking 
 *)
signature MDL_TYPING =
sig
   structure Ast      : MDL_AST
   structure TypeUtil : MDL_TYPE_UTILS
   structure Env      : MDL_ENV
   structure Comp     : MDL_COMPILE
     sharing Env.Ast = TypeUtil.Ast = Ast 
     sharing Comp.Env = Env

   val isPolymorphic : Ast.ty -> bool
   val typeCheck : Comp.md -> Ast.decl -> Ast.decl * Env.env
end
