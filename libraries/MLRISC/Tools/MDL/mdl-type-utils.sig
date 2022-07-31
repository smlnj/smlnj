(*
 * Utilities for manipulating types
 *)
signature MDL_TYPE_UTILS =
sig

   structure Ast   : MDL_AST
   structure AstPP : MDL_AST_PRETTY_PRINTER
      sharing AstPP.Ast = Ast
   type level = int

   val init    : unit -> unit
   val newVar  : level -> Ast.ty
   val newIVar : level -> Ast.ty
   val unify   : (unit -> string) * Ast.ty * Ast.ty -> unit
   val inst    : level -> Ast.exp * Ast.ty -> Ast.exp * Ast.ty
   val gen     : level -> Ast.exp * Ast.ty -> Ast.exp * Ast.ty
   val lambda  : level -> Ast.ty -> Ast.ty
   val apply   : string * Ast.ty * Ast.ty list -> Ast.ty
   val poly    : Ast.ty list * Ast.ty -> Ast.ty  
   val newType : Ast.datatypebind -> Ast.ty list * Ast.ty
   val deref   : Ast.ty -> Ast.ty

end
