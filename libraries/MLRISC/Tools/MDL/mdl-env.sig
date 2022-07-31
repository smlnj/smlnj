(*
 * A machine description environment.
 *)
signature MDL_ENV =
sig
   structure Ast : MDL_AST
   type env

   val empty : env              (* empty environment *)
   val ++    : env * env -> env (* combine environments *)

   (* Elaborate *)
   val var   : env -> Ast.ty
   val inst  : env -> Ast.exp * Ast.ty -> Ast.exp * Ast.ty
   val gen   : env -> Ast.exp * Ast.ty -> Ast.exp * Ast.ty
   val lambda: env -> Ast.ty -> Ast.ty
   val elab  : env -> Ast.decl -> env 

   val VALbind  : Ast.id * Ast.exp * Ast.ty -> env
   val TYPEbind : Ast.id * Ast.ty -> env
   val STRbind  : Ast.id * Ast.decl list * env -> env

   (* Lookup functions *)
   val lookupStr  : env -> Ast.ident -> env
   val lookupVal  : env -> Ast.ident -> Ast.exp * Ast.ty
   val lookupVal' : (Ast.id -> unit) -> env -> Ast.ident -> Ast.exp * Ast.ty
   val lookupTy   : env -> Ast.ident -> Ast.ty
   val datatypeDefinitions : env -> Ast.datatypebind list

   (* Iterators *)
   val foldVal : (Ast.id * Ast.exp * Ast.ty * 'a -> 'a) -> 'a -> env -> 'a 

   (* Lookup code from nested structures/signatures *)
   val declOf   : env -> Ast.id -> Ast.decl 
   val fctArgOf : env -> Ast.id -> Ast.decl
   val typeOf   : env -> Ast.id -> Ast.decl
end
